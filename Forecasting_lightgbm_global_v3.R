install.packages("openxlsx")
library(openxlsx)

data <- read.xlsx(file.choose())
df<-data
# INSTALL PACKAGES IF MISSING
# install.packages(c("data.table", "tidyverse", "lubridate", "lightgbm"))
install.packages("lightgbm")
library(data.table)
library(tidyverse)
library(lubridate)
library(lightgbm)

# ==============================================================================
# 1. DATA PREPARATION
# ==============================================================================

# Assuming your dataset is named 'df' in your environment.
# Convert to data.table
dt <- setDT(df)

# --- A. Filter Region & Clean Data ---
target_country <- "United Kingdom"
print(paste("Filtering for:", target_country))

dt_clean <- dt[Country == target_country]

# Remove cancellations and free items
dt_clean <- dt_clean[Quantity > 0 & UnitPrice > 0]

# --- FIX: Convert Excel Serial Date ---
dt_clean[, InvoiceDate := as.Date(as.numeric(InvoiceDate), origin = "1899-12-30")]
dt_clean[, Date := InvoiceDate]

# --- B. Aggregate to Monthly Level ---
dt_monthly <- dt_clean[, .(
  Quantity = sum(Quantity),
  AvgPrice = mean(UnitPrice) 
), by = .(StockCode, YearMonth = floor_date(Date, "month"))]

# ==============================================================================
# FIX: EXTRAPOLATE DECEMBER 2011
# ==============================================================================
max_date <- max(dt_clean$Date)
dec_scaling_factor <- 31 / day(max_date)

# Apply scaling ONLY to December 2011
dt_monthly[YearMonth == as.Date("2011-12-01"), Quantity := Quantity * dec_scaling_factor]
print(paste("Dec 2011 scaled by factor:", round(dec_scaling_factor, 2)))

# Fill missing months with 0
all_months <- seq(min(dt_monthly$YearMonth), max(dt_monthly$YearMonth), by="month")
all_codes <- unique(dt_monthly$StockCode)
grid <- as.data.table(expand.grid(StockCode = all_codes, YearMonth = all_months))

dt_full <- merge(grid, dt_monthly, by=c("StockCode", "YearMonth"), all.x=TRUE)
dt_full[is.na(Quantity), Quantity := 0]

# Forward fill price
setorder(dt_full, StockCode, YearMonth)
dt_full[, AvgPrice := nafill(AvgPrice, type="locf"), by=StockCode]
dt_full[is.na(AvgPrice), AvgPrice := 0] 

# ==============================================================================
# 2. FEATURE ENGINEERING (With MEAN Imputation)
# ==============================================================================
print("Generating Features...")

setorder(dt_full, StockCode, YearMonth)

# --- A. Create Lags ---
dt_full[, `:=`(
  lag_1 = shift(Quantity, 1, type="lag"),
  lag_2 = shift(Quantity, 2, type="lag"),
  lag_3 = shift(Quantity, 3, type="lag"),
  lag_price_1 = shift(AvgPrice, 1, type="lag") 
), by = StockCode]

# --- B. Rolling Stats ---
dt_full[, roll_mean_3 := frollmean(Quantity, 3), by=StockCode]

# --- C. Date Features ---
dt_full[, Month := month(YearMonth)]

# --- FIX: FILL MISSING LAGS WITH ITEM MEAN (Instead of 0) ---
# Calculate global mean for every item
item_means <- dt_full[, .(GlobalMean = mean(Quantity, na.rm=TRUE)), by=StockCode]
dt_full <- merge(dt_full, item_means, by="StockCode", all.x=TRUE)

# Fill NAs in lags with the GlobalMean
# This helps the model know "roughly" what the item sells if history is missing (Jan 2011)
cols_to_fix <- c("lag_1", "lag_2", "lag_3", "roll_mean_3")

for (j in cols_to_fix) {
  # For each column, if NA, replace with value from GlobalMean column
  dt_full[is.na(get(j)), (j) := GlobalMean]
}

# Fill missing Price lag with current Price
dt_full[is.na(lag_price_1), lag_price_1 := AvgPrice]

# Create Training Data (Now we don't need na.omit because we filled them!)
train_data <- copy(dt_full)
train_data[, StockCode_Num := as.numeric(as.factor(StockCode))]

# ==============================================================================
# 3. MODEL TRAINING
# ==============================================================================
print("Training Global LightGBM Model...")

feature_cols <- c("lag_1", "lag_2", "lag_3", "lag_price_1", "roll_mean_3", "Month", "StockCode_Num")
target_col <- "Quantity"

dtrain <- lgb.Dataset(
  data = as.matrix(train_data[, ..feature_cols]), 
  label = train_data[[target_col]],
  categorical_feature = "StockCode_Num"
)

params <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.05,
  num_leaves = 31,
  feature_fraction = 0.8,
  min_data_in_leaf = 20
)

model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 600,
  verbose = -1
)

# ==============================================================================
# 4. ROBUST FORECASTING LOOP
# ==============================================================================
print("Generating 6-Month Forecast...")

future_horizon <- seq(as.Date("2012-01-01"), as.Date("2012-06-01"), by="month")
history_data <- copy(dt_full) 
code_map <- unique(train_data[, .(StockCode, StockCode_Num, GlobalMean)]) # Keep GlobalMean for filling
forecast_results <- list()

for (future_date in future_horizon) {
  
  print(paste("Forecasting for:", future_date))
  
  # 1. Prepare New Row
  next_step <- data.table(
    StockCode = code_map$StockCode, 
    StockCode_Num = code_map$StockCode_Num,
    GlobalMean = code_map$GlobalMean, # Carry this forward
    YearMonth = as.Date(future_date)
  )
  
  # 2. Get Price (Last known)
  last_prices <- history_data[order(-YearMonth), .(LastPrice = first(AvgPrice)), by=StockCode]
  next_step <- merge(next_step, last_prices, by="StockCode", all.x=TRUE)
  setnames(next_step, "LastPrice", "AvgPrice")
  next_step[is.na(AvgPrice), AvgPrice := mean(dt_monthly$AvgPrice, na.rm=TRUE)]
  
  next_step[, Quantity := NA_real_]
  
  # 3. Bind & Calculate Lags
  relevant_history <- history_data[YearMonth >= (as.Date(future_date) - months(4))]
  temp_bind <- rbind(relevant_history, next_step, fill=TRUE)
  setorder(temp_bind, StockCode, YearMonth)
  
  temp_bind[, `:=`(
    lag_1 = shift(Quantity, 1, type="lag"),
    lag_2 = shift(Quantity, 2, type="lag"),
    lag_3 = shift(Quantity, 3, type="lag"),
    lag_price_1 = shift(AvgPrice, 1, type="lag"), 
    roll_mean_3 = frollmean(Quantity, 3)
  ), by = StockCode]
  
  temp_bind[, Month := month(YearMonth)]
  
  # 4. Predict
  pred_set <- temp_bind[YearMonth == future_date]
  
  # If lags are somehow still NA (new items), fill with GlobalMean again
  for (col in cols_to_fix) {
     if(col %in% names(pred_set)) {
       pred_set[is.na(get(col)), (col) := GlobalMean]
     }
  }

  preds <- predict(model, as.matrix(pred_set[, ..feature_cols]))
  preds <- pmax(preds, 0)
  
  # 5. Save & Update
  pred_set[, Predicted_Quantity := preds]
  pred_set[, Quantity := preds]
  
  forecast_results[[as.character(future_date)]] <- pred_set[, .(StockCode, YearMonth, Predicted_Quantity)]
  
  update_row <- pred_set[, .(StockCode, YearMonth, Quantity, AvgPrice, GlobalMean)]
  history_data <- rbind(history_data, update_row, fill=TRUE)
}

# ==============================================================================
# 5. FINAL OUTPUT & VISUALIZATION (DASHBOARD)
# ==============================================================================
final_forecast <- rbindlist(forecast_results)

# Save CSV
fwrite(final_forecast, "forecast_2012_UK.csv")
print("CSV Saved.")

# --- PDF GENERATION ---
library(ggplot2)
library(scales)

print("Generating PDF report...")

plot_history <- dt_monthly[, .(StockCode, YearMonth, Quantity, Type = "Actual")]
plot_forecast <- final_forecast[, .(StockCode, YearMonth, Quantity = Predicted_Quantity, Type = "Forecast")]
plot_data <- rbind(plot_history, plot_forecast)

pdf("forecast_all_stockcodes.pdf", width = 14, height = 8.5)

all_codes <- unique(plot_data$StockCode)
plots_per_page <- 12
code_chunks <- split(all_codes, ceiling(seq_along(all_codes) / plots_per_page))

for(chunk in code_chunks) {
  page_data <- plot_data[StockCode %in% chunk]
  
  p <- ggplot(page_data, aes(x = YearMonth, y = Quantity, color = Type)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.5) +
    geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype="dashed", alpha=0.5) +
    facet_wrap(~StockCode, scales = "free_y", ncol = 4, nrow = 3) +
    theme_light() +
    theme(legend.position = "bottom") +
    labs(title = "StockCode Forecasts", y = "Quantity", x = "")
  
  print(p)
}
dev.off()
print("PDF Saved.")



# ==============================================================================
# 6. SIMPLE VISUALIZATION (Total & Top 6) 
# ==============================================================================
library(ggplot2)
library(scales) # For nice axis formatting

print("Generating Summary Graphs...")

# --- Step A: Combine History and Forecast for Plotting ---
# 1. Prepare History Data (2011)
plot_history <- dt_monthly[, .(StockCode, YearMonth, Quantity, Type = "Actual")]

# 2. Prepare Forecast Data (2012)
plot_forecast <- final_forecast[, .(StockCode, YearMonth, Quantity = Predicted_Quantity, Type = "Forecast")]

# 3. Combine them
plot_data <- rbind(plot_history, plot_forecast)


# --- GRAPH 1: Total Monthly Sales (Aggregated) ---
# This checks if the "Overall Trend" looks correct (e.g. seasonal drop in Jan)
total_sales_plot <- plot_data[, .(TotalQuantity = sum(Quantity)), by = .(YearMonth, Type)]

p1 <- ggplot(total_sales_plot, aes(x = YearMonth, y = TotalQuantity, color = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype="dashed", color="gray50") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme_minimal() +
  labs(
    title = "Total Monthly Sales Forecast: United Kingdom",
    subtitle = "Aggregated across all StockCodes",
    y = "Total Quantity Sold",
    x = ""
  )

print(p1)


# --- GRAPH 2: Top 6 Best-Selling Products ---
# This checks if your "Key Items" are behaving sanely
# 1. Identify top 6 items based on historical volume
top_items <- dt_monthly[, .(TotalVol = sum(Quantity)), by = StockCode][order(-TotalVol)][1:6, StockCode]

# 2. Filter data for these items
subset_plot_data <- plot_data[StockCode %in% top_items]

p2 <- ggplot(subset_plot_data, aes(x = YearMonth, y = Quantity, color = Type)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype="dashed", alpha=0.5) +
  facet_wrap(~StockCode, scales = "free_y") + 
  theme_light() +
  labs(
    title = "Forecast for Top 6 StockCodes",
    subtitle = "Individual product trends (History vs Forecast)",
    y = "Quantity",
    x = ""
  )

print(p2)



