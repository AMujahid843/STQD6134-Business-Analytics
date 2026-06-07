# ==============================================================================
# PART 1: SETUP & PACKAGES
# ==============================================================================
required_packages <- c("openxlsx", "data.table", "tidyverse", "lubridate", 
                       "lightgbm", "shiny", "scales", "gridExtra", "DT")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(openxlsx)
library(data.table)
library(tidyverse)
library(lubridate)
library(lightgbm)
library(shiny)
library(scales)
library(DT)

# ==============================================================================
# PART 2: DATA LOADING & PREPARATION (GLOBAL)
# ==============================================================================

# 1. Load Data
print("Select your Excel file...")
 df <- read.xlsx(file.choose())
dt <- setDT(df)

# List of StockCodes to remove
codes_to_remove <- c("AMAZONFEE", "B", "BANK CHARGES", "C2", "CRUK", "D", "DOT",
                     "gift_0001_10", "gift_0001_20", "gift_0001_30", "gift_0001_40",
                     "gift_0001_50", "M", "POST", "S")

# Filter: Positive Quantity/Price AND Exclude specific codes known is not correct
dt_clean <- dt[Quantity > 0 & UnitPrice > 0 & !(StockCode %in% codes_to_remove)]

# Fix Date
dt_clean[, InvoiceDate := as.Date(as.numeric(InvoiceDate), origin = "1899-12-30")]
dt_clean[, Date := InvoiceDate]

# 3. Aggregate to Monthly Level (Group by StockCode AND Country)
dt_monthly <- dt_clean[, .(
  Quantity = sum(Quantity),
  AvgPrice = mean(UnitPrice)
), by = .(StockCode, Country, YearMonth = floor_date(Date, "month"))]

# ==============================================================================
# PART 3: FIX DECEMBER & EXPAND GRID (SCALE DATA)
# ==============================================================================
print("Scaling December 2011...")

# 1. Extrapolate December
max_date <- max(dt_clean$Date)
dec_scaling_factor <- 31 / day(max_date)
dt_monthly[YearMonth == as.Date("2011-12-01"), Quantity := Quantity * dec_scaling_factor]

# 2. Create Grid (StockCode + Country combinations over time)
all_months <- seq(min(dt_monthly$YearMonth), max(dt_monthly$YearMonth), by="month")
time_grid <- data.table(YearMonth = all_months)

unique_pairs <- unique(dt_monthly[, .(StockCode, Country)])

# --- FIX: USE DUMMY COLUMN FOR CROSS JOIN ---
# We assign a temporary key 'k' to both tables to force them to merge
unique_pairs[, k := 1]
time_grid[, k := 1]

# Now they have a common column 'k', so they can merge
grid <- merge(unique_pairs, time_grid, by="k", allow.cartesian=TRUE)

# Remove the dummy column
grid[, k := NULL]
unique_pairs[, k := NULL] # Clean up source as well

# 3. Merge Actuals
dt_full <- merge(grid, dt_monthly, by=c("StockCode", "Country", "YearMonth"), all.x=TRUE)
dt_full[is.na(Quantity), Quantity := 0]

# 4. Fill Price (LOCF per Country)
setorder(dt_full, Country, StockCode, YearMonth)
dt_full[, AvgPrice := nafill(AvgPrice, type="locf"), by=.(StockCode, Country)]

# If start is NA (item new to that month), fill with global item average
item_global_prices <- dt_monthly[, .(GlobalPrice = mean(AvgPrice, na.rm=TRUE)), by=StockCode]
dt_full <- merge(dt_full, item_global_prices, by="StockCode", all.x=TRUE)

dt_full[is.na(AvgPrice), AvgPrice := GlobalPrice]
dt_full[is.na(AvgPrice), AvgPrice := 0] # Final fallback

print("Grid expansion complete.")
# ==============================================================================
# PART 4: FEATURE ENGINEERING (GLOBAL)
# ==============================================================================
print("Engineering Features...")
setorder(dt_full, Country, StockCode, YearMonth)

# 1. Lags & Rolling (Grouped by Country!)
dt_full[, `:=`(
  lag_1 = shift(Quantity, 1, type="lag"),
  lag_2 = shift(Quantity, 2, type="lag"),
  lag_3 = shift(Quantity, 3, type="lag"),
  lag_price_1 = shift(AvgPrice, 1, type="lag"),
  roll_mean_3 = frollmean(Quantity, 3)
), by = .(StockCode, Country)]

dt_full[, Month := month(YearMonth)]

# 2. Mean Imputation for missing lags
# Calculate Item-Country specific mean
item_country_means <- dt_full[, .(LocalMean = mean(Quantity, na.rm=TRUE)), by=.(StockCode, Country)]
dt_full <- merge(dt_full, item_country_means, by=c("StockCode", "Country"), all.x=TRUE)

cols_to_fix <- c("lag_1", "lag_2", "lag_3", "roll_mean_3")
for (j in cols_to_fix) {
  dt_full[is.na(get(j)), (j) := LocalMean]
}
dt_full[is.na(lag_price_1), lag_price_1 := AvgPrice]

# 3. Encode Categoricals
dt_full[, StockCode_Num := as.numeric(as.factor(StockCode))]
dt_full[, Country_Num := as.numeric(as.factor(Country))]

# ==============================================================================
# PART 5: TRAIN MODELS (MEAN + CONFIDENCE INTERVALS)
# ==============================================================================
print("Training Quantile Models (Lower, Mean, Upper)...")

train_data <- copy(dt_full)
feature_cols <- c("lag_1", "lag_2", "lag_3", "lag_price_1", "roll_mean_3", 
                  "Month", "StockCode_Num", "Country_Num")
target_col <- "Quantity"

# Create Dataset
dtrain <- lgb.Dataset(
  data = as.matrix(train_data[, ..feature_cols]), 
  label = train_data[[target_col]],
  categorical_feature = c("StockCode_Num", "Country_Num")
)

# Function to train a specific quantile
train_quantile <- function(alpha) {
  params <- list(
    objective = "quantile",
    alpha = alpha, # 0.5 = Median, 0.1 = Lower, 0.9 = Upper
    metric = "quantile",
    learning_rate = 0.05,
    num_leaves = 40,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 5
  )
  
  lgb.train(params = params, data = dtrain, nrounds = 600, verbose = -1)
}

# Train the 3 models
model_lower  <- train_quantile(0.10) # 10% Lower Bound
model_median <- train_quantile(0.50) # 50% Main Forecast
model_upper  <- train_quantile(0.90) # 90% Upper Bound

print("Models Trained.")

# --- RESIDUAL CALCULATION (For Shiny Analysis) ---
print("Calculating Historical Residuals...")
# Predict on the training data to see how wrong we were in the past
train_preds <- predict(model_median, as.matrix(train_data[, ..feature_cols]))
train_data[, Fitted := train_preds]
train_data[, Residual := Quantity - Fitted]
train_data[, Type := "Actual"]

# ==============================================================================
# PART 6: FORECAST LOOP (WITH BOUNDS)
# ==============================================================================
print("Forecasting for ALL Regions...")

future_horizon <- seq(as.Date("2012-01-01"), as.Date("2012-06-01"), by="month")
history_data <- copy(dt_full)
forecast_results <- list()

# Get Map for Loop
code_map <- unique(train_data[, .(StockCode, StockCode_Num, Country, Country_Num, LocalMean)])

for (future_date in future_horizon) {
  print(paste("Predicting:", future_date))
  
  # 1. Prepare Next Step
  next_step <- copy(code_map)
  next_step[, YearMonth := as.Date(future_date)]
  
  # 2. Get Last Price
  last_prices <- history_data[order(-YearMonth), .(LastPrice = first(AvgPrice)), by=.(StockCode, Country)]
  next_step <- merge(next_step, last_prices, by=c("StockCode", "Country"), all.x=TRUE)
  setnames(next_step, "LastPrice", "AvgPrice")
  next_step[is.na(AvgPrice), AvgPrice := 0]
  
  next_step[, Quantity := NA_real_]
  
  # 3. Bind & Calc Lags
  relevant_history <- history_data[YearMonth >= (as.Date(future_date) - months(4))]
  temp_bind <- rbind(relevant_history, next_step, fill=TRUE)
  setorder(temp_bind, Country, StockCode, YearMonth)
  
  temp_bind[, `:=`(
    lag_1 = shift(Quantity, 1, type="lag"),
    lag_2 = shift(Quantity, 2, type="lag"),
    lag_3 = shift(Quantity, 3, type="lag"),
    lag_price_1 = shift(AvgPrice, 1, type="lag"),
    roll_mean_3 = frollmean(Quantity, 3)
  ), by = .(StockCode, Country)]
  
  temp_bind[, Month := month(YearMonth)]
  
  # 4. Predict (ALL 3 MODELS)
  pred_set <- temp_bind[YearMonth == future_date]
  
  for (col in cols_to_fix) {
    if(col %in% names(pred_set)) pred_set[is.na(get(col)), (col) := LocalMean]
  }
  
  # Predict Bounds & Median
  p_low <- predict(model_lower, as.matrix(pred_set[, ..feature_cols]))
  p_med <- predict(model_median, as.matrix(pred_set[, ..feature_cols]))
  p_upp <- predict(model_upper, as.matrix(pred_set[, ..feature_cols]))
  
  # Ensure non-negative
  p_low <- pmax(p_low, 0)
  p_med <- pmax(p_med, 0)
  p_upp <- pmax(p_upp, 0)
  
  # Ensure logical ordering (Lower <= Median <= Upper)
  p_upp <- pmax(p_upp, p_med)
  p_low <- pmin(p_low, p_med)
  
  # Save
  pred_set[, Predicted_Quantity := p_med]
  pred_set[, Quantity := p_med] # Use Median for next lag calculation
  pred_set[, Lower_Bound := p_low]
  pred_set[, Upper_Bound := p_upp]
  
  forecast_results[[as.character(future_date)]] <- pred_set[, .(StockCode, Country, YearMonth, Predicted_Quantity, Lower_Bound, Upper_Bound)]
  
  # Update History
  update_row <- pred_set[, .(StockCode, Country, YearMonth, Quantity, AvgPrice, LocalMean, StockCode_Num, Country_Num)]
  history_data <- rbind(history_data, update_row, fill=TRUE)
}

final_forecast <- rbindlist(forecast_results)

# ==============================================================================
# PART 7: POST-PROCESSING (REVENUE + BOUNDS + RESIDUALS)
# ==============================================================================
print("Calculating Revenue & Finalizing...")

# --- STEP 1: SAFETY CLEAN-UP ---
cols_to_remove <- c("LatestPrice", "Revenue", "Type", "Dead")
existing_cols <- intersect(names(final_forecast), cols_to_remove)
if(length(existing_cols) > 0) final_forecast[, (existing_cols) := NULL]

# --- STEP 2: FILTER DEAD ITEMS ---
recent_hist <- dt_monthly[YearMonth >= as.Date("2011-07-01"), .(SumQ = sum(Quantity)), by=.(StockCode, Country)]
dead_pairs <- recent_hist[SumQ < 5] 
final_forecast <- merge(final_forecast, dead_pairs[, .(StockCode, Country, Dead=TRUE)], by=c("StockCode", "Country"), all.x=TRUE)

# Zero out dead items (Bounds too)
final_forecast[Dead == TRUE, `:=`(Predicted_Quantity = 0, Lower_Bound = 0, Upper_Bound = 0)]
final_forecast[, Dead := NULL]

# --- STEP 3: REVENUE ---
last_known_prices <- dt_monthly[order(-YearMonth), .(LatestPrice = first(AvgPrice)), by = .(StockCode, Country)]
final_forecast <- merge(final_forecast, last_known_prices, by = c("StockCode", "Country"), all.x = TRUE)
final_forecast[is.na(LatestPrice), LatestPrice := 0]
final_forecast[, Revenue := Predicted_Quantity * LatestPrice]
final_forecast[, Type := "Forecast"]

# --- STEP 4: PREPARE HISTORY (WITH RESIDUALS) ---
# We grab the residuals we calculated in Part 5
history_rev <- merge(
  dt_monthly[, .(StockCode, Country, YearMonth, Quantity, AvgPrice)],
  train_data[, .(StockCode, Country, YearMonth, Residual, Fitted)], # Join Residuals
  by = c("StockCode", "Country", "YearMonth"),
  all.x = TRUE
)

history_rev[, `:=`(
  LatestPrice = AvgPrice, 
  Revenue = Quantity * AvgPrice, 
  Type = "Actual",
  Predicted_Quantity = Fitted, # For history, Predicted = Fitted
  Lower_Bound = NA_real_,
  Upper_Bound = NA_real_
)]

# Clean up columns for binding
history_rev[, c("AvgPrice", "Fitted") := NULL]

# --- STEP 5: COMBINE ---
full_data <- rbind(
  history_rev, 
  final_forecast[, .(StockCode, Country, YearMonth, Quantity = Predicted_Quantity, 
                     Predicted_Quantity, Lower_Bound, Upper_Bound, 
                     LatestPrice, Revenue, Type, Residual = NA_real_)]
)

print("Data combined successfully!")

# ==============================================================================
# PART 8: SHINY SERVER (Global Dashboard with "All" Option)
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  # 1. UPDATED TITLE
  titlePanel("Global Sales Forecast Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # 2. UPDATED INPUT WITH "All" OPTION
      selectInput("countryInput", "Select Region:", 
                  choices = c("All", sort(unique(full_data$Country))), 
                  selected = "All"),
      hr(),
      downloadButton("downloadData", "Download CSV")
    ),
    
    mainPanel(
      tabsetPanel(
        # TAB 1: FORECAST
        tabPanel("Forecast Graph", 
                 br(),
                 plotOutput("forecastPlot"),
                 p("Shaded area represents the 80% Confidence Interval (10th to 90th percentile)."),
                 br(),
                 plotOutput("topProductsPlot")
        ),
        
        # TAB 2: REVENUE
        tabPanel("Revenue Analysis", 
                 br(),
                 plotOutput("monthlyRevPlot"),
                 plotOutput("quarterlyRevPlot"),
                 DTOutput("quarterlyTable")
        ),
        
        # TAB 3: RESIDUALS
        tabPanel("Model Diagnostics", 
                 br(),
                 h4("Residual Analysis (Training Data)"),
                 p("These charts analyze the errors (Actual - Predicted) to check model health."),
                 br(),
                 
                 # 1. ACF Plot
                 h5("1. Autocorrelation (ACF)"),
                 p("Checks if errors follow a pattern over time. Bars outside the blue lines suggest missed seasonality."),
                 plotOutput("residACFPlot", height = "300px"),
                 
                 # 2. Histogram
                 h5("2. Distribution (Histogram)"),
                 p("Checks if errors are normal. We filter out the top 5% extreme outliers to show the true shape."),
                 plotOutput("residHistPlot", height = "300px"),
                 
                 # 3. Time Plot
                 h5("3. Errors over Time"),
                 p("Checks if the model consistently over/under predicts at specific times."),
                 plotOutput("residTimePlot", height = "300px")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # --- REACTIVE DATA HANDLER ---
  country_data <- reactive({
    req(input$countryInput)
    if(input$countryInput == "All") {
      return(full_data) # Return everything
    } else {
      return(full_data[Country == input$countryInput])
    }
  })
  
  # --- Plot 1: Forecast with CONFIDENCE INTERVALS ---
  output$forecastPlot <- renderPlot({
    # Aggregate Mean, Lower, Upper across ALL selected data
    agg <- country_data()[, .(
      TotalQ = sum(Quantity, na.rm=TRUE),
      TotalLow = sum(Lower_Bound, na.rm=TRUE),
      TotalUpp = sum(Upper_Bound, na.rm=TRUE)
    ), by=.(YearMonth, Type)]
    
    ggplot(agg, aes(x=YearMonth, y=TotalQ, color=Type, fill=Type)) +
      geom_ribbon(data=agg[Type=="Forecast"], aes(ymin=TotalLow, ymax=TotalUpp), alpha=0.2, color=NA) +
      geom_line(size=1.2) + 
      geom_point(size=2) +
      geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype="dashed") +
      scale_y_continuous(labels = comma) +
      theme_minimal() + 
      labs(title="Total Sales Forecast (with 80% CI)", y="Total Quantity", x="")
  })
  
  # --- Plot 2: Top Products (Corrected: Based on Forecast Volume) ---
  output$topProductsPlot <- renderPlot({
    data_sub <- country_data()
    
    # 1. Identify Top Items based on HIGHEST FORECAST SUM
    # logic changed from 'Type == "Actual"' to 'Type == "Forecast"'
    top_items <- data_sub[Type == "Forecast", .(Vol = sum(Quantity, na.rm=TRUE)), by=StockCode][order(-Vol)]
    
    # Safe subset (take top 6, removing NAs if fewer than 6 exist)
    top_items <- na.omit(top_items$StockCode[1:6])
    
    if(length(top_items) == 0) return(NULL)
    
    plot_sub <- data_sub[StockCode %in% top_items]
    
    # 2. AGGREGATE BY STOCKCODE & DATE (For "All" selection support)
    plot_agg <- plot_sub[, .(
      Quantity = sum(Quantity, na.rm=TRUE),
      Lower_Bound = sum(Lower_Bound, na.rm=TRUE),
      Upper_Bound = sum(Upper_Bound, na.rm=TRUE)
    ), by=.(StockCode, YearMonth, Type)]
    
    # Set factor levels so the plot orders them from #1 to #6 correctly
    plot_agg[, StockCode := factor(StockCode, levels=top_items)]
    
    ggplot(plot_agg, aes(x=YearMonth, y=Quantity, color=Type)) +
      geom_ribbon(aes(ymin=Lower_Bound, ymax=Upper_Bound, fill=Type), alpha=0.1, color=NA) +
      geom_line() +
      facet_wrap(~StockCode, scales="free_y") +
      geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype="dashed", alpha=0.5) +
      theme_light() + 
      labs(title="Top 6 Products (By Highest Forecasted Demand)", y="Quantity", x="")
  })
  
  # --- Plot 3 & 4 (Revenue) ---
  output$monthlyRevPlot <- renderPlot({
    agg_rev <- country_data()[, .(TotalRev = sum(Revenue, na.rm=TRUE)), by=.(YearMonth, Type)]
    ggplot(agg_rev, aes(x=YearMonth, y=TotalRev, fill=Type)) +
      geom_col(alpha=0.8) + scale_y_continuous(labels = dollar_format(prefix="$")) +
      theme_minimal() + labs(title="Monthly Revenue", y="Revenue", x="")
  })
  
  output$quarterlyRevPlot <- renderPlot({
    dt_q <- country_data()
    dt_q[, QDate := floor_date(YearMonth, "quarter")]
    dt_q[, QLabel := paste0(year(QDate), " Q", quarter(QDate))]
    agg_q <- dt_q[, .(TotalRev = sum(Revenue, na.rm=TRUE)), by=.(QLabel, Type)]
    ggplot(agg_q, aes(x=QLabel, y=TotalRev, fill=Type)) +
      geom_col(width=0.6) + scale_y_continuous(labels = dollar_format(prefix="$")) +
      scale_fill_manual(values=c("Actual"="#2ca02c", "Forecast"="#d62728")) +
      theme_light() + labs(title="Quarterly Revenue", y="Revenue", x="Quarter")
  })
  
  output$quarterlyTable <- renderDT({
    dt_q <- country_data()
    dt_q[, QDate := floor_date(YearMonth, "quarter")]
    dt_q[, QLabel := paste0(year(QDate), " Q", quarter(QDate))]
    agg_q <- dt_q[, .(TotalRev = sum(Revenue, na.rm=TRUE)), by=.(QLabel, Type)]
    datatable(agg_q, options = list(pageLength = 10)) %>% formatCurrency('TotalRev', '$')
  })
  
  # --- RESIDUAL ANALYSIS (3 GRAPHS) ---
  
  # 1. ACF Plot
  output$residACFPlot <- renderPlot({
    res_data <- country_data()[Type == "Actual" & !is.na(Residual)]
    
    # Aggregate residuals by month to get a clean time series for ACF
    agg_res <- res_data[, .(MeanResid = mean(Residual, na.rm=TRUE)), by=YearMonth][order(YearMonth)]
    
    if(nrow(agg_res) > 2) {
      acf(agg_res$MeanResid, main="ACF of Monthly Average Residuals", lwd=2, col="darkblue")
    } else {
      plot.new()
      text(0.5, 0.5, "Not enough data for ACF")
    }
  })

  # 2. Histogram (ROBUST / OUTLIER FILTERED)
  output$residHistPlot <- renderPlot({
    raw_data <- country_data()[Type == "Actual" & !is.na(Residual)]
    
    # Filter to middle 95% to avoid the "Single Block" issue caused by extreme outliers
    low_cutoff <- quantile(raw_data$Residual, 0.025, na.rm=TRUE)
    high_cutoff <- quantile(raw_data$Residual, 0.975, na.rm=TRUE)
    plot_data <- raw_data[Residual >= low_cutoff & Residual <= high_cutoff]
    
    ggplot(plot_data, aes(x=Residual)) +
      geom_histogram(bins=30, fill="#5D6D7E", color="white", alpha=0.8) +
      theme_minimal() +
      labs(title="Distribution of Residuals (Middle 95%)", 
           subtitle = paste0("Outliers removed for visibility. Range: ", 
                             round(low_cutoff, 1), " to ", round(high_cutoff, 1)),
           x="Residual Error", y="Count")
  })
  
  # 3. Time Plot
  output$residTimePlot <- renderPlot({
    res_data <- country_data()[Type == "Actual" & !is.na(Residual)]
    agg_res <- res_data[, .(MeanResid = mean(Residual, na.rm=TRUE)), by=YearMonth]
    
    ggplot(agg_res, aes(x=YearMonth, y=MeanResid)) +
      geom_hline(yintercept=0, color="red", linetype="dashed") +
      geom_line(color="steelblue", size=1) + geom_point() +
      theme_minimal() +
      labs(title="Average Residual Error over Time", 
           subtitle="Deviation from 0 indicates monthly bias",
           y="Mean Residual", x="")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("forecast_data_", input$countryInput, ".csv", sep="") },
    content = function(file) { fwrite(country_data(), file) }
  )
}

shinyApp(ui = ui, server = server)