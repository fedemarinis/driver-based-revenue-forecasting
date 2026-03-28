# Load libraries for data import and manipulation
library(readr)        # Read CSV and data files
library(dplyr)        # Data manipulation verbs
library(lubridate)    # Date and time handling
library(tidyr)  # For pivot_longer function


# Libraries for time series analysis and forecasting
library(forecast)     # Forecasting methods like ETS, ARIMA
library(fable)        # Modern forecasting framework
library(feasts)       # Time series features and decomposition
library(fabletools)   # Tools for forecasting models
library(tseries)      # Time series statistical tests

# Libraries for visualization and layout
library(ggplot2)      # Plotting
library(gridExtra)    # Arrange multiple ggplots
library(patchwork) 

# Libraries for reporting and tables
library(knitr)        # Dynamic report generation
library(tibble)       # Modern data frames
library(kableExtra)   # Enhanced tables for reports



# Read CSV file
sales_data <- read_csv("Supplement_Sales_Weekly_Expanded.csv")

# View first few rows
head(sales_data)

# View last few rows
tail(sales_data)

# Check structure of dataset
str(sales_data)

# View column names
colnames(sales_data)

# Number of rows and columns
nrow(sales_data)
ncol(sales_data)

# Summary statistics for each column
summary(sales_data)

# Check data types
sapply(sales_data, class)

# Check for missing values in each column
colSums(is.na(sales_data))

# Remove rows with missing values 
sales_data <- na.omit(sales_data)

# Find duplicate rows
duplicated(sales_data)

# Remove duplicate rows
sales_data <- sales_data[!duplicated(sales_data), ]

# Quick view of unique values in each column
lapply(sales_data, unique)


# EDA

# 1. Total Units Sold over Time (All Products)
sales_data %>%
  group_by(Date) %>%
  summarise(total_units = sum(`Units Sold`)) %>%
  ggplot(aes(x = Date, y = total_units)) +
  geom_line(color = "blue") +
  labs(title = "Total Units Sold Over Time", x = "Date", y = "Units Sold")


# 2. Find top 5 products by total revenue
top_products <- sales_data %>%
  group_by(`Product Name`) %>%
  summarise(total_revenue = sum(Revenue), .groups = "drop") %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 5) %>%
  pull(`Product Name`)

sales_data %>%
  filter(`Product Name` %in% top_products) %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth, `Product Name`) %>%
  summarise(total_revenue = sum(Revenue), .groups = "drop") %>%
  ggplot(aes(x = YearMonth, y = total_revenue, fill = `Product Name`)) +
  geom_col(show.legend = FALSE) +  # hide legend since we facet
  facet_wrap(~ `Product Name`, scales = "free_y") +
  labs(
    title = "Monthly Revenue Trend for Top 5 Products",
    x = "Month",
    y = "Revenue"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )


# 3. Average Discount by Product Category
sales_data %>%
  group_by(Category) %>%
  summarise(avg_discount = mean(Discount)) %>%
  ggplot(aes(x = reorder(Category, avg_discount), y = avg_discount, fill = Category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Discount by Category", x = "Category", y = "Average Discount")

# 4. Total Revenue by Location
sales_data %>%
  group_by(Location) %>%
  summarise(total_revenue = sum(Revenue)) %>%
  ggplot(aes(x = Location, y = total_revenue, fill = Location)) +
  geom_col() +
  labs(title = "Total Revenue by Location", x = "Location", y = "Revenue")

# 5. Price Distribution Across All Products
ggplot(sales_data, aes(x = Price)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Price Distribution", x = "Price", y = "Frequency")



# 6. Units Returned by Product Category
sales_data %>%
  group_by(Category) %>%
  summarise(total_returns = sum(`Units Returned`), .groups = "drop") %>%
  ggplot(aes(x = reorder(Category, total_returns), y = total_returns, fill = Category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total Units Returned by Category", x = "Category", y = "Units Returned")


# 7. Revenue by Location (Faceted by Year
sales_data %>%
  mutate(Year = year(Date),
         Month = floor_date(Date, "month")) %>%
  group_by(Year, Month, Location) %>%
  summarise(total_revenue = sum(Revenue), .groups = "drop") %>%
  ggplot(aes(x = Month, y = total_revenue, color = Location)) +
  geom_line() +
  facet_wrap(~Year, scales = "free_x") +
  labs(title = "Monthly Revenue by Location (Faceted by Year)",
       x = "Month", y = "Revenue") +
  theme_minimal()


# 8. Discount Distribution by Platform
ggplot(sales_data, aes(x = Discount, fill = Platform)) +
  geom_histogram(binwidth = 0.02, position = "dodge", color = "black") +
  labs(title = "Discount Distribution by Platform", x = "Discount", y = "Count")

# 9. Units Sold vs Price (scatter plot)
ggplot(sales_data, aes(x = Price, y = `Units Sold`)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Units Sold vs Price", x = "Price", y = "Units Sold")


# 10. Revenue vs Units Sold with Discount Level


sales_data %>%
  ggplot(aes(x = `Units Sold`, y = Revenue, color = Discount, size = Discount)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Revenue vs Units Sold with Discount Level",
    x = "Units Sold",
    y = "Revenue",
    color = "Discount",
    size = "Discount"
  ) +
  theme_minimal()



# 11. Revenue vs Units Sold by Category, Platform, and Location
sales_data %>%
  ggplot(aes(x = `Units Sold`, y = Revenue,
             color = Category, shape = Platform)) +
  geom_point(alpha = 0.6, size = 3) +
  facet_wrap(~ Location) +
  labs(
    title = "Revenue vs Units Sold by Category, Platform, and Location",
    x = "Units Sold",
    y = "Revenue",
    color = "Category",
    shape = "Platform"
  ) +
  theme_minimal()





# 12.Aggregate weekly revenue to monthly revenue per product
monthly_df <- sales_data %>%
  filter(`Product Name` %in% c("Whey Protein", "Vitamin C", "Multivitamin", "Pre-Workout")) %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Product = `Product Name`, Month) %>%
  summarise(Monthly_Revenue = sum(Revenue, na.rm = TRUE)) %>%
  ungroup()

# Faceted line plot per product
ggplot(monthly_df, aes(x = Month, y = Monthly_Revenue)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~Product, scales = "free_y") +
  labs(title = "Monthly Revenue by Product",
       x = "Month",
       y = "Revenue") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))







# Plot trend & seasonality

# Function to plot time series and STL decomposition for a given product
plot_ts_and_stl <- function(product_name, data) {
  # Filter data for the specified product and sort by date
  prod_data <- data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  # Create a weekly time series object with frequency 52 weeks
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # Plot the original weekly revenue time series
  p_ts <- autoplot(ts_prod) +
    ggtitle(paste(product_name, "- Weekly Revenue Time Series")) +
    ylab("Revenue") + xlab("Week") +
    theme_minimal()
  print(p_ts)
  
  # Perform STL decomposition to separate trend, seasonality, and remainder components
  decomp <- stl(ts_prod, s.window = "periodic")
  
  # Plot the STL decomposition results
  p_stl <- autoplot(decomp) +
    ggtitle(paste(product_name, "- STL Decomposition")) +
    theme_minimal()
  print(p_stl)
}

# List of products to analyze
products <- c("Whey Protein", "Vitamin C", "Multivitamin", "Pre-Workout")

# Loop through each product and generate the plots
for (prod in products) {
  plot_ts_and_stl(prod, sales_data)
}

# Individual calls for each product (optional repetition)
# Whey Protein
plot_ts_and_stl("Whey Protein", sales_data)

# Vitamin C
plot_ts_and_stl("Vitamin C", sales_data)

# Multivitamin
plot_ts_and_stl("Multivitamin", sales_data)


# Function to create ACF and PACF
plot_acf_pacf <- function(product_name) {
  cat("\n--- ACF and PACF for", product_name, "---\n")
  
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  p_acf <- ggAcf(ts_prod, lag.max = 52) + 
    ggtitle(paste(product_name, "- ACF")) + 
    theme_minimal()
  
  p_pacf <- ggPacf(ts_prod, lag.max = 52) + 
    ggtitle(paste(product_name, "- PACF")) + 
    theme_minimal()
  
  grid.arrange(p_acf, p_pacf, ncol = 2)
}

# Run for each product one by one
plot_acf_pacf("Whey Protein")
plot_acf_pacf("Vitamin C")
plot_acf_pacf("Multivitamin")
plot_acf_pacf("Pre-Workout")


# Function to calculate seasonality strength and plot seasonal component
plot_seasonality_strength <- function(product_name) {
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # STL decomposition
  decomp <- stl(ts_prod, s.window = "periodic")
  
  # Calculate seasonality strength
  var_remainder <- var(decomp$time.series[, "remainder"])
  var_seasonal_remainder <- var(decomp$time.series[, "seasonal"] + decomp$time.series[, "remainder"])
  seasonality_strength <- max(0, 1 - (var_remainder / var_seasonal_remainder))
  
  # Prepare data for plotting seasonal component
  seasonal_df <- data.frame(
    Week = time(ts_prod),
    Seasonal = decomp$time.series[, "seasonal"]
  )
  
  p <- ggplot(seasonal_df, aes(x = Week, y = Seasonal)) +
    geom_line(color = "steelblue") +
    ggtitle(paste(product_name, "\nSeasonality Strength =", round(seasonality_strength, 3))) +
    ylab("Seasonal Component") +
    xlab("Week") +
    theme_minimal()
  
  return(p)
}

# Create plots for each product
p1 <- plot_seasonality_strength("Whey Protein")
p2 <- plot_seasonality_strength("Vitamin C")
p3 <- plot_seasonality_strength("Multivitamin")
p4 <- plot_seasonality_strength("Pre-Workout")

# Arrange in 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2)


# Stationarity checks (ADF test, KPSS test)

products <- c("Whey Protein", "Vitamin C", "Multivitamin", "Pre-Workout")

# Initialize an empty data frame to store stationarity test results
stationarity_summary <- data.frame(
  Product = character(),
  ADF_Statistic = numeric(),
  ADF_p_value = numeric(),
  KPSS_Statistic = numeric(),
  KPSS_p_value = numeric(),
  Stationarity = character(),
  stringsAsFactors = FALSE
)

for (product_name in products) {
  # Filter data for the current product and order by date
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  # Create weekly time series object with 52 weeks frequency
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # Perform Augmented Dickey-Fuller (ADF) test for unit root (null: non-stationary)
  adf_result <- adf.test(ts_prod)
  
  # Perform KPSS test for stationarity (null: stationary)
  kpss_result <- kpss.test(ts_prod, null = "Level")
  
  # Decision logic:
  # Stationary if ADF rejects null (p < 0.05) and KPSS does not reject null (p > 0.05)
  stationary_flag <- ifelse(adf_result$p.value < 0.05 & kpss_result$p.value > 0.05,
                            "Stationary", "Non-Stationary")
  
  # Append results for current product to summary data frame
  stationarity_summary <- rbind(stationarity_summary, data.frame(
    Product = product_name,
    ADF_Statistic = round(adf_result$statistic, 3),
    ADF_p_value = round(adf_result$p.value, 4),
    KPSS_Statistic = round(kpss_result$statistic, 3),
    KPSS_p_value = round(kpss_result$p.value, 4),
    Stationarity = stationary_flag
  ))
}

# Print the stationarity test summary table with styling for clarity
stationarity_summary %>%
  kable(caption = "Stationarity Test Results (ADF & KPSS) for Products") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Variance checks (Box–Cox λ estimate)

summary_comments <- data.frame(
  Product = character(),
  BoxCox_Lambda = numeric(),
  Trend = character(),
  Seasonality = character(),
  Autocorrelation = character(),
  stringsAsFactors = FALSE
)

for (product_name in products) {
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # Box-Cox lambda estimate for variance stabilizing transform
  lambda <- BoxCox.lambda(ts_prod)
  
  # STL decomposition for trend and seasonality
  decomp <- stl(ts_prod, s.window = "periodic")
  trend_strength <- max(0, 1 - var(decomp$time.series[, "remainder"]) / var(decomp$time.series[, "trend"] + decomp$time.series[, "remainder"]))
  seasonality_strength <- max(0, 1 - var(decomp$time.series[, "remainder"]) / var(decomp$time.series[, "seasonal"] + decomp$time.series[, "remainder"]))
  
  # based on strength thresholds (arbitrary cutoffs)
  trend_comment <- ifelse(trend_strength > 0.4, "Strong", "Weak or no")
  seasonality_comment <- ifelse(seasonality_strength > 0.4, "Strong", "Weak or no")
  
  # Autocorrelation strength based on ACF at lag 1 to 5
  acf_vals <- acf(ts_prod, plot = FALSE)$acf[2:6]
  acf_strength <- mean(abs(acf_vals))
  autocorr_comment <- ifelse(acf_strength > 0.3, "Strong", "Weak or no")
  
  # Append results
  summary_comments <- rbind(summary_comments, data.frame(
    Product = product_name,
    BoxCox_Lambda = round(lambda, 3),
    Trend = paste(trend_comment, "trend"),
    Seasonality = paste(seasonality_comment, "seasonality"),
    Autocorrelation = paste(autocorr_comment, "autocorrelation")
  ))
}



summary_comments %>%
  kable(caption = "Variance Checks and Comments on Time Series Features") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)


# Model Analysis 

model_summary <- tibble(
  Product = character(),
  Reasoning = character(),
  ETS_AICc = numeric(),
  ARIMA_AICc = numeric(),
  ETS_RMSE = numeric(),
  ARIMA_RMSE = numeric(),
  ETS_MAE = numeric(),
  ARIMA_MAE = numeric(),
  ETS_MAPE = numeric(),
  ARIMA_MAPE = numeric(),
  Best_Model = character()
)

for (product_name in products) {
  cat("\nProcessing:", product_name, "\n")
  
  # Filter data for current product and order by date
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  # Create a weekly time series object with frequency = 52 weeks
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # Plot the time series to visually check trend, seasonality, and anomalies
  print(
    autoplot(ts_prod) + 
      ggtitle(paste(product_name, "- Weekly Revenue Time Series")) +
      ylab("Revenue") + xlab("Week") +
      theme_minimal()
  )
  # The plot helps identify overall trend and seasonal patterns to guide model selection.
  
  # Perform STL decomposition to separate trend, seasonality, and remainder components
  decomp <- stl(ts_prod, s.window = "periodic")
  
  # Plot the decomposition for visual assessment
  print(
    autoplot(decomp) + 
      ggtitle(paste(product_name, "- STL Decomposition")) +
      theme_minimal()
  )
  # Decomposition confirms strength and stability of trend and seasonality components.
  
  # Calculate strength metrics for trend and seasonality based on variance explained
  trend_strength <- max(0, 1 - var(decomp$time.series[, "remainder"]) / var(decomp$time.series[, "trend"] + decomp$time.series[, "remainder"]))
  seasonality_strength <- max(0, 1 - var(decomp$time.series[, "remainder"]) / var(decomp$time.series[, "seasonal"] + decomp$time.series[, "remainder"]))
  
  # Reasoning text describing the observed trend and seasonality strengths
  reasoning <- paste0("Trend strength: ", round(trend_strength, 2), 
                      ", Seasonality strength: ", round(seasonality_strength, 2))
  # comment: This quantifies the relative importance of trend and seasonality in the data.
  
  # Fit ETS model using STL-based forecasting to better handle complex seasonality
  fit_ets <- stlf(ts_prod, method = "ets", robust = TRUE)
  
  # Fit ARIMA model automatically selecting order and differencing
  fit_arima <- auto.arima(ts_prod)
  
  # Calculate accuracy metrics (RMSE, MAE, MAPE) comparing fitted values vs original data
  acc_ets <- accuracy(fitted(fit_ets), ts_prod)[1, ]
  acc_arima <- accuracy(fitted(fit_arima), ts_prod)[1, ]
  # comment: Accuracy on fitted values gives only an initial in-sample assessment and must be complemented
  # by out-of-sample validation for any forecasting conclusion.
  
  # Extract corrected AIC (AICc) to compare model parsimony and fit quality
  aicc_ets <- fit_ets$model$aicc
  aicc_arima <- fit_arima$aicc
  
  # Select the preferred univariate model based on lowest AICc.
  # This is an in-sample comparison criterion and should not be treated as the final forecasting judgment.
  best_model <- ifelse(aicc_ets < aicc_arima, "ETS", "ARIMA")
  
  # Add results for current product to summary table
  model_summary <- add_row(model_summary,
                           Product = product_name,
                           Reasoning = reasoning,
                           ETS_AICc = round(aicc_ets, 2),
                           ARIMA_AICc = round(aicc_arima, 2),
                           ETS_RMSE = round(acc_ets["RMSE"], 2),
                           ARIMA_RMSE = round(acc_arima["RMSE"], 2),
                           ETS_MAE = round(acc_ets["MAE"], 2),
                           ARIMA_MAE = round(acc_arima["MAE"], 2),
                           ETS_MAPE = round(acc_ets["MAPE"], 2),
                           ARIMA_MAPE = round(acc_arima["MAPE"], 2),
                           Best_Model = best_model
  )
  
  # Print which model was selected as best for current product
  cat("Best model for", product_name, ":", best_model, "\n")
  # comment: Printing final model choice helps track decision during iterative analysis.
}

# Print complete summary table for all products
print(model_summary)
# Summary table enables quick comparison of model performance across all products.


# Print model summary table with formatting
model_summary %>%
  kable(caption = "Model Comparison Summary for Products") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)


comparison_table <- model_summary %>%
  select(Product, Best_Model, ETS_AICc, ARIMA_AICc, ETS_RMSE, ARIMA_RMSE, ETS_MAE, ARIMA_MAE, ETS_MAPE, ARIMA_MAPE) %>%
  arrange(Product)

# Print the table nicely formatted
comparison_table %>%
  kable(caption = "Best Model Comparison Summary for Products", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

### Model Selection Comment

# Across all four products, ARIMA shows lower AICc values than ETS, indicating a better in-sample
# trade-off between fit and parsimony within the univariate comparison.
#
# However, this should not be interpreted as evidence of superior forecasting performance by itself.
# In the paper, cross-model comparison is based on out-of-sample accuracy (RMSE, MAE, MAPE)
# under a common 26-week holdout, not on AICc alone.
#
# The in-sample comparison therefore serves mainly as a diagnostic step for the univariate models.
# Final forecasting conclusions should rely on the out-of-sample validation results reported in Part 2.


# Model Comparsion Bar Graph 
performance_long <- model_summary %>%
  select(Product, 
         ETS_RMSE, ARIMA_RMSE, 
         ETS_MAE, ARIMA_MAE, 
         ETS_MAPE, ARIMA_MAPE) %>%
  pivot_longer(
    cols = -Product,
    names_to = c("Model", "Metric"),
    names_sep = "_",
    values_to = "Value"
  )

# Capitalize model names for neatness
performance_long$Model <- toupper(performance_long$Model)

# Plot grouped bar chart
ggplot(performance_long, aes(x = Product, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Model Performance Comparison by Product",
       y = "Metric Value",
       x = "Product") +
  theme_minimal() +
  scale_fill_manual(values = c("ETS" = "steelblue", "ARIMA" = "tomato")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))



plots <- list()  # Initialize an empty list to store plots for each product

# Loop through all products and create residual boxplots
for (product_name in products) {
  # Filter sales data for the current product and sort by date
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  # Create a weekly time series object with frequency = 52 weeks
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # Fit ETS model using STL forecasting to handle seasonality robustly
  fit_ets <- stlf(ts_prod, method = "ets", robust = TRUE)
  
  # Fit ARIMA model with automatic order selection
  fit_arima <- auto.arima(ts_prod)
  
  # Combine residuals from both models into a data frame for plotting
  residuals_df <- data.frame(
    Residuals = c(residuals(fit_ets), residuals(fit_arima)),  # Residual values from both models
    Model = rep(c("ETS", "ARIMA"), each = length(ts_prod))    # Label residuals by model type
  )
  
  # Create a boxplot comparing residual distributions of ETS vs ARIMA
  p <- ggplot(residuals_df, aes(x = Model, y = Residuals, fill = Model)) +
    geom_boxplot(alpha = 0.7) +  # Semi-transparent boxplots
    labs(title = paste(product_name, "- Residuals Boxplot"), y = "Residuals") +
    theme_minimal() +
    scale_fill_manual(values = c("ETS" = "steelblue", "ARIMA" = "tomato")) +  # Custom colors
    theme(legend.position = "none")  # Hide legend since Model labels are on x-axis
  
  # Store the plot in the list with the product name as key
  plots[[product_name]] <- p
}

# Arrange all product residual boxplots in a grid of 2 rows and 2 columns for easy comparison
grid.arrange(grobs = plots, nrow = 2, ncol = 2)




# Forecast next 26 weeks per product
plot_forecast_for_product <- function(product_name, forecast_horizon = 26) {
  # Filter sales data for the specified product and arrange by date
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  # Create a weekly time series starting at the first observation week
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,  # weekly data with yearly seasonality
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # Fit ETS model with STL decomposition to handle seasonality robustly
  fit_ets <- stlf(ts_prod, method = "ets", robust = TRUE)
  
  # Fit an automatic ARIMA model
  fit_arima <- auto.arima(ts_prod)
  
  # Choose the best model based on corrected AIC (AICc) criterion
  best_model <- ifelse(fit_ets$model$aicc < fit_arima$aicc, "ETS", "ARIMA")
  
  # Generate a forecast for the chosen model for the specified horizon (26 weeks)
  forecast_best <- if (best_model == "ETS") {
    forecast(fit_ets, h = forecast_horizon)
  } else {
    forecast(fit_arima, h = forecast_horizon)
  }
  
  # Prepare a data frame for prediction intervals to visualize uncertainty (95% confidence)
  ribbon_df <- data.frame(
    time = time(forecast_best$mean),
    lower = forecast_best$lower[, 2],  # Lower 95% bound
    upper = forecast_best$upper[, 2]   # Upper 95% bound
  )
  
  # Plot historical data and overlay the forecast line with prediction interval ribbon
  p <- autoplot(ts_prod) +
    autolayer(forecast_best$mean, series = paste(product_name, "Forecast"), color = "blue") +
    geom_ribbon(data = ribbon_df, aes(x = time, ymin = lower, ymax = upper),
                fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
    ggtitle(paste(product_name, "- 26-Week Forecast vs Historical")) +
    ylab("Revenue") + xlab("Week") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Print the plot
  print(p)
}

# Call the function separately for each product to visualize forecasts
plot_forecast_for_product("Whey Protein")
plot_forecast_for_product("Vitamin C")
plot_forecast_for_product("Multivitamin")
plot_forecast_for_product("Pre-Workout")



# Residual Plot

plot_residual_acf_hist <- function(product_name) {
  # Filter sales data for the given product and order by date
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  # Convert revenue data into a weekly time series
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # Fit ETS model with STL to handle seasonality robustly
  fit_ets <- stlf(ts_prod, method = "ets", robust = TRUE)
  # Fit an automatic ARIMA model
  fit_arima <- auto.arima(ts_prod)
  
  # Choose best model based on corrected AIC (AICc)
  best_model <- ifelse(fit_ets$model$aicc < fit_arima$aicc, "ETS", "ARIMA")
  # Select the model object for residual analysis
  fit <- if (best_model == "ETS") fit_ets else fit_arima
  
  # Extract residuals from the selected model
  res <- residuals(fit)
  
  # Plot Autocorrelation Function (ACF) of residuals to check for autocorrelation
  p_acf <- ggAcf(res) +
    ggtitle(paste(product_name, "- ACF of Residuals")) +
    theme_minimal()
  
  # Plot histogram of residuals to check residuals distribution (normality)
  p_hist <- ggplot(data.frame(residuals = res), aes(x = residuals)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    ggtitle(paste(product_name, "- Histogram of Residuals")) +
    xlab("Residuals") + ylab("Frequency") +
    theme_minimal()
  
  # Display both plots side-by-side for visual inspection
  grid.arrange(p_acf, p_hist, ncol = 2)
}

# Call the function separately for each product to inspect residual diagnostics
plot_residual_acf_hist("Whey Protein")
plot_residual_acf_hist("Vitamin C")
plot_residual_acf_hist("Multivitamin")
plot_residual_acf_hist("Pre-Workout")



# Residual Diagnostics 
plot_residual_diagnostics <- function(product_name) {
  # Filter and sort data for the given product
  prod_data <- sales_data %>%
    filter(`Product Name` == product_name) %>%
    arrange(Date)
  
  # Create weekly time series object
  ts_prod <- ts(prod_data$Revenue,
                frequency = 52,
                start = c(year(min(prod_data$Date)), week(min(prod_data$Date))))
  
  # Fit ETS and ARIMA models
  fit_ets <- stlf(ts_prod, method = "ets", robust = TRUE)
  fit_arima <- auto.arima(ts_prod)
  
  # Select best model based on lowest AICc
  best_model <- ifelse(fit_ets$model$aicc < fit_arima$aicc, "ETS", "ARIMA")
  fit <- if (best_model == "ETS") fit_ets else fit_arima
  
  # Extract residuals and fitted values from best model
  residuals_vec <- residuals(fit)
  fitted_vec <- fitted(fit)
  
  # Residuals vs Fitted Values plot to check heteroscedasticity and patterns
  df_res_fit <- data.frame(Fitted = fitted_vec, Residuals = residuals_vec)
  p_res_fit <- ggplot(df_res_fit, aes(x = Fitted, y = Residuals)) +
    geom_point(color = "darkred", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle(paste(product_name, "- Residuals vs Fitted")) +
    xlab("Fitted values") + ylab("Residuals") +
    theme_minimal()
  
  # Normal Q-Q plot to assess normality of residuals
  qq_df <- data.frame(sample = residuals_vec)
  p_qq <- ggplot(qq_df, aes(sample = sample)) +
    stat_qq(color = "steelblue") +
    stat_qq_line() +
    ggtitle(paste(product_name, "- Normal Q-Q Plot")) +
    theme_minimal()
  
  # Arrange the two plots side by side
  grid.arrange(p_res_fit, p_qq, ncol = 2)
}

# Run residual diagnostics for each product
plot_residual_diagnostics("Whey Protein")
plot_residual_diagnostics("Vitamin C")
plot_residual_diagnostics("Multivitamin")
plot_residual_diagnostics("Pre-Workout")



### Model Performance Summary

# Based on the in-sample comparison table, Whey Protein shows the lowest error values overall,
# particularly under ETS, and can be viewed as the most stable series among the four products
# in this preliminary univariate assessment.
#
# At the same time, the paper does not conclude that ARIMA consistently outperforms ETS across
# all products. Instead, it shows that both univariate approaches deliver relatively high
# out-of-sample error, reflecting the weak trend, weak seasonality, and limited internal
# time-series structure in the data.
#
# For that reason, these in-sample error comparisons should be interpreted cautiously and only
# as an intermediate diagnostic step before the out-of-sample validation and ARIMAX extension.



# Overall Project Conclusion

# This project analyzes weekly revenue series for four supplement products using univariate
# time-series benchmarks (ETS and ARIMA), followed by an out-of-sample comparison with
# ARIMAX models including Price and Discount as exogenous drivers.
#
# Diagnostic plots, STL decomposition, stationarity checks, and residual analysis are used
# to characterize the data and motivate model choice. In line with the paper, AICc is used
# only for univariate specification support, while final model comparison is based on
# out-of-sample RMSE, MAE, and MAPE under a common 26-week holdout.
#
# The paper’s main conclusion is that the series exhibit weak intrinsic structure, so ETS and
# ARIMA offer limited forecasting leverage. By contrast, ARIMAX substantially improves
# out-of-sample accuracy, showing that short-horizon revenue variation is better explained by
# contemporaneous commercial drivers than by internal time-series dynamics alone.


# ============================================================
# PART 2 — OUT-OF-SAMPLE EVALUATION + ARIMAX EXTENSIONS
# ============================================================

# ---------- 0) Keep the merged "model_features_summary" ----------
# This table combines *previously shown* stationarity results (ADF, KPSS)
# with *previously shown* Box-Cox and trend/seasonality/autocorr comments.
# => So this table is a consolidation of earlier results; not new analysis.
model_features_summary <- stationarity_summary %>%
  dplyr::left_join(summary_comments, by = "Product") %>%
  dplyr::select(
    Product,
    ADF_Statistic, ADF_p_value, KPSS_Statistic, KPSS_p_value, Stationarity,
    BoxCox_Lambda, Trend, Seasonality, Autocorrelation
  ) %>%
  dplyr::arrange(Product)

model_features_summary %>%
  kableExtra::kable(
    caption = "Summary of Time Series Characteristics per Product"
  ) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)


# ---------- 1) Shared settings & helper for train/test split ----------
# Forecast horizon for test set: 26 weeks (half a year), used consistently.
h <- 26

# Helper: build weekly ts for a product's Revenue
.make_ts <- function(prod_df) {
  ts(
    prod_df$Revenue,
    frequency = 52,
    start = c(lubridate::year(min(prod_df$Date)), lubridate::week(min(prod_df$Date)))
  )
}

# Helper: robust train/test split using a single time boundary derived from `time()`.
# One boundary is used for *all* models to ensure strict comparability.
.split_train_test <- function(ts_y, h) {
  n <- length(ts_y)
  if (h >= n) stop("Forecast horizon h must be smaller than the series length.")
  end_idx <- n - h
  list(
    train = window(ts_y, end = time(ts_y)[end_idx]),
    test  = window(ts_y, start = time(ts_y)[end_idx + 1])
  )
}


# ---------- 2) Out-of-sample validation: ETS & ARIMA ----------
# NOTE: This is NEW analysis (previous part mainly reported in-sample/fitted metrics).
# We compute test-set accuracy for ETS and ARIMA on the same split.
validation_summary <- tibble::tibble(
  Product = character(),
  Model = character(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric(),
  Ljung_Box_pvalue = numeric()
)

for (product_name in products) {
  
  # Prepare product data (ordered, with Date retained for later alignment/plots)
  prod_data <- sales_data %>%
    dplyr::filter(`Product Name` == product_name) %>%
    dplyr::arrange(Date)
  
  ts_y <- .make_ts(prod_data)
  sp   <- .split_train_test(ts_y, h = h)
  y_tr <- sp$train
  y_te <- sp$test
  
  # --- ETS (stlf as in your earlier section; residuals definition consistent) ---
  fit_ets    <- forecast::stlf(y_tr, method = "ets", robust = TRUE)
  fcast_ets  <- forecast::forecast(fit_ets, h = length(y_te))
  acc_ets    <- forecast::accuracy(fcast_ets, y_te)
  lb_ets_p   <- Box.test(residuals(fit_ets), lag = 20, type = "Ljung-Box")$p.value
  
  validation_summary <- dplyr::bind_rows(
    validation_summary,
    tibble::tibble(
      Product = product_name,
      Model   = "ETS",
      RMSE    = round(acc_ets["Test set", "RMSE"], 2),
      MAE     = round(acc_ets["Test set", "MAE"],  2),
      MAPE    = round(acc_ets["Test set", "MAPE"], 2),
      Ljung_Box_pvalue = round(lb_ets_p, 4)
    )
  )
  
  # --- ARIMA (automatic order selection on the same train/test split) ---
  fit_arima   <- forecast::auto.arima(y_tr)
  fcast_arima <- forecast::forecast(fit_arima, h = length(y_te))
  acc_arima   <- forecast::accuracy(fcast_arima, y_te)
  lb_arima_p  <- Box.test(residuals(fit_arima), lag = 20, type = "Ljung-Box")$p.value
  
  validation_summary <- dplyr::bind_rows(
    validation_summary,
    tibble::tibble(
      Product = product_name,
      Model   = "ARIMA",
      RMSE    = round(acc_arima["Test set", "RMSE"], 2),
      MAE     = round(acc_arima["Test set", "MAE"],  2),
      MAPE    = round(acc_arima["Test set", "MAPE"], 2),
      Ljung_Box_pvalue = round(lb_arima_p, 4)
    )
  )
}

# Display: these are NEW (out-of-sample) results for ETS/ARIMA only.
validation_summary %>%
  dplyr::arrange(Product, Model) %>%
  kableExtra::kable(
    caption = "Out-of-Sample Forecast Accuracy — ETS vs ARIMA (Common Train/Test Split)"
  ) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)


# ---------- 3) ARIMAX with raw covariates (Price, Discount) ----------
# NOTE: Valid because at forecast time, Price and Discount are assumed known here.
# We use the *same* train/test boundary used above to maintain comparability.

cor(sales_data$Price, sales_data$Discount)
cor(sales_data$Price, sales_data$Revenue)
cor(sales_data$Discount, sales_data$Revenue)
cor(sales_data[, c("Price", "Discount", "Revenue")])

arimax_raw_summary <- tibble::tibble(
  Product = character(),
  Model = character(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric(),
  Ljung_Box_pvalue = numeric()
)

for (product_name in products) {
  
  prod_data <- sales_data %>%
    dplyr::filter(`Product Name` == product_name) %>%
    dplyr::arrange(Date)
  
  ts_y <- .make_ts(prod_data)
  sp   <- .split_train_test(ts_y, h = h)
  y_tr <- sp$train
  y_te <- sp$test
  
  # Build regressors matrix (RAW, unstandardized) aligned with the series
  xreg_all <- as.matrix(prod_data %>% dplyr::select(Price, Discount))
  # Split regressors by length to match train/test parts of the TS
  xreg_tr  <- xreg_all[seq_len(length(y_tr)), , drop = FALSE]
  xreg_te  <- xreg_all[(length(y_tr) + 1):(length(y_tr) + length(y_te)), , drop = FALSE]
  
  # Fit ARIMAX with raw regressors
  fit_arimax_raw   <- forecast::auto.arima(y_tr, xreg = xreg_tr)
  fcast_arimax_raw <- forecast::forecast(fit_arimax_raw, xreg = xreg_te, h = length(y_te))
  acc_arimax_raw   <- forecast::accuracy(fcast_arimax_raw, y_te)
  lb_arimax_raw_p  <- Box.test(residuals(fit_arimax_raw), lag = 20, type = "Ljung-Box")$p.value
  
  arimax_raw_summary <- dplyr::bind_rows(
    arimax_raw_summary,
    tibble::tibble(
      Product = product_name,
      Model   = "ARIMAX (Price + Discount)",
      RMSE    = round(acc_arimax_raw["Test set", "RMSE"], 2),
      MAE     = round(acc_arimax_raw["Test set", "MAE"],  2),
      MAPE    = round(acc_arimax_raw["Test set", "MAPE"], 2),
      Ljung_Box_pvalue = round(lb_arimax_raw_p, 4)
    )
  )
}

arimax_raw_summary %>%
  dplyr::arrange(Product) %>%
  kableExtra::kable(
    caption = "ARIMAX — Out-of-Sample Forecast Accuracy"
  ) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)


# ---------- 5) Combined comparison table ----------
# This table is NEW and aggregates NEW out-of-sample results:
#  • ETS & ARIMA (from 'validation_summary' computed above in this "test-set" section)
#  • ARIMAX (Raw) using Price + Discount as xreg (from 'arimax_raw_summary')
# NOTE: The ARIMAX (standardized covariates) section has been REMOVED,
#       so no standardized rows appear here anymore.

comparison_all <- validation_summary %>%
  dplyr::bind_rows(arimax_raw_summary) %>%   # <-- only RAW ARIMAX kept
  dplyr::arrange(Product, Model)

comparison_all %>%
  kableExtra::kable(
    caption = "Out-of-Sample Accuracy: ETS vs ARIMA vs ARIMAX"
  ) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                            full_width = FALSE)

# ---------- 6) Visualization (ARIMAX only) ----------
# As requested: ONLY plot historical data (train), actual test values, and ARIMAX (RAW) predictions.
# X-axis is Date for consistency with earlier plots and to avoid fractional time on axes.
# This plot is NEW (was not present in the univariate-only section) and focuses solely on ARIMAX (Raw).

.plot_arimax_test <- function(product_name, forecast_horizon = h) {
  
  # --- Subset and order -------------------------------------------------------
  prod_data <- sales_data %>%
    dplyr::filter(`Product Name` == product_name) %>%
    dplyr::arrange(Date)
  
  # --- Build the target series and split into train/test ----------------------
  # Uses helper defined earlier: .make_ts(prod_data) -> weekly ts (freq = 52)
  # and .split_train_test(ts_y, h) -> list(train=..., test=...) with a single boundary.
  ts_y <- .make_ts(prod_data)
  sp   <- .split_train_test(ts_y, h = forecast_horizon)
  y_tr <- sp$train
  y_te <- sp$test
  
  # --- Build regressors (RAW covariates, no standardization) ------------------
  # IMPORTANT: These are known at forecast time in your current context.
  xreg_all <- as.matrix(prod_data %>% dplyr::select(Price, Discount))
  xreg_tr  <- xreg_all[seq_len(length(y_tr)), , drop = FALSE]
  xreg_te  <- xreg_all[(length(y_tr) + 1):(length(y_tr) + length(y_te)), , drop = FALSE]
  
  # --- Fit ARIMAX on train and forecast over test -----------------------------
  fit_arimax <- forecast::auto.arima(y_tr, xreg = xreg_tr)
  fcast_ax   <- forecast::forecast(fit_arimax, xreg = xreg_te, h = length(y_te))
  
  # --- Assemble a Date-aware plotting data frame ------------------------------
  # NOTE: We use the actual Date column to keep a proper calendar axis.
  df_train <- data.frame(
    Date    = prod_data$Date[seq_len(length(y_tr))],
    Revenue = as.numeric(y_tr),
    Type    = "Actual_Train"
  )
  
  df_test <- data.frame(
    Date    = prod_data$Date[(length(y_tr) + 1):(length(y_tr) + length(y_te))],
    Revenue = as.numeric(y_te),
    Type    = "Actual_Test"
  )
  
  df_fcast <- data.frame(
    Date    = df_test$Date,
    Revenue = as.numeric(fcast_ax$mean),
    Type    = "ARIMAX_Forecast"
  )
  
  df_plot <- rbind(df_train, df_test, df_fcast)
  
  # --- Plot (ARIMAX only) -----------------------------------------------------
  ggplot2::ggplot(df_plot, ggplot2::aes(x = Date, y = Revenue, color = Type)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(
      title = paste0(product_name, " — Test: Actual vs ARIMAX"),
      x = "Date",
      y = "Revenue"
    ) +
    ggplot2::scale_color_manual(values = c(
      "Actual_Train"     = "black",
      "Actual_Test"      = "darkgreen",
      "ARIMAX_Forecast"  = "red"
    )) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

# Produce ONLY ARIMAX plots — no ETS/ARIMA lines.
for (p in products) {
  print(.plot_arimax_test(p, forecast_horizon = h))
}
