source('managePackages.R')

adf_test <- function(data){
  adf_test_result <- adf.test(data)
  print(adf_test_result)
}

easy_plot <- function(x, y, title){
  data <- data.frame(x = x, y = y)
  ggplot(data = data, aes(x = x, y = y)) + 
    geom_point(color = "blue") +
    geom_line(color = "red") +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
    labs(title = title, x = "Date", y = "Y Value") +
    theme_minimal()
}

plot_hpfilter <- function(time, data, title){
  hp_result <- hpfilter(data, freq = 14400)
  trend <- hp_result$trend
  cycle <- hp_result$cycle
  
  print(adf.test(cycle))
  
  df <- data.frame(
    Time = time,
    Original = as.numeric(data),
    Trend = as.numeric(trend),
    Cycle = as.numeric(cycle)
  )
  ggplot(data = df, aes(x = Time)) + 
    geom_line(aes(y = Original, color = "Original"), linewidth = 1) +
    geom_line(aes(y = Trend, color = "Trend"), linewidth = 1, linetype = "dashed") +
    geom_line(aes(y = Cycle, color = "Cycle"), linewidth = 1, linetype = "dotted") +
    scale_color_manual(values = c("Original" = "black", "Trend" = "blue", "Cycle" = "red")) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
    labs(title = "HP Filter Decomposition", x = "Time", y = "Value") +
    theme_minimal() + 
    theme(legend.title = element_blank())
}