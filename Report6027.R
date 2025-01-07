library(tidyverse)
library(cluster)
library(ggplot2)
library(corrplot)
library(factoextra)


### Read file
#Dataset folder
getwd()
data_path <- "data/"
file_names <- c("2014_Financial_Data.csv", 
                "2015_Financial_Data.csv", 
                "2016_Financial_Data.csv", 
                "2017_Financial_Data.csv", 
                "2018_Financial_Data.csv")

#Storing data in an empty list
financial_data <- list()
for (file in file_names) {
  file_path <- file.path(data_path, file)
  year <- as.integer(gsub("_Financial_Data.csv", "", file))
  df <- suppressWarnings(
    read_csv(file_path, col_names = TRUE, show_col_types = FALSE) %>%
      rename(StockCode = `...1`) %>%
      mutate(Year = year) 
  )
  financial_data[[as.character(year)]] <- df
}


#### Filtering related columns
#Define the required columns
required_columns <- c(
  "StockCode",           
  "Sector",            
  "Year",               
  "Revenue",         
  "Net Income",        
  "Gross Profit",      
  "Operating Income",   
  "R&D Expenses",        # Research and development expenditure
  "SG&A Expense",        # Selling, general and administrative expenses
  "EBIT",                # Earnings before interest and tax (EBIT)
  "Profit Margin",      
  "Return on Equity",  
  "Return on Assets"   
)

filtered_data <- list()
#Select data of each year
for (year in names(financial_data)) {
  df <- financial_data[[year]]
  valid_columns <- required_columns[required_columns %in% colnames(df)]
  filtered_data[[year]] <- df %>%
    select(all_of(valid_columns))
}


#### Missing value
#Missing value
cleaned_data <- list()
for (year in names(filtered_data)) {
  df <- filtered_data[[year]]
  
  #Delete missing row
  df_clean <- df %>% drop_na()
  cleaned_data[[year]] <- df_clean
  cat("Original rows: ", nrow(filtered_data[[year]]), "\n")
  cat("processing rows: ", nrow(df_clean), "\n")
  cat("\n")
}


df_2014 <- cleaned_data[["2014"]] %>% mutate(Year = 2014)
df_2015 <- cleaned_data[["2015"]] %>% mutate(Year = 2015)
df_2016 <- cleaned_data[["2016"]] %>% mutate(Year = 2016)
df_2017 <- cleaned_data[["2017"]] %>% mutate(Year = 2017)
df_2018 <- cleaned_data[["2018"]] %>% mutate(Year = 2018)

### Data analysis
#### 1. cluster analysis ###
#Cluster analysis
perform_clustering <- function(df, year, k = 3, cluster_columns) {
  cat("Year:", year, "\n")
  cluster_data <- df %>%
    select(all_of(cluster_columns)) %>%
    scale()
  
#Elbow
  wss <- numeric()
  for (k_temp in 2:9) {
    clustering <- kmeans(cluster_data, centers = k_temp, nstart = 25)
    wss <- c(wss, clustering$tot.withinss)
  }
  plot(2:9, wss, type = "b", 
       xlab = "Number of Clusters (k)", 
       ylab = "Total Within-Cluster Sum of Squares (WSS)",
       main = paste("Elbow Method for", year))
  final_clustering <- kmeans(cluster_data, centers = k, nstart = 25)
  df$Cluster <- as.factor(final_clustering$cluster)
  cat("Distribution of clustering results：\n")
  print(table(df$Cluster))
  cat("\ncentre of clustering ：\n")
  print(final_clustering$centers)
  
#Data visualisation
  plot <- fviz_cluster(final_clustering, data = cluster_data, 
                       geom = "point", ellipse.type = "convex", 
                       main = paste("K-means Clustering (k =", k, ") for", year), 
                       ggtheme = theme_minimal())
  print(plot)
  
#Clustered feature means
  cluster_summary <- df %>%
    group_by(Cluster) %>%
    summarise(
      Revenue = mean(Revenue, na.rm = TRUE),
      Net_Income = mean(`Net Income`, na.rm = TRUE),
      Gross_Profit = mean(`Gross Profit`, na.rm = TRUE),
      Operating_Income = mean(`Operating Income`, na.rm = TRUE),
      EBIT = mean(EBIT, na.rm = TRUE)
    )
  print(cluster_summary)
  
  return(df)
}
cluster_columns <- c("Revenue", "Net Income", "Gross Profit", "Operating Income", "EBIT")

#Cluster analysis
clustered_2014 <- perform_clustering(df_2014, 2014, k = 3, cluster_columns)
clustered_2015 <- perform_clustering(df_2015, 2015, k = 3, cluster_columns)
clustered_2016 <- perform_clustering(df_2016, 2016, k = 3, cluster_columns)
clustered_2017 <- perform_clustering(df_2017, 2017, k = 3, cluster_columns)
clustered_2018 <- perform_clustering(df_2018, 2018, k = 3, cluster_columns)


#### 2. Sector analysis
analyse_by_sector <- function(df, year) {
  sector_analysis <- df %>%
    group_by(Sector) %>%
    summarise(
      Avg_Revenue = mean(Revenue, na.rm = TRUE),
      Avg_Net_Income = mean(`Net Income`, na.rm = TRUE),
      Avg_Gross_Profit = mean(`Gross Profit`, na.rm = TRUE),
      Avg_Operating_Income = mean(`Operating Income`, na.rm = TRUE),
      Avg_EBIT = mean(EBIT, na.rm = TRUE),
      Count = n()
    ) %>%
    arrange(desc(Avg_Revenue))
  plot <- ggplot(sector_analysis, aes(x = reorder(Sector, -Avg_Revenue), y = Avg_Revenue)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme_minimal() +
    labs(title = paste("Average Income by Sector in", year), x = "Sector", y = "Average Revenue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(plot)
  return(sector_analysis)
}

#Data visualisation
sector_analysis_2014 <- analyse_by_sector(df_2014, 2014)
sector_analysis_2015 <- analyse_by_sector(df_2015, 2015)
sector_analysis_2016 <- analyse_by_sector(df_2016, 2016)
sector_analysis_2017 <- analyse_by_sector(df_2017, 2017)
sector_analysis_2018 <- analyse_by_sector(df_2018, 2018)


sector_analysis_2014
sector_analysis_2015
sector_analysis_2016
sector_analysis_2017
sector_analysis_2018

#### 3. Individual stock time analysis
analyse_top_stocks <- function(df, year) {
  stock_time_analysis <- df %>%
    group_by(StockCode) %>%
    summarise(
      Avg_Revenue = mean(Revenue, na.rm = TRUE),
      Avg_Net_Income = mean(`Net Income`, na.rm = TRUE),
      Avg_Gross_Profit = mean(`Gross Profit`, na.rm = TRUE),
      Avg_Operating_Income = mean(`Operating Income`, na.rm = TRUE),
      Avg_EBIT = mean(EBIT, na.rm = TRUE)
    ) %>%
    arrange(desc(Avg_Revenue))
  
  top_10_stocks <- head(stock_time_analysis, 10)
  plot <- ggplot(top_10_stocks, aes(x = reorder(StockCode, -Avg_Revenue), y = Avg_Revenue)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme_minimal() +
    labs(title = paste("Top 10 Stocks by Income in", year), x = "Stock Code", y = "Average Revenue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(plot)
  
  return(top_10_stocks)
}

top_10_2014 <- analyse_top_stocks(df_2014, 2014)
top_10_2015 <- analyse_top_stocks(df_2015, 2015)
top_10_2016 <- analyse_top_stocks(df_2016, 2016)
top_10_2017 <- analyse_top_stocks(df_2017, 2017)
top_10_2018 <- analyse_top_stocks(df_2018, 2018)

top_10_2014
top_10_2015
top_10_2016
top_10_2017
top_10_2018

#### 4. Correlation between indicators
analyse_correlation <- function(df, year) {
  correlation_columns <- c("Revenue", "Net Income", "Gross Profit", "Operating Income", "EBIT")
  correlation_data <- df %>%
    select(all_of(correlation_columns)) %>%
    drop_na()
  
  correlation_matrix <- cor(correlation_data)
  print(correlation_matrix)
  
  corrplot(correlation_matrix, 
           method = "circle", 
           type = "full", 
           title = paste(year, "Financial Indicators Correlation"), 
           mar = c(0, 0, 2, 0),
           tl.col = "black",
           tl.cex = 0.8,
           addCoef.col = "white", 
           number.cex = 0.7) 
}

analyse_correlation(df_2014, 2014)
analyse_correlation(df_2015, 2015)
analyse_correlation(df_2016, 2016)
analyse_correlation(df_2017, 2017)
analyse_correlation(df_2018, 2018)