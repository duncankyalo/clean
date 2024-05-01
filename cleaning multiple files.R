# Load necessary libraries
library(tidyverse)  # For data manipulation and analysis
library(readr)    # For reading CSV files

# Set working directory (replace with your actual path)
setwd("E:/R/climate")

# Get the current working directory
working_dir <- getwd()

# List files in the working directory
file_list <- list.files(working_dir)

# Print the file list
print(file_list)

# Read each CSV file into a separate data frame
df1 <- read_csv("climate governance data.csv")
df2 <- read_csv("rain_data.csv")
df3 <- read_csv("temp_data.csv")

#DATA CLEANING
# Load libraries
library(tidyverse)
library(readr)


# Function to clean data frames
clean_data <- function(df) {
  # 1. Check structure and data types
  str(df)
  
  # 2. Handle missing values (replace with mean)
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
    }
  }
  
  # 3. Standardize text data (lowercase)
  df <- mutate_if(df, is.character, tolower)
  
  # 4. Ensure date/time columns are in the correct format (if applicable)
  # Adapt the format based on your data
  date_cols <- c("date_column1", "date_column2")  # Replace with actual date column names
  df <- mutate_at(date_cols, ~ as.Date(., format = "%Y-%m-%d"))
  
  # 5. Handle outliers (replace with median)
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) {
      outliers <- boxplot(df[[col]], plot = FALSE)$out
      df[[col]][df[[col]] %in% outliers] <- median(df[[col]], na.rm = TRUE)
    }
  }
  
  return(df)
}

# Read and clean each data frame
df1 <- clean_data(read_csv("climate_governance_data.csv"))
df2 <- clean_data(read_csv("rain_data.csv"))
df3 <- clean_data(read_csv("temp_data.csv"))

# Arrange rows (optional, depending on your sorting criteria)
df1 <- arrange(df1, column_to_sort_by)  # Replace with actual column name
df2 <- arrange(df2, column_to_sort_by)
df3 <- arrange(df3, column_to_sort_by)