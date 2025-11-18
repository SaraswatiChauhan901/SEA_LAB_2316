


##############################################################################
# Title: COVID-19 Data Visualization Script
# Description: This R script loads the 'country_wise_latest.csv' file,
# cleans the data, and generates 5 different plots (Bar, Pie, Stacked Bar,
# Line, Histogram) to analyze the pandemic's impact.

# --- 1. SETUP ---
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr) # For pivot_longer

theme_set(theme_minimal())

# --- 2. LOAD AND CLEAN DATA ---
tryCatch({
  
  # Load your CSV file (using your path)
  data <- read_csv("C:\\Users\\Saraswati\\Downloads\\country_wise_latest (1).csv")
  
  # Clean column names to make them R-friendly
  data_clean <- data %>%
    rename(
      Country = `Country/Region`,
      Deaths_per_100_Cases = `Deaths / 100 Cases`,
      Recovered_per_100_Cases = `Recovered / 100 Cases`,
      Deaths_per_100_Recovered = `Deaths / 100 Recovered`,
      Confirmed_last_week = `Confirmed last week`,
      One_week_change = `1 week change`,
      One_week_perc_increase = `1 week % increase`,
      WHO_Region = `WHO Region`,
      New_cases = `New cases`,
      New_deaths = `New deaths`,
      New_recovered = `New recovered`
    )
  
  print("Data loaded and cleaned successfully.")
  
  # --- 3. DATA PREPARATION FOR PLOTS ---
  # Get Top 10 countries by confirmed cases
  top_10_confirmed <- data_clean %>%
    arrange(desc(Confirmed)) %>%
    slice_head(n = 10) # Modern tidyverse replacement for top_n
  
  print("Prepared data for Top 10 Bar Chart.")
  
  # Summarize data by WHO Region
  region_summary <- data_clean %>%
    group_by(WHO_Region) %>%
    summarise(
      Total_Confirmed = sum(Confirmed, na.rm = TRUE),
      Total_Deaths = sum(Deaths, na.rm = TRUE),
      Total_Recovered = sum(Recovered, na.rm = TRUE),
      Total_Active = sum(Active, na.rm = TRUE)
    ) %>%
    mutate(
      Conf_Percentage = Total_Confirmed / sum(Total_Confirmed)
    )
  
  print("Prepared data for Regional Pie and Stacked Bar Charts.")
  
  # Data for Stacked Bar Chart (Case Breakdown)
  region_summary_long <- region_summary %>%
    select(WHO_Region, Total_Deaths, Total_Recovered, Total_Active) %>%
    pivot_longer(
      cols = -WHO_Region,
      names_to = "Case_Type",
      values_to = "Count"
    ) %>%
    mutate(Case_Type = str_replace(Case_Type, "Total_", ""))
  
  # Data for Line Chart (1-week growth for top 10)
  top_10_growth <- data_clean %>%
    arrange(desc(Confirmed)) %>%
    slice_head(n = 10) %>%
    select(Country, Confirmed_last_week, Confirmed) %>%
    pivot_longer(
      cols = -Country,
      names_to = "Time_Point",
      values_to = "Cases"
    ) %>%
    mutate(Time_Point = ifelse(Time_Point == "Confirmed", "This Week", "Last Week"))
  
  print("Prepared data for 1-Week Growth Line Chart.")
  
  # --- 4. GENERATE AND SAVE PLOTS ---
  
  # Plot 1: Bar Chart
  print("Generating Plot 1: Bar Chart...")
  p1 <- ggplot(top_10_confirmed, aes(x = reorder(Country, Confirmed), y = Confirmed, fill = Country)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = "Top 10 Countries by Total Confirmed Cases",
      x = "Country",
      y = "Total Confirmed Cases"
    ) +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme(legend.position = "none")
  
  ggsave("1_bar_chart_top10_confirmed.png", p1, width = 10, height = 6)
  
  # Plot 2: Pie Chart
  print("Generating Plot 2: Pie Chart...")
  p2 <- ggplot(region_summary, aes(x = "", y = Conf_Percentage, fill = WHO_Region)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(
      aes(label = scales::percent(Conf_Percentage, accuracy = 0.1)),
      position = position_stack(vjust = 0.5)
    ) +
    labs(
      title = "Proportion of Total Confirmed Cases by WHO Region",
      fill = "WHO Region"
    ) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  ggsave("2_pie_chart_region_confirmed.png", p2, width = 10, height = 7)
  
  # Plot 3: Stacked Bar Chart
  print("Generating Plot 3: Stacked Bar Chart...")
  p3 <- ggplot(region_summary_long, aes(x = reorder(WHO_Region, -Count), y = Count, fill = Case_Type)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = "Case Breakdown (Active, Deaths, Recovered) by WHO Region",
      x = "WHO Region",
      y = "Total Cases"
    ) +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("3_stacked_bar_chart_region_breakdown.png", p3, width = 11, height = 7)
  
  # Plot 4: Line Chart
  print("Generating Plot 4: Line Chart...")
  p4 <- ggplot(top_10_growth, aes(x = Time_Point, y = Cases, color = Country, group = Country)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "1-Week Confirmed Case Growth (Top 10 Countries)",
      x = "Time Point",
      y = "Confirmed Cases",
      color = "Country"
    ) +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  ggsave("4_line_chart_1week_growth.png", p4, width = 12, height = 7)
  
  # Plot 5: Histogram
  print("Generating Plot 5: Histogram...")
  p5 <- ggplot(data_clean, aes(x = Deaths_per_100_Cases)) +
    geom_histogram(
      binwidth = 1,
      fill = "#0072B2",
      color = "white",
      alpha = 0.8
    ) +
    labs(
      title = "Distribution of Case Fatality Rate (CFR)",
      subtitle = "CFR = Deaths per 100 Cases",
      x = "Case Fatality Rate (%)",
      y = "Number of Countries"
    ) +
    scale_x_continuous(breaks = seq(0, max(data_clean$Deaths_per_100_Cases, na.rm = TRUE), by = 2))
  
  ggsave("5_histogram_fatality_rate.png", p5, width = 10, height = 6)
  
  print("All visualizations have been generated and saved as PNG files in your working directory.")
  
}, error = function(e) {
  print("An error occurred. Please ensure 'C:/Users/Saraswati/Downloads/country_wise_latest.csv' is in the correct location and the CSV column names are correct.")
  print(paste("Error message:", e$message))
})



