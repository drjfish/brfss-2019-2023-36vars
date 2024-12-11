#########################
## Quick Intro

# This is a script to explore the Behavioral Risk Factor Surveillance System (BRFSS) data from 2019-2023.
# The script reads in the data, identifies the variables available across all years, and selects a refined list of variables to explore.
# It then calculates the number of non-NA rows for each variable and creates an R Shiny app to interactively explore the data.

# The source data_files used below are not included in this repository due to size constraints, but links to them are provided 
# at https://dataforpublichealth.com/public/post-details/wrangling-brfss-2011-2023 
#########################

#########################
## Determine what variables are available across all years of data (2011-2023)
#########################

# Import data
all_data <- data.frame(year = character(), variable = character(), stringsAsFactors = FALSE)

# Define the data files
data_files <- c("./data/2011_450.csv",
                "./data/2012_354.csv",
                "./data/2013_330.csv",
                "./data/2014_275.csv",
                "./data/2015_330.csv",
                "./data/2016_275.csv",
                "./data/2017_358.csv",
                "./data/2018_275.csv",
                "./data/2019_342.csv",
                "./data/2020_279.csv",
                "./data/2021_303.csv",
                "./data/2022_326.csv",
                "./data/2023_350.csv")

# Pull out the year and variable names from each file
for (file in data_files) {
  year <- substr(basename(file), 1, 4)
  data <- read.csv(file)
  variables <- colnames(data)
  temp_data <- data.frame(
    year = rep(year, length(variables)),
    variable = variables,
    stringsAsFactors = FALSE
  )
  all_data <- rbind(all_data, temp_data)
}

# Create a dataframe with every unique variable in all_data
unique_vars <- sort(unique(all_data$variable))
years <- 2011:2023

# Initialize the dataframe
vars <- data.frame(variable = unique_vars, stringsAsFactors = FALSE)

# Add columns for each year
for (year in years) {
  vars[[as.character(year)]] <- ""
}

# Populate the dataframe
for (i in 1:nrow(vars)) {
  var <- vars$variable[i]
  for (year in years) {
    if (any(all_data$variable == var & all_data$year == as.character(year))) {
      vars[i, as.character(year)] <- "x"
    }
  }
}

head(vars)

# Save the dataframe to a CSV file
write.csv(vars, "variable_coverage.csv", row.names = FALSE)

#########################
## Pull initial list of potential variables to explore (2019-2023)
#########################

# Import data
vars_of_interest <- read.csv("variable_coverage.csv")

# Keep variables that have an x in 2019-2023
vars_of_interest <- vars_of_interest[rowSums(vars_of_interest[, 10:14] == "x") > 4, ]

# Save variable column values to a list
vars_of_interest <- as.vector(vars_of_interest$variable)

# Load vars_of_interest from 2019-2023 BRFSS data
brfss_162_2019 <- read.csv("./data/2019_342.csv")[vars_of_interest]
brfss_162_2020 <- read.csv("./data/2020_279.csv")[vars_of_interest]
brfss_162_2021 <- read.csv("./data/2021_303.csv")[vars_of_interest]
brfss_162_2022 <- read.csv("./data/2022_326.csv")[vars_of_interest]
brfss_162_2023 <- read.csv("./data/2023_350.csv")[vars_of_interest]

# Add a year column to each dataframe
brfss_162_2019$year <- 2019
brfss_162_2020$year <- 2020
brfss_162_2021$year <- 2021
brfss_162_2022$year <- 2022
brfss_162_2023$year <- 2023

# Combine the dataframes
brfss_162_2019to2023 <- rbind(brfss_162_2019, brfss_162_2020, brfss_162_2021, brfss_162_2022, brfss_162_2023)

# Save the combined dataframe to a CSV file
write.csv(brfss_162_2019to2023, "brfss_162_2019to2023.csv", row.names = FALSE)


#########################
## Define refined list of variables to explore (2019-2023) and calculate the number 
## of non-NA rows for each variable
#########################

# Define refined list of variables to explore (2019-2023)
refined_vars_of_interest <- c(
  # Population segment vars (5)
  "x_AGEG5YR", "SEXVAR", "x_EDUCAG", "x_STATE", "x_URBSTAT",
  
  # Selected BRFSS survey question vars (2019-2023)
  "ADDEPEV3", "ASTHMA3", "ASTHNOW", "BLIND", "CAREGIV1", "CHCKDNY2",
  "CRGVEXPT", "CVDCRHD4", "CVDINFR4", "CVDSTRK3", "DEAF", "DECIDE",
  "DIABETE4", "DIFFALON", "DIFFDRES", "DIFFWALK", "EMPLOY1", "FLUSHOT7",
  "GENHLTH", "INSULIN1", "MARITAL", "PNEUVAC4", "RENTHOM1", "SHINGLE2",
  "TRNSGNDR", "USENOW3", "VETERAN3", "x_ASTHMS1", "x_BMI5CAT",
  "x_SMOKER3", "x_TOTINDA",

  # Keep the year variable, to delineate data by year
  "year"
)

# Load the combined dataframe
brfss_36_2019to2023 <- read.csv("brfss_162_2019to2023.csv")[refined_vars_of_interest]

# Print first 5 rows of the data
head(brfss_36_2019to2023)

# Save the refined dataframe to a CSV file (for R Shiny App)
write.csv(brfss_36_2019to2023, ".data/brfss_36_2019to2023.csv", row.names = FALSE)

# Quick Exploratory Data Analysis (EDA) - Calculate the number of non-NA rows for each variable
# create a new dataframe with a) variable name, b) number of non-NA rows, c) percent of non-NA rows
summary_df <- data.frame(
  variable = character(),
  non_na_count = numeric(),
  percent_non_na = numeric(),
  stringsAsFactors = FALSE
)

# Calculate the number of non-NA rows and the percentage of non-NA rows for each variable
for (var in refined_vars_of_interest) {
  non_na_count <- sum(!is.na(brfss_36_2019to2023[[var]]))
  percent_non_na <- non_na_count / nrow(brfss_36_2019to2023) * 100
  summary_df <- rbind(summary_df, data.frame(variable = var, non_na_count = non_na_count, percent_non_na = percent_non_na))
}

# Sort the summary dataframe by the percentage of non-NA rows
summary_df <- summary_df[order(summary_df$percent_non_na, decreasing = TRUE), ]

# Print the summary dataframe
print(summary_df)

#########################
## Explore the data using an R Shiny App (2019-2023)
## 36 Total Variables: 5 Population Segment Vars + 31 Selected BRFSS Survey Question Vars
#########################

# Load necessary libraries
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(rlang)
library(DT)

# Load data and label map
data <- read.csv("./data/brfss_36_2019to2023.csv") # Can be built from previous steps using base files from link at top
variable_map <- read.csv("./data/selected.csv")
label_map <- readRDS("./data/BRFSS_label_map_2011-2023.rds")

# Create combined variable:question labels for the dropdown
variable_map <- variable_map %>%
  mutate(label = paste(variable, ": ", question, sep = ""))

# Helper function to translate and sort filter values
translate_and_sort <- function(variable, data, label_map, ascending = TRUE) {
  if (variable %in% names(label_map)) {
    labels <- label_map[[variable]]
    sorted_values <- factor(unique(data[[variable]]), levels = names(labels), labels = labels)
    if (ascending) {
      return(sort(sorted_values))
    } else {
      return(sort(sorted_values, decreasing = TRUE))
    }
  } else {
    if (ascending) {
      return(sort(unique(data[[variable]])))
    } else {
      return(sort(unique(data[[variable]]), decreasing = TRUE))
    }
  }
}

# Define UI
ui <- fluidPage(
  tags$style(
    HTML("
      body {
        margin-right: 20px;
      }
    ")
  ),
  titlePanel("BRFSS Data Explorer: 2019-2023"),
  
  # Full-width variable dropdown
  fluidRow(
    column(
      width = 12,
      pickerInput(
        inputId = "selected_variable",
        label = "Select Variable to Explore:",
        choices = setNames(variable_map$variable, variable_map$label),
        selected = "ADDEPEV3",
        width = "100%"
      )
    )
  ),
  
  # Filters and visualizations
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "year_filter",
        label = "Select Year(s):",
        choices = sort(unique(data$year), decreasing = TRUE),
        selected = sort(unique(data$year), decreasing = TRUE), # Default to all years
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE) # Add Select All/Unselect All and Search
      ),
      pickerInput(
        inputId = "age_filter",
        label = "Select Age Group(s):",
        choices = translate_and_sort("x_AGEG5YR", data, label_map, ascending = TRUE), # Sort ascending
        selected = translate_and_sort("x_AGEG5YR", data, label_map, ascending = TRUE), # Default to all
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE) # Add Select All/Unselect All and Search
      ),
      pickerInput(
        inputId = "sex_filter",
        label = "Select Sex:",
        choices = translate_and_sort("SEXVAR", data, label_map),
        selected = translate_and_sort("SEXVAR", data, label_map), # Default to all
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE) # Add Select All/Unselect All and Search
      ),
      pickerInput(
        inputId = "education_filter",
        label = "Select Educational Attainment:",
        choices = translate_and_sort("x_EDUCAG", data, label_map),
        selected = translate_and_sort("x_EDUCAG", data, label_map), # Default to all
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE) # Add Select All/Unselect All and Search
      ),
      pickerInput(
        inputId = "state_filter",
        label = "Select State(s):",
        choices = translate_and_sort("x_STATE", data, label_map, ascending = TRUE), # Sort ascending
        selected = translate_and_sort("x_STATE", data, label_map, ascending = TRUE), # Default to all
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE) # Add Select All/Unselect All and Search
      ),
      pickerInput(
        inputId = "urban_filter",
        label = "Select Urban/Rural Status:",
        choices = translate_and_sort("x_URBSTAT", data, label_map),
        selected = translate_and_sort("x_URBSTAT", data, label_map), # Default to all
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE) # Add Select All/Unselect All and Search
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summary_table")),
        tabPanel("Bar Plot", plotlyOutput("bar_plot")),
        tabPanel("Trend Over Time", plotlyOutput("trend_plot"))
      ),
      hr(),
      fluidRow(
        column(
          width = 12,
          h4(textOutput("selected_variable_label")),
          p(textOutput("selected_variable_question"))
        )
      )
    )
  )
)

# Helper function to reverse map labels back to numeric values
reverse_map <- function(input_values, variable, label_map) {
  if (variable %in% names(label_map)) {
    labels <- label_map[[variable]]
    # Find the numeric values corresponding to the input labels
    numeric_values <- names(labels)[match(input_values, labels)]
    return(as.numeric(numeric_values))
  } else {
    return(as.numeric(input_values))
  }
}

# Define Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    # Reverse map labels to numeric values for filtering
    age_values <- reverse_map(input$age_filter, "x_AGEG5YR", label_map)
    sex_values <- reverse_map(input$sex_filter, "SEXVAR", label_map)
    education_values <- reverse_map(input$education_filter, "x_EDUCAG", label_map)
    state_values <- reverse_map(input$state_filter, "x_STATE", label_map)
    urban_values <- reverse_map(input$urban_filter, "x_URBSTAT", label_map)
    
    result <- data %>%
      filter(
        year %in% as.numeric(input$year_filter), # Convert to numeric
        x_AGEG5YR %in% age_values, # Use reverse mapped numeric values
        SEXVAR %in% sex_values, # Use reverse mapped numeric values
        x_EDUCAG %in% education_values, # Use reverse mapped numeric values
        x_STATE %in% state_values, # Use reverse mapped numeric values
        x_URBSTAT %in% urban_values # Use reverse mapped numeric values
      )
    result
  })
  
  # Translate numeric values to labels
  translated_data <- reactive({
    selected_var <- input$selected_variable
    if (selected_var %in% names(label_map)) {
      labels <- label_map[[selected_var]]
      result <- filtered_data() %>%
        mutate(!!selected_var := factor(get(selected_var), levels = names(labels), labels = labels))
      result
    } else {
      print("No Translation Needed")
      filtered_data()
    }
  })
  
  output$summary_table <- renderDT({
    # Filter out NA values for the selected variable
    filtered_data <- translated_data() %>%
      filter(!is.na(!!sym(input$selected_variable))) 

    # Group by the selected variable and calculate counts and percentages
    result <- filtered_data %>%
      group_by_at(vars(input$selected_variable)) %>%
      summarise(
        Count = n(),
        .groups = "drop"
      ) %>%
      mutate(`% of Total` = paste0(round((Count / sum(Count)) * 100, 2), "%")) # Append '%' to percentages

    # Render the DataTable
    datatable(result, options = list(pageLength = 10))
  })
  
  output$bar_plot <- renderPlotly({
    gg <- ggplot(translated_data(), aes(x = !!sym(input$selected_variable))) +
      geom_bar(fill = "blue") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) + # Add commas to the y-axis numbers
      labs(title = "Bar Plot", x = input$selected_variable, y = "Count")
    ggplotly(gg)
  })

  output$trend_plot <- renderPlotly({
    trend_data <- translated_data() %>%
      group_by(year, !!sym(input$selected_variable)) %>%
      summarise(Count = n(), .groups = "drop")
    
    gg <- ggplot(trend_data, aes(x = year, y = Count, color = as.factor(!!sym(input$selected_variable)))) +
      geom_line(linewidth = 1.2) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) + # Use scales::comma to format axis numbers
      labs(title = "Trend Over Time", x = "Year", y = "Count", color = input$selected_variable)
    ggplotly(gg)
  })

}

# Run the application
shinyApp(ui = ui, server = server)