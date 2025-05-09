# ALPB Analytics Platform

A Shiny application that provides detailed analytics for Atlantic League Professional Baseball (ALPB) players based on count, including pitch outcomes, contact rates, and performance metrics.

## Features

- Search and analyze player data by name
- Filter data by count (0-0, 1-2, 3-2, etc.)
- Visualization of batting and pitching metrics
- Date-based filtering for time-series analysis
- Downloadable PDF reports with selected and general filter

## Setup

1. Clone this repository
2. Create a `config.R` file based on `config.dummy.R` with your actual API credentials
3. Install required R packages:
   ```r
   install.packages(c("shiny", "shinyjs", "dplyr", "ggplot2", "tidyr", "httr", 
                     "jsonlite", "shinyWidgets", "RColorBrewer", "shinythemes", 
                     "knitr", "rmarkdown", "plotly"))
