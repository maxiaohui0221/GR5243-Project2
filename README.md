# Project 2: DataPilot Shiny Toolkit

This repository contains an R Shiny web application for uploading, cleaning, transforming, and exploring datasets interactively. The app was designed to match the Project 2 requirements for dataset loading, preprocessing, feature engineering, EDA, and a user-friendly interface.

## Files

- `app.R`: main Shiny application
- `report.md`: short project report describing the app and how to use it
- `Project 2.pdf`: assignment prompt

## Features

- Upload data in `CSV`, `TSV/TXT`, `Excel`, `JSON`, and `RDS` formats
- Use built-in sample datasets: `airquality` and `iris`
- Clean data with options for:
  - standardizing column names
  - trimming whitespace
  - removing duplicates
  - imputing or removing missing values
  - scaling numeric variables
  - handling outliers
  - label encoding categorical variables
- Engineer new features with:
  - log transforms
  - square-root transforms
  - binning
  - interaction terms
  - ratio features
  - date-part extraction
  - one-hot encoding
  - delete controls for removing engineered columns
- Explore data with:
  - interactive filters
  - histograms
  - scatter plots
  - box plots
  - bar charts
  - correlation heatmaps
  - summary statistics tables
- Download the engineered dataset as CSV

## How to Run

1. Open R or RStudio in this project directory.
2. Install any missing packages if needed:

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "readxl", "jsonlite"))
```

3. Launch the app:

```r
shiny::runApp("app.R")
```

## Suggested Demo Flow

1. Start with the built-in `airquality` dataset or upload a tabular dataset in one of the supported formats.
2. On the `Cleaning` tab, remove missing values or apply imputation and scaling.
3. On the `Feature Engineering` tab, create a transformed or ratio-based feature.
4. On the `EDA` tab, filter the dataset and generate plots.
5. Switch to `iris` to demonstrate categorical exploration and one-hot encoding.

## Deployment

This environment does not allow direct online deployment, so the repository includes a deployment-ready Shiny app but not a live hosted URL.

To deploy manually to [shinyapps.io](https://www.shinyapps.io/), run:

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "YOUR_ACCOUNT_NAME", token = "YOUR_TOKEN", secret = "YOUR_SECRET")
rsconnect::deployApp(appDir = ".")
```

## Notes

- The app is designed to work with a wide variety of small-to-medium tabular datasets.
- Some feature engineering actions require compatible column types, such as numeric columns for ratio features and date columns for date-part extraction.
