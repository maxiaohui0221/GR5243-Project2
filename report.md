# Project 2 Report: DataPilot Shiny Toolkit

## Overview

DataPilot Shiny Toolkit is an interactive R Shiny web application for loading, cleaning, transforming, and exploring datasets. The app was designed as a flexible workflow for users who may not have programming experience but still need to inspect and prepare data for analysis. It supports both uploaded files and built-in example datasets, so users can test the interface immediately and then reuse the same workflow on their own data.

The application covers all core requirements from the assignment. Users can upload `CSV`, `TSV/TXT`, `Excel`, `JSON`, and `RDS` files, or choose built-in datasets such as `airquality` and `iris`. The cleaning tab provides interactive preprocessing tools including column-name standardization, whitespace trimming, duplicate removal, missing-value handling, numeric scaling, outlier handling, and label encoding. The feature engineering tab allows users to create transformed, binned, interaction, ratio, date-based, and one-hot encoded features. The EDA tab includes interactive filtering, summary statistics, multiple plot types, and a correlation heatmap for numeric variables.

## How to Use the App

The app is organized as a workflow across tabs. Users begin on the `Data` tab by selecting either a built-in dataset or uploading their own file. After loading the data, the `Cleaning` tab updates automatically and lets the user choose preprocessing steps. The app immediately shows how many rows, columns, and missing cells remain after each choice, along with a transformation log and preview table. Next, the `Feature Engineering` tab allows the user to add new columns without modifying the original raw dataset. Finally, the `EDA` tab lets the user filter the current dataset, inspect summary statistics, and produce visualizations such as histograms, scatter plots, box plots, bar charts, and a correlation heatmap.

The interface includes a dedicated `Guide` tab so that first-time users can understand the workflow without reading external documentation. This improves usability and makes the app easier to present during grading or team demonstrations.

## Deployment and Team Contribution

The source code is deployment-ready for `shinyapps.io`, but this coding environment does not permit direct online deployment, so a live public link could not be created here. Manual deployment can be completed with the `rsconnect::deployApp()` workflow documented in the repository `README.md`.

Team contribution placeholder:

- Team member 1: application structure, data loading, cleaning workflow
- Team member 2: feature engineering and EDA interface
- Team member 3: report writing, README, testing, deployment


