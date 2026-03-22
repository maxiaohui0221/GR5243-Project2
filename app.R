library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readxl)
library(jsonlite)

builtin_datasets <- list(
  "airquality" = datasets::airquality,
  "iris" = datasets::iris
)

safe_names <- function(x) {
  x <- trimws(x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  make.names(x, unique = TRUE)
}

mode_value <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) {
    return(NA)
  }
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

min_max_scale <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / diff(rng)
}

cap_outliers <- function(x) {
  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  pmin(pmax(x, lower), upper)
}

remove_outlier_rows <- function(df, cols) {
  keep <- rep(TRUE, nrow(df))
  for (col in cols) {
    x <- df[[col]]
    if (!is.numeric(x)) {
      next
    }
    q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
    q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    keep <- keep & (is.na(x) | (x >= lower & x <= upper))
  }
  df[keep, , drop = FALSE]
}

coerce_dates <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct")) {
    return(x)
  }
  if (!is.character(x)) {
    return(x)
  }
  parsed <- suppressWarnings(as.Date(x))
  share_parsed <- mean(!is.na(parsed))
  if (is.nan(share_parsed) || share_parsed < 0.8) {
    return(x)
  }
  parsed
}

read_uploaded_data <- function(path, ext) {
  ext <- tolower(ext)
  if (ext == "csv") {
    return(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (ext %in% c("tsv", "txt")) {
    return(read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (ext %in% c("xls", "xlsx")) {
    return(readxl::read_excel(path) |> as.data.frame(check.names = FALSE))
  }
  if (ext == "json") {
    payload <- jsonlite::fromJSON(path, flatten = TRUE)
    if (is.data.frame(payload)) {
      return(payload)
    }
    if (is.list(payload)) {
      data_frame_index <- which(vapply(payload, is.data.frame, logical(1)))[1]
      if (!is.na(data_frame_index)) {
        return(payload[[data_frame_index]])
      }
      return(as.data.frame(payload, check.names = FALSE))
    }
  }
  if (ext == "rds") {
    payload <- readRDS(path)
    if (is.data.frame(payload)) {
      return(payload)
    }
    return(as.data.frame(payload, check.names = FALSE))
  }
  stop("Unsupported file type. Please upload CSV, TSV/TXT, Excel, JSON, or RDS.")
}

preview_table <- function(df, n = 10) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame(Message = "No rows available to display."))
  }
  utils::head(df, n)
}

ui <- page_navbar(
  title = "DataPilot Shiny Toolkit",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1b4965"),
  nav_panel(
    "Guide",
    layout_column_wrap(
      width = 1/2,
      card(
        full_screen = TRUE,
        card_header("Project Summary"),
        p("This Shiny app helps users upload their own dataset, or start with a built-in sample dataset, then clean it, engineer features, and explore it interactively."),
        tags$ol(
          tags$li("Pick a built-in sample dataset or upload your own file in CSV, TSV/TXT, Excel, JSON, or RDS format."),
          tags$li("Use the Cleaning tab to standardize names, remove duplicates, handle missing values, scale features, and manage outliers."),
          tags$li("Use the Feature Engineering tab to add transformed, binned, interaction, ratio, date-part, or one-hot encoded variables."),
          tags$li("Use the EDA tab to filter data, inspect summaries, and build interactive plots.")
        )
      ),
      card(
        card_header("Built-In Datasets"),
        tags$ul(
          tags$li(tags$b("airquality"), ": includes missing values and numeric features, which makes it useful for demonstrating cleaning and preprocessing."),
          tags$li(tags$b("iris"), ": includes both numeric and categorical variables, which makes it useful for feature engineering and visualization.")
        ),
        p("These examples are included for users who want to test the app before uploading their own data.")
      ),
      card(
        card_header("Assignment Coverage"),
        tags$ul(
          tags$li("Dataset loading with multiple formats and built-in examples"),
          tags$li("Interactive cleaning and preprocessing with instant feedback"),
          tags$li("Feature engineering tools with new-column previews"),
          tags$li("Exploratory analysis with filters, plots, summaries, and correlations"),
          tags$li("User guide and responsive multi-tab interface")
        )
      ),
      card(
        card_header("Recommended Workflow"),
        p("Choose your data source on the Data tab first. Each later tab updates automatically from the previous stage so users can immediately see the impact of each transformation.")
      )
    )
  ),
  nav_panel(
    "Data",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("source_type", "Data source", choices = c("Built-in dataset", "Upload file")),
        conditionalPanel(
          condition = "input.source_type === 'Built-in dataset'",
          selectInput("builtin_name", "Choose a sample dataset", choices = names(builtin_datasets))
        ),
        conditionalPanel(
          condition = "input.source_type === 'Upload file'",
          fileInput("upload_file", "Upload dataset", accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".json", ".rds")),
          helpText("Supported formats: CSV, TSV/TXT, Excel, JSON, and RDS.")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Dataset Overview"),
        layout_column_wrap(
          width = 1/3,
          value_box(title = "Rows", value = textOutput("raw_rows")),
          value_box(title = "Columns", value = textOutput("raw_cols")),
          value_box(title = "Missing Cells", value = textOutput("raw_missing"))
        ),
        h5("Column types"),
        tableOutput("raw_types"),
        h5("Preview"),
        tableOutput("raw_preview")
      )
    )
  ),
  nav_panel(
    "Cleaning",
    layout_sidebar(
      sidebar = sidebar(
        checkboxInput("standardize_names", "Standardize column names", TRUE),
        checkboxInput("trim_whitespace", "Trim whitespace in character columns", TRUE),
        checkboxInput("coerce_date_columns", "Auto-detect date-like columns", FALSE),
        checkboxInput("remove_duplicates", "Remove duplicate rows", FALSE),
        selectInput(
          "numeric_missing",
          "Numeric missing values",
          choices = c("None", "Remove rows", "Median imputation", "Mean imputation", "Replace with 0")
        ),
        selectInput(
          "categorical_missing",
          "Categorical missing values",
          choices = c("None", "Remove rows", "Mode imputation", "Replace with 'Missing'")
        ),
        selectInput("scaling_method", "Scaling", choices = c("None", "Z-score", "Min-Max")),
        uiOutput("scaling_columns_ui"),
        selectInput("outlier_method", "Outlier handling", choices = c("None", "Cap with IQR bounds", "Remove rows with outliers")),
        uiOutput("outlier_columns_ui"),
        selectInput("encoding_method", "Categorical encoding", choices = c("None", "Label encode")),
        uiOutput("encoding_columns_ui")
      ),
      card(
        full_screen = TRUE,
        card_header("Cleaning Results"),
        layout_column_wrap(
          width = 1/3,
          value_box(title = "Rows After Cleaning", value = textOutput("clean_rows")),
          value_box(title = "Columns After Cleaning", value = textOutput("clean_cols")),
          value_box(title = "Remaining Missing Cells", value = textOutput("clean_missing"))
        ),
        h5("Transformation log"),
        verbatimTextOutput("clean_log"),
        h5("Preview"),
        tableOutput("clean_preview")
      )
    )
  ),
  nav_panel(
    "Feature Engineering",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "feature_action",
          "Feature engineering action",
          choices = c(
            "None",
            "Log transform",
            "Square root transform",
            "Binning",
            "Interaction term",
            "Ratio feature",
            "Date parts",
            "One-hot encode"
          )
        ),
        uiOutput("feature_controls"),
        textInput("new_feature_name", "New feature name", value = "new_feature"),
        actionButton("add_feature", "Add feature"),
        uiOutput("delete_feature_ui"),
        actionButton("delete_feature", "Delete selected features"),
        helpText("If you change cleaning settings, the engineered dataset resets to the latest cleaned data.")
      ),
      card(
        full_screen = TRUE,
        card_header("Engineered Dataset"),
        layout_column_wrap(
          width = 1/3,
          value_box(title = "Rows", value = textOutput("engineered_rows")),
          value_box(title = "Columns", value = textOutput("engineered_cols")),
          value_box(title = "Most Recent Change", value = textOutput("feature_message"))
        ),
        h5("Preview"),
        tableOutput("engineered_preview"),
        downloadButton("download_engineered", "Download engineered dataset")
      )
    )
  ),
  nav_panel(
    "EDA",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("filter_column", "Filter column", choices = "None"),
        uiOutput("filter_control"),
        selectInput(
          "plot_type",
          "Plot type",
          choices = c("Histogram", "Scatter plot", "Box plot", "Bar chart", "Correlation heatmap")
        ),
        uiOutput("plot_x_ui"),
        uiOutput("plot_y_ui"),
        uiOutput("plot_color_ui")
      ),
      layout_column_wrap(
        width = 1,
        card(
          full_screen = TRUE,
          card_header("Interactive EDA Plot"),
          div(
            style = "min-height: 620px; padding-bottom: 1rem;",
            plotOutput("eda_plot", height = "600px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Summary Statistics"),
          tableOutput("eda_summary")
        ),
        card(
          full_screen = TRUE,
          card_header("Filtered Data Preview"),
          tableOutput("eda_preview")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    if (identical(input$source_type, "Built-in dataset")) {
      return(as.data.frame(builtin_datasets[[input$builtin_name]], check.names = FALSE))
    }

    req(input$upload_file)
    ext <- tools::file_ext(input$upload_file$name)
    df <- read_uploaded_data(input$upload_file$datapath, ext)
    as.data.frame(df, check.names = FALSE)
  })

  output$raw_rows <- renderText({
    nrow(raw_data())
  })

  output$raw_cols <- renderText({
    ncol(raw_data())
  })

  output$raw_missing <- renderText({
    sum(is.na(raw_data()))
  })

  output$raw_types <- renderTable({
    df <- raw_data()
    data.frame(
      Column = names(df),
      Type = vapply(df, function(col) paste(class(col), collapse = ", "), character(1)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$raw_preview <- renderTable({
    preview_table(raw_data())
  }, striped = TRUE, bordered = TRUE, width = "100%")

  cleaned_result <- reactive({
    df <- raw_data()
    log_entries <- c()

    if (isTRUE(input$standardize_names)) {
      names(df) <- safe_names(names(df))
      log_entries <- c(log_entries, "Standardized column names.")
    }

    if (isTRUE(input$trim_whitespace)) {
      char_cols <- names(df)[vapply(df, is.character, logical(1))]
      if (length(char_cols)) {
        df[char_cols] <- lapply(df[char_cols], trimws)
        log_entries <- c(log_entries, "Trimmed whitespace in character columns.")
      }
    }

    if (isTRUE(input$coerce_date_columns)) {
      df[] <- lapply(df, coerce_dates)
      log_entries <- c(log_entries, "Converted date-like character columns where possible.")
    }

    if (isTRUE(input$remove_duplicates)) {
      before <- nrow(df)
      df <- unique(df)
      log_entries <- c(log_entries, paste0("Removed ", before - nrow(df), " duplicate rows."))
    }

    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    categorical_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]

    if (input$numeric_missing == "Remove rows" && length(numeric_cols)) {
      before <- nrow(df)
      keep <- stats::complete.cases(df[numeric_cols])
      df <- df[keep, , drop = FALSE]
      log_entries <- c(log_entries, paste0("Removed ", before - nrow(df), " rows with numeric missing values."))
    } else if (input$numeric_missing %in% c("Median imputation", "Mean imputation", "Replace with 0")) {
      for (col in numeric_cols) {
        x <- df[[col]]
        if (!anyNA(x)) {
          next
        }
        fill_value <- switch(
          input$numeric_missing,
          "Median imputation" = stats::median(x, na.rm = TRUE),
          "Mean imputation" = mean(x, na.rm = TRUE),
          "Replace with 0" = 0
        )
        if (is.na(fill_value)) {
          fill_value <- 0
        }
        x[is.na(x)] <- fill_value
        df[[col]] <- x
      }
      log_entries <- c(log_entries, paste("Applied", input$numeric_missing, "to numeric columns."))
    }

    if (input$categorical_missing == "Remove rows" && length(categorical_cols)) {
      before <- nrow(df)
      keep <- stats::complete.cases(df[categorical_cols])
      df <- df[keep, , drop = FALSE]
      log_entries <- c(log_entries, paste0("Removed ", before - nrow(df), " rows with categorical missing values."))
    } else if (input$categorical_missing %in% c("Mode imputation", "Replace with 'Missing'")) {
      for (col in categorical_cols) {
        x <- as.character(df[[col]])
        if (!anyNA(x) && !any(x == "")) {
          next
        }
        fill_value <- if (input$categorical_missing == "Mode imputation") mode_value(x) else "Missing"
        x[is.na(x) | x == ""] <- fill_value
        df[[col]] <- x
      }
      log_entries <- c(log_entries, paste("Applied", input$categorical_missing, "to categorical columns."))
    }

    if (input$scaling_method != "None" && length(input$scaling_columns)) {
      for (col in input$scaling_columns) {
        if (!is.numeric(df[[col]])) {
          next
        }
        df[[col]] <- if (input$scaling_method == "Z-score") {
          as.numeric(scale(df[[col]]))
        } else {
          min_max_scale(df[[col]])
        }
      }
      log_entries <- c(log_entries, paste("Applied", input$scaling_method, "scaling to:", paste(input$scaling_columns, collapse = ", ")))
    }

    if (input$outlier_method == "Cap with IQR bounds" && length(input$outlier_columns)) {
      for (col in input$outlier_columns) {
        if (is.numeric(df[[col]])) {
          df[[col]] <- cap_outliers(df[[col]])
        }
      }
      log_entries <- c(log_entries, paste("Capped outliers in:", paste(input$outlier_columns, collapse = ", ")))
    } else if (input$outlier_method == "Remove rows with outliers" && length(input$outlier_columns)) {
      before <- nrow(df)
      df <- remove_outlier_rows(df, input$outlier_columns)
      log_entries <- c(log_entries, paste0("Removed ", before - nrow(df), " rows with outliers."))
    }

    if (input$encoding_method == "Label encode" && length(input$encoding_columns)) {
      for (col in input$encoding_columns) {
        df[[paste0(col, "_encoded")]] <- as.integer(factor(df[[col]]))
      }
      log_entries <- c(log_entries, paste("Label encoded:", paste(input$encoding_columns, collapse = ", ")))
    }

    if (!length(log_entries)) {
      log_entries <- "No cleaning transformation selected."
    }

    list(data = df, log = log_entries)
  })

  cleaned_data <- reactive(cleaned_result()$data)

  observe({
    df <- cleaned_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    categorical_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]

    updateCheckboxGroupInput(session, "scaling_columns", choices = numeric_cols, selected = numeric_cols)
    updateCheckboxGroupInput(session, "outlier_columns", choices = numeric_cols, selected = numeric_cols)
    updateCheckboxGroupInput(session, "encoding_columns", choices = categorical_cols, selected = categorical_cols)
  })

  output$scaling_columns_ui <- renderUI({
    df <- cleaned_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    checkboxGroupInput("scaling_columns", "Numeric columns to scale", choices = numeric_cols, selected = numeric_cols)
  })

  output$outlier_columns_ui <- renderUI({
    df <- cleaned_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    checkboxGroupInput("outlier_columns", "Numeric columns for outlier handling", choices = numeric_cols, selected = numeric_cols)
  })

  output$encoding_columns_ui <- renderUI({
    df <- cleaned_data()
    categorical_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]
    checkboxGroupInput("encoding_columns", "Categorical columns to encode", choices = categorical_cols, selected = categorical_cols)
  })

  output$clean_rows <- renderText({
    nrow(cleaned_data())
  })

  output$clean_cols <- renderText({
    ncol(cleaned_data())
  })

  output$clean_missing <- renderText({
    sum(is.na(cleaned_data()))
  })

  output$clean_log <- renderText({
    paste(cleaned_result()$log, collapse = "\n")
  })

  output$clean_preview <- renderTable({
    preview_table(cleaned_data())
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$feature_controls <- renderUI({
    df <- cleaned_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    date_cols <- names(df)[vapply(df, inherits, logical(1), what = "Date")]
    categorical_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]

    switch(
      input$feature_action,
      "Log transform" = tagList(
        selectInput("feature_num_col", "Numeric column", choices = numeric_cols),
        helpText("Creates log1p(x), which safely handles zeros.")
      ),
      "Square root transform" = tagList(
        selectInput("feature_num_col", "Numeric column", choices = numeric_cols),
        helpText("Creates sqrt(pmax(x, 0)) to avoid invalid values.")
      ),
      "Binning" = tagList(
        selectInput("feature_num_col", "Numeric column", choices = numeric_cols),
        numericInput("bin_count", "Number of bins", value = 4, min = 2, max = 20)
      ),
      "Interaction term" = tagList(
        selectInput("feature_num_col", "First numeric column", choices = numeric_cols),
        selectInput("feature_num_col_2", "Second numeric column", choices = numeric_cols)
      ),
      "Ratio feature" = tagList(
        selectInput("feature_num_col", "Numerator column", choices = numeric_cols),
        selectInput("feature_num_col_2", "Denominator column", choices = numeric_cols)
      ),
      "Date parts" = tagList(
        selectInput("feature_date_col", "Date column", choices = date_cols)
      ),
      "One-hot encode" = tagList(
        selectInput("feature_cat_col", "Categorical column", choices = categorical_cols)
      ),
      helpText("Select a feature action to generate new columns.")
    )
  })

  engineered_state <- reactiveValues(
    data = NULL,
    engineered_cols = character(0),
    message = "No engineered feature created."
  )

  observeEvent(cleaned_data(), {
    engineered_state$data <- cleaned_data()
    engineered_state$engineered_cols <- character(0)
    engineered_state$message <- "Engineered dataset reset from the latest cleaning step."
  })

  observeEvent(input$add_feature, {
    req(engineered_state$data)
    df <- engineered_state$data
    new_cols <- character(0)

    if (input$feature_action == "None") {
      engineered_state$message <- "Choose a feature action before adding a feature."
      return()
    }

    new_name <- safe_names(input$new_feature_name)
    if (!nzchar(new_name)) {
      new_name <- "new_feature"
    }

    if (input$feature_action == "Log transform") {
      if (is.null(input$feature_num_col) || !nzchar(input$feature_num_col) || !input$feature_num_col %in% names(df)) {
        engineered_state$message <- "Select a numeric column before adding the log-transformed feature."
        return()
      }
      df[[new_name]] <- log1p(pmax(df[[input$feature_num_col]], 0))
      new_cols <- new_name
      engineered_state$message <- paste("Created log-transformed feature from", input$feature_num_col)
    } else if (input$feature_action == "Square root transform") {
      if (is.null(input$feature_num_col) || !nzchar(input$feature_num_col) || !input$feature_num_col %in% names(df)) {
        engineered_state$message <- "Select a numeric column before adding the square-root feature."
        return()
      }
      df[[new_name]] <- sqrt(pmax(df[[input$feature_num_col]], 0))
      new_cols <- new_name
      engineered_state$message <- paste("Created square-root feature from", input$feature_num_col)
    } else if (input$feature_action == "Binning") {
      if (is.null(input$feature_num_col) || !nzchar(input$feature_num_col) || !input$feature_num_col %in% names(df)) {
        engineered_state$message <- "Select a numeric column before creating bins."
        return()
      }
      bins <- max(2, input$bin_count)
      df[[new_name]] <- cut(df[[input$feature_num_col]], breaks = bins, include.lowest = TRUE)
      new_cols <- new_name
      engineered_state$message <- paste("Created binned feature from", input$feature_num_col)
    } else if (input$feature_action == "Interaction term") {
      if (is.null(input$feature_num_col) || is.null(input$feature_num_col_2) ||
          !nzchar(input$feature_num_col) || !nzchar(input$feature_num_col_2) ||
          !input$feature_num_col %in% names(df) || !input$feature_num_col_2 %in% names(df)) {
        engineered_state$message <- "Select two numeric columns before creating an interaction term."
        return()
      }
      df[[new_name]] <- df[[input$feature_num_col]] * df[[input$feature_num_col_2]]
      new_cols <- new_name
      engineered_state$message <- paste("Created interaction term from", input$feature_num_col, "and", input$feature_num_col_2)
    } else if (input$feature_action == "Ratio feature") {
      if (is.null(input$feature_num_col) || is.null(input$feature_num_col_2) ||
          !nzchar(input$feature_num_col) || !nzchar(input$feature_num_col_2) ||
          !input$feature_num_col %in% names(df) || !input$feature_num_col_2 %in% names(df)) {
        engineered_state$message <- "Select numerator and denominator columns before creating a ratio feature."
        return()
      }
      denominator <- ifelse(df[[input$feature_num_col_2]] == 0, NA, df[[input$feature_num_col_2]])
      df[[new_name]] <- df[[input$feature_num_col]] / denominator
      new_cols <- new_name
      engineered_state$message <- paste("Created ratio feature from", input$feature_num_col, "and", input$feature_num_col_2)
    } else if (input$feature_action == "Date parts") {
      if (is.null(input$feature_date_col) || !nzchar(input$feature_date_col) || !input$feature_date_col %in% names(df)) {
        engineered_state$message <- "Select a date column before extracting date parts."
        return()
      }
      prefix <- new_name
      new_cols <- c(paste0(prefix, "_year"), paste0(prefix, "_month"), paste0(prefix, "_weekday"))
      df[[new_cols[1]]] <- format(df[[input$feature_date_col]], "%Y")
      df[[new_cols[2]]] <- format(df[[input$feature_date_col]], "%m")
      df[[new_cols[3]]] <- weekdays(df[[input$feature_date_col]])
      engineered_state$message <- paste("Extracted date parts from", input$feature_date_col)
    } else if (input$feature_action == "One-hot encode") {
      if (is.null(input$feature_cat_col) || !nzchar(input$feature_cat_col) || !input$feature_cat_col %in% names(df)) {
        engineered_state$message <- "Select a categorical column before one-hot encoding."
        return()
      }
      dummy <- stats::model.matrix(~ . - 1, data = data.frame(value = factor(df[[input$feature_cat_col]])))
      dummy <- as.data.frame(dummy)
      names(dummy) <- paste0(new_name, "_", safe_names(sub("^value", "", names(dummy))))
      new_cols <- names(dummy)
      df <- bind_cols(df, dummy)
      engineered_state$message <- paste("Created one-hot encoded columns from", input$feature_cat_col)
    }

    engineered_state$data <- df
    engineered_state$engineered_cols <- unique(c(engineered_state$engineered_cols, new_cols))
  })

  observeEvent(input$delete_feature, {
    req(engineered_state$data)
    cols_to_delete <- intersect(input$delete_features, names(engineered_state$data))
    if (!length(cols_to_delete)) {
      engineered_state$message <- "Select at least one engineered feature to delete."
      return()
    }

    engineered_state$data <- engineered_state$data[, setdiff(names(engineered_state$data), cols_to_delete), drop = FALSE]
    engineered_state$engineered_cols <- setdiff(engineered_state$engineered_cols, cols_to_delete)
    engineered_state$message <- paste("Deleted engineered feature(s):", paste(cols_to_delete, collapse = ", "))
  })

  engineered_data <- reactive({
    req(engineered_state$data)
    engineered_state$data
  })

  output$engineered_rows <- renderText({
    nrow(engineered_data())
  })

  output$engineered_cols <- renderText({
    ncol(engineered_data())
  })

  output$feature_message <- renderText({
    engineered_state$message
  })

  output$engineered_preview <- renderTable({
    preview_table(engineered_data())
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$delete_feature_ui <- renderUI({
    checkboxGroupInput(
      "delete_features",
      "Engineered features",
      choices = engineered_state$engineered_cols,
      selected = character(0)
    )
  })

  output$download_engineered <- downloadHandler(
    filename = function() {
      paste0("engineered_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(engineered_data(), file, row.names = FALSE)
    }
  )

  observe({
    df <- engineered_data()
    cols <- names(df)
    updateSelectInput(session, "filter_column", choices = c("None", cols), selected = "None")

    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    categorical_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]
    all_cols <- names(df)

    updateSelectInput(session, "plot_x", choices = all_cols)
    updateSelectInput(session, "plot_y", choices = numeric_cols)
    updateSelectInput(session, "plot_color", choices = c("None", categorical_cols))
  })

  output$filter_control <- renderUI({
    df <- engineered_data()
    req(input$filter_column)

    if (input$filter_column == "None" || !input$filter_column %in% names(df)) {
      return(helpText("Choose a column to activate interactive filtering."))
    }

    col <- df[[input$filter_column]]
    if (is.numeric(col)) {
      rng <- range(col, na.rm = TRUE)
      sliderInput("filter_range", "Numeric range", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
    } else {
      choices <- unique(as.character(col))
      selectInput("filter_levels", "Categories", choices = choices, selected = choices, multiple = TRUE)
    }
  })

  filtered_data <- reactive({
    df <- engineered_data()
    req(input$filter_column)

    if (input$filter_column == "None" || !input$filter_column %in% names(df)) {
      return(df)
    }

    col <- df[[input$filter_column]]
    if (is.numeric(col) && !is.null(input$filter_range)) {
      keep <- !is.na(col) & col >= input$filter_range[1] & col <= input$filter_range[2]
      return(df[keep, , drop = FALSE])
    }
    if (!is.numeric(col) && !is.null(input$filter_levels)) {
      return(df[as.character(col) %in% input$filter_levels, , drop = FALSE])
    }
    df
  })

  output$plot_x_ui <- renderUI({
    df <- filtered_data()
    cols <- names(df)
    selectInput("plot_x", "X variable", choices = cols, selected = cols[1])
  })

  output$plot_y_ui <- renderUI({
    df <- filtered_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (!length(numeric_cols)) {
      return(NULL)
    }
    selectInput("plot_y", "Y variable", choices = numeric_cols, selected = numeric_cols[min(2, length(numeric_cols))])
  })

  output$plot_color_ui <- renderUI({
    df <- filtered_data()
    categorical_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]
    selectInput("plot_color", "Color group", choices = c("None", categorical_cols), selected = "None")
  })

  output$eda_plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)

    use_color <- !is.null(input$plot_color) && input$plot_color != "None" && input$plot_color %in% names(df)

    if (input$plot_type == "Histogram") {
      req(input$plot_x, is.numeric(df[[input$plot_x]]))
      ggplot(df, aes(x = .data[[input$plot_x]])) +
        geom_histogram(fill = "#1b4965", color = "white", bins = 20) +
        labs(x = input$plot_x, y = "Count")
    } else if (input$plot_type == "Scatter plot") {
      req(input$plot_x, input$plot_y, is.numeric(df[[input$plot_x]]), is.numeric(df[[input$plot_y]]))
      if (use_color) {
        ggplot(df, aes(x = .data[[input$plot_x]], y = .data[[input$plot_y]], color = .data[[input$plot_color]])) +
          geom_point(size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", se = FALSE, color = "#5fa8d3")
      } else {
        ggplot(df, aes(x = .data[[input$plot_x]], y = .data[[input$plot_y]])) +
          geom_point(size = 3, alpha = 0.7, color = "#1b4965") +
          geom_smooth(method = "lm", se = FALSE, color = "#5fa8d3")
      }
    } else if (input$plot_type == "Box plot") {
      req(input$plot_x, input$plot_y)
      if (use_color) {
        ggplot(df, aes(x = .data[[input$plot_x]], y = .data[[input$plot_y]], fill = .data[[input$plot_color]])) +
          geom_boxplot(alpha = 0.7) +
          theme(axis.text.x = element_text(angle = 35, hjust = 1))
      } else {
        ggplot(df, aes(x = .data[[input$plot_x]], y = .data[[input$plot_y]])) +
          geom_boxplot(alpha = 0.7, fill = "#62b6cb") +
          theme(axis.text.x = element_text(angle = 35, hjust = 1))
      }
    } else if (input$plot_type == "Bar chart") {
      req(input$plot_x)
      if (use_color) {
        ggplot(df, aes(x = .data[[input$plot_x]], fill = .data[[input$plot_color]])) +
          geom_bar(position = "dodge") +
          theme(axis.text.x = element_text(angle = 35, hjust = 1))
      } else {
        ggplot(df, aes(x = .data[[input$plot_x]])) +
          geom_bar(fill = "#62b6cb") +
          theme(axis.text.x = element_text(angle = 35, hjust = 1))
      }
    } else {
      numeric_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
      shiny::validate(shiny::need(ncol(numeric_df) >= 2, "Correlation heatmap needs at least two numeric columns."))
      corr <- stats::cor(numeric_df, use = "pairwise.complete.obs")
      corr_df <- as.data.frame(as.table(corr))
      ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
        geom_tile() +
        scale_fill_gradient2(low = "#cae9ff", mid = "white", high = "#1d3557", midpoint = 0) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = NULL, y = NULL, fill = "Correlation")
    }
  }, height = function() 600, res = 110)

  output$eda_summary <- renderTable({
    df <- filtered_data()
    numeric_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
    if (!ncol(numeric_df)) {
      return(data.frame(Message = "No numeric columns available for summary statistics."))
    }

    data.frame(
      Variable = names(numeric_df),
      Mean = vapply(numeric_df, mean, numeric(1), na.rm = TRUE),
      Median = vapply(numeric_df, stats::median, numeric(1), na.rm = TRUE),
      SD = vapply(numeric_df, stats::sd, numeric(1), na.rm = TRUE),
      Missing = vapply(numeric_df, function(col) sum(is.na(col)), numeric(1)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$eda_preview <- renderTable({
    preview_table(filtered_data())
  }, striped = TRUE, bordered = TRUE, width = "100%")
}

shinyApp(ui, server)
