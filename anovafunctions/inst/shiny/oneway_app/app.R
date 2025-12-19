#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
# app.R
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(gt)  
library(gtExtras)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "quartz"),
  titlePanel("One-way ANOVA Explorer (Upload or Manual Input)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1) Load data"),
      fileInput("file", "Upload CSV", accept = c(".csv")),
      tags$small("OR paste CSV data below (must include a header row):"),
      textAreaInput(
        "manual_csv",
        label = NULL,
        rows = 8,
        placeholder = "Example:\nGroup,Y,X\nA,10,2\nA,12,3\nB,15,4\nB,14,5"
      ),
      checkboxInput("stringsAsFactors", "Treat text columns as categorical factors", value = TRUE),
      actionButton("load_btn", "Load / Refresh Data", class = "btn-primary"),
      
      hr(),
      h4("2) Analysis controls"),
      uiOutput("var_selectors"),
      
      hr(),
      checkboxInput("drop_na", "Drop rows with missing values in selected variables", value = TRUE),
      checkboxInput("show_points", "Show jittered points on boxplot", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data preview",
                 DTOutput("preview"),
                 br(),
                 verbatimTextOutput("data_info")
        ),
        
        tabPanel("Descriptive statistics",
                 h4("Overall"),
                 tableOutput("desc_overall"),
                 br(),
                 h4("By Group (if a group variable is selected)"),
                 tableOutput("desc_by_group")
        ),
        
        tabPanel("Box plot (categorical factor)",
                 plotOutput("boxplot", height = 420),
                 br(),
                 verbatimTextOutput("boxplot_note")
        ),
        
        tabPanel("Scatter + regression (numeric factor)",
                 plotOutput("scatter", height = 420),
                 br(),
                 verbatimTextOutput("lm_summary")
        ),
        
        tabPanel("One-way ANOVA",
                 h4("ANOVA table"),
                 gt_output("anova_gt")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---------- Helpers ----------
  parse_manual_csv <- function(txt) {
    txt <- trimws(txt)
    if (nchar(txt) == 0) return(NULL)
    con <- textConnection(txt)
    on.exit(close(con), add = TRUE)
    read.csv(con, stringsAsFactors = FALSE, check.names = FALSE)
  }
  
  coerce_types <- function(df, stringsAsFactors = TRUE) {
    if (is.null(df)) return(NULL)
    if (stringsAsFactors) {
      for (nm in names(df)) {
        if (is.character(df[[nm]])) df[[nm]] <- as.factor(df[[nm]])
      }
    }
    df
  }
  
  # ---------- Load data ----------
  raw_data <- eventReactive(input$load_btn, {
    df <- NULL
    
    if (!is.null(input$file)) {
      df <- tryCatch(
        read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )
    }
    
    if (is.null(df)) {
      df <- tryCatch(parse_manual_csv(input$manual_csv), error = function(e) NULL)
    }
    
    df <- coerce_types(df, input$stringsAsFactors)
    
    validate(
      need(!is.null(df) && ncol(df) >= 2, "Please upload a CSV or paste valid CSV data (at least 2 columns).")
    )
    
    df
  }, ignoreInit = FALSE)
  
  # ---------- Variable selectors ----------
  output$var_selectors <- renderUI({
    df <- raw_data()
    nms <- names(df)
    
    num_vars <- nms[sapply(df, is.numeric)]
    cat_vars <- nms[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    validate(need(length(num_vars) >= 1, "Your dataset needs at least one numeric column."))
    
    tagList(
      h5("Choose variables"),
      selectInput("y_num", "Numeric response (Y)", choices = num_vars, selected = num_vars[1]),
      selectInput("group_cat", "Group (categorical factor)", choices = c("", cat_vars), selected = ""),
      hr(),
      h5("Plots"),
      selectInput("box_factor", "Boxplot factor (categorical)", choices = c("", cat_vars), selected = ""),
      selectInput("box_y", "Boxplot Y (numeric)", choices = num_vars, selected = num_vars[1]),
      hr(),
      selectInput("x_num", "Scatter X (numeric)", choices = c("", num_vars), selected = ""),
      selectInput("scatter_y", "Scatter Y (numeric)", choices = num_vars, selected = num_vars[1])
    )
  })
  
  # ---------- Preview ----------
  output$preview <- renderDT({
    datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_info <- renderPrint({
    df <- raw_data()
    cat("Rows:", nrow(df), "\nCols:", ncol(df), "\n\n")
    cat("Column types:\n")
    print(sapply(df, function(x) paste(class(x), collapse = "/")))
  })
  
  # ---------- Descriptives ----------
  output$desc_overall <- renderTable({
    df <- raw_data()
    nums <- names(df)[sapply(df, is.numeric)]
    if (length(nums) == 0) return(NULL)
    
    data.frame(
      variable = nums,
      n = sapply(nums, function(v) sum(!is.na(df[[v]]))),
      mean = sapply(nums, function(v) mean(df[[v]], na.rm = TRUE)),
      sd = sapply(nums, function(v) sd(df[[v]], na.rm = TRUE)),
      median = sapply(nums, function(v) median(df[[v]], na.rm = TRUE)),
      IQR = sapply(nums, function(v) IQR(df[[v]], na.rm = TRUE)),
      min = sapply(nums, function(v) min(df[[v]], na.rm = TRUE)),
      max = sapply(nums, function(v) max(df[[v]], na.rm = TRUE)),
      row.names = NULL,
      check.names = FALSE
    )
  }, digits = 4)
  
  output$desc_by_group <- renderTable({
    df <- raw_data()
    g <- input$group_cat
    y <- input$y_num
    
    if (is.null(g) || g == "" || is.null(y) || y == "") return(NULL)
    validate(need(g %in% names(df) && y %in% names(df), "Select a valid group and Y."))
    
    d <- df[, c(g, y)]
    if (input$drop_na) d <- d[complete.cases(d), , drop = FALSE]
    d[[g]] <- as.factor(d[[g]])
    
    groups <- levels(d[[g]])
    out <- do.call(rbind, lapply(groups, function(gr) {
      yy <- d[d[[g]] == gr, y]
      data.frame(
        group = gr,
        n = length(yy),
        mean = mean(yy),
        sd = sd(yy),
        median = median(yy),
        IQR = IQR(yy),
        min = min(yy),
        max = max(yy),
        row.names = NULL
      )
    }))
    out
  }, digits = 4)
  
  # ---------- Boxplot (colored by group) ----------
  output$boxplot <- renderPlot({
    df <- raw_data()
    f <- input$box_factor
    y <- input$box_y
    
    validate(need(!is.null(f) && f != "", "Pick a categorical factor for the boxplot."))
    validate(need(!is.null(y) && y != "", "Pick a numeric Y for the boxplot."))
    
    d <- df[, c(f, y)]
    if (input$drop_na) d <- d[complete.cases(d), , drop = FALSE]
    d[[f]] <- as.factor(d[[f]])
    
    p <- ggplot(d, aes(x = .data[[f]], y = .data[[y]], fill = .data[[f]])) +
      geom_boxplot() +
      labs(x = f, y = y, title = paste("Boxplot of", y, "by", f)) +
      guides(fill = "none")
    
    if (isTRUE(input$show_points)) {
      p <- p + geom_jitter(width = 0.15, color = "black")
    }
    p
  })
  
  output$boxplot_note <- renderPrint({
    f <- input$box_factor
    y <- input$box_y
    if (is.null(f) || f == "" || is.null(y) || y == "") return(invisible())
    cat("Each group is colored differently (fill mapped to the factor).\n")
  })
  
  # ---------- Scatter + regression ----------
  output$scatter <- renderPlot({
    df <- raw_data()
    x <- input$x_num
    y <- input$scatter_y
    
    validate(need(!is.null(x) && x != "", "Pick a numeric X for the scatter plot."))
    validate(need(!is.null(y) && y != "", "Pick a numeric Y for the scatter plot."))
    
    d <- df[, c(x, y)]
    if (input$drop_na) d <- d[complete.cases(d), , drop = FALSE]
    
    ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      labs(title = paste("Scatter:", y, "vs", x), x = x, y = y)
  })
  
  output$lm_summary <- renderPrint({
    df <- raw_data()
    x <- input$x_num
    y <- input$scatter_y
    if (is.null(x) || x == "" || is.null(y) || y == "") return(invisible())
    
    d <- df[, c(x, y)]
    if (input$drop_na) d <- d[complete.cases(d), , drop = FALSE]
    
    fit <- lm(d[[y]] ~ d[[x]])
    summary(fit)
  })
  
  # ---------- One-way ANOVA ----------
  anova_fit <- reactive({
    df <- raw_data()
    y <- input$y_num
    g <- input$group_cat
    
    validate(need(!is.null(y) && y != "", "Select a numeric response Y."))
    validate(need(!is.null(g) && g != "", "Select a categorical group factor."))
    
    d <- df[, c(y, g)]
    if (input$drop_na) d <- d[complete.cases(d), , drop = FALSE]
    d[[g]] <- as.factor(d[[g]])
    
    validate(need(nlevels(d[[g]]) >= 2, "Group factor must have at least 2 levels."))
    validate(need(nrow(d) >= 3, "Not enough rows after filtering missing values."))
    
    aov(d[[y]] ~ d[[g]])
  })
  
  output$anova_gt <- render_gt({
    fit <- anova_fit()
    an <- summary(fit)[[1]]
    
    df <- data.frame(
      Term     = rownames(an),
      Df       = an$Df,
      `Sum Sq` = an$`Sum Sq`,
      `Mean Sq`= an$`Mean Sq`,
      `F value`= an$`F value`,
      `Pr(>F)` = an$`Pr(>F)`,
      row.names = NULL,
      check.names = FALSE
    )
    
    # Manual p-value formatting
    df$`Pr(>F)` <- ifelse(
      is.na(df$`Pr(>F)`),
      NA,
      ifelse(df$`Pr(>F)` < 0.001, "<0.001", sprintf("%.3f", df$`Pr(>F)`))
    )
    
    gt(df) |>
      fmt_number(columns = c("Sum Sq", "Mean Sq", "F value"), decimals = 4) |>
      fmt_number(columns = "Df", decimals = 0) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
      ) |>
      tab_options(table.font.size = px(14))
  })
}

shinyApp(ui, server)

