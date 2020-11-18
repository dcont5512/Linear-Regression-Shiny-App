## libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggfortify)
library(car)
library(GGally)
library(ggpubr)
library(kableExtra)
library(magrittr)
library(gbRd)
library(data.table)
library(DT)
library(waiter)
library(DescTools)
library(mlbench)
library(AppliedPredictiveModeling)
library(shinyjs)
library(plotly)
library(readxl)

## bug list
## make sure that wrong file type message appears in sidebar

options(shiny.maxRequestSize = 30*1024^2)

## text objects
text_inst <- "This app allows users to interactively build and tune linear models. Users are able to select
from more than 100 pre-loaded datasets, or upload their own dataset, and build their model. To upload a dataset,
the data must be in .RData format. After selecting a dataset, you can begin to build your model immediately,
or explore the data using tabs 1 and 2."

text_data <- "<b>Description: </b> This page provides an overview of the selected data, including 
the data in its raw form, as well as the help file associated with any R datasets. Note that 
while the data on this page does reflect recoding of variables, it does not reflect any variable 
transformations.</p>"

text_summ <- "<b>Description: </b> This page provides a number of useful charts which summarise the 
selected data. This includes a correlation matrix of all continuous variables, which show their univariate 
distributions, scatterplots, and correlation coefficients. To generate this plot, click the ‘Generate 
Matrix’ button below. Please be aware that correlation matrices can take some time to render, particularly
for larger datasets, and that all other application operations will be queued until the correlation matrix
is finishing rendering. Also shown on this page are bar plots for any categorical variables in the dataset.
If the dataset does not include any categorical variables, then this section will be blank. Note that while 
does not reflect any variable transformations, it does reflect recoding of variables.</p>"

text_cont <- "<b>Description: </b>This page shows four plots to better understand a selected continuous 
variable of interest. These include: (1) a histogram which shows the underlying distribution of the variable;
(2) a density plot which  uses a kernel density estimate to show the probability density function of the 
variable; (3) a quantile-quantile, or QQ, plot to compare the distribution of that variable to a normal 
distribution; and (4) a scatterplot showing the relationship between that variable and the specified target 
variable. Users are able to color these charts by specifying a categorical variable in the ‘Select Fill’ field. 
For a variable to be available for selection or fill, it must be specified in the ‘model terms’ component of 
the application. Users should also be aware that all charts on this page are responsive to any specified variable 
transformations. Note that polynomial transformations will only be reflected as a polynomial curve in the 
scatterplot as this type transformation does not change the underlying data, but rather adds additional terms 
to it.</p>"

text_cat <- "<b>Description: </b>This page shows two plots to better understand a categorical variable of interest. 
These include: (1) a bar plot showing counts of all distinct values in the variable; and (2) a stacked bar plot 
which shows percentage totals that each of these values account for among all observations. Users are able to color 
these charts by specifying an additional categorical variable in the ‘Select Fill’ field. Please be aware that even
when no coloring variable is specified, these charts will still be colored according the distinct values in the 
primary variable as doing so provides better visual differentiation among values, particularly in the stacked bar 
plot. For a categorical variable to be available for selection or fill, it must be specified in the ‘model terms’ 
component of the application.</p>"

text_lm <- "<b>Description: </b>This page provides summary information about the linear model created based on the 
selections in the model terms, variable transformation, and interaction terms components of the application. Note 
that you must select a target variable and at least one predictor variable to generate a linear model. This page 
shows the linear model summary object, which includes the model's f-statistic, variable coefficients, significance 
values, residual standard error, and r-squared values, among others. Also shown on this page are the variance 
inflation factors (VIF) for any continuous variables in the dataset, which can be used to identify multi-collinearity
among predictors. Note that the VIF factors are only available for models containing at least two continuous variables,
and that these figures are generated from a model which does not incorporate categorical variables, or interaction 
terms. Note however that VIF factors are responsive to variable transformations, with the exception of polynomial 
transformations, which are excluded.</p>"

text_diag <- "<b>Description: </b>This page provides diagnostic plots for the linear model created based on the 
selections in the model terms, variable transformation, and interaction terms components of the application.  
Note that you must select a target variable and at least one predictor variable to generate a linear model and its 
associated diagnostic plots. These plots can be used to determine whether your model satisfies the assumptions of 
a regression model. The plots shown include: (1) a residuals vs fitted plot which can be used to diagnose non-linearity;
(2) a quantile quantile, or QQ, plot which can be used to check whether residuals are normally distributed; (3) a 
scale-location plot to check whether the residuals are spread equally along the ranges of predictors and thus identify
heteroskedasticity/homoscedasticity; and (4) a residuals vs. leverage plot to identify high-leverage observations in
the data which can have an outsized impact on model coefficients and standard errors.</p>"

text_comp <- "<b>Description: </b>It is often beneficial to compare two models to find out if a more complex model 
is significantly better at capture the data than the more simple model. This is because even if one model performs 
nominally better than another model, that does not mean that the better performing model is meaningfully different 
than the comparison model. In such cases, it is generally recommended to choose the less complex of the two models. 
The tool below can be used to perform this analysis and compare two specified models using an analysis of variance 
(ANOVA) test. To use this tool, copy and paste each model’s formula from the ‘Linear Model’ page into the boxes below
and click 'Compare Models'. </p>"

# source packages for data frames
pkg_list <- c("mlbench", "AppliedPredictiveModeling", "datasets")
# obtain names of data objects in each package
object_list <- pkg_list %>%
  map(~list(.x = data(package = .x)$results %>%
              data.frame %>% .$Item %>% sort %>% as.character)) %>% unlist %>% as.character
# exclude any objects with parenthesis in name
object_list <- object_list[!grepl("\\(", object_list)]
# load all remaining objects into environment
data(list = object_list)
# determine whether object is a data frame
data_list <- object_list %>% map(~(is.data.frame(get(.x)))) %>% as.character
names(data_list) <- object_list
# keep only names of data frames
choice_list <- data_list %>% .[matches("TRUE", vars=.)] %>% names %>% sort

## variable names function
var_names_func <-  function(x) {
  list(continuous = x %>% select_if(is.numeric) %>% colnames %>% sort,
       categorical = c("is.factor", "is.character") %>% 
         map(~select_if(x, .x) %>% 
               colnames) %>% unlist %>% sort)
}  

## not like function
`%notlike%` <- Negate(`%like%`)

## transformation options
tran_opts <- list("Simple" = c("None",  "Reciprocal", "Squared", "Square Root"),
                  "Logarithmic" = c("Natural", "Base 2", "Base 10"),
                  "Polynomial" = c(paste (seq(from = 2, to = 5), rep("degree", 5))))

## transformation function
tran_func <- function(pred, trans, usage) {
  if(usage == "scatter" & trans == "Reciprocal") {paste0("1/", pred)}
  else if(usage == "model" & trans == "Reciprocal") {paste0("I(1/", pred, ")")}
  else if(usage == "scatter" & trans == "Squared") {paste0(pred, "^2")}
  else if(usage == "model" & trans == "Squared") {paste0("I(", pred, "^2)")}
  else {
    switch(trans,
           "None" = pred,
           "Square Root" = paste0("sqrt(", pred, ")"),
           "Natural" = paste0("log(", pred, ")"),
           "Base 2" = paste0("log2(", pred, ")"),
           "Base 10" = paste0("log10(", pred, ")"),
           "2 degree" = paste0("poly(", pred, ", 2)"),
           "3 degree" = paste0("poly(", pred, ", 3)"),
           "4 degree" = paste0("poly(", pred, ", 4)"),
           "5 degree" = paste0("poly(", pred, ", 5)"))
  }
}

## categorical plots data function
plot_cat_dat <- function(data, pred_cat_selected, pred_cat_fill) {
  if(pred_cat_fill  %>% length == 0) {
    df <- data %>% 
      count(get(pred_cat_selected), name = "total") %>% 
      `colnames<-`(c(pred_cat_selected, "total")) 
  } else {
    df <- data %>% 
      count(get(pred_cat_selected), get(pred_cat_fill), name = "total") %>% 
      `colnames<-`(c(pred_cat_selected, pred_cat_fill, "total")) %>% 
      group_by_at(1)
  }
  df %>% 
    mutate(pct_total = (total/sum(total)) * 100)
}
## categorical bar plot function
plot_cat_bar <- function(data, pred_cat_selected, pred_cat_fill) {
  df <- plot_cat_dat(data, pred_cat_selected, pred_cat_fill) 
  if(pred_cat_fill %>% length == 0) {
    plot <- df %>% 
      ggplot(aes_string(x = pred_cat_selected, y = "total", fill = pred_cat_selected)) +
      geom_bar(stat = "identity")
  } else {
    plot <- df %>% 
      ggplot(aes_string(x = pred_cat_selected, y = "total", fill = pred_cat_fill)) +
      geom_bar(stat = "identity", position = "dodge")
  }
  plot + theme(legend.position = "none")
}
## stacked bar plot function
plot_cat_stack <- function(data, pred_cat_selected, pred_cat_fill) {
  df <- plot_cat_dat(data, pred_cat_selected, pred_cat_fill)
  if(pred_cat_fill %>% length == 0) {
    plot <- df %>% 
      mutate(var_name = pred_cat_selected) %>% 
      ggplot(aes_string(x = "var_name", y = "pct_total", fill = pred_cat_selected))
  } else {
    plot <- df %>% 
      ggplot(aes_string(x = pred_cat_selected, y = "pct_total", fill = pred_cat_fill))
  }
  plot + 
    geom_bar(stat = "identity", position = "stack") +
    theme(legend.position = "none")
}

ui <-  dashboardPage(
  dashboardHeader(title = "Interactive Linear Model Builder", titleWidth = 370),
  ## dashboard sidebar
  dashboardSidebar(
    width = 370,
    sidebarMenu(
      menuItem("Select Data",
               ## data select
               fluidRow(column(width = 6, 
                               radioButtons(inputId = "data_source", label = "Data Source:",
                                            choices = c("R", "Upload"), 
                                            selected = "R",
                                            inline = T)),
                        column(width = 6,
                               radioButtons(inputId = "data_complete", label = "Complete Rows:",
                                            choices = c("Yes", "No"), 
                                            selected = "No",
                                            inline = TRUE))),
               uiOutput(outputId = "data_select_ui"),
               div(style = "height:2px")),
      menuItem("Recode Data",
               uiOutput("data_recode")),
      menuItem("Model Terms", 
               fluidRow(
                 column(width = 6,
                        ## target variable ui
                        uiOutput("target_ui")),
                 column(width = 6, 
                        ## target variable transformation (exclude polynomials)
                        uiOutput("target_trans_ui"))),
               fluidRow(
                 column(width = 6,
                        ## continuous variables ui
                        uiOutput("preds_cont_ui")),
                 column(width = 6,
                        ## categorical variables ui
                        uiOutput("preds_cat_ui")))),
      ## continuous variable transformation ui
      menuItem("Variable Transformations",
               uiOutput(outputId = "preds_tran_ui")),
      ## interaction terms ui
      menuItem("Interaction Terms",
               div(style = "height:5px"),
               fluidRow(
                 column(width = 5, 
                        uiOutput("intTermAdd_ui")),
                 column(width = 5, 
                        uiOutput("intTermRemove_ui"))),
               div(style = "height:5px"),
               uiOutput(outputId = "preds_int_ui"))
    )
  ),
  dashboardBody(
    modal_confirm <- modalDialog(
      HTML("This app allows you to interactively build a linear model and see in real time how various parameter
      selections impact different components of the model. To begin, click 'Select Data' at the left and select 
      either a preloaded dataset from R, or upload one of your own. Note that the app automatically filters out
      date objects from all files. After selecting your dataset, you can then recode variables as necessary; select 
      your target and predictor variables; transform variables; and add interaction terms. The pages on the right-side 
      of the app provide detailed, real-time information about your data and specific variables within it, as well 
      as information about your linear model.</p>
      <b>Author:</b> Dominic Contreras</p>
      <b>Email:</b> dcont5512 at gmail.com"),
      title = "Welcome"
    ),
    fluidRow(
      use_waiter(),
      tabBox(width = 12, height = NULL,
             ## data overview, including table and help file
             tabPanel("Data Overview",
                      HTML(text = text_data),
                      dataTableOutput(outputId = "data"), 
                      div(style = "height:5px"),
                      htmlOutput(outputId = "data_dictionary")),
             ## correlation matrix
             tabPanel("Summary Plots",
                      HTML(text = text_summ),
                      actionButton(inputId = "corr_matrix_generate", label = "Generate Matrix"),
                      div(style = "height:10px"),
                      uiOutput(outputId = "corr_matrix_ui"),
                      plotOutput(outputId = "bar_plot")),
             ## continuous variable plots
             tabPanel("Cont. Predictors",
                      HTML(text = text_cont),
                      fluidRow(
                        ## variable select
                        column(width = 3,
                               selectizeInput(inputId = "cont_plot_var", label = "Select Variable", 
                                              choices = "", 
                                              multiple = TRUE,
                                              options = list(placeholder = "None",
                                                             maxItems = 1))),
                        ## fill select
                        column(width = 3,
                               selectizeInput(inputId = "cont_plot_fill", 
                                              label = "Select Fill", 
                                              choices = "", 
                                              multiple = TRUE,
                                              options = list(placeholder = "None",
                                                             maxItems = 1))),
                        # plot objects
                        column(width = 6,
                               plotOutput(outputId = "cont_plot_legend", height = "75px"))),
                      fluidRow(c("cont_plot_hist", "cont_plot_dens") %>% 
                                 map(~column(width = 6, 
                                             plotOutput(outputId = .x, height = "300px")))),
                      fluidRow(c("cont_plot_scatter", "cont_plot_qq") %>% 
                                 map(~column(width = 6, 
                                             plotOutput(outputId = .x, height = "300px"))))),
             ## categorical variable plots
             tabPanel("Cat. Predictors", 
                      HTML(text = text_cat),
                      fluidRow(
                        ## variable select
                        column(width = 3,
                               selectizeInput(inputId = "cat_plot_var", label = "Select Variable",
                                              choices = "",
                                              multiple = TRUE,
                                              options = list(placeholder = "None",
                                                             maxItems = 1))),
                        ## fill select ui
                        column(width = 3, uiOutput("cat_plot_fill_ui")),
                        ## plot objects
                        column(width = 6,
                               plotOutput("cat_plot_legend", height = "75px"))),
                      fluidRow(c("cat_plot_bar", "cat_plot_stack") %>% 
                                 map(~column(width = 6, plotOutput(.x))))),
             ## linear model
             tabPanel("Linear Model",
                      HTML(text = text_lm),
                      fluidRow(
                        ## formula and summary
                        column(width = 8, 
                               htmlOutput(outputId = "lm_formula"),
                               verbatimTextOutput(outputId = "lm_summary")),
                        ## vif statistics
                        column(width = 4, 
                               HTML(text = "<b>Variance Inflation Factor (VIF): </b></p>"),
                               dataTableOutput(outputId = "lm_vif")))),
             ## linear model diagnostic plots
             tabPanel("Diagnostic Plots",
                      HTML(text = text_diag),
                      plotOutput("lm_plots", height = "600px")),
             ## model comparisons
             tabPanel("Model Comparison",
                      HTML(text = text_comp),
                      fluidRow(c(1, 2) %>% 
                                 map(~column(width = 4, 
                                             textInput(inputId = paste0("lm_comp_", .x), 
                                                       label = paste0("Model ", .x), 
                                                       value = "", placeholder = "Linear model formula")))),
                      div(style = "height:2px"),
                      actionButton(inputId = "lm_comp_run", label = "Compare Models"),
                      div(style = "height:2px"),
                      verbatimTextOutput("lm_comp_summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  ## welcome message
  observe({
    showModal(modal_confirm)
  }) 
  ## data select
  output$data_select_ui <- renderUI({
    if(input$data_source == "R") {
      selectizeInput(inputId = "data_file", label = "Select Dataset:",
                     choices = choice_list,
                     multiple = T,
                     options = list(placeholder = "Click to select",
                                    maxItems = 1))
    } else {
      file_types <- ".Rdata, .xlsx, .xls, .csv"
      fileInput(inputId = "data_file", label =  "Upload File:", 
                placeholder =  file_types, 
                accept = file_types)
    }
  })
  ## data environment for app usage
  dat <- reactive({
    if(input$data_source == "R") {
      df <- get(input$data_file)
    } else {
      file_type <- tools::file_ext(input$data_file$name)
      df <- switch(file_type,
                   RData = load(input$data_file$datapath),
                   xlsx = read_xlsx(input$data_file$datapath),
                   xls = read_xls(input$data_file$datapath),
                   csv = read_csv(input$data_file$datapath),
                   validate("Invalid file; Please upload a .RData file")
      )
      df <- switch(file_type, 
                   RData =  sub(".RData$", "", basename(input$data_file$name)) %>% get,
                   xlsx = df,
                   xls = df,
                   csv = df)
    }
    if (input$data_complete == "Yes") {
      df <- na.omit(df)
    }
    df_elig_vars <- var_names_func(df) %>% unlist %>% as.character
    df %>% select(df_elig_vars)
  })
  ## data recode default options function
  var_orig_func <- function(x) {
    sapply(x, function(y) if(is.factor(y)) "factor" else toString(class(y))) %>% 
      data.frame %>% 
      rownames_to_column() %>% 
      set_names(c("var_name", "var_type")) %>% 
      mutate(var_type = ifelse(var_type %in% c("numeric", "integer"), "continuous", "categorical")) %>% 
      arrange(var_name)
  }
  ## data recode default options
  var_recode_default <- reactive(var_orig_func(dat()))
  ## generate recode 
  observeEvent(input$data_file, {
    output$data_recode <- renderUI({
      # recode selectors function
      var_recode_select <-  function(var_name, var_type) {
        input_name <- paste0(var_name, "_recode")
        radioButtons(inputId = input_name,
                     label = var_name,
                     choices = c("continuous", "categorical"),
                     selected = var_type)
      }
      ## left-side recode number by row (i.e. row 1 = 1, row 2 = 3, row 3 = 5)
      recode_row_idx <- var_recode_default() %>% nrow %>% seq_len
      recode_row_idx <- recode_row_idx[recode_row_idx %% 2 == 1]
      ## recode row filter function
      recode_row_func <- function(x) {var_recode_default() %>% filter(row_number() == x)}
      ## generate selectors
      ## render re-coding options
      recode_row_idx %>%
        map(~ if((.x + 1) %in% (var_recode_default() %>% nrow %>% seq_len)) {
          recode_left_name <- recode_row_func(.x) %>% .$var_name
          recode_left_type <- recode_row_func(.x) %>% .$var_type
          recode_right_name <- recode_row_func(.x + 1) %>% .$var_name
          recode_right_type <- recode_row_func(.x +1) %>% .$var_type
          fluidRow(c("left", "right") %>% 
                     map(~column(width = 6,
                                 var_recode_select(var_name = get(paste0("recode_", .x, "_name")),
                                                   var_type = get(paste0("recode_", .x, "_type"))))))
        } else {
          ## generate ui when last predictor is an odd number
          recode_left_name <- recode_row_func(.x) %>% .$var_name
          recode_left_type <- recode_row_func(.x) %>% .$var_type
          fluidRow(column(width = 6,
                          var_recode_select(var_name = recode_left_name,
                                            var_type = recode_left_type)))
        })
    })
  })
  ## generate reactive data frame based on variable recoding
  dat_recode <- reactive({
    vn <- var_recode_default() %>% .$var_name
    vrc <- vn %>%
      map(~input[[paste0(.x, "_recode")]]) %>% unlist
    if (length(vrc)!=length(vn)) {
      vrc <- var_orig_func(dat()) %>% 
        .$var_type
    }
    data.frame(var = vn,
               var_recode = vrc) %>%
      mutate(var_recode_statement = paste0(var, " %>% ",
                                           ifelse(var_recode == "continuous", "as.numeric", "as.factor"))) %>%
      pmap(~dat() %>%
             transmute(!! ..1 :=
                         eval(rlang::parse_expr(..3)))) %>%
      bind_cols(.)
  })
  # data table render
  output$data <- renderDataTable({
    if(length(input$data_file) != 0) {
      datatable(dat_recode(), 
                options = list(pageLength = 5,
                               lengthMenu = list(c(5, 10, 25, 50), 
                                                 c("5", "10", "25", "50")),
                               scrollX = TRUE),
                rownames = FALSE)
    }
  })
  ## data help file
  output$data_dictionary <- renderUI({
    if(length(input$data_file) != 0) {
      if(input$data_source == "R") {
        Rd <- Rd_fun(help(input$data_file))
        outfile <- tempfile(fileext = ".html")
        Rd2HTML(Rd, outfile, package = "",
                stages = c("install", "render"))
        includeHTML(outfile)
      } else {
        HTML("<b>Data Dictionary: </b>: Data dictionaries are not available for uploaded files.")
      }
    }
  })
  ## variable names
  dat_vars <- reactive({
    var_names_func(dat_recode())
  })
  observeEvent(input$data_file, {
    ## target variable ui
    output$target_ui <- renderUI({
      selectizeInput(inputId = "target", label = "Target Variable:", 
                     choices = dat_vars()[["continuous"]],
                     multiple = TRUE,
                     selected = NULL,
                     options = list(placeholder = "Click to select",
                                    maxItems = 1))
    })
    output$target_trans_ui <- renderUI({
      selectizeInput(inputId = "tran_target", 
                     label = "Transformation:",
                     choices = tran_opts[c("Simple", "Logarithmic")],
                     multiple = FALSE,
                     selected = "None")
    })
    ## continuous variable ui
    output$preds_cont_ui <- renderUI({
      selectizeInput(inputId = "preds_cont", 
                     label = "Cont. Predictors:", 
                     choices = setdiff(dat_vars()[["continuous"]], input$target),
                     multiple = TRUE,
                     options = list(placeholder = "None"))
    })
    ## categorical variable ui
    output$preds_cat_ui <- renderUI({
      selectizeInput(inputId = "preds_cat",
                     label = "Cat. Predictors:",
                     choices = dat_vars()[["categorical"]],
                     multiple = TRUE,
                     options = list(placeholder = "None"))
    })
    ## update variable selections when data changes
    observeEvent(input$data_file, {
      updateSelectizeInput(session, inputId = "preds_cont", selected = "")
      updateSelectizeInput(session, inputId = "preds_cat", selected = "")
    })
  })
  ## correlation matrix ui
  observeEvent(input$corr_matrix_generate, {
    output$corr_matrix_ui <- renderUI(plotOutput(outputId = "corr_matrix", height = 600))
  })
  ## correlation matrix render
  observeEvent(input$corr_matrix_generate, {
    browser()
    output$corr_matrix <- renderPlot({
      Waiter$new(id = "corr_matrix")$show()
      dat_recode() %>% select_if(is.numeric) %>% ggpairs})
  })
  ## clear correlation matrix when data changes - broken
  observeEvent(input$data_file, {
    shinyjs::hide("corr_matrix_ui")
  })
  ## bar plot render
  observeEvent(input$data_file, {
    output$bar_plot <- renderPlot({
      if(length(dat_vars()[["categorical"]]) != 0) {
        dat_recode() %>% 
          select(dat_vars()[["categorical"]]) %>%
          gather %>% 
          count(key, value, name = "total") %>% 
          ggplot(aes(x = value, y = total)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          facet_wrap(. ~ key, ncol = 2, scales = "free") 
      } 
    })
  }) 
  observeEvent(input$data_file, {
    ## continuous variable transformation ui
    output$preds_tran_ui <- renderUI({
      ## variable transformation select function
      tran_select_preds <-  function(predsContName) {
        input_name <- paste0("trans_", predsContName)
        selectizeInput(inputId = input_name, 
                       label = predsContName,
                       choices = tran_opts,
                       multiple = FALSE,
                       selected = isolate(input[[input_name]])) 
      }
      ## left-side transformation number by row (i.e. row 1 = 1, row 2 = 3, row 3 = 5)
      tran_row_idx <- length(input$preds_cont) %>% seq_len
      tran_row_idx <- tran_row_idx[tran_row_idx %% 2 == 1]
      ## generate ui when last predictor is an even number
      tran_row_idx %>% 
        map(~ if(!is.na(input$preds_cont[.x + 1])) {
          tran_left <- input$preds_cont[.x]
          tran_right <- input$preds_cont[.x + 1]
          fluidRow(column(width = 6, 
                          tran_select_preds(predsContName = tran_left)),
                   column(width = 6, 
                          tran_select_preds(predsContName = tran_right)))
        } else {
          ## generate ui when last predictor is an odd number
          tran_left <- input$preds_cont[.x]
          fluidRow(column(width = 6, 
                          tran_select_preds(predsContName = tran_left)))
        }
        )
    })
    ## interaction terms ui
    output$intTermAdd_ui <- renderUI({
      actionButton(inputId = "intTermAdd", label = "Add Interaction",
                   width = "140px")
    })
    output$intTermRemove_ui <- renderUI({
      actionButton(inputId = "intTermRemove", label = "Remove Interaction",
                   width = "140px")
    })
  })
  ## generate data frame to store variable transformations
  dat_tran <- reactive({
    tran_preds <- input$preds_cont %>% 
      map(~input[[paste0("trans_", .x)]]) %>% 
      unlist
    ## create data frame with column 1 = variable name and column 2 = transformation
    var_name <- c(input$target, input$preds_cont)
    tran_list <- c(input$tran_target, tran_preds)
    tran_list <- if((length(var_name) == length(tran_list)) == FALSE) {
      miss_term <- length(var_name) - length(tran_list)
      tran_terms <- c(tran_list, rep("None", miss_term))
      data.frame(var_name = var_name,
                 tran_list = tran_terms,
                 stringsAsFactors = FALSE)
    } else {
      data.frame(var_name = var_name,
                 tran_list = tran_list,
                 stringsAsFactors = F)
    }
    tran_list %>%
      ## use transformation function to obtain transformation statements for model/plots
      rowwise %>%
      mutate(tran_form = tran_func(var_name, tran_list, "model"),
             tran_form_scatter = tran_func(var_name, tran_list, "scatter")) %>%
      ungroup %>%
      select(-tran_list) %>%
      ## append categorical variable names as data frame also used for interaction terms
      rbind(data.frame(var_name = input$preds_cat,
                       tran_form = input$preds_cat,
                       tran_form_scatter = input$preds_cat))
  })
  ## interaction term counter (stops at zero)
  click_total <- reactiveVal(0)
  observeEvent(input$intTermAdd, click_total(click_total() + 1))
  observeEvent(input$intTermRemove, click_total(max(0, click_total() - 1 )))
  ## interaction term ui
  output$preds_int_ui <- renderUI({
    ## interaction term select function
    intTermSelector <- function(intTermId, intNumber, intTermPosition) {
      selectizeInput(inputId = intTermId, 
                     label = paste("Interaction", intNumber, " - Term", intTermPosition),
                     choices = c("None", input$preds_cont, input$preds_cat),
                     multiple = FALSE,
                     selected = isolate(input[[intTermId]]))
    }
    ## generate interaction term ui
    click_total() %>%
      seq_len %>%
      map(~ {
        int_left <- paste0("int", .x, "term1")
        int_right <- paste0("int", .x, "term2")
        fluidRow(column(width = 6,
                        intTermSelector(intTermId = int_left,
                                        intNumber = .x,
                                        intTermPosition = 1)),
                 column(width = 6,
                        intTermSelector(intTermId = int_right,
                                        intNumber = .x,
                                        intTermPosition = 2)))
      })
  })
  ## generate interaction term statements
  dat_int <- reactive({
    ## data frame in which column 1 = interaction term left side and column 2 = right side
    if(click_total() >= 1) {
      int_list <- data.frame(int_term_left = seq_len(click_total()) %>% 
                               map(~input[[paste0("int", .x, "term1")]]) %>% unlist,
                             int_term_right = seq_len(click_total()) %>% 
                               map(~input[[paste0("int", .x, "term2")]]) %>% unlist) %>% 
        ## filter out interactions with 'none' or equivalent terms 
        filter((int_term_left != "None" | int_term_right != "None") & 
                 as.character(int_term_left) != as.character(int_term_right))
      ## filter out combinations (i.e. ab == ba, keep only one)
      int_list <- if(nrow(int_list) != 0) {
        int_list <- unique(t(apply(int_list, 1, sort))) %>%
          data.frame %>%
          `colnames<-`(c("int_term_left", "int_term_right"))
        ## obtain transformation selections for interaction terms and generate statement
        int_list %>%
          inner_join(dat_tran(), by = c("int_term_left" = "var_name")) %>%
          inner_join(dat_tran(), by = c("int_term_right" = "var_name")) %>% 
          select(int_term_left = 3, int_term_right = 5) %>% 
          mutate(int_form = paste0(int_term_left, "*", int_term_right)) %>%
          .$int_form
      }
    }
  })
  ## update variable choices for continuous variable plots
  vars_cont <- reactive(c(input$target, input$preds_cont))
  observeEvent(vars_cont(), {
    updateSelectizeInput(session, "cont_plot_var", 
                         choices = vars_cont(), selected = isolate(input$cont_plot_var))
  })
  ## update fill choices for continuous variable plots
  observeEvent(input$preds_cat, {
    updateSelectizeInput(session, "cont_plot_fill",
                         choices = input$preds_cat, selected = isolate(input$cont_plot_fill))
  })
  ## obtain transformed variable statements for continuous variable plots, excluding target
  plot_selected <- reactive({
    dat_tran() %>% 
      filter(var_name == input$cont_plot_var) %>% 
      ## if transformation polynomial, use only variable name
      mutate(tran_form = ifelse(tran_form %like% '%poly%', var_name,
                                ifelse(tran_form == tran_form_scatter, 
                                       tran_form, tran_form_scatter))) %>% 
      .$tran_form
  })
  ## obtain transformed variable statement for target variable
  plot_target <- reactive({
    dat_tran() %>% filter(var_name == input$target) %>% .$tran_form
  })
  ## plot input requirements
  plot_reqs <- function(x) {
    req(input$target)
    req(input$cont_plot_var)
  }
  ## generate histogram
  output$cont_plot_hist <- renderPlot({
    plot_reqs()
    ## no fill
    if(input$cont_plot_fill %>% length == 0) {
      plot <- dat_recode() %>% ggplot(aes_string(x = plot_selected())) + geom_histogram()
    } else {
      ## fill
      plot <- dat_recode() %>% ggplot(aes_string(x = plot_selected(), 
                                                 fill = input$cont_plot_fill)) +
        geom_histogram(alpha = 0.5)
    }
    plot + theme(legend.position = "none")
  })
  ## generate density plot
  output$cont_plot_dens <- renderPlot({
    plot_reqs()
    ## no fill
    if(input$cont_plot_fill %>% length == 0) {
      plot <- dat_recode() %>% ggplot(aes_string(x = plot_selected()))
    } else {
      ## fill
      plot <- dat_recode() %>% ggplot(aes_string(x = plot_selected(), fill = input$cont_plot_fill))
    }
    plot + geom_density(alpha = 0.5) + theme(legend.position = "none")
  })
  ## generate quantile-quantile plot
  output$cont_plot_qq <- renderPlot({
    plot_reqs()
    dat_recode() %>%
      ggplot(aes_string(sample = plot_selected())) +
      stat_qq() +
      stat_qq_line()
  })
  ## scatter plot function, stored as function to extract legend later
  plot_scatter_func <- function(x, fill) {
    ## no fill
    if(fill %>% length == 0) {
      plot <- dat_recode() %>% ggplot(aes_string(x = x,
                                                 y = plot_target()))
    } else {
      ## fill
      plot <-  dat_recode() %>% ggplot(aes_string(x = x,
                                                  y = plot_target(),
                                                  color = fill))
    }
    plot + geom_point()
  }
  ## generate scatter plot 
  output$cont_plot_scatter <- renderPlot({
    plot_reqs()
    plot <- plot_scatter_func(x = plot_selected(), fill = input$cont_plot_fill) +
      theme(legend.position = "none")
    ## add polynomial curves
    select_trans <- dat_tran() %>% 
      filter(var_name == input$cont_plot_var) %>% 
      .$tran_form
    if(select_trans %like% '%poly%') {
      select_trans_poly <- str_extract_all(select_trans, "[0-9]") %>% as.numeric
      plot + stat_smooth(method = "lm", se = TRUE, fill = NA,
                         formula= y ~ poly(x, select_trans_poly, raw = TRUE), colour="red")
    } else {
      plot
    }
  })
  ## isolate and generate legend for continuous variable plots
  output$cont_plot_legend <- renderPlot({
    plot_reqs()
    plot_fill_legend <- plot_scatter_func(x = plot_selected(), fill = input$cont_plot_fill) +
      theme(legend.position = "bottom", 
            legend.title=element_text(size = 14,
                                      family = "Helvetica Neue",
                                      face = "bold"),
            legend.key.size = unit(1.5, "line")) +
      guides(colour = guide_legend(override.aes = list(size=5)))
    get_legend(plot_fill_legend) %>% as_ggplot
  })
  ## update variable choices for categorical variable plots
  observeEvent(input$preds_cat, {
    updateSelectizeInput(session, "cat_plot_var", 
                         choices = input$preds_cat, selected = isolate(input$cat_plot_var))
  })
  ## update fill choices for categorical variable plots
  output$cat_plot_fill_ui <- renderUI({
    selectizeInput(inputId = "cat_fill", 
                   label = "Select Fill", 
                   choices = setdiff(input$preds_cat, input$cat_plot_var), 
                   multiple = TRUE,
                   selected = isolate(input$cat_fill),
                   options = list(placeholder = "None",
                                  maxItems = 1))
  })
  ## generate bar plot
  output$cat_plot_bar <- renderPlot({
    req(input$cat_plot_var)
    plot_cat_bar(data = dat_recode(), 
                 pred_cat_selected = input$cat_plot_var, 
                 pred_cat_fill = input$cat_fill)
  })
  ## generate stacked percentage bar plot
  output$cat_plot_stack <- renderPlot({
    req(input$cat_plot_var)
    plot_cat_stack(data = dat_recode(), 
                   pred_cat_selected = input$cat_plot_var, 
                   pred_cat_fill = input$cat_fill)
  })
  ## isolate and generate legend for categorical variable plots
  output$cat_plot_legend <- renderPlot({
    req(input$cat_plot_var)
    plot_fill_legend <- plot_cat_stack(data = dat_recode(), 
                                       pred_cat_selected = input$cat_plot_var, 
                                       pred_cat_fill = input$cat_fill) +
      theme(legend.position = "bottom", 
            legend.title=element_text(size = 14,
                                      family = "Helvetica Neue",
                                      face = "bold"))
    get_legend(plot_fill_legend) %>% as_ggplot
  })
  ## determine whether linear model ready for generation (i.e. target + >= 1 predictor)
  lm_ready <- reactive({
    if(length(input$target) == 1 & sum(length(input$preds_cont), length(input$preds_cat)) >= 1) {
      TRUE
    } else {
      FALSE
    }
  })
  ## function to generate linear model formula for full model and vif model
  lm_formula_func <- function(vif_model) {
    if(vif_model == FALSE) {
      ## full model
      model_terms <- c(dat_tran() %>% .$tran_form, dat_int())
    } else if (length(input$preds_cat) == 0) {
      ## vif model w/ no categorical variables selected, discard polynomials
      model_terms <- c(dat_tran() %>% .$tran_form %>% 
                         discard(.p=str_detect, pattern = "poly"))
    } else {
      ## vif model excluding selected categorical variables
      model_terms <- c(dat_tran() %>% .$tran_form %>% setdiff(input$preds_cat) %>% 
                         discard(.p=str_detect, pattern = "poly"))
    }
    ## generate model formula - note first term always target
    paste0(model_terms[1], " ~ ", 
           paste0(model_terms[2:length(model_terms)],  
                  collapse = " + "))
  }
  ## linear model formula text
  lm_formula_txt <- reactive({
    if(lm_ready() == TRUE) {
      lm_formula_func(vif_model = FALSE)
    }
  })
  ## print linear model formula
  output$lm_formula <- renderUI({
    if(lm_ready() != TRUE) {
      HTML(text = "<b>Model Formula: </b>")
    } else {
      HTML(text = paste0("<b>Model Formula: </b>", lm_formula_txt(), "<p>"))
    }
  })
  ## run linear model
  lm_model <- reactive({
    if(lm_ready() == TRUE) {
      lm(formula = lm_formula_txt(), data = dat_recode()) 
    }
  })
  ## generate linear model summary
  observeEvent(input$preds_cont, {
    output$lm_summary <- renderPrint({
      if(lm_ready() == TRUE) {
        summary(lm_model())
      }
    })
  })
  ## generate vif statistics (requirement: >= 2 continuous predictors)
  output$lm_vif <- renderDataTable({
    if(length(input$preds_cont) >= 2) {
      lm(formula = lm_formula_func(vif_model = TRUE), data = dat_recode()) %>%
        vif %>% 
        data.frame %>%
        rownames_to_column(var = "Variable") %>%
        set_colnames(c("Variable", "VIF")) %>%
        mutate(VIF = VIF %>% round(digits = 3)) %>%
        arrange(-VIF) %>% 
        datatable(options=list(dom='t'), rownames = F)
    }
  })
  ## generate linear model comparison anova
  observeEvent(input$lm_comp_run, {
    output$lm_comp_summary <- renderPrint({
      if(length(input$lm_comp_1) >= 1 & length(input$lm_comp_2) >= 1) {
        model_1 <- lm(formula = formula(input$lm_comp_1), data = dat_recode())
        model_2 <- lm(formula = formula(input$lm_comp_2), data = dat_recode())
        anova(model_1, model_2)
      }
    })
  })
  ## linear model diagnostic plots
  output$lm_plots <- renderPlot({
    if(max(length(input$preds_cont), length(input$preds_cat)) >= 1) {
      par(mfrow = c(2,2))
      plot(lm_model())
    }
  })
}

shinyApp(ui, server)