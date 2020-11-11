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

## bug list
## make sure that wrong file type message appears in sidebar
## correlation matrix hide feature not working
## bar_plot() not working with data explorer

options(shiny.maxRequestSize = 30*1024^2)

## text objects
text_inst <- "This app allows users to interactively build and tune linear models. Users are able to select
from more than 100 pre-loaded datasets, or upload their own dataset, and build their model. To upload a dataset,
the data must be in .RData format. After selecting a dataset, you can begin to build your model immediately,
or explore the data using tabs 1 and 2."

text_data <- "<b>Data Overview: </b>This page allows you to examine your data in its raw form, independent
of any model transformations. Use the search, sort, and filter options to further explore your data. When 
using a pre-loaded dataset, this page also shows the data's help file.</p>"

text_corr <- "<b>Correlation Matrix: </b>Shown below is a correlation matrix which includes univariate 
distributions, scatterplots, and correlation coefficients for all continuous variables in the dataset. Note
that correlation matrices can take some time to render; to output a correlation matrix, please click the
'generate matrix' button below. Other application operations will be queued until the matrix is finished
rendering.</p>"

text_bar <- "</p><b>Bar Plots: </b>Shown below are bar plots for categorical variables in the dataset. If the 
dataset does not include any categorical variables, then this section will be blank.</p>"

text_lm <- "<b>Linear Model: </b>This page shows the summary output for your linear model. Note that you 
must select a target variable and at least one predictor variable, whether continuous or categorical, to
generate a linear model.</p>"

text_vif <- "<b>Variable Inflation Factor (VIF) to Diagnose Multicollinearity: </b> You must select at least 
two continuous predictors to obtain VIF statistics. Note that the VIF statistics are generated from a model 
which does not do not incorporate polynomials, categorical variables, or interaction terms.</p>"

text_comp <- "<b>Model Comparisons: </b>It is often beneficial to compare two models to find out if their
different forms are statistically different. Even if one model performs nominally better than another model,
that does not mean that the better performing model is meaningfully different than the comparison model, and
it is usually recommended to select the less complex model. The tool below compares two linear models using
an analysis of variance (ANOVA) test. To use it, copy and paste the formula from the 'model formula' section
above and click 'Compare Models'.</p>"

text_cont <- "<b>Continuous Variables: </b>This page shows four different charts to better understand a 
continuous variable of interest. Note that the 'select fill' option can be used to color the histogram, 
density, and scatter plots by a selected categorical variable. All charts on this page are responsive to 
the variable transformations specified on the side-bar menu. Users should know that that polynomial 
transformations do not change the underlying data, so these transformations will only appear on the 
scatter plot</p>"

text_diag <- "<b>Diagnostic Plots: </b>This page shows four key model diagnostic plots. These plots can be
used to determine whether your model satisfies regression model assumptions. All charts on this page are 
responsive to the variable transformations specified on the side-bar menu. Note that you must selected a
target variable and at least one predictor variable, whether continuous or categorical, to generate a linear
model and its associated diagnostic plots</p>"

text_cat <- "<b>Categorical Variables: </b>This page shows two charts to better understand a categorical
variable of interest; a bar plot showing counts by category; and a stacked bar-chart which shows percentage
totals per category. Note that the 'select fill' option can be used to color plots by another categorical 
variable. You must selected at least two categorical variables for the fill option to be available. When no 
fill is selected, the plot are still colored, although the fill is based on the underlying variable categories.</p>"

## source packages for data frames
#pkg_list <- c("mlbench", "AppliedPredictiveModeling", "datasets")
## obtain names of data objects in each package
#object_list <- pkg_list %>% 
#m ap(~list(.x = data(package = .x)$results %>%
#            data.frame %>% .$Item %>% sort %>% as.character)) %>% unlist %>% as.character
## exclude any objects with parenthesis in name
# object_list <- object_list[!grepl("\\(", object_list)]
## load all remaining objects into environment
# data(list = object_list)
## determine whether object is a data frame
# data_list <- object_list %>% map(~(is.data.frame(get(.x)))) %>% as.character 
# names(data_list) <- object_list
## keep only names of data frames
# choice_list <- data_list %>% .[matches("TRUE", vars=.)] %>% names %>% sort
choice_list <- c("diamonds", "mtcars")

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
  dashboardHeader(title = "Dom's Linear Model Builder", titleWidth = 375),
  ## dashboard sidebar
  dashboardSidebar(
    width = 375,
    sidebarMenu(
      menuItem("Select Data",
               ## data select
               radioButtons(inputId = "data_source", label = "Data Source:",
                            choices = c("BaseR", "Upload"), 
                            selected = "BaseR",
                            inline = T),
               uiOutput(outputId = "data_select_ui"),
               div(style = "height:5px")),
      menuItem("Filter Data",
               ## data select
               fluidRow(column(width = 6,
                               radioButtons(inputId = "data_complete", label = "Complete Rows Only:",
                                            choices = c("Yes", "No"), 
                                            selected = "No",
                                            inline = FALSE)),
                        column(width = 6, 
                               radioButtons(inputId = "data_sample", label = "Sample Rows:",
                                            choices = c("Yes", "No"), 
                                            selected = "No",
                                            inline = FALSE))),
               uiOutput("data_sample_ui")),
      menuItem("Recode Data",
               uiOutput("data_recode")),
      menuItem("Model Terms", 
               fluidRow(
                 column(width = 6,
                        ## target variable ui
                        uiOutput("target_ui")),
                 column(width = 6, 
                        ## target variable transformation (exclude polynomials)
                        selectizeInput(inputId = "tran_target", 
                                       label = "Transformation:",
                                       choices = tran_opts[c("Simple", "Logarithmic")],
                                       multiple = FALSE,
                                       selected = "None"))),
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
                 column(width = 6, 
                        actionButton(inputId = "intTermAdd", label = "Add Interaction",
                                     width = "140px")),
                 column(width = 6, 
                        actionButton(inputId = "intTermRemove", label = "Remove Interaction",
                                     width = "140px"))),
               div(style = "height:5px"),
               uiOutput(outputId = "preds_int_ui"))
    )
  ),
  dashboardBody(
    fluidRow(
      use_waiter(),
      tabBox(width = 12, height = NULL,
             ## data overview, including table and help file
             tabPanel("Data Overview",
                      HTML(text = text_data),
                      dataTableOutput(outputId = "data"), 
                      htmlOutput(outputId = "data_dictionary")),
             ## correlation matrix
             tabPanel("Summary Plots",
                      HTML(text = text_corr),
                      actionButton(inputId = "corr_matrix_generate", label = "Generate Matrix"),
                      div(style = "height:10px"),
                      uiOutput(outputId = "corr_matrix_ui"),
                      HTML(text = text_bar),
                      uiOutput(outputId = "bar_plot")),
             ## continuous variable plots
             tabPanel("Continuous Variables",
                      
                      HTML(text = text_cont),
                      fluidRow(
                        ## variable select
                        column(width = 2,
                               selectizeInput(inputId = "cont_plot_var", label = "Select Variable", 
                                              choices = "", 
                                              multiple = TRUE,
                                              options = list(placeholder = "None",
                                                             maxItems = 1))),
                        ## fill select
                        column(width = 2,
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
             tabPanel("Categorical Variables", 
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
                               HTML(text = text_vif),
                               dataTableOutput(outputId = "lm_vif"))),
                      ## model comparisons
                      fluidRow(column(width = 8, HTML(text = text_comp))),
                      fluidRow(c(1, 2) %>% 
                                 map(~column(width = 4, 
                                             textInput(inputId = paste0("lm_comp_", .x), 
                                                       label = paste0("Model ", .x), 
                                                       value = "", placeholder = "Linear model formula")))),
                      div(style = "height:2px"),
                      actionButton(inputId = "lm_comp_run", label = "Compare Models"),
                      div(style = "height:2px"),
                      verbatimTextOutput("lm_comp_summary")),
             ## linear model diagnostic plots
             tabPanel("Diagnostic Plots",
                      HTML(text = text_diag),
                      plotOutput("lm_plots", height = "600px"))
      )
    )
  )
)

server <- function(input, output, session) {
  ## data select
  output$data_select_ui <- renderUI({
    if(input$data_source == "BaseR") {
      selectizeInput(inputId = "data_file", label = "Select Dataset:",
                     choices = c("diamonds", choice_list),
                     multiple = T,
                     options = list(placeholder = "Click to select",
                                    maxItems = 1))
    } else {
      fileInput(inputId = "data_file", label =  "Upload File:", accept = c(".RData"))
    }
  })
  ## data environment for sample size
  dat_sample <- reactive({
    if(input$data_source == "BaseR") {
      dat <-  get(input$data_file)
    } else {
      file_type <- tools::file_ext(input$data_file$name)
      switch(file_type,
             RData = load(input$data_file$datapath),
             validate("Invalid file; Please upload a .RData file")
      )
      dat <- sub(".RData$", "", basename(input$data_file$name)) %>% get
    }
    if(input$data_complete == "Yes") {
      na.omit(dat)
    } else {
      dat
    } 
  })
  ## data sample ui
  output$data_sample_ui <- renderUI({
    if(input$data_sample == "Yes") {
      sliderInput(inputId = "data_sample_size", label = "Sample Size:",
                  min = 1,
                  max = nrow(dat_sample()),
                  step = 1,
                  value = nrow(dat_sample()), width = "100%")
    }
  })
  ## data environment for app usage
  dat <- reactive({
    if(input$data_source == "BaseR") {
      dat <-  get(input$data_file)
    } else {
      file_type <- tools::file_ext(input$data_file$name)
      switch(file_type,
             RData = load(input$data_file$datapath),
             validate("Invalid file; Please upload a .RData file")
      )
      dat <- sub(".RData$", "", basename(input$data_file$name)) %>% get
    }
    if(input$data_complete == "Yes" & input$data_sample == "Yes") {
      na.omit(dat) %>% 
        sample_n(input$data_sample_size)
    } else if (input$data_complete == "Yes" & input$data_sample == "No") {
      na.omit(dat)
    } else if (input$data_complete == "No" & input$data_sample == "Yes") {
      dat %>% sample_n(input$data_sample_size)
    } else {
      dat
    } 
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
      if(input$data_source == "BaseR") {
        Rd <- Rd_fun(help(input$data_file))
        outfile <- tempfile(fileext = ".html")
        Rd2HTML(Rd, outfile, package = "",
                stages = c("install", "render"))
        includeHTML(outfile)
      } else {
        "Data dictionaries are not available for uploaded files"
      }
    }
  })
  ## variable names
  dat_vars <- reactive({
    list(continuous = dat_recode() %>% select_if(is.numeric) %>% colnames %>% sort,
         categorical = c("is.factor", "is.character") %>% 
           map(~select_if(dat_recode(), .x) %>% 
                 colnames) %>% unlist %>% sort)
  })
  ## target variable ui
  output$target_ui <- renderUI({
    selectizeInput(inputId = "target", label = "Target Variable:", 
                   choices = dat_vars()[["continuous"]],
                   multiple = TRUE,
                   selected = NULL,
                   options = list(placeholder = "Click to select",
                                  maxItems = 1))
  })
  ## continuous variable ui
  output$preds_cont_ui <- renderUI({
    selectizeInput(inputId = "preds_cont", 
                   label = "Continuous Variables:", 
                   choices = setdiff(dat_vars()[["continuous"]], input$target),
                   multiple = TRUE,
                   options = list(placeholder = "None"))
  })
  ## categorical variable ui
  output$preds_cat_ui <- renderUI({
    selectizeInput(inputId = "preds_cat",
                   label = "Categorical Variables:",
                   choices = dat_vars()[["categorical"]],
                   multiple = TRUE,
                   options = list(placeholder = "None"))
  })
  ## update variable selections when data changes
  observeEvent(input$data_file, {
    updateSelectizeInput(session, inputId = "preds_cont", selected = "")
    updateSelectizeInput(session, inputId = "preds_cat", selected = "")
  })
  ## correlation matrix ui
  observeEvent(input$corr_matrix_generate, {
    output$corr_matrix_ui <- renderUI(plotOutput(outputId = "corr_matrix", height = 600))
  })
  ## correlation matrix render
  observeEvent(input$corr_matrix_generate, {
    output$corr_matrix <- renderPlot({
      Waiter$new(id = "corr_matrix")$show()
      dat_recode() %>% select_if(is.numeric) %>% ggpairs})
  })
  ## clear correlation matrix when data changes - broken
  observeEvent(input$data_file, {
    shinyjs::hide("corr_matrix_ui")
  })
  ## bar plot ui
  output$bar_plot <- renderUI({
    if(length(dat_vars()[["categorical"]]) != 0) {
      plotOutput(outputId = "bar_plots", height = 300)
    }
  })
  ## bar plot render output$bar_plots <- renderPlot(dat() %>% plot_bar())
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
  ## generate data frame to store variable transformations
  dat_tran <- reactive({
    tran_preds <- input$preds_cont %>% 
      map(~input[[paste0("trans_", .x)]]) %>% 
      unlist
    ## create data frame with column 1 = variable name and column 2 = transformation
    tran_list <- data.frame(var_name = c(input$target, input$preds_cont),
                            tran_list = c(input$tran_target, tran_preds),
                            stringsAsFactors = F) %>%
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
      model_terms <- c(dat_tran() %>% .$tran_form %>% .[.!= input$preds_cat] %>% 
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
  output$lm_summary <- renderPrint({
    if(lm_ready() == TRUE) {
      summary(lm_model())
    }
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