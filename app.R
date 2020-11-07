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
library(DataExplorer)
library(waiter)

options(shiny.maxRequestSize = 30*1024^2)

## Text objects

text_fun <- function(x) {
  div(style = "height:10px")
  HTML(x)
  div(style = "height:10px")
}

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
which does not do not incorporate categorical variables or interaction terms.</p>"

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

## set transformation options for target variable and continuous predictors
tran_opts <- list(
  "Simple" = c("None",  "Reciprocal", "Squared", "Square Root"),
  "Logarithmic" = c("Natural", "Base 2", "Base 10"),
  "Polynomial" = c(paste (seq(from = 2, to = 5), rep("degree", 5)))
)

## create function to implement transformation(s) on target/predictors
tran_func <- function(pred, trans) {
  switch(trans,
         "None" = pred,
         "Reciprocal" = paste0("I(1/", pred, ")"),
         "Squared" = paste0("I(", pred, "^2)"),
         "Square Root" = paste0("sqrt(", pred, ")"),
         "Natural" = paste0("log(", pred, ")"),
         "Base 2" = paste0("log2(", pred, ")"),
         "Base 10" = paste0("log10(", pred, ")"),
         "2 degree" = paste0("poly(", pred, ", 2)"),
         "3 degree" = paste0("poly(", pred, ", 3)"),
         "4 degree" = paste0("poly(", pred, ", 4)"),
         "5 degree" = paste0("poly(", pred, ", 5)"),
  )
}

## function to obtain counts of selected categorical variable and fill (if specified)
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
## function to render categorical variable bar chart with fill (if specified)
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
## function to render stacked categorical bar chart with fill (if specified)
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

## obtain list of loaded data sets
# pkg_names <- setdiff(loadedNamespaces(), 
#                      c("viridisLite", "xtable", "forcats", "shinyWidgets", "lubridate"))
# pkg_list <- pkg_names %>% 
#   map(~data(package = .x)$results[, "Item"]) 
# names(pkg_list) <- pkg_names
# pkg_list <- pkg_list[lapply(pkg_list,length)>0]

data_list <- ls("package:datasets") %>% 
  map(~get(.x))
names(data_list) <- ls("package:datasets")
data_list <- data_list[lapply(data_list,class) == "data.frame"] %>% names

## create base UI
ui <-  dashboardPage(
  dashboardHeader(title = "Dom's Linear Model Builder", titleWidth = 375),
  dashboardSidebar(
    width = 375,
    sidebarMenu(
      menuItem("Instructions", tabName = "dataset"),
      menuItem("Select Dataset", tabName = "dataset",
               radioButtons("type", "Data Source:",
                            choices = c("BaseR", "Upload"), selected = "BaseR",
                            inline = T),
               uiOutput("dataselect"),
               div(style = "height:5px")),
      # ls("package:datasets")
      fluidRow(
        column(width = 6,
               ## target variable selector
               selectizeInput(inputId = "target", 
                              label = "Target Variable:", 
                              choices = "",
                              multiple = TRUE,
                              selected = NULL,
                              options = list(placeholder = "Click to select",
                                             maxItems = 1))),
        column(width = 6, 
               ## target variable transformation options (excludes polynomials)
               selectizeInput(inputId = "tran_target", 
                              label = "Transformation:",
                              choices = tran_opts[c("Simple", "Logarithmic")],
                              multiple = FALSE,
                              selected = "None"))),
      fluidRow(
        column(width = 6,
               ## continuous predictors selector
               selectizeInput(inputId = "preds_cont", 
                              label = "Continuous Variables:", 
                              choices = "",
                              multiple = TRUE,
                              options = list(placeholder = "None"))),
        column(width = 6,
               ## categorical predictors selector
               selectizeInput(inputId = "preds_cat",
                              label = "Categorical Variables:",
                              choices = "",
                              selected = NULL,
                              multiple = TRUE,
                              options = list(placeholder = "None")))),
      ## dynamic UIs for continuous variable transformations and interaction terms
      menuItem("Variable Transformations", tabName = "test",
               uiOutput(outputId = "preds_tran_ui")),
      menuItem("Interaction Terms", tabname = "test2",
               div(style = "height:5px"),
               ## buttons to add and remove interaction terms
               fluidRow(
                 column(width = 6, 
                        actionButton(inputId = "intTermAdd", 
                                     label = "Add Interaction",
                                     width = "140px")),
                 column(width = 6, 
                        actionButton(inputId = "intTermRemove", 
                                     label = "Remove Interaction",
                                     width = "140px"))),
               div(style = "height:5px"),
               uiOutput(outputId = "preds_int_ui"))
      ##,actionButton("blah", label = "Apply Changes", width = "100%")
    )
  ),
  dashboardBody(
    fluidRow(
      use_waiter(),
      tabBox(width = 12, height = NULL,
             ## data dictionary tab
             tabPanel("Data Overview",
                      HTML(text_data),
                      dataTableOutput("dataframe"), 
                      htmlOutput(outputId = "data_dictionary")),
             ## correlation matrix (can take long time to load)
             tabPanel("Summary Plots",
                      HTML(text_corr),
                      actionButton("corr_generate",
                                   "Generate Matrix"),
                      div(style = "height:10px"),
                      uiOutput("cor_matrix_ui"),
                      HTML(text_bar),
                      uiOutput("bar_plot_ui")),
             ## plots for continuous variables transformation analysis
             tabPanel("Cont. Variables",
                      HTML(text_cont),
                      ## continuous variable selector
                      fluidRow(column(width = 3,
                                      selectizeInput(inputId = "cont_plot", 
                                                     label = "Select Variable", 
                                                     choices = "", 
                                                     selected = NULL,
                                                     multiple = TRUE,
                                                     options = list(placeholder = "None",
                                                                    maxItems = 1))),
                               column(width = 3,
                                      selectizeInput(inputId = "plot_fill", 
                                                     label = "Select Fill", 
                                                     choices = "", 
                                                     selected = NULL,
                                                     multiple = TRUE,
                                                     options = list(placeholder = "None",
                                                                    maxItems = 1))),
                               column(width = 6,
                                      plotOutput("legend", height = "75px"))),
                      # histogram, qq plot, and scatter plots for continuous variable selected
                      fluidRow(column(width = 6, 
                                      plotOutput(outputId = "plot_hist", height = "300px")),
                               column(width = 6, 
                                      plotOutput(outputId = "plot_dens", height = "300px"))),
                      fluidRow(column(width = 6, 
                                      plotOutput(outputId = "plot_scatter", height = "300px")),
                               column(width = 6, 
                                      plotOutput(outputId = "plot_qq", height = "300px")))),
             ## categorical selector for plot diagnostics
             tabPanel("Cat. Variables", 
                      HTML(text_cat),
                      fluidRow(column(width = 3,
                                      selectizeInput(inputId = "cat_plot",
                                                     label = "Select Variable",
                                                     choices = "",
                                                     selected = NULL,
                                                     multiple = TRUE,
                                                     options = list(placeholder = "None",
                                                                    maxItems = 1))),
                               column(width = 3, uiOutput("cat_fill_ui")),
                               column(width = 6,
                                      plotOutput("plot_cat_legend", height = "75px"))),
                      fluidRow(column(width = 6, plotOutput("plot_bar")),
                               column(width = 6, plotOutput("plot_stack")))),
             ## linear model output generator, includes formula, summary, and VIF statistics
             tabPanel("Linear Model",
                      HTML(text_lm),
                      fluidRow(column(width = 8, 
                                      htmlOutput(outputId = "lm_formula"),
                                      verbatimTextOutput(outputId = "lm_summary")),
                               column(width = 4, 
                                      HTML(text_vif),
                                      dataTableOutput(outputId = "lm_vif_stats"))),
                      fluidRow(column(width = 8, HTML(text_comp))),
                      fluidRow(column(width = 4, 
                                      textInput(inputId = "comp_lm_1", label = "Model 1", 
                                                value = "", placeholder = "Linear model formula")),
                               column(width = 4, 
                                      textInput(inputId = "comp_lm_2", label = "Model 2", 
                                                value = "", placeholder = "Linear model formula"))),
                      div(style = "height:2px"),
                      actionButton(inputId = "lm_comp_run", label = "Compare Models"),
                      verbatimTextOutput("lm_comp_summary")),
             ## linear model diagnostic plots
             tabPanel("Diagnostic Plots",
                      HTML(text_diag),
                      plotOutput("lm_diagnostics", height = "600px"))
      )
    )
  )
)

server <- function(input, output, session) {
  ## render data select UI
  output$dataselect <- renderUI({
    if(input$type == "BaseR") {
      selectizeInput("file", "Select Dataset",
                     choices = c("mtcars", "diamonds"),
                     multiple = T,
                     options = list(placeholder = "Click to select",
                                    maxItems = 1))
    } else {
      fileInput("file", NULL, accept = c(".RData"))
    }
  })
  ## define data frame and variable choice lists
  dat <- reactive({
    if(input$type == "BaseR") {
      get(input$file)
    } else {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      switch(ext,
             RData = load(input$file$datapath),
             validate("Invalid file; Please upload a .RData file")
      )
      return(sub(".RData$", "", basename(input$file$name))) %>% get
    }
  })
  var_names <- reactive({
    req(input$file)
    list(continuous = dat() %>% select_if(is.numeric) %>% colnames %>% sort,
         categorical = c(dat() %>% select_if(is.factor) %>% colnames,
                         dat() %>% select_if(is.factor) %>% colnames) %>% sort)
  })
  ## update target selector based on data frame
  observeEvent(input$file, {
    updateSelectizeInput(session, "target", choices = var_names()[["continuous"]])
  })
  ## update continuous variable selector options to exclude target variable
  observeEvent(input$target, {
    updateSelectizeInput(session, inputId = "preds_cont", 
                         choices = var_names()[["continuous"]] %>% .[.!=input$target],
                         selected = isolate(input$preds_cont))
  })
  ## update continuous variable selector when data frame changes
  observeEvent(input$file, {
    updateSelectizeInput(session, inputId = "preds_cont",
                         selected = "")
    updateSelectizeInput(session, inputId = "preds_cat",
                         selected = "")
  })
  ## update categorical variable selector based on data frame
  observeEvent(input$file, {
    updateSelectizeInput(session, "preds_cat", choices = var_names()[["categorical"]])
  })
  ## render data table
  output$dataframe <- renderDataTable({
    if(length(input$file) != 0) {
      data.table(dat(), 
                 options = list(pageLength = 5,
                                lengthMenu = list(c(5, 10, 25, 50), 
                                                  c("5", "10", "25", "50"))))
    }
  })
  ## render help file from selected data set (if available)
  output$data_dictionary <- renderUI({
    if(length(input$file) != 0) {
      if(input$type == "BaseR") {
        Rd <- Rd_fun(help(input$file))
        outfile <- tempfile(fileext = ".html")
        Rd2HTML(Rd, outfile, package = "",
                stages = c("install", "render"))
        includeHTML(outfile)
      } else {
        "Data dictionaries are not available for uploaded files"
      }
    }
  })
  ## corr matrix ui
  observeEvent(input$corr_generate, {
    output$cor_matrix_ui <- renderUI(plotOutput(outputId = "cor_matrix", height = 600))
  })
  ## render correlation matrix for continuous variables
  observeEvent(input$corr_generate, {
    output$cor_matrix <- renderPlot({
      Waiter$new(id = "cor_matrix")$show()
      dat() %>% select_if(is.numeric) %>% ggpairs})
  })
  ## clear bar plots when switching datasets
  observeEvent(input$dataset, {
    output$cor_matrix_ui <- NULL
  })
  ## render bar plot ui
  output$bar_plot_ui <- renderUI({
    if(length(var_names()[["categorical"]]) != 0) {
      plotOutput(outputId = "bar_plots", height = 300)  
    }
  })
  ## render bar plots matrix for categorical variables
  output$bar_plots <- renderPlot(dat() %>% plot_bar())
  ## render UI for continuous variable transformations
  output$preds_tran_ui <- renderUI({
    ## function to generate transformation selector for each continuous variable
    tran_select_preds <-  function(predsContName) {
      input_name <- paste0("trans_", predsContName)
      selectizeInput(inputId = input_name, 
                     label = predsContName,
                     choices = tran_opts,
                     multiple = FALSE,
                     selected = isolate(input[[input_name]])) 
    }
    ## to generate a zig-zag UI for the continuous variable transformations, 
    ## we first generate a sequence of numbers the length of the number of 
    ## continuous variables; we keep only odd numbers from this sequence
    ## as these serve as row indices for the transformation selectors  
    tran_row_idx <- length(input$preds_cont) %>% seq_len
    tran_row_idx <- tran_row_idx[tran_row_idx %% 2 == 1]
    ## we next generate the UI itself by iterating through the list of odd numbers, 
    ## at each step using the row index and the row index + 1 to generate the
    ## actual selector. a conditional if/else format is used to ensure that if the
    ## number of continuous variables is odd, then the last line will only include
    ## a single selector
    tran_row_idx %>% 
      map(~ if(!is.na(input$preds_cont[.x + 1])) {
        tran_left <- input$preds_cont[.x]
        tran_right <- input$preds_cont[.x + 1]
        fluidRow(column(width = 6, 
                        tran_select_preds(predsContName = tran_left)),
                 column(width = 6, 
                        tran_select_preds(predsContName = tran_right)))
      } else {
        tran_left <- input$preds_cont[.x]
        fluidRow(column(width = 6, 
                        tran_select_preds(predsContName = tran_left)))
      }
      )
  })
  ## to generate the UI for interaction terms, we must first identify the number
  ## of interaction terms the user would like to create based on the sum of their
  ## clicks of the add and remove interaction term buttons. the code below ensures
  ## that the counter stops at zero and does not continue into negative numbers
  click_total <- reactiveVal(0)
  observeEvent(input$intTermAdd, click_total(click_total() + 1))
  observeEvent(input$intTermRemove, click_total(max(0, click_total() - 1 )))
  ## render UI for model interaction terms
  output$preds_int_ui <- renderUI({
    ## function to generate variable selectors for interaction terms
    intTermSelector <- function(intTermId, intNumber, intTermPosition) {
      selectizeInput(inputId = intTermId, 
                     label = paste("Interaction", intNumber, " - Term", intTermPosition),
                     choices = c("None", input$preds_cont, input$preds_cat),
                     multiple = FALSE,
                     selected = isolate(input[[intTermId]]))
    }
    ## to generate UI for interaction terms, iterate through a sequence of numbers the
    ## length of the interaction terms, creating two selectors for interaction (i.e. 
    ## left and right sides of the interaction respectively)
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
  ## here, we create a reactive data frame which stores any transformations associated
  ## with the target variable and continuous predictors. the first step is to create a 
  ## vector with list of all continuous variable transformations
  dat_tran <- reactive({
    tran_preds <- input$preds_cont %>% 
      map(~input[[paste0("trans_", .x)]]) %>% 
      unlist
    ## create data frame in which first column includes names of target and continuous
    ## predictors, and second column includes any transformations of these variables.
    ## next, apply the transformation function using these two columns as inputs. note
    ## that this process simply creates the transformation statement, rather than 
    ## transforming the underlying data frame itself
    tran_list <- data.frame(var_list = c(input$target, input$preds_cont),
                            tran_list = c(input$tran_target, tran_preds),
                            stringsAsFactors = F) %>%
      rowwise %>%
      mutate(tran_form = tran_func(var_list, tran_list)) %>%
      ungroup %>%
      select(-tran_list) %>%
      ## finally, append the categorical variable names to this data frame, even though
      ## they cannot have transformations performed on them
      rbind(data.frame(var_list = input$preds_cat,
                       tran_form = input$preds_cat))
  })
  ## create an object which stores the text of interaction terms to be included in linear 
  ## model. the first step in this process is to create a two-column data frame in which
  ## each row corresponds to one set of interaction terms with the left and right-hand 
  ## sides of the interaction corresponding to columns 1 and 2 respectively
  dat_int <- reactive({
    if(click_total() >= 1) {
      dummy <- data.frame(intTerm1 = seq_len(click_total()) %>% 
                            map(~input[[paste0("int", .x, "term1")]]) %>% unlist,
                          intTerm2 = seq_len(click_total()) %>% 
                            map(~input[[paste0("int", .x, "term2")]]) %>% unlist) %>% 
        ## filter out any interaction terms in which either selected is set to 'none' or
        ## in which both selectors are set to the same variable
        filter((intTerm1 != "None" | intTerm2 != "None") & 
                 as.character(intTerm1) != as.character(intTerm2))
      ## filter out duplicate combinations of interaction terms in different order (i.e. c(x, y) == c(y, x))
      if(nrow(dummy) != 0) {
        dummy <- unique(t(apply(dummy, 1, sort))) %>%
          data.frame %>%
          `colnames<-`(c("intTerm1", "intTerm2"))
        ## join list of interaction terms to transformation list to obtain correct variable form and 
        ## then combine both sides of interaction term into a single column and extract that column
        dummy %>%
          inner_join(dat_tran(), by = c("intTerm1" = "var_list")) %>%
          inner_join(dat_tran(), by = c("intTerm2" = "var_list")) %>% 
          select(intTerm1 = 3, intTerm2 = 4) %>% 
          mutate(int_form = paste0(intTerm1, "*", intTerm2)) %>%
          .$int_form
      }
    }
  })
  ## update the plot input choices to reflect continuous variable selections, including both target
  ## variable and selected continuous predictors
  vars_cont <- reactive(c(input$target, input$preds_cont))
  observeEvent(vars_cont(), {
    updateSelectizeInput(session, "cont_plot", 
                         choices = vars_cont(), selected = isolate(input$cont_plot))
  })
  ## update the plot fill choices to reflect categorical variable selections
  observeEvent(input$preds_cat, {
    updateSelectizeInput(session, "plot_fill",
                         choices = input$preds_cat, selected = isolate(input$plot_fill))
  })
  ## obtain transformation statements for target variable and selected continuous variable
  ## from transformation data frame. these will be used in create plot statements
  plot_selected <- reactive({
    dat_tran() %>% filter(var_list == input$cont_plot) %>% .$tran_form
  })
  plot_target <- reactive({
    dat_tran() %>% filter(var_list == input$target) %>% .$tran_form
  })
  ## create function to store input requirements for plots
  plot_reqs <- function(x) {
    req(input$target)
    req(input$cont_plot)
  }
  ## generate histogram showing distribution of selected continuous variable factoring in
  ## chart type and fill
  output$plot_hist <- renderPlot({
    plot_reqs()
    if(input$plot_fill %>% length != 0) {
      plot <-  dat() %>% ggplot(aes_string(x = plot_selected(), fill = input$plot_fill)) +
        geom_histogram(alpha = 0.5)
    } else {
      plot <- dat() %>% ggplot(aes_string(x = plot_selected())) + geom_histogram()
    }
    plot + theme(legend.position = "none")
  })
  output$plot_dens <- renderPlot({
    plot_reqs()
    if(input$plot_fill %>% length != 0) {
      plot <-  dat() %>% ggplot(aes_string(x = plot_selected(), fill = input$plot_fill))
    } else {
      plot <- dat() %>% ggplot(aes_string(x = plot_selected()))
    }
    plot + geom_density(alpha = 0.5) + theme(legend.position = "none")
  })
  ## function to generate scatter plot where y-axis is target variable and x-axis is specified
  ## continuous variable. note that unlike the histogram, we store this as a function, as it 
  ## will be used in the next step to extract the legend
  plot_scatter_func <- function(x, fill) {
    if(fill %>% length != 0) {
      plot <-  dat() %>% ggplot(aes_string(x = x,
                                           y = plot_target(),
                                           color = fill))
    } else {
      plot <- dat() %>% ggplot(aes_string(x = x,
                                          y = plot_target()))
    }
    plot + geom_point()
  }
  ## generate scatter plot 
  output$plot_scatter <- renderPlot({
    plot_reqs()
    plot_scatter_func(x = plot_selected(), fill = input$plot_fill) +
      theme(legend.position = "none")
  })
  ## legend render
  output$legend <- renderPlot({
    plot_reqs()
    plot_fill_legend <- plot_scatter_func(x = plot_selected(), fill = input$plot_fill) +
      theme(legend.position = "bottom", 
            legend.title=element_text(size = 14,
                                      family = "Helvetica Neue",
                                      face = "bold"),
            legend.key.size = unit(1.5, "line")) +
      guides(colour = guide_legend(override.aes = list(size=5)))
    get_legend(plot_fill_legend) %>% as_ggplot
  })
  ## generate qq plot of selected continuous variable
  output$plot_qq <- renderPlot({
    plot_reqs()
    dat() %>%
      ggplot(aes_string(sample = plot_selected())) +
      stat_qq() +
      stat_qq_line()
  })
  ## update categorical variable selector for plots
  observeEvent(input$preds_cat, {
    updateSelectizeInput(session, "cat_plot", 
                         choices = input$preds_cat, selected = isolate(input$cat_plot))
  })
  ## update categorical variable fill selector for plots
  output$cat_fill_ui <- renderUI({
    selectizeInput(inputId = "cat_fill", 
                   label = "Select Fill", 
                   choices = input$preds_cat %>% .[.!=input$cat_plot], 
                   selected = isolate(input$cat_fill),
                   multiple = TRUE,
                   options = list(placeholder = "None",
                                  maxItems = 1))
  })
  ## render categorical variable bar plot
  output$plot_bar <- renderPlot({
    req(input$cat_plot)
    plot_cat_bar(data = dat(), pred_cat_selected = input$cat_plot, pred_cat_fill = input$cat_fill)
  })
  ## render categorical variable stacked plot
  output$plot_stack <- renderPlot({
    req(input$cat_plot)
    plot_cat_stack(data = dat(), pred_cat_selected = input$cat_plot, pred_cat_fill = input$cat_fill)
  })
  ## render legend for categorical variables
  output$plot_cat_legend <- renderPlot({
    req(input$cat_plot)
    plot_fill_legend <- plot_cat_stack(data = dat(), pred_cat_selected = input$cat_plot, pred_cat_fill = input$cat_fill) +
      theme(legend.position = "bottom", 
            legend.title=element_text(size = 14,
                                      family = "Helvetica Neue",
                                      face = "bold"))
    get_legend(plot_fill_legend) %>% as_ggplot
  })
  ## update header of linear model formula based on whether a target and at least one predictor 
  ## has been selected. if this condition is true, print linear model formula
  lm_ready <- reactive({
    if(length(input$target) == 1 & max(length(input$preds_cont), length(input$preds_cat)) >= 1) {
      TRUE
    } else {
      FALSE
    }
  })
  output$lm_formula <- renderUI({
    if(lm_ready() != TRUE) {
      HTML("<b>Model Formula: </b>")
    } else {
      HTML(paste0("<b>Model Formula: </b>", lm_formula_txt(), "<p>"))
    }
  })
  ## lm formula text
  lm_formula_txt <- reactive({
    if(lm_ready() == TRUE) {
      lm_formula_func()
    }
  })
  ## generate text of linear model formula. note that the first object in the transformation
  ## data frame will always be the target variable
  lm_formula_func <- function(vif_model = FALSE) {
    if(vif_model == FALSE) {
      model_terms <- c(dat_tran() %>% .$tran_form, dat_int())
    } else if (length(input$preds_cat) == 0) {
      model_terms <- c(dat_tran() %>% .$tran_form)
    } else {
      model_terms <- c(dat_tran() %>% .$tran_form %>% .[.!= input$preds_cat])
    }
    paste0(model_terms[1], " ~ ", 
           paste0(model_terms[2:length(model_terms)],  
                  collapse = " + "))
  }
  ## run regression and output model summary
  regression_model <- reactive({
    if(lm_ready() == TRUE) {
      lm(formula = lm_formula_txt(), data = dat()) 
    }
  })
  output$lm_summary <- renderPrint({
    if(lm_ready() == TRUE) {
      summary(regression_model())
    }
  })
  ## output VIF statistics if >= 2 continuous predictors
  output$lm_vif_stats <- renderDataTable({
    if(length(input$preds_cont) >= 2) {
      lm(formula = lm_formula_func(vif_model = TRUE), data = dat()) %>%
        vif %>% 
        data.frame %>%
        rownames_to_column(var = "Variable") %>%
        set_colnames(c("Variable", "VIF")) %>%
        mutate(VIF = VIF %>% round(digits = 3)) %>%
        arrange(-VIF) %>% 
        datatable(options=list(dom='t'), rownames = F)
    }
  })
  ## output regression diagnostic plots
  output$lm_diagnostics <- renderPlot({
    if(max(length(input$preds_cont), length(input$preds_cat)) >= 1) {
      par(mfrow = c(2,2))
      plot(regression_model())
    }
  })
  ## test model differences using anova
  observeEvent(input$lm_comp_run, {
    output$lm_comp_summary <- renderPrint({
      if(length(input$comp_lm_1) >= 1 & length(input$comp_lm_2) >= 1) {
        comp_lm_1 <- lm(formula = formula(input$comp_lm_1), data = dat())
        comp_lm_2 <- lm(formula = formula(input$comp_lm_2), data = dat())
        anova(comp_lm_1, comp_lm_2)
      }
    })
  })
}

shinyApp(ui, server)