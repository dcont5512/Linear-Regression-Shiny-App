## libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggfortify)

## table ~ y, carat, x 

## define data frame and variable choice lists
dat <- diamonds %>% sample_n(10000)
var_names <- list(continuous = dat %>% select_if(is.numeric) %>% colnames %>% sort,
                  categorical = dat %>% select_if(is.factor) %>% colnames %>% sort)

## set transformation options for target variable and continuous predictors
## note that target variable options excludes polynomials
tran_opts_target <- list("Simple" = c("None",  "Reciprocal", "Squared", "Square Root"),
                         "Logarithmic" = c("Natural", "Base 2", "Base 10"))
tran_opts_preds <- tran_opts_target
tran_opts_preds[["Polynomial"]] <-c(paste (seq(from = 2, to = 5), rep("degree", 5)))
tran_opts_preds <- tran_opts_preds[c("Simple", "Logarithmic", "Polynomial")]

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

## create base UI
ui <-  dashboardPage(
  dashboardHeader(title = "Dom's Linear Model Builder", titleWidth = 450),
  dashboardSidebar(
    width = 450,
    sidebarMenu(
      id = "tabs",
      fluidRow(column(width = 5, 
                      selectizeInput(inputId = "target", 
                                     label = "Target Variable:", 
                                     choices = var_names[["continuous"]],
                                     multiple = TRUE,
                                     selected = NULL,
                                     options = list(placeholder = "Click to select",
                                                    maxItems = 1))),
               column(width = 5, 
                      selectizeInput(inputId = "tran_target", 
                                     label = "Transformation:",
                                     choices = tran_opts_target,
                                     multiple = FALSE,
                                     selected = "None"))),
      fluidRow(column(width = 5,
                      selectizeInput(inputId = "preds_cont", 
                                     label = "Select Continuous Predictors:", 
                                     choices = c(""),
                                     multiple = TRUE,
                                     options = list(placeholder = "None"))),
               ## input selector for categorical predictors
               column(width = 5,
                      selectizeInput(inputId = "preds_cat",
                                     label = "Select Categorical Predictors:",
                                     choices = var_names[["categorical"]],
                                     selected = NULL,
                                     multiple = TRUE,
                                     options = list(placeholder = "None")))),
      ## create tabs for continuous variable transformations and interaction terms
      ## and dynamic UIs for each. Note that here we also include buttons to add
      ## and remove interaction terms
      tabsetPanel(
        type = "tabs",
        tabPanel("Variable Transformations", 
                 div(style = "height:25.5px"),
                 uiOutput("preds_tran_ui")),
        tabPanel("Interaction Terms", 
                 div(style = "height:25.5px"),
                 fluidRow(column(width = 5, actionButton("intTermAdd", "Add interaction term")),
                          column(width = 5, actionButton("intTermRemove", "Remove interaction term"))),
                 uiOutput("preds_int_ui"))
      )
    )
  ),
  dashboardBody(
            tabBox(width = 12, height = 400,
                   tabPanel("Plots",
                            selectizeInput("plot_x", "Select X Variable", 
                                           choices = "", 
                                           selected = NULL,
                                           multiple = TRUE,
                                           options = list(placeholder = "None",
                                                          maxItems = 1)),
                            fluidRow(column(width = 6, plotOutput("plot1")),
                                     column(width = 6, plotOutput("plot2")))),
                   tabPanel("Linear Model",
                            htmlOutput("lm_formula"),
                            div(style = "height:12.5px"),
                            verbatimTextOutput("model_results")),
                   tabPanel("Diagnostic Plots",
                            plotOutput(("diagnostics")))
            
    )
  )
)

server <- function(input, output, session) {
  ## update continuous variable options to exclude target variable
  observeEvent(input$target, {
    updateSelectizeInput(session, "preds_cont", 
                         choices = var_names[["continuous"]] %>% .[.!=input$target])
  })
  ## here, we render the UI for the continuous variable transformations
  output$preds_tran_ui <- renderUI({
    ## function to generate transformation selector for each continuous variable
    tran_select_preds <-  function(preds_cont_name) {
      input_name <- paste0("trans_", preds_cont_name)
      selectizeInput(inputId = input_name, 
                     label = preds_cont_name,
                     choices = tran_opts_preds,
                     multiple = FALSE,
                     selected = isolate(input[[input_name]])) 
    }
    ## generate zig-zag UI for the continuous variable transformations; first, 
    ## we generate a sequence of numbers the length of the number of continuous 
    ## predictors and keep only the odd numbers - these odd numbers serve as 
    ## the row index for each transformation line
    pred_trans_row_idx <- length(input$preds_cont) %>% seq_len
    pred_trans_row_idx <- pred_trans_row_idx[pred_trans_row_idx %% 2 == 1]
    ## generate transformation selectors for rows with two options 
    pred_trans_row_idx %>% 
      map(~ if(!is.na(input$preds_cont[.x + 1])) {
        preds_cont_left_name <- input$preds_cont[.x]
        preds_cont_right_name <- input$preds_cont[.x + 1]
        fluidRow(column(width = 5, 
                        tran_select_preds(preds_cont_name = preds_cont_left_name)),
                 column(width = 5, 
                        tran_select_preds(preds_cont_name = preds_cont_right_name)))
      } else {
        preds_cont_left_name <- input$preds_cont[.x]
        fluidRow(column(width = 5, 
                        tran_select_preds(preds_cont_name = preds_cont_left_name)))
      }
      )
  })
  ## generate UI for interaction terms; first step is to identify number of
  ## interaction terms created based on the sum of clicks for add interaction 
  ## term/remove interaction term buttons
  click_total <- reactiveVal(0)
  observeEvent(input$intTermAdd, click_total(click_total() + 1))
  observeEvent(input$intTermRemove, click_total( max(0, click_total() - 1 )))
  ## function to generate interaction term selectors
  output$preds_int_ui <- renderUI({
    intTermsFun <- function(intTermName, intTermNumber, intTermVar) {
      selectizeInput(inputId = intTermName, 
                     label = paste("Interaction", intTermNumber, " - Term", intTermVar),
                     choices = c("None", input$preds_cont, input$preds_cat),
                     multiple = FALSE,
                     selected = isolate(input[[intTermName]]))
    }
    ## generate interaction term UI
    click_total() %>% 
      seq_len %>% 
      map(~ {
        int_term_left_name <- paste0("int", .x, "term1")
        inter_term_right_name <- paste0("int", .x, "term2")
        fluidRow(column(width = 5, 
                        intTermsFun(intTermName = int_term_left_name,
                                    intTermNumber = .x,
                                    intTermVar = 1)),
                 column(width = 5, 
                        intTermsFun(intTermName = inter_term_right_name,
                                    intTermNumber = .x,
                                    intTermVar = 2)))
      })
  })
  ## reactive data frame
  dat_trans <- reactive({
    tran_preds <- input$preds_cont %>% 
      map(~input[[paste0("trans_", .x)]]) %>% 
      unlist
    tran_list <- data.frame(var_list = c(input$target, input$preds_cont),
                            tran_list = c(input$tran_target, tran_preds),
                            stringsAsFactors = F) %>% 
      rowwise %>% 
      mutate(tran_form = tran_func(var_list, tran_list)) %>% 
      ungroup %>% 
      select(-tran_list) %>% 
      rbind(data.frame(var_list = input$preds_cat,
                       tran_form = input$preds_cat))
  })
  ## update the plot input choices to reflect continuous predictor selections
  observeEvent(input$preds_cont, {
    updateSelectizeInput(session, "plot_x", choices = input$preds_cont)
  })
  plot_y_name <- reactive({
    dat_trans() %>% filter(var_list == input$target) %>% .$tran_form
  })
  plot_x_name <- reactive({
    dat_trans() %>% filter(var_list == input$plot_x) %>% .$tran_form
  })
  ## generate scatter plot for target variable and selected continuous predictor
  output$plot1 <- renderPlot({
    req(input$target)
    req(input$plot_x)
    browser()
    dat %>% 
      ggplot(aes_string(x = plot_x_name(),
                        y = plot_y_name())) +
      geom_point()
  })
  ## generate plot outputs
  output$plot2 <- renderPlot({
    req(input$target)
    req(input$plot_x)
    dat %>%
      ggplot(aes_string(sample = plot_x_name())) +
      stat_qq() +
      stat_qq_line()
  })
  lm_formula_txt <- reactive({
    req(input$target)
    target_preds_cont <- dat_trans() %>% .$tran_form
    preds_cat <- input$preds_cat
    model_terms <- c(target_preds_cont, intdf())
    paste0(model_terms[1], " ~ ", 
           paste0(model_terms[2:length(model_terms)],  
                  collapse = " + "))
  })
  output$lm_formula <- renderUI({
    if(max(length(input$preds_cont), length(input$preds_cat)) >= 1) {
      HTML(paste0("<b> Formula: </b> ", lm_formula_txt()))
    }
  })
  intdf <- reactive({
    if(click_total() >= 1) {
      dummy <- data.frame(x = seq_len(click_total()) %>% 
                            map(~input[[paste0("int", .x, "term2")]]) %>% unlist,
                          y = seq_len(click_total()) %>% 
                            map(~input[[paste0("int", .x, "term1")]]) %>% unlist) %>% 
        filter(x != "None" & y != "None" & as.character(x) != as.character(y))
      if(nrow(dummy) != 0) {
        dummy <- unique(t(apply(dummy, 1, sort))) %>%
          data.frame %>%
          `colnames<-`(c("X123", "X234"))
        dummy %>%
          inner_join(dat_trans(), by = c("X123" = "var_list")) %>%
          inner_join(dat_trans(), by = c("X234" = "var_list")) %>%
          mutate(int_form = paste0(tran_form.x, "*", tran_form.y)) %>%
          .$int_form
      }
    }
  })
  output$model_results <- renderPrint({
    if(max(length(input$preds_cont), length(input$preds_cat)) >= 1) {
      summary(lm(formula = lm_formula_txt(), data = dat))
    }
  })
  output$diagnostics <- renderPlot({
    if(max(length(input$preds_cont), length(input$preds_cat)) >= 1) {
      par(mfrow = c(2,2))
      autoplot(lm(formula = lm_formula_txt(), data = dat), label.size = 3)
    }
  })
}

shinyApp(ui, server)