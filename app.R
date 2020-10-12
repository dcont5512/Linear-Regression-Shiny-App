## libraries
library(tidyverse)
library(shiny)

## stored data objects, including variable and transformation names
dat <- mtcars
var_names <- dat %>% colnames %>% sort
pred_count <- length(var_names) - 1
tran_names <- c("None", "Logarithmic", "Polynomial", "Reciprocal", "Root", "Square")

## create conditional UI for transformation parameters
param_select <- function(object_name) {
  blank_row <- div(style = "height:73.5px")
  tabsetPanel(
    id = object_name,
    type = "hidden",
    tabPanel("None",
             fluidRow(blank_row)),
    tabPanel("Reciprocal",
             fluidRow(blank_row)),
    tabPanel("Square",
             fluidRow(blank_row)),
    tabPanel("Logarithmic",
             fluidRow(column(width = 3, 
                             selectInput(inputId = "log_param",
                                         label = "Type:",
                                         choices = c("Natural", "Base 2", "Base 10"))))),
    tabPanel("Polynomial", 
             fluidRow(column(width = 3,  
                             numericInput(inputId = "poly_param",
                                          label = "Degrees:",
                                          min = 1,
                                          value = 1)))),
    tabPanel("Root", 
             fluidRow(column(width = 3,  
                             selectInput(inputId = "root_param",
                                         label = "Type:",
                                         choices = c("Square", "Cubed"))))))
}

param_list <- c("targetParam", paste0(rep("tranParam", pred_count), seq_len(pred_count)))
param_selectors <- param_list %>% map(~param_select(object_name = .x))
names(param_selectors) <- param_list

## clean work space
rm(param_list, param_select)

## create base UI
ui <- fluidPage(
  fluidRow(column(width = 2, 
                  ## input selector for target variable
                  selectInput(inputId = "target", 
                              label = "Target Variable", 
                              choices = var_names)),
           column(width = 2, 
                  ## input selector for target variable transformation, excluding polynomials
                  selectInput(inputId = "target_trans", 
                              label = "Select Transformation", 
                              choices = tran_names %>% .[.!="Polynomial"])),
           column(width = 5, param_selectors[["targetParam"]])),
  ## input selector for number of predictors
  numericInput(inputId = "preds_n", 
               label = "Select Number of Predictors", 
               value = 1, 
               min = 1,
               max = pred_count,
               width = "190px"),
  ## stored layout for dynamic UI
  fluidRow(column(width = 2, uiOutput("preds_ui")),
           column(width = 2, uiOutput("pred_trans_ui")),
           column(width = 8, uiOutput("pred_param_ui")))
)

server <- function(input, output, session) {
  ## parameter selections for target variable
  observeEvent(input$target_trans, {
    updateTabsetPanel(session = session, inputId = "targetParam", selected = input$target_trans)
  }) 
  ## create objects to store individual predictors
  preds <- reactive(paste0("Predictor", seq_len(input$preds_n)))
  output$preds_ui <- renderUI({
    preds() %>% map(~ selectInput(inputId = .x, 
                                  label = .x, 
                                  choices = var_names,
                                  selected = isolate(input[[.x]])) %||% "")
  })
  ## create objects to store individual predictors transformations
  pred_trans <- reactive(paste0("Transformation", seq_len(input$preds_n)))
  output$pred_trans_ui <- renderUI({
    pred_trans() %>% map(~ selectInput(inputId = .x, 
                                       label = .x, 
                                       choices = tran_names,
                                       selected = isolate(input[[.x]])) %||% "")
  })
  ## create objects to store individual predictors transformations parameters
  pred_params <- reactive(paste0("tranParam", seq_len(input$preds_n)))
  output$pred_param_ui <- renderUI({
    req(input$preds_n)
    pred_params() %>% 
      map(~fluidRow(column(width = 8, param_selectors[[.x]])))
  })
  ## parameter selections for target variable
  observeEvent(input$preds_n, {
    seq_len(input$preds_n) %>% 
      map(~observeEvent(input[[paste0("Transformation", .x)]], {
        updateTabsetPanel(session = session, 
                          inputId = paste0("tranParam", .x), 
                          selected = input[[paste0("Transformation", .x)]])
      }))
  })
}
  
  shinyApp(ui, server)