## libraries
library(tidyverse)
library(shiny)

## stored data objects, including selector options, and count of predictors
dat <- mtcars %>% 
  mutate(qual = sample(c("good", "bad", "horrible"), size = nrow(mtcars), replace = TRUE) %>% as.factor,
         speed = sample(c("fast", "slow", "buzz"), size = nrow(mtcars), replace = TRUE) %>% as.factor)
choice_list <- list(continuous = dat %>% select_if(is.numeric) %>% colnames %>% sort,
                    categorical = dat %>% select_if(is.factor) %>% colnames %>% sort,
                    transformations = c("None", "Logarithmic", "Polynomial", 
                                        "Reciprocal", "Root", "Square"))
pred_count <- length(choice_list[["continuous"]]) - 1

## create conditional UI for transformation parameters
param_select <- function(object_name) {
  blank_row <- div(style = "height:76.5px")
  tabsetPanel(
    id = object_name,
    type = "hidden",
    tabPanel("None",
             blank_row),
    tabPanel("Reciprocal",
             blank_row),
    tabPanel("Square",
             blank_row),
    tabPanel("Logarithmic",
             selectInput(inputId = "log_param",
                         label = "Type:",
                         choices = c("Natural", "Base 2", "Base 10"))),
    tabPanel("Polynomial", 
             numericInput(inputId = "poly_param",
                          label = "Degrees:",
                          min = 1,
                          value = 1)),
    tabPanel("Root", 
             selectInput(inputId = "root_param",
                         label = "Type:",
                         choices = c("Square", "Cubed"))))
}

## generate individual tabsetPanels for each continuous predictor that can be transformed
param_names <- c("targetParam", paste0(rep("tranParam", pred_count), seq_len(pred_count)))
param_selectors <- param_names %>% 
  map(~param_select(object_name = .x))
names(param_selectors) <- param_names

## clean work space
rm(param_list, param_select, param_names)

## create base UI
ui <- fluidPage(
  fluidRow(
    ## input selector for target variable
    column(width = 2, 
           selectInput(inputId = "target", 
                       label = "Target Variable:", 
                       choices = choice_list[["continuous"]])),
    ## input selector for target variable transformation, excluding polynomials
    column(width = 2, 
           selectInput(inputId = "target_trans", 
                       label = "Transformation:", 
                       choices = choice_list[["transformations"]] %>%  .[.!="Polynomial"])),
    column(width = 5, param_selectors[["targetParam"]])),
  fluidRow(
    ## input selector for categorical predictors
    column(width = 2,
           selectInput(inputId = "cat_preds",
                       label = "Select Categorical Predictors:",
                       choices = choice_list[["categorical"]],
                       multiple = TRUE,
                       width = "190px")),
    ## input selector for number of predictors
    column(width = 2,
           numericInput(inputId = "preds_n", 
                        label = "Number of Continuous Predictors:", 
                        value = 1, 
                        min = 1,
                        max = pred_count,
                        width = "190px"))),
  HTML("<p style=\"font-size:15px\"><b>Select Continuous Predictors: </b></p>"),
  ## stored layout for dynamic UI
  fluidRow(column(width = 2, uiOutput("preds_ui")),
           column(width = 2, uiOutput("pred_trans_ui")),
           column(width = 8, uiOutput("pred_param_ui")))
)

server <- function(input, output, session) {
  ## generate parameter selections for target variable
  observeEvent(input$target_trans, {
    updateTabsetPanel(session = session, inputId = "targetParam", selected = input$target_trans)
  }) 
  ## filter continuous predictors to omit selected target variable
  continuous_preds <- reactive(choice_list[["continuous"]] %>%  .[.!=input$target])
  ## create objects to store individual predictors
  preds <- reactive(paste0("Predictor", seq_len(input$preds_n)))
  output$preds_ui <- renderUI({
    preds() %>% map(~ selectInput(inputId = .x, 
                                  label = .x, 
                                  choices = continuous_preds(),
                                  selected = isolate(input[[.x]])) %||% "")
  })
  ## create objects to store individual predictors transformations
  pred_trans <- reactive(paste0("Transformation", seq_len(input$preds_n)))
  output$pred_trans_ui <- renderUI({
    pred_trans() %>% map(~ selectInput(inputId = .x, 
                                       label = .x, 
                                       choices = choice_list[["transformations"]],
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