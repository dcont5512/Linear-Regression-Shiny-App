## libraries
library(tidyverse)
library(shiny)

## stored data objects, including selector options, and count of predictors
dat <- mtcars %>% 
  mutate(qual = sample(c("good", "bad", "horrible"), 
                       size = nrow(mtcars), 
                       replace = TRUE) %>% as.factor,
         speed = sample(c("fast", "slow", "buzz"), 
                        size = nrow(mtcars), 
                        replace = TRUE) %>% as.factor)

choice_list <- list(continuous = dat %>% select_if(is.numeric) %>% colnames %>% sort,
                    categorical = dat %>% select_if(is.factor) %>% colnames %>% sort)

## transformation options
target_trans <- list("Simple" = c("None", "Reciprocal", "Squared"),
                     "Logarithmic" = c("Natural", "Base 2", "Base 10"),
                     "Root" = c("Cubed", "Square"))
pred_trans <- target_trans
pred_trans[["Polynomial"]] <-c(paste (seq_len(5), rep("degree", 5)))
pred_trans <- pred_trans[c("Simple", "Logarithmic", "Polynomial", "Root")]

## create base UI
ui <- 
  fluidPage(
    fluidRow(
      column(width = 2, 
             ## input selector for target variable
             selectizeInput(inputId = "target", 
                            label = "Target Variable:", 
                            choices = choice_list[["continuous"]],
                            multiple = TRUE,
                            selected = NULL,
                            options = list(placeholder = "Click to select",
                                           maxItems = 1))),
      column(width = 2, 
             ## input selector for target variable transformation, excluding polynomials
             selectizeInput(inputId = "trans", 
                            label = "Transformation:",
                            choices = target_trans,
                            multiple = FALSE,
                            selected = "None"))),
    fluidRow(
      ## input selector for number of predictors
      column(width = 2,
             selectizeInput(inputId = "cont_preds", 
                            label = "Select Continuous Predictors", 
                            choices = c(""),
                            multiple = TRUE,
                            options = list(placeholder = "Select target variable"))),
      ## input selector for categorical predictors
      column(width = 2,
             selectizeInput(inputId = "cat_preds",
                            label = "Select Categorical Predictors:",
                            choices = choice_list[["categorical"]],
                            selected = NULL,
                            multiple = TRUE,
                            options = list(placeholder = "None")))),
    tabsetPanel(
      type = "tabs",
      
      tabPanel("Variable Transformations", 
               div(style = "height:25.5px"),
               uiOutput("preds_ui")),
      tabPanel("Interaction Terms", 
               div(style = "height:25.5px"),
               fluidRow(column(width = 2, actionButton("intTermAdd", "Add interaction term")),
                        column(width = 2, actionButton("intTermRemove", "Remove interaction term"))),
               div(style = "height:25.5px"),
               uiOutput("intUI"))
    )
  )

server <- function(input, output, session) {
  ## filter continuous predictors to omit selected target variable
  continuous_preds <- reactive(choice_list[["continuous"]] %>%  .[.!=input$target])
  ## generate continuous variables selector
  observeEvent(input$target, {
    updateSelectizeInput(session, "cont_preds", choices = continuous_preds(),
                         options = list(placeholder = "None"))
  })
  ## objects to store continuous variable transformations
  output$preds_ui <- renderUI({
    row_idx <- length(input$cont_preds) %>% seq_len
    row_idx <- row_idx[row_idx %% 2 == 1]
    trans_selector <-  function(var_name) {
      selectizeInput(inputId = paste0(var_name, "trans"), 
                     label = paste(var_name, "Transformation:"),
                     choices = pred_trans,
                     multiple = TRUE,
                     options = list(placeholder = "None",
                                    maxItems = 1),
                     selected = isolate(input[[paste0(var_name, "trans")]])) 
    }
    row_idx %>% 
      map(~ if(!is.na(input$cont_preds[.x + 1])) {
        var_name_odd <- input$cont_preds[.x]
        var_name_even <- input$cont_preds[.x + 1]
        fluidRow(
          column(width = 2, 
                 trans_selector(var_name_odd)  
          ),
          column(width = 2, 
                 trans_selector(var_name_even)  
          ))
      } else {
        var_name_odd <- input$cont_preds[.x]
        fluidRow(
          column(width = 2, 
                 trans_selector(var_name_odd)
          )
        )
      }
      )
  })
  ## generate UI for interaction terms
  removes <- reactive({
    ifelse(input$intTermRemove <= 0, 0, input$intTermRemove)
  })
  
  total <- reactiveVal( 0 )
  observeEvent( input$intTermAdd, total( total() + 1 ))
  observeEvent( input$intTermRemove, total( max( 0, total() - 1 )))
  
  output$intUI <- renderUI({
    intTermCountSeq <- total() %>% seq_len
    intTermsFun <- function(intTermName, intTermNumber, intTermVar) {
      selectizeInput(
        inputId = intTermName, 
        label = paste("Interaction", intTermNumber, " - Term", intTermVar),
        choices = c(input$cont_preds, input$cat_preds),
        multiple = TRUE,
        options = list(placeholder = "None",
                       maxItems = 1),
        selected = isolate(input[[intTermName]]))
    }    
    intTermCountSeq %>% 
      map(~ {
        intTerm1_name <- paste0("int", .x, "term1")
        intTerm2_name <- paste0("int", .x + 1, "term2")
        fluidRow(
          column(width = 2, 
                 intTermsFun(intTermName = intTerm1_name,
                             intTermNumber = .x,
                             intTermVar = 1)
          ),
          column(width = 2, 
                 intTermsFun(intTermName = intTerm2_name,
                             intTermNumber = .x,
                             intTermVar = 2)))
      })
  })
}

shinyApp(ui, server)