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

pred_count <- length(choice_list[["continuous"]]) - 1

## create base UI
ui <- fluidPage(
  fluidRow(
    ## input selector for target variable
    column(width = 2, 
           selectizeInput(inputId = "target", 
                          label = "Target Variable:", 
                          choices = choice_list[["continuous"]],
                          multiple = TRUE,
                          selected = NULL,
                          options = list(placeholder = "Click to select",
                                         maxItems = 1))),
    ## input selector for target variable transformation, excluding polynomials
    column(width = 2, 
           selectizeInput(inputId = "trans", 
                          label = "Transformation:",
                          choices = target_trans,
                          multiple = FALSE,
                          selected = "None"))),
  fluidRow(
    ## input selector for number of predictors
    column(width = 2,
           uiOutput("cont_selector")),
    ## input selector for categorical predictors
    column(width = 2,
           selectizeInput(inputId = "cat_preds",
                          label = "Select Categorical Predictors:",
                          choices = choice_list[["categorical"]],
                          selected = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "None")))),
  HTML("<p style=\"font-size:14.5px\"><b>Continuous Predictor <br> Transformations: </b></p>"),
  ## stored layout for dynamic UI
  uiOutput("preds_ui"),
  fluidRow(column(width = 2, actionButton("intTermAdd", "Add interaction term")),
           column(width = 2, actionButton("intTermRemove", "Remove interaction term"))),
  div(style = "height:25.5px"),
  uiOutput("intUI")
)

server <- function(input, output, session) {
  ## filter continuous predictors to omit selected target variable
  continuous_preds <- reactive(choice_list[["continuous"]] %>%  .[.!=input$target])
  ## generate continuous variables selector
  output$cont_selector <- renderUI(
    selectizeInput(inputId = "cont_preds", 
                   label = "Select Continuous Predictors", 
                   choices = continuous_preds(),
                   multiple = TRUE,
                   options = list(placeholder = ifelse(length(input$target) == 0, 
                                                       "Select target variable", 
                                                       "None")))
  )
  output$preds_ui <- renderUI({
    row_idx <- length(input$cont_preds) %>% seq_len
    row_idx <- row_idx[row_idx %% 2 == 1]
    row_idx %>% 
      map(~ if(!is.na(input$cont_preds[.x + 1])) {
        fluidRow(
          column(width = 2, 
                 selectizeInput(
                   inputId = paste0("cont_pred_trans", .x), 
                   label = paste(input$cont_preds[.x], "Transformation:"),
                   choices = pred_trans,
                   multiple = TRUE,
                   options = list(placeholder = "None",
                                  maxItems = 1),
                   selected = isolate(input[[paste0("cont_pred_trans", .x)]]))
          ),
          column(width = 2, 
                 selectizeInput(
                   inputId = paste0("cont_pred_trans", .x + 1), 
                   label = paste(input$cont_preds[.x + 1], "Transformation:"),
                   choices = pred_trans,
                   multiple = TRUE,
                   options = list(placeholder = "None",
                                  maxItems = 1),
                   selected = isolate(input[[paste0("cont_pred_trans", .x + 1)]]))
          ))
      } else {
        fluidRow(
          column(width = 2, 
                 selectizeInput(inputId = paste0("cont_pred_trans", .x), 
                                label = paste(input$cont_preds[.x], "Transformation:"),
                                choices = pred_trans,
                                multiple = TRUE,
                                options = list(placeholder = "None",
                                               maxItems = 1),
                                selected = isolate(input[[paste0("cont_pred_trans", .x + 1)]]))
          )
        )
      }
      )
  })
  output$intUI <- renderUI({
    intChoices <- c(input$cont_preds, input$cat_preds)
    intTermCount <- input$intTermAdd + input$intTermRemove * - 1
    intTermCountSeq <- seq_len(intTermCount)
    intTermCountSeq %>% 
      map(~fluidRow(
        column(width = 2, 
               selectizeInput(
                 inputId = paste0("term", .x), 
                 label = paste("Interaction", .x, "Term 1"),
                 choices = intChoices,
                 multiple = TRUE,
                 options = list(placeholder = "None",
                                maxItems = 1))),
        column(width = 2, 
               selectizeInput(
                 inputId = paste0("term", .x + 1), 
                 label = paste("Interaction", .x, "Term 2"),,
                 choices = intChoices,
                 multiple = TRUE,
                 options = list(placeholder = "None",
                                maxItems = 1))
        )))
  })
}

shinyApp(ui, server)