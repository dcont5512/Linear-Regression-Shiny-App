## libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggfortify)
library(car)
library(GGally)
library(ggpubr)

## define data frame and variable choice lists
dat <- diamonds %>% sample_n(500)
var_names <- list(continuous = dat %>% select_if(is.numeric) %>% colnames %>% sort,
                  categorical = dat %>% select_if(is.factor) %>% colnames %>% sort)

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

## create function to extract help documentation (include stack citation)
extract_help <- function(pkg, fn = NULL, to = c("txt", "html", "latex", "ex"))
{
  to <- match.arg(to)
  rdbfile <- file.path(find.package(pkg), "help", pkg)
  rdb <- tools:::fetchRdDB(rdbfile, key = fn)
  convertor <- switch(to, 
                      html  = tools::Rd2HTML, 
  )
  f <- function(x) capture.output(convertor(x))
  if(is.null(fn)) lapply(rdb, f) else f(rdb)
}

## function to obtain counts of selected categorical variable and fill (if specified)
plot_cat_dat <- function(pred_cat_selected, pred_cat_fill) {
  if(pred_cat_fill  %>% length == 0) {
    df <- dat %>% 
      count(get(pred_cat_selected), name = "total") %>% 
      `colnames<-`(c(pred_cat_selected, "total")) 
  } else {
    df <- dat %>% 
      count(get(pred_cat_selected), get(pred_cat_fill), name = "total") %>% 
      `colnames<-`(c(pred_cat_selected, pred_cat_fill, "total")) %>% 
      group_by_at(1)
  }
  df %>% 
    mutate(pct_total = (total/sum(total)) * 100)
}
## function to render categorical variable bar chart with fill (if specified)
plot_cat_bar <- function(pred_cat_selected, pred_cat_fill) {
  df <- plot_cat_dat(pred_cat_selected, pred_cat_fill) 
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
plot_cat_stack <- function(pred_cat_selected, pred_cat_fill) {
  df <- plot_cat_dat(pred_cat_selected, pred_cat_fill)
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

## create base UI
ui <-  dashboardPage(
  dashboardHeader(title = "Dom's Linear Model Builder", titleWidth = 450),
  dashboardSidebar(
    width = 450,
    sidebarMenu(
      fluidRow(
        column(width = 5, 
               ## target variable selector
               selectizeInput(inputId = "target", 
                              label = "Target Variable:", 
                              choices = var_names[["continuous"]],
                              multiple = TRUE,
                              selected = NULL,
                              options = list(placeholder = "Click to select",
                                             maxItems = 1))),
        column(width = 5, 
               ## target variable transformation options (excludes polynomials)
               selectizeInput(inputId = "tran_target", 
                              label = "Transformation:",
                              choices = tran_opts[c("Simple", "Logarithmic")],
                              multiple = FALSE,
                              selected = "None"))),
      fluidRow(
        column(width = 5,
               ## continuous predictors selector
               selectizeInput(inputId = "preds_cont", 
                              label = "Select Continuous Predictors:", 
                              choices = c(""),
                              multiple = TRUE,
                              options = list(placeholder = "None"))),
        column(width = 5,
               ## categorical predictors selector
               selectizeInput(inputId = "preds_cat",
                              label = "Select Categorical Predictors:",
                              choices = var_names[["categorical"]],
                              selected = NULL,
                              multiple = TRUE,
                              options = list(placeholder = "None")))),
      ## dynamic UIs for continuous variable transformations and interaction terms
      tabsetPanel(
        type = "tabs", 
        tabPanel("Transformations", 
                 div(style = "height:25.5px"),
                 uiOutput(outputId = "preds_tran_ui")), 
        tabPanel("Interaction Terms", 
                 div(style = "height:25.5px"),
                 ## buttons to add and remove interaction terms
                 fluidRow(
                   column(width = 5, 
                          actionButton(inputId = "intTermAdd", 
                                       label = "Add interaction term")),
                   column(width = 5, 
                          actionButton(inputId = "intTermRemove", 
                                       label = "Remove interaction term"))),
                 uiOutput(outputId = "preds_int_ui"))
      )
    )
  ),
  dashboardBody(
    fluidRow(
      tabBox(width = 12, height = NULL,
             ## data dictionary tab
             tabPanel("Data Dictionary",
                      htmlOutput(outputId = "data_dictionary")),
             ## correlation matrix (can take long time to load)
             tabPanel("Correlation Matrix",
                      plotOutput(outputId = "cor_matrix", height = 600)),
             ## plots for continuous variables transformation analysis
             tabPanel("Plots",
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
                                      plotOutput("legend", height = "75px"))
                      ),
                      radioButtons(inputId = "type_plot", 
                                   label = "Visualize Distribution Using:", 
                                   choices = c("Histogram", "Density Plot"), 
                                   selected = "Histogram",
                                   inline = T),
                      ## histogram, qq plot, and scatter plots for continuous variable selected
                      fluidRow(column(width = 6, 
                                      plotOutput(outputId = "plot_hist", height = "300px")),
                               column(width = 6, 
                                      plotOutput(outputId = "plot_scatter", height = "300px"))),
                      plotOutput(outputId = "plot_qq", height = "300px")),
             ## categorical selector for plot diagnostics
             tabPanel("Plots2", 
                      fluidRow(column(width = 3,
                                      selectizeInput(inputId = "cat_plot",
                                                     label = "Select Variable",
                                                     choices = "",
                                                     selected = NULL,
                                                     multiple = TRUE,
                                                     options = list(placeholder = "None",
                                                                    maxItems = 1))),
                               column(width = 3, 
                                      selectizeInput(inputId = "cat_fill", 
                                                     label = "Select Fill", 
                                                     choices = "", 
                                                     selected = NULL,
                                                     multiple = TRUE,
                                                     options = list(placeholder = "None",
                                                                    maxItems = 1))),
                               column(width = 6,
                                      plotOutput("plot_cat_legend", height = "75px"))),
                      fluidRow(column(width = 6, plotOutput("plot_bar")),
                               column(width = 6, plotOutput("plot_stack")))),
             ## linear model output generator, includes formula, summary, and VIF statistics
             tabPanel("Linear Model",
                      htmlOutput(outputId = "lm_formula"),
                      div(style = "height:12.5px"),
                      verbatimTextOutput(outputId = "lm_summary"),
                      htmlOutput(outputId = "lm_vif_header"),
                      verbatimTextOutput(outputId = "lm_vif_stats")),
             ## linear model diagnostic plots
             tabPanel("Diagnostic Plots",
                      plotOutput("lm_diagnostics", height = "600px")),
             tabPanel("Model Comparisons",
                      fluidRow(column(width = 4, 
                                      textInput(inputId = "comp_lm_1", label = "Model 1", 
                                                value = "", placeholder = "Linear model formula")),
                               column(width = 4, 
                                      textInput(inputId = "comp_lm_2", label = "Model 2", 
                                                value = "", placeholder = "Linear model formula")),
                               column(width = 3,
                                      div(style = "height:24px"),
                                      actionButton(inputId = "lm_comp_run", label = "Compare"))),
                      verbatimTextOutput("lm_comp_summary")
             )
      )
    )
  )
)

server <- function(input, output, session) {
  ## render help file from selected data set (if available)
  output$data_dictionary <- renderText(extract_help("ggplot2", "diamonds", to="html"))
  ## render correlation matrix for continuous variables
  output$cor_matrix <- renderPlot(dat %>% select_if(is.numeric) %>% ggpairs)
  ## update continuous variable selector options to exclude target variable
  observeEvent(input$target, {
    updateSelectizeInput(session, inputId = "preds_cont", 
                         choices = var_names[["continuous"]] %>% .[.!=input$target])
  })
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
        fluidRow(column(width = 5, 
                        tran_select_preds(predsContName = tran_left)),
                 column(width = 5, 
                        tran_select_preds(predsContName = tran_right)))
      } else {
        tran_left <- input$preds_cont[.x]
        fluidRow(column(width = 5, 
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
        fluidRow(column(width = 5, 
                        intTermSelector(intTermId = int_left,
                                        intNumber = .x,
                                        intTermPosition = 1)),
                 column(width = 5, 
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
    updateSelectizeInput(session, "cont_plot", choices = vars_cont())
  })
  ## update the plot fill choices to reflect categorical variable selections
  observeEvent(input$preds_cat, {
    updateSelectizeInput(session, "plot_fill", choices = input$preds_cat)
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
      plot <-  dat %>% ggplot(aes_string(x = plot_selected(), fill = input$plot_fill))
    } else {
      plot <- dat %>% ggplot(aes_string(x = plot_selected()))
    }
    if(input$type_plot == "Histogram") {
      plot <- plot + geom_histogram()
    } else {
      plot <- plot + geom_density(alpha = 0.5)
    }
    plot + theme(legend.position = "none")
  })
  ## function to generate scatter plot where y-axis is target variable and x-axis is specified
  ## continuous variable. note that unlike the histogram, we store this as a function, as it 
  ## will be used in the next step to extract the legend
  plot_scatter_func <- function(x, fill) {
    if(fill %>% length != 0) {
      plot <-  dat %>% ggplot(aes_string(x = x,
                                         y = plot_target(),
                                         color = fill))
    } else {
      plot <- dat %>% ggplot(aes_string(x = x,
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
                                      face = "bold"))
    get_legend(plot_fill_legend) %>% as_ggplot
  })
  ## generate qq plot of selected continuous variable
  output$plot_qq <- renderPlot({
    plot_reqs()
    dat %>%
      ggplot(aes_string(sample = plot_selected())) +
      stat_qq() +
      stat_qq_line()
  })
  ## update categorical variable selector for plots
  observeEvent(input$preds_cat, {
    updateSelectizeInput(session, "cat_plot", choices = input$preds_cat)
  })
  ## update categorical variable fill selector for plots
  observeEvent(input$cat_plot, {
    updateSelectizeInput(session, "cat_fill", choices = input$preds_cat %>% .[.!=input$cat_plot])
  })
  ## render categorical variable bar plot
  output$plot_bar <- renderPlot({
    req(input$cat_plot)
    plot_cat_bar(pred_cat_selected = input$cat_plot, pred_cat_fill = input$cat_fill)
  })
  ## render categorical variable stacked plot
  output$plot_stack <- renderPlot({
    req(input$cat_plot)
    plot_cat_stack(pred_cat_selected = input$cat_plot, pred_cat_fill = input$cat_fill)
  })
  ## render legend for categorical variables
  output$plot_cat_legend <- renderPlot({
    req(input$cat_plot)
    plot_fill_legend <- plot_cat_stack(pred_cat_selected = input$cat_plot, pred_cat_fill = input$cat_fill) +
      theme(legend.position = "bottom", 
            legend.title=element_text(size = 14,
                                      family = "Helvetica Neue",
                                      face = "bold"))
    get_legend(plot_fill_legend) %>% as_ggplot
  })
  ## update header of linear model formula based on whether a target and at least one predictor 
  ## has been selected. if this condition is true, print linear model formula
  output$lm_formula <- renderUI({
    if(max(length(input$preds_cont), length(input$preds_cat)) == 0) {
      HTML(paste0("<b> Select a target variable and at least one continuous or categorical predictor to 
                  generate a linear model. </b> "))
    } else {
      HTML(paste0("<b> Formula: </b> ", lm_formula_txt()))
    }
  })
  ## update VIF statistic header text
  output$lm_vif_header <- renderUI({
    if(max(length(input$preds_cont), length(input$preds_cat)) == 0) {
      HTML("")
    }
    else if(length(input$preds_cont) <= 1) {
      HTML("<b>Variable Inflation Factor (VIF) to Diagnose Multicollinearity: </b>
           <br> Select at least two continuous predictors to obtain VIF statistics.")
    } else {
      HTML("<b>VIF Statistics to Diagnose Multicollinearity: </b> <br> Note that it is 
      easiest to diagnose multicollinearity prior to performing variable transformations 
      and without including categorical predictors.")
    }
  })
  ## generate text of linear model formula. note that the first object in the transformation
  ## data frame will always be the target variable
  lm_formula_txt <- reactive({
    model_terms <- c(dat_tran() %>% .$tran_form, dat_int())
    paste0(model_terms[1], " ~ ", 
           paste0(model_terms[2:length(model_terms)],  
                  collapse = " + "))
  })
  ## run regression and output model summary
  regression_model <- reactive(lm(formula = lm_formula_txt(), data = dat))
  output$lm_summary <- renderPrint({
    if(max(length(input$preds_cont), length(input$preds_cat)) >= 1) {
      summary(regression_model())
    }
  })
  ## output VIF statistics if >= 2 continuous predictors
  output$lm_vif_stats <- renderPrint(
    if(length(input$preds_cont) >= 2) {
      vif(regression_model())
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
        comp_lm_1 <- lm(formula = formula(input$comp_lm_1), data = dat)
        comp_lm_2 <- lm(formula = formula(input$comp_lm_2), data = dat)
        anova(comp_lm_1, comp_lm_2)
      }
    })
  })
}

shinyApp(ui, server)