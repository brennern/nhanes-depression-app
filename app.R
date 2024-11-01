#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

##### LIBRARIES #####
library(shiny)
library(shinythemes)
library(RNHANES)
library(tidyverse)
library(janitor)
library(purrr)
library(gtsummary)
library(rstatix)
library(ggpubr)
library(gt)
library(ggdist)
library(ggbeeswarm)
library(arrow)
library(FactoMineR)
library(factoextra)
library(plotly)
library(caret)
library(tidymodels)
#####################

### HELPER FILES ###
source("utils.R")
####################

##### DATA PRE-PROCESSING #####
demo <- nhanes_load_data("DEMO_H", "2013-2014") |> 
  clean_names()
dep <- nhanes_load_data("DPQ_H", "2013-2014") |> 
  clean_names()
diet <- nhanes_load_data("DR1TOT_H", "2013-2014") |> 
  clean_names()
vitd <- nhanes_load_data("VID_H", "2013-2014") |> # vitamin d
  clean_names()
cbc <- nhanes_load_data("CBC_H", "2013-2014") |> # complete blood count
  clean_names()
glu <- nhanes_load_data("GLU_H", "2013-2014") |> # glucose
  clean_names()
sel <- nhanes_load_data("CUSEZN_H", "2013-2014") |> # copper, selenium, and zinc
  clean_names()
fatty <- nhanes_load_data("FAS_H", "2013-2014") |> # fatty acids
  clean_names()
dataset_list <- list(demo, dep, diet, vitd, cbc, glu, sel, fatty)
dat <- reduce(dataset_list, full_join, by = "seqn")

dat <- dat |> 
  mutate(
    Gender = factor(
      riagendr,
      levels = c(1,2),
      labels = c("Male", "Female")
    ),
    Race = factor(
      ridreth3,
      levels = c(1,2,3,4,6,7),
      labels = c("Mexican American", "Other Hispanic", "NH White", "NH Black", "NH Asian", "Other Race including Multi-Racial")
    ),
    Education = factor(
      dmdeduc2,
      levels = c(1,2,3,4,5,7,9),
      labels = c("Less than 9th grade", "9-11th grade", "High school graduate/GED or equivalent", "Some college or AA degree", "College graduate or above", "Refused", "Don’t know")
    ),
    SES = case_when(
      indfmpir < 1.3 ~ "Low SES",
      indfmpir >= 1.3 & indfmpir <= 3.5 ~ "Middle SES",
      indfmpir > 3.5 ~ "High SES",
      TRUE ~ NA_character_
    ),
    SES = factor(
      SES,
      levels = c("Low SES", "Middle SES", "High SES")
    ),
    Age = case_when(
      ridageyr < 20 ~ "Teenager",
      ridageyr >= 20 & ridageyr < 30 ~ "Twenties",
      ridageyr >= 30 & ridageyr < 40 ~ "Thirties",
      ridageyr >= 40 & ridageyr < 50 ~ "Fourties",
      ridageyr >= 50 & ridageyr < 60 ~ "Fifties",
      ridageyr >= 60 & ridageyr < 70 ~ "Sixties",
      ridageyr >= 70 & ridageyr <= 80 ~ "Seventies and 80"
    ),
    Age = factor(
      Age,
      levels = c("Twenties", "Thirties", "Fourties", "Fifties", "Sixties", "Seventies and 80")
    )
  ) |> 
  rowwise() |> 
  mutate(
    dep_total_score = sum(pick(dpq010:dpq100)),
    dep_avg_score = dep_total_score / 10
  ) |> 
  ungroup() |> 
  mutate(
    Depression_Category = case_when(
      dep_total_score <= 4 ~ "None or Minimal Depression",
      dep_total_score <= 9 ~ "Mild Depression",
      dep_total_score <= 14 ~ "Moderate Depression",
      dep_total_score <= 19 ~ "Moderately Severe Depression",
      dep_total_score > 19 ~ "Severe Depression"
    ),
    Depression_Category = factor(
      Depression_Category,
      levels = c("None or Minimal Depression", "Mild Depression", "Moderate Depression", "Moderately Severe Depression", "Severe Depression")
    )
  ) |> 
  mutate(
    Protein = dr1tprot,
    TotalCalories = dr1tkcal,
    Fiber = dr1tfibe,
    Cholesterol = dr1tchol,
    Sodium = dr1tsodi
  )

# Parquet File Creation
write_parquet(dat, "dat.parquet")
###############################

##### UI #####
ui <- fluidPage(
  
    # Shiny Theme
    theme = shinytheme("yeti"),

    # Application title
    titlePanel("NHANES: Exploration of Depression Data"),
    
    # Navigation Bar
    navbarPage(
      "~",
      
      ### TAB 1 ###
      tabPanel(
        "Data Exploration",
        sidebarLayout(
          sidebarPanel(
            selectInput("data_type",
                        label = "Select data type:",
                        choices = c("Demographic", "Dietary"),
                        selected = "Demographic"),
            
            conditionalPanel(
              condition = "input.data_type == 'Demographic'",
              selectInput("plot_type",
                          label = "Select plot type:",
                          choices = c("Boxplot", "Violin", "Heatmap"),
                          selected = "Boxplot")
            ),
            
            conditionalPanel(
              condition = "input.data_type == 'Dietary'",
              selectInput("plot_type",
                          label = "Select plot type:",
                          choices = c("Boxplot", "Violin"),
                          selected = "Boxplot")
            ),
            
            uiOutput("varSelectInput"),

            conditionalPanel(
              condition = "input.plot_type == 'Heatmap'",
              uiOutput("varSelectInput2")
            ),
            
            actionButton(
              "plot_button",
              "Generate Plot",
              icon = icon("play")
            ),
            
          ),
          
          mainPanel(
            plotOutput("boxPlot")
          )
          
        )
      ),
      #############
      
      ### TAB 2 ###
      tabPanel(
        "Coefficient Exploration",
        sidebarLayout(
          sidebarPanel(
            
            selectInput("data_type_coef",
                        label = "Select data type:",
                        choices = c("Demographic", "Dietary"),
                        selected = "Demographic"),
            
            conditionalPanel(
              condition = "input.data_type_coef == 'Demographic'",
              selectInput("ref_race",
                          label = "Select reference group for Race:",
                          choices = c("Mexican American", "Other Hispanic", "NH White", 
                                      "NH Black", "NH Asian", "Other Race including Multi-Racial"),
                          selected = "Mexican American")
            ),
            
            conditionalPanel(
              condition = "input.data_type_coef == 'Demographic'",
              selectInput("ref_education",
                          label = "Select reference group for Education:",
                          choices = c("Less than 9th grade", "9-11th grade", 
                                      "High school graduate/GED or equivalent", 
                                      "Some college or AA degree", "College graduate or above"),
                          selected = "Less than 9th grade")
            ),
            
            conditionalPanel(
              condition = "input.data_type_coef == 'Demographic'",
              selectInput("ref_ses",
                          label = "Select reference group for SES:",
                          choices = c("Low SES", "Middle SES", "High SES"),
                          selected = "Low SES")
            ),
            
            conditionalPanel(
              condition = "input.data_type_coef == 'Demographic'",
              selectInput("ref_gender",
                          label = "Select reference group for Gender:",
                          choices = c("Male", "Female"),
                          selected = "Male")
            ),
            
            conditionalPanel(
              condition = "input.data_type_coef == 'Demographic'",
              selectInput("ref_age",
                          label = "Select reference group for Age:",
                          choices = c("Twenties", "Thirties", "Fourties", "Fifties", "Sixties", "Seventies and 80"),
                          selected = "Twenties")
            ),
            
            actionButton(
              "coefficient_button",
              "Generate Plot",
              icon = icon("play")
            ),
            
          ), # sidebarPanel
          
          mainPanel(
            plotOutput("coefficientPlot")
          )
          
        ) # sidebarLayout
      ), # tabPanel
      #############
      
      ### TAB 3 ###
      tabPanel(
        "Dimensionality Reduction",
        sidebarLayout(
          sidebarPanel(
            selectInput("data_type_pca",
                        label = "Select data type:",
                        choices = c("Demographic", "Dietary", "Mixed"),
                        selected = "Demographic"),
            
            selectInput("choice",
                        label = "Select number of dimensions:",
                        choices = c(3,4,5,6,7),
                        selected = 5),
            
            selectInput("plotly_color",
                        label = "Color data points by:",
                        choices = c("Cluster", "Depression_Category"),
                        selected = "Cluster"),
            
            actionButton(
              "pca_button",
              "Run Analysis",
              icon = icon("play")
            ),
            
          ), # sidebarPanel
          
          mainPanel(
            plotOutput("clusterPlot"),
            plotlyOutput("movingPlot")
          )
          
        ) # sidebarLayout
      ),
      #############
      
      ### TAB 4 ###
      tabPanel(
        "Score Prediction",
        sidebarLayout(
          sidebarPanel(
            selectInput("race_pred",
                        label = "Select race:",
                        choices = c("Mexican American", "Other Hispanic", "NH White", 
                                    "NH Black", "NH Asian", "Other Race including Multi-Racial"),
                        selected = "Mexican American"),
            
            selectInput("education_pred",
                        label = "Select education:",
                        choices = c("Less than 9th grade", "9-11th grade", 
                                    "High school graduate/GED or equivalent", 
                                    "Some college or AA degree", "College graduate or above"),
                        selected = "Less than 9th grade"),
            
            selectInput("ses_pred",
                        label = "Select socioeconomic status:",
                        choices = c("Low SES", "Middle SES", "High SES"),
                        selected = "Low SES"),
            
            actionButton(
              "pred_button",
              "Run Analysis",
              icon = icon("play")
            ),
            
          ), # sidebarPanel
          
          mainPanel(
            textOutput("predText")
          )
          
        ) # sidebarLayout
      ),
      
      
    ) # navbarPage
)
##############

######## SERVER ########
server <- function(input, output) {
  
  # Function to read in parquet data
  dat_parquet <- reactive({
    read_parquet("dat.parquet")
  })
  
  ##### TAB 1 #####
  
  ### ACTION BUTTON ###
  
  reactive_plot_data <- eventReactive(input$plot_button, {
    
    list(
      data_type = input$data_type,
      plot_var = input$plot_var,
      plot_var2 = input$plot_var2,
      plot_type = input$plot_type,
      data_source = dat_parquet()
    )
    
  })
  
  #####################
  
    # First Variable: Inputs for Boxplots, Violin Plots, and Heatmaps
    output$varSelectInput <- renderUI({
      if (input$data_type == "Demographic") {
        selectInput("plot_var",
                    label = "Select variable to plot:",
                    choices = c("Race", "Education", "SES", "Gender", "Age"),
                    selected = "Race")
      } else {
        selectInput("plot_var",
                    label = "Select dietary variable to plot:",
                    choices = c("Protein", "TotalCalories", "Fiber", "Cholesterol", "Sodium"),
                    selected = "Protein")
      }
    })
    
    # Second Variable: Second Input for Heatmaps
    output$varSelectInput2 <- renderUI({
      if (input$data_type == "Demographic") {
        selectInput("plot_var2",
                    label = "Select variable to plot:",
                    choices = c("Race", "Education", "SES", "Gender", "Age"),
                    selected = "Education")
      } else {
        selectInput("plot_var2",
                    label = "Select dietary variable to plot:",
                    choices = c("Protein", "TotalCalories", "Fiber", "Cholesterol", "Sodium"),
                    selected = "TotalCalories")
      }
    })

    ##################### TAB 1 PLOT #####################

    # THE PLOT #
    output$boxPlot <- renderPlot({
      
      plot_data <- reactive_plot_data()
      
      plot_var <- plot_data$plot_var
      plot_var2 <- plot_data$plot_var2
      plot_type <- plot_data$plot_type
      data_type <- plot_data$data_type
      data_source <- plot_data$data_source
      
      filtered_data <- if (data_type == "Demographic") {
        data_source |> 
          filter(
            !is.na(!!sym(plot_var)),  # Remove NA values from selected variable
            !!sym(plot_var) != "Refused",  # Remove "Refused" from education levels
            !!sym(plot_var) != "Don’t know"  # Remove "Don’t know" from education levels
          ) |>
          filter(
            !is.na(!!sym(plot_var2)),  # Remove NA values from selected variable
            !!sym(plot_var2) != "Refused",  # Remove "Refused" from education levels
            !!sym(plot_var2) != "Don’t know"  # Remove "Don’t know" from education levels
          )
      } else {
        dat |> 
          filter(!is.na(!!sym(plot_var))) |> 
          filter(!is.na(!!sym(plot_var2)))
      }
      
      comparisons <- if (data_type == "Demographic") {
        switch(
          plot_var,
          "Race" = race_comparisons,
          "Education" = education_comparisons,
          "SES" = ses_comparisons,
          "Gender" = gender_comparisons,
          "Age" = age_comparisons
        )
      } else {
        depression_comparisons
      }
      
      if (plot_type == "Boxplot") {
        create_boxplot(filtered_data, data_type, plot_var, comparisons)
      } else if (plot_type == "Violin") {
        create_violinplot(filtered_data, data_type, plot_var, comparisons)
      } else {
        create_heatmap(filtered_data, data_type, plot_var, plot_var2)
      }

    })
    
    ##################### TAB 2 ##################### 
    
    reactive_coef_data <- eventReactive(input$coefficient_button, {
      
      data_source <- dat_parquet()
      
      data_source$Race <- relevel(factor(data_source$Race), ref = input$ref_race)
      data_source$Education <- relevel(factor(data_source$Education), ref = input$ref_education)
      data_source$SES <- relevel(factor(data_source$SES), ref = input$ref_ses)
      data_source$Gender <- relevel(factor(data_source$Gender), ref = input$ref_gender)
      data_source$Age <- relevel(factor(data_source$Age), ref = input$ref_age)
      
      list(
        data_type_coef = input$data_type_coef,
        ref_race = input$ref_race,
        ref_education = input$ref_education,
        ref_ses = input$ref_ses,
        ref_gender = input$ref_gender,
        ref_age = input$ref_age,
        data_source = data_source
      )
      
    })
    
    output$coefficientPlot <- renderPlot({
      
      coef_data <- reactive_coef_data()
      data_type_coef <- coef_data$data_type_coef
      data_source <- coef_data$data_source
      ref_race <- coef_data$ref_race
      ref_education <- coef_data$ref_education
      ref_ses <- coef_data$ref_ses
      ref_gender <- coef_data$ref_gender
      ref_age <- coef_data$ref_age
      
      if (data_type_coef == "Demographic") {
        factor_levels <- generate_factor_levels_demo(
          data = data_source,
          ref_race = ref_race,
          ref_education = ref_education,
          ref_ses = ref_ses,
          ref_gender = ref_gender,
          ref_age = ref_age
        )
        create_coefficient_plot(data_source, demo_formula, factor_levels)
      } else {
        create_coefficient_plot(data_source, diet_formula, diet_levels)
      }

    })
    
    ##################
    
    ##################### TAB 3 ##################### 
    
    reactive_pca_data <- eventReactive(input$pca_button, {
      
      list(
        data_type_pca = input$data_type_pca,
        choice = input$choice,
        color = input$plotly_color,
        data_source = dat_parquet()
      )
      
    })
    
    output$clusterPlot <- renderPlot({
      
      pca_data <- reactive_pca_data()
      data_type_pca <- pca_data$data_type_pca
      choice <- as.numeric(pca_data$choice)
      data_source <- pca_data$data_source
      
      if (data_type_pca == "Demographic") {
        create_mca_cluster(data_source, choice)
      } else if (data_type_pca == "Dietary") {
        create_pca_cluster(data_source, choice)
      } else if (data_type_pca == "Mixed") {
        create_famd_cluster(data_source, choice)
      }
      
    })
    
    output$movingPlot <- renderPlotly({
      pca_data <- reactive_pca_data()
      data_type_pca <- pca_data$data_type_pca
      choice <- as.numeric(pca_data$choice)
      color <- pca_data$color
      data_source <- pca_data$data_source
      
      if (data_type_pca == "Demographic") {
        create_mca_plotly(data_source, choice, color)
      } else if (data_type_pca == "Dietary") {
        create_pca_plotly(data_source, choice, color)
      } else if (data_type_pca == "Mixed") {
        create_famd_plotly(data_source, choice, color)
      }
    })
    
    ##################### TAB 4 ##################### 
    
    reactive_pred_data <- eventReactive(input$pred_button, {
      
      list(
        race_pred = input$race_pred,
        education_pred = input$education_pred,
        ses_pred = input$ses_pred,
        data_source = dat_parquet()
      )
      
    })
    
    output$predText <- renderText({
      
      pred_data <- reactive_pred_data()
      race_pred <- pred_data$race_pred
      education_pred <- pred_data$education_pred
      ses_pred <- pred_data$ses_pred
      
      lm_fit <- develop_prediction_model(data = dat)
      
      test_data <- tibble(
        Race = factor(race_pred, levels = c("Mexican American", "Other Hispanic", "NH White", "NH Black", "NH Asian", "Other Race including Multi-Racial")),
        Education = factor(education_pred, levels = c("Less than 9th grade", "9-11th grade", "High school graduate/GED or equivalent", "Some college or AA degree", "College graduate or above")),
        SES = factor(ses_pred, levels = c("Low SES", "Middle SES", "High SES"))
      )
      
      prediction <- predict(lm_fit, test_data)
      
      paste("Predicted Depression Score:", round(prediction$.pred, 2))
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
