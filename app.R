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
library(arrow)
library(FactoMineR)
library(factoextra)
library(plotly)
library(caret)
library(tidymodels)
library(reshape2)
library(shinycssloaders)
library(broom)
library(kernlab)
library(xgboost)
library(shinydashboard)
#####################

### HELPER FILES ###
source("utils_vA3.R")
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
    Total_Depression_Score = sum(pick(dpq010:dpq100)),
    dep_avg_score = Total_Depression_Score / 10
  ) |> 
  ungroup() |> 
  mutate(
    Depression_Category = case_when(
      Total_Depression_Score <= 4 ~ "None or Minimal Depression",
      Total_Depression_Score <= 9 ~ "Mild Depression",
      Total_Depression_Score <= 14 ~ "Moderate Depression",
      Total_Depression_Score <= 19 ~ "Moderately Severe Depression",
      Total_Depression_Score > 19 ~ "Severe Depression"
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
  ) |> 
  mutate(
    Vitamin_D = lbxvidms,
    White_Blood_Cells = lbxwbcsi,
    Glucose = lbxglu,
    GLA_Omega_6 = lbxgla,
    Copper = lbxscu,
    Saturated_Fatty_Acids = lbxpm1
  )

# Parquet File Creation
write_parquet(dat, "dat.parquet")
###############################

##### UI #####
ui <- fluidPage(
  
  # CSS
  includeCSS("www/styles.css"),
  
  # Shiny Theme
  theme = shinytheme("yeti"),
  
  # Application title
  div(
    style = "text-align: center;",
    titlePanel("NHANES: Exploration of Depression Data")
  ),
  
  # Navigation Bar
  navbarPage(
    "~",
    
    ### TAB 1 ###
    tabPanel(
      "Data Exploration",
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          plotOutput("boxPlot", width = "100%", height = "600px") |> withSpinner(type = 6, color = "#007bff")
        )
      ),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          wellPanel(
            
            div(
              tags$label("Select data type:",
                         `for` = "data_type"),
              icon("question-circle", id = "data_type_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
              selectInput("data_type",
                          label = NULL,
                          choices = c("Demographic", "Dietary", "Laboratory"),
                          selected = "Demographic"),
              bsPopover("data_type_info", title = "",
                        content = "Choose the type of data you want to explore. Demographic includes age, gender, etc.; Dietary includes nutrition intake; Laboratory includes lab results.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            ),
            
            div(
              tags$label("Select plot type:",
                         `for` = "plot_type"),
              icon("question-circle", id = "plot_type_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
              conditionalPanel(
                condition = "input.data_type == 'Demographic'",
                selectInput("plot_type",
                            label = NULL,
                            choices = c("Boxplot", "Violin", "Heatmap"),
                            selected = "Boxplot")
              ),
              bsPopover("plot_type_info", title = "",
                        content = "Choose the type of plot you want to generate. Boxplots and violin plots plot one variable of interest against depression scores/categories, while heatmaps will allow for the analysis of two variables of interest.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            ),
            
            conditionalPanel(
              condition = "input.data_type == 'Dietary' || input.data_type == 'Laboratory'",
              selectInput("plot_type",
                          label = NULL,
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
            )
          )
        )
      )
      
      
    ),
    #############
    
    ### TAB 2 ###
    tabPanel(
      "Coefficient Exploration",
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          plotOutput("coefficientPlot") |> withSpinner(type = 6, color = "#007bff")
        )
      ),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          wellPanel(
            
            div(
              tags$label("Select data type:",
                         `for` = "data_type_coef"),
              icon("question-circle", id = "data_type_coef_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
              selectInput("data_type_coef",
                          label = NULL,
                          choices = c("Demographic", "Dietary", "Laboratory"),
                          selected = "Demographic"),
              bsPopover("data_type_coef_info", title = "",
                        content = "Choose the type of data you want to explore. Demographic includes age, gender, etc.; Dietary includes nutrition intake; Laboratory includes lab results.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            ),
              
            
            conditionalPanel(
              condition = "input.data_type_coef == 'Demographic'",
              
              div(
                tags$label("Select reference race:",
                           `for` = "ref_race"),
                icon("question-circle", id = "ref_race_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                selectInput("ref_race",
                            label = NULL,
                            choices = c("Mexican American", "Other Hispanic", "NH White", 
                                        "NH Black", "NH Asian", "Other Race including Multi-Racial"),
                            selected = "Mexican American")
              ),
              
              div(
                tags$label("Select reference education:",
                           `for` = "ref_education"),
                icon("question-circle", id = "ref_education_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                selectInput("ref_education",
                            label = NULL,
                            choices = c("Less than 9th grade", "9-11th grade", 
                                        "High school graduate/GED or equivalent", 
                                        "Some college or AA degree", "College graduate or above"),
                            selected = "Less than 9th grade")
              ),
              
              div(
                tags$label("Select reference SES:",
                           `for` = "ref_ses"),
                icon("question-circle", id = "ref_ses_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                selectInput("ref_ses",
                            label = NULL,
                            choices = c("Low SES", "Middle SES", "High SES"),
                            selected = "Low SES")
              ),
              
              div(
                tags$label("Select reference gender:",
                           `for` = "ref_gender"),
                icon("question-circle", id = "ref_gender_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                selectInput("ref_gender",
                            label = NULL,
                            choices = c("Male", "Female"),
                            selected = "Male")
              ),
              
              div(
                tags$label("Select reference group for age:",
                           `for` = "ref_age"),
                icon("question-circle", id = "ref_age_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                selectInput("ref_age",
                           label = NULL,
                           choices = c("Twenties", "Thirties", "Fourties", "Fifties", "Sixties", "Seventies and 80"),
                           selected = "Twenties")
              )
            
            ),
            
            actionButton(
              "coefficient_button",
              "Generate Plot",
              icon = icon("play")
            )
          )
        )
      )
    ), # tabPanel
    #############
    
    ### TAB 3 ###
    tabPanel(
      "Dimensionality Reduction",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          wellPanel(
            h4("Data Type and Dimension Selection"),
            
            div(
              tags$label("Select data type:",
                         `for` = "data_type_pca"),
              icon("question-circle", id = "data_type_pca_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
              selectInput("data_type_pca",
                          label = NULL,
                          choices = c("Demographic", "Dietary", "Laboratory", "Mixed"),
                          selected = "Demographic"),
              bsPopover("data_type_pca_info", title = "",
                        content = "Choose the data type for the dimensionality reduction. Choosing Demographic will run a Multiple Correspondance Analysis, Dietary or Laboratory will run a Principal Component Analysis, and Mixed will conduct a Factor Analysis of Mixed Data.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            ),
            
            div(
              tags$label("Select number of dimensions:",
                         `for` = "choice"),
              icon("question-circle", id = "choice_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
              selectInput("choice",
                          label = NULL,
                          choices = c(3,4,5,6,7),
                          selected = 5),
              bsPopover("choice_info", title = "",
                        content = "Choose the number of dimensions/features for the dimensionality reduction. Choosing a low number of dimensions will attempt to capture the variance of the dataset in few features, and choosing a high number will ensure variance capture but with many features.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            ),
            
            div(
              tags$label("Color data points by:",
                         `for` = "plotly_color"),
              icon("question-circle", id = "plotly_color_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
              selectInput("plotly_color",
                          label = NULL,
                          choices = c("Cluster", "Depression_Category"),
                          selected = "Cluster"),
              bsPopover("plotly_color_info", title = "",
                        content = "Color the data points by how they cluster or by their depression category.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            )

          ),
          
          wellPanel(
            h4("Density Plot Settings"),
            
            div(
              tags$label("Select density variable:",
                         `for` = "density_var"),
              icon("question-circle", id = "density_var_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
              conditionalPanel(
                condition = "input.data_type_pca == 'Demographic'",
                selectInput("density_var",
                            label = NULL,
                            choices = c("Race", "Education", "SES"),
                            selected = "Race")
              ),
              bsPopover("density_var_info", title = "",
                        content = "Choose which variable to visualize in the density plot.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            ),
            
            conditionalPanel(
              condition = "input.data_type_pca == 'Mixed'",
              selectInput("density_var",
                          label = NULL,
                          choices = c("Race", "Education", "SES", "lbxvidms", "lbxwbcsi", "lbxglu", "lbxgla", "lbxscu", "lbxpm1", "dr1tprot", "dr1tsugr", "dr1tcarb", "dr1tfibe", "dr1tchol", "dr1tsodi", "dr1tmois", "dr1tkcal", "dr1tsfat", "dr1tmfat", "dr1tpfat", "dr1talco", "dr1ts100"),
                          selected = "Race")
            )
          ),
          
          actionButton(
            "pca_button",
            "Run Analysis",
            icon = icon("play")
          )
        ),
        
        mainPanel(
          width = 9,
          tabsetPanel(
            
            tabPanel(
              "Complete Analysis",
              fluidRow(
                column(
                  width = 6,
                  plotOutput("clusterPlot_all") |> withSpinner(type = 6, color = "#007bff")
                ),
                column(
                  width = 6,
                  plotlyOutput("movingPlot_all") |> withSpinner(type = 6, color = "#007bff")
                )
              ),
              
              fluidRow(
                column(
                  width = 6,
                  plotOutput("loadingHeatmapPlot_all") |> withSpinner(type = 6, color = "#007bff")
                ),
                column(
                  width = 6,
                  plotlyOutput("densityPlot_all") |> withSpinner(type = 6, color = "#007bff"),
                ),
              )
            ),
            
            tabPanel(
              "Cluster Analysis",
              plotOutput("clusterPlot") |> withSpinner(type = 6, color = "#007bff")
            ),
            
            tabPanel(
              "3D Plotly Analysis",
              plotlyOutput("movingPlot", height = "700px", width = "100%") |> withSpinner(type = 6, color = "#007bff")
            ),
            
            tabPanel(
              "Loadings Analysis",
              plotOutput("loadingHeatmapPlot") |> withSpinner(type = 6, color = "#007bff")
            ),
            
            tabPanel(
              "Density Analysis",
              plotlyOutput("densityPlot") |> withSpinner(type = 6, color = "#007bff")
            )
            
          )
        )
      )
    ),
    
    #############
    
    ### TAB 4 ###
    tabPanel(
      "Score Prediction",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          wellPanel(
            h4("Model Selection"),
            
            div(
              tags$label("Select model type:",
                         `for` = "model_type"),
              icon("question-circle", id = "model_type_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
              selectInput("model_type",
                          label = NULL,
                          choices = c("Linear Model", "Random Forest", "Decision Tree", "Support Vector Machine", "Gradient Boosting Machine", "Neural Network"),
                          selected = "Linear Model"),
              bsPopover("model_type_info", title = "",
                        content = "Choose the model to predict depression scores based on your inputs, and to compare all actual vs. predicted score. Linear Models assume linear relationships, suiting it for simple regression tasks. Random Forest, Decision Trees, and Gradient Boosting Machines are tree-based models. Support Vector Machines utilize hyperplanes to classify data point in high-dimensional spaces, and neural networks use interconnected nodes to generate complex relationships.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            )

          ),
          
          tabsetPanel(
            tabPanel(
              title = div("Demographic Variables", icon("question-circle", id = "tab1_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;")),
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
              
              selectInput("sex_pred",
                          label = "Select gender:",
                          choices = c("Male", "Female"),
                          selected = "Male"),
              
              selectInput("age_pred",
                          label = "Select age:",
                          choices = c("Twenties", "Thirties", "Fourties", "Fifties", "Sixties", "Seventies and 80"),
                          selected = "Twenties"),
              
              bsPopover("tab1_info", title = "",
                        content = "Select the demographic variables to be used in the depression score prediction.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            ),
            
            tabPanel(
              title = div("Dietary Variables", icon("question-circle", id = "tab2_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;")),
              sliderInput("protein",
                          label = "Protein Intake (g)",
                          min = 0,
                          max = 850,
                          value = 70,
                          step = 10),
              
              sliderInput("calories",
                          label = "Total Calories Intake",
                          min = 100,
                          max = 12000,
                          value = 2000,
                          step = 100),
              
              sliderInput("fiber",
                          label = "Fiber Intake",
                          min = 0,
                          max = 130,
                          value = 15,
                          step = 5),
              
              sliderInput("cholesterol",
                          label = "Cholesterol Intake",
                          min = 0,
                          max = 130,
                          value = 15,
                          step = 5),
              
              sliderInput("sodium",
                          label = "Sodium Intake",
                          min = 0,
                          max = 21000,
                          value = 3000,
                          step = 200),
              
              bsPopover("tab2_info", title = "",
                        content = "Select the dietary variables to be used in the depression score prediction.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            ),
            
            tabPanel(
              title = div("Laboratory Variables", icon("question-circle", id = "tab3_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;")),
              sliderInput("vitamin_d",
                          label = "Vitamin D Intake",
                          min = 10,
                          max = 300,
                          value = 60,
                          step = 10),
              
              sliderInput("wbc",
                          label = "White Blood Cell Count",
                          min = 2,
                          max = 54,
                          value = 8,
                          step = 2),
              
              sliderInput("glucose",
                          label = "Glucose Level",
                          min = 60,
                          max = 420,
                          value = 100,
                          step = 20),
              
              sliderInput("gla",
                          label = "GLA Omega 6 Level",
                          min = 5,
                          max = 350,
                          value = 50,
                          step = 10),
              
              sliderInput("copper",
                          label = "Copper Level",
                          min = 25,
                          max = 300,
                          value = 125,
                          step = 25),
              
              sliderInput("saturated_fats",
                          label = "Saturated Fats Level",
                          min = 800,
                          max = 18500,
                          value = 2500,
                          step = 100),
              
              bsPopover("tab3_info", title = "",
                        content = "Select the laboratory variables to be used in the depression score prediction.",
                        placement = "right", trigger = "click",
                        options = list(container = "body"))
            )
          ),
          
          actionButton(
            "pred_button",
            "Run Analysis",
            icon = icon("play")
          )
          
        ), # sidebarPanel
        
        mainPanel(
          
          tabsetPanel(
            tabPanel(
              "Depression Score Prediction",
              fluidRow(
                column(
                  width = 5,
                  infoBoxOutput("predScoreBox") |> withSpinner(type = 6, color = "#007bff")
                )
              ),
              
              column(width = 12,
                     fluidRow(
                       column(
                         width = 6,
                         plotOutput("modelPerformancePlot") |> withSpinner(type = 6, color = "#007bff")
                       ),
                       column(
                         width = 6,
                         plotOutput("predictedVsActualPlot") |> withSpinner(type = 6, color = "#007bff")
                       )
                     )
              )
            ),
            
            tabPanel(
              "Model Comparisons",
              fluidRow(
                column(
                  width = 12,
                  plotlyOutput("overlappingResidualPlot", height = "400px") |> withSpinner(type = 6, color = "#007bff")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  plotOutput("facetedResidualPlot", height = "400px") |> withSpinner(type = 6, color = "#007bff")
                )
              )
            )
            
          )
        )
      ) # sidebarLayout
    ) # tabPanel
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
      div(
        tags$label("Select demographic variable to plot:",
                   `for` = "plot_var"),
        icon("question-circle", id = "plot_var_demo_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
        selectInput("plot_var",
                    label = NULL,
                    choices = c("Race", "Education", "SES", "Gender", "Age"),
                    selected = "Race"),
        bsPopover("plot_var_demo_info", title = "", content = "Select the demographic variable to include in the plot.",
                  placement = "right", trigger = "click",
                  options = list(container = "body"))
      )
    } else if (input$data_type == "Dietary") {
      div(
        tags$label("Select dietary variable to plot:",
                   `for` = "plot_var"),
        icon("question-circle", id = "plot_var_diet_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
        selectInput("plot_var",
                    label = NULL,
                    choices = c("Protein", "TotalCalories", "Fiber", "Cholesterol", "Sodium"),
                    selected = "Protein"),
        bsPopover("plot_var_diet_info", title = "", content = "Select the dietary variable to include in the plot.",
                  placement = "right", trigger = "click",
                  options = list(container = "body"))
      )
    } else {
      div(
        tags$label("Select laboratory variable to plot:",
                   `for` = "plot_var"),
        icon("question-circle", id = "plot_var_lab_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
        selectInput("plot_var",
                    label = NULL,
                    choices = c("Vitamin_D", "White_Blood_Cells", "Glucose", "GLA_Omega_6", "Copper", "Saturated_Fatty_Acids"),
                    selected = "Vitamin_D"),
        bsPopover("plot_var_lab_info", title = "", content = "Select the laboratory variable to include in the plot.",
                  placement = "right", trigger = "click",
                  options = list(container = "body"))
      )
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
  
  output$boxPlot <- renderPlot({
    
    if (is.null(input$plot_button) || input$plot_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      default_data <- data_source |> filter(!is.na(Race))  # Filter out NA values for Race
      comparisons <- get_significant_comparisons("Demographic", default_data, "Race")
      return(create_default_boxplot(default_data, comparisons))
    }
    
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
      data_source |> 
        filter(!is.na(!!sym(plot_var))) |> 
        filter(!is.na(!!sym(plot_var2))) |> 
        filter(!is.na(Depression_Category))
    }
    
    comparisons <- get_significant_comparisons(data_type, filtered_data, plot_var)
    
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
  
  observeEvent(input$data_type_coef, {
    if (input$data_type_coef == "Demographic") {
      bsPopover("data_type_coef_info", title = "",
                content = "Choose the type of data you want to explore. Demographic includes age, gender, etc.; Dietary includes nutrition intake; Laboratory includes lab results.",
                placement = "right", trigger = "click",
                options = list(container = "body"))
  
      bsPopover("ref_race_info", title = "",
                content = "Choose the reference Race for the analysis. For example, choosing 'Mexican American' will compare all other Race groups to 'Mexican American'.",
                placement = "right", trigger = "click",
                options = list(container = "body"))

      bsPopover("ref_education_info", title = "",
                content = "Choose the reference Education for the analysis. For example, choosing 'College graduate or above' will compare all other Race groups to 'College graduate or above'.",
                placement = "right", trigger = "click",
                options = list(container = "body"))

      bsPopover("ref_ses_info", title = "",
                content = "Choose the reference SES for the analysis. For example, choosing 'Low SES' will compare all other Race groups to 'Low SES'.",
                placement = "right", trigger = "click",
                options = list(container = "body"))
      
      bsPopover("ref_gender_info", title = "",
                content = "Choose the reference gender for the analysis. For example, choosing 'Male' will compare all other Race groups to 'Male'.",
                placement = "right", trigger = "click",
                options = list(container = "body"))

      bsPopover("ref_age_info", title = "",
                content = "Choose the reference age group for the analysis. For example, choosing 'Male' will compare all other Race groups to 'Male'.",
                placement = "right", trigger = "click",
                options = list(container = "body"))
    }
    
  })
  
  output$coefficientPlot <- renderPlot({
    if (is.null(input$coefficient_button) || input$coefficient_button == 0) {
      default_data <- dat_parquet()
      return(create_default_coefficient_plot(default_data))
    }
  })
  
  observeEvent(input$coefficient_button, {
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
      } else if (data_type_coef == "Dietary") {
        create_coefficient_plot(data_source, diet_formula, diet_levels)
      } else {
        create_coefficient_plot(data_source, lab_formula, lab_levels)
      }
      
    })
  })
  
  ##################
  
  ##################### TAB 3 ##################### 
  
  reactive_pca_data <- eventReactive(input$pca_button, {
    
    list(
      data_type_pca = input$data_type_pca,
      choice = input$choice,
      color = input$plotly_color,
      density_var = input$density_var,
      data_source = dat_parquet()
    )
    
  })
  
  output$clusterPlot_all <- renderPlot({
    
    if (is.null(input$pca_button) || input$pca_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      return(create_default_cluster(data_source))
    }
    
    pca_data <- reactive_pca_data()
    data_type_pca <- pca_data$data_type_pca
    choice <- as.numeric(pca_data$choice)
    data_source <- pca_data$data_source
    
    if (data_type_pca == "Demographic") {
      create_mca_cluster(data_source, choice)
    } else if (data_type_pca == "Dietary") {
      create_pca_cluster(data_type = "Dietary", data = data_source, choice = choice)
    } else if (data_type_pca == "Laboratory") {
      create_pca_cluster(data_type = "Laboratory", data = data_source, choice = choice)
    } else if (data_type_pca == "Mixed") {
      create_famd_cluster(data_source, choice)
    }
    
  })
  
  output$movingPlot_all <- renderPlotly({
    
    if (is.null(input$pca_button) || input$pca_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      return(create_default_plotly(data_source))
    }
    
    pca_data <- reactive_pca_data()
    data_type_pca <- pca_data$data_type_pca
    choice <- as.numeric(pca_data$choice)
    color <- pca_data$color
    data_source <- pca_data$data_source
    
    if (data_type_pca == "Demographic") {
      create_mca_plotly(data_source, choice, color)
    } else if (data_type_pca == "Dietary") {
      create_pca_plotly(data_type = "Dietary", data_source, choice, color)
    } else if (data_type_pca == "Laboratory") {
      create_pca_plotly(data_type = "Laboratory", data_source, choice, color)
    }else if (data_type_pca == "Mixed") {
      create_famd_plotly(data_source, choice, color)
    }
  })
  
  output$loadingHeatmapPlot_all <- renderPlot({
    
    if (is.null(input$pca_button) || input$pca_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      return(create_default_loading_heatmap(data_source))
    }
    
    pca_data <- reactive_pca_data()
    data_type_pca <- pca_data$data_type_pca
    choice <- as.numeric(pca_data$choice)
    color <- pca_data$color
    data_source <- pca_data$data_source
    
    if (data_type_pca == "Demographic") {
      create_mca_loading_heatmap(data_source, choice)
    } else if (data_type_pca == "Dietary") {
      create_pca_loading_heatmap(data_type = "Dietary", data_source)
    } else if (data_type_pca == "Laboratory") {
      create_pca_loading_heatmap(data_type = "Laboratory", data_source)
    } else if (data_type_pca == "Mixed") {
      create_famd_loading_heatmap(data_source, choice)
    }
  })
  
  output$densityPlot_all <- renderPlotly({
    
    if (is.null(input$pca_button) || input$pca_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      return(create_default_density_plots(data_source))
    }
    
    pca_data <- reactive_pca_data()
    data_type_pca <- pca_data$data_type_pca
    choice <- as.numeric(pca_data$choice)
    color <- pca_data$color
    data_source <- pca_data$data_source
    density_var <- pca_data$density_var
    
    if (data_type_pca == "Demographic") {
      create_mca_density_plots(data_source, choice, density_var)
    } else if (data_type_pca == "Dietary") {
      create_pca_density_plots(data_type = "Dietary", data_source)
    } else if (data_type_pca == "Laboratory") {
      create_pca_density_plots(data_type = "Laboratory", data_source)
    } else if (data_type_pca == "Mixed") {
      create_famd_density_plots(data_source, choice, density_var)
    }
  })
  
  output$clusterPlot <- renderPlot({
    
    if (is.null(input$pca_button) || input$pca_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      return(create_default_cluster(data_source))
    }
    
    pca_data <- reactive_pca_data()
    data_type_pca <- pca_data$data_type_pca
    choice <- as.numeric(pca_data$choice)
    data_source <- pca_data$data_source
    
    if (data_type_pca == "Demographic") {
      create_mca_cluster(data_source, choice)
    } else if (data_type_pca == "Dietary") {
      create_pca_cluster(data_type = "Dietary", data = data_source, choice = choice)
    } else if (data_type_pca == "Laboratory") {
      create_pca_cluster(data_type = "Laboratory", data = data_source, choice = choice)
    } else if (data_type_pca == "Mixed") {
      create_famd_cluster(data_source, choice)
    }
    
  })
  
  output$movingPlot <- renderPlotly({
    
    if (is.null(input$pca_button) || input$pca_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      return(create_default_plotly(data_source))
    }
    
    pca_data <- reactive_pca_data()
    data_type_pca <- pca_data$data_type_pca
    choice <- as.numeric(pca_data$choice)
    color <- pca_data$color
    data_source <- pca_data$data_source
    
    if (data_type_pca == "Demographic") {
      create_mca_plotly(data_source, choice, color)
    } else if (data_type_pca == "Dietary") {
      create_pca_plotly(data_type = "Dietary", data_source, choice, color)
    } else if (data_type_pca == "Laboratory") {
      create_pca_plotly(data_type = "Laboratory", data_source, choice, color)
    }else if (data_type_pca == "Mixed") {
      create_famd_plotly(data_source, choice, color)
    }
  })
  
  output$loadingHeatmapPlot <- renderPlot({
    
    if (is.null(input$pca_button) || input$pca_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      return(create_default_loading_heatmap(data_source))
    }
    
    pca_data <- reactive_pca_data()
    data_type_pca <- pca_data$data_type_pca
    choice <- as.numeric(pca_data$choice)
    color <- pca_data$color
    data_source <- pca_data$data_source
    
    if (data_type_pca == "Demographic") {
      create_mca_loading_heatmap(data_source, choice)
    } else if (data_type_pca == "Dietary") {
      create_pca_loading_heatmap(data_type = "Dietary", data_source)
    } else if (data_type_pca == "Laboratory") {
      create_pca_loading_heatmap(data_type = "Laboratory", data_source)
    } else if (data_type_pca == "Mixed") {
      create_famd_loading_heatmap(data_source, choice)
    }
  })
  
  output$densityPlot <- renderPlotly({
    
    if (is.null(input$pca_button) || input$pca_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      return(create_default_density_plots(data_source))
    }
    
    pca_data <- reactive_pca_data()
    data_type_pca <- pca_data$data_type_pca
    choice <- as.numeric(pca_data$choice)
    color <- pca_data$color
    data_source <- pca_data$data_source
    density_var <- pca_data$density_var
    
    if (data_type_pca == "Demographic") {
      create_mca_density_plots(data_source, choice, density_var)
    } else if (data_type_pca == "Dietary") {
      create_pca_density_plots(data_type = "Dietary", data_source)
    } else if (data_type_pca == "Laboratory") {
      create_pca_density_plots(data_type = "Laboratory", data_source)
    } else if (data_type_pca == "Mixed") {
      create_famd_density_plots(data_source, choice, density_var)
    }
  })
  
  ##################### TAB 4 ##################### 
  
  reactive_pred_data <- eventReactive(input$pred_button, {
    
    list(
      race_pred = input$race_pred,
      education_pred = input$education_pred,
      ses_pred = input$ses_pred,
      sex_pred = input$sex_pred,
      age_pred = input$age_pred,
      protein = input$protein,
      calories = input$calories,
      fiber = input$fiber,
      cholesterol = input$cholesterol,
      sodium = input$sodium,
      vitamin_d = input$vitamin_d,
      wbc = input$wbc,
      glucose = input$glucose,
      gla = input$gla,
      copper = input$copper,
      saturated_fats = input$saturated_fats,
      model_type = input$model_type,
      data_source = dat_parquet()
    )
    
  })
  
  output$predScoreBox <- renderInfoBox({
    
    if (is.null(input$pred_button) || input$pred_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      lm_fit <- default_develop_prediction_model(data_source)
      prediction <- predict(lm_fit, default_test_data)
      return(infoBox(
        "Predicted Depression Score",
        paste0(round(prediction, 2)),
        icon = icon("stethoscope"),
        color = "blue",
        fill = TRUE
      ))
    }
    
    pred_data <- reactive_pred_data()
    race_pred <- pred_data$race_pred
    education_pred <- pred_data$education_pred
    ses_pred <- pred_data$ses_pred
    sex_pred <- pred_data$sex_pred
    age_pred <- pred_data$age_pred
    protein <- pred_data$protein
    calories <- pred_data$calories
    fiber <- pred_data$fiber
    cholesterol <- pred_data$cholesterol
    sodium <- pred_data$sodium
    vitamin_d <- pred_data$vitamin_d
    wbc <- pred_data$wbc
    glucose <- pred_data$glucose
    gla <- pred_data$gla
    copper <- pred_data$copper
    saturated_fats <- pred_data$saturated_fats
    model_type <- pred_data$model_type
    data_source <- dat_parquet()
    
    set.seed(123)
    
    lm_fit <- develop_prediction_model(data = data_source, model_type)
    
    test_data <- tibble(
      Race = factor(race_pred, levels = c("Mexican American", "Other Hispanic", "NH White", "NH Black", "NH Asian", "Other Race including Multi-Racial")),
      Education = factor(education_pred, levels = c("Less than 9th grade", "9-11th grade", "High school graduate/GED or equivalent", "Some college or AA degree", "College graduate or above")),
      SES = factor(ses_pred, levels = c("Low SES", "Middle SES", "High SES")),
      Gender = factor(sex_pred, levels = c("Male", "Female")),
      Age = factor(age_pred, levels = c("Twenties", "Thirties", "Fourties", "Fifties", "Sixties", "Seventies and 80")),
      Protein = protein,
      TotalCalories = calories,
      Fiber = fiber,
      Cholesterol = cholesterol,
      Sodium = sodium,
      Vitamin_D = vitamin_d,
      White_Blood_Cells = wbc,
      Glucose = glucose,
      GLA_Omega_6 = gla,
      Copper = copper,
      Saturated_Fatty_Acids = saturated_fats
    )
    
    prediction <- predict(lm_fit, test_data)
    
    infoBox(
      "Predicted Depression Score",
      paste0(round(prediction, 2)),
      icon = icon("stethoscope"),
      color = "blue",
      fill = TRUE
    )
    
  })
  
  output$modelPerformancePlot <- renderPlot({
    
    if (is.null(input$pred_button) || input$pred_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      lm_fit <- default_develop_prediction_model(data_source)
      return(default_create_model_performance_plot(data_source, lm_fit))
    }
    
    pred_data <- reactive_pred_data()
    model_type <- pred_data$model_type
    data_source <- pred_data$data_source
    
    lm_fit <- develop_prediction_model(data = dat, model_type)
    
    create_model_performance_plot(data_source, lm_fit)
    
  })
  
  
  output$predictedVsActualPlot <- renderPlot({
    
    if (is.null(input$pred_button) || input$pred_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      lm_fit <- default_develop_prediction_model(data_source)
      return(create_predicted_vs_actual_plot(data_source, lm_fit))
    }
    
    pred_data <- reactive_pred_data()
    model_type <- pred_data$model_type
    data_source <- pred_data$data_source
    
    lm_fit <- develop_prediction_model(data = dat, model_type)
    
    create_predicted_vs_actual_plot(data_source, lm_fit)
    
  })
  
  output$overlappingResidualPlot <- renderPlotly({
    
    if (is.null(input$pred_button) || input$pred_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      residuals_data <- create_residual_data(data_source)
      return(create_overlapping_residual_plot(residuals_data))
    }
    
    pred_data <- reactive_pred_data()
    data_source <- pred_data$data_source
    
    residuals_data <- create_residual_data(data_source)
    create_overlapping_residual_plot(residuals_data)
    
  })
  
  output$facetedResidualPlot <- renderPlot({
    
    if (is.null(input$pred_button) || input$pred_button == 0) {
      data_source <- dat_parquet()  # Load data for default plot
      residuals_data <- create_residual_data(data_source)
      return(create_facet_wrap_overlapping_residual_plot(residuals_data))
    }
    
    pred_data <- reactive_pred_data()
    data_source <- pred_data$data_source
    
    residuals_data <- create_residual_data(data_source)
    create_facet_wrap_overlapping_residual_plot(residuals_data)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
