# NHANES Depression Analysis R Shiny App
The goal of this R Shiny app is to allow for an interactive exploration of NHANES depression data alongside demographic, dietary, and laboratory data. This app involves the significance testing between depression and other variables, coefficient analysis, dimensionality reduction, and depression score prediction based on various modeling techniques.

## Project Data and Objective
Combining both interviews and physical examinations, the National Health and Nutrition Examination Survey (NHANES) provides an assessment of the health and nutritional characteristics of a nationally representative sample of Americans. The significance of this dataset is showcased through it being the basis for various national standards, including height, weight, and blood pressure. The NHANES survey includes a dedicated mental health/depression screener, involving nine items based on DSM-IV depression diagnostic criteria. The data from this specific NHANES questionnaire served as the primary data for my Shiny app. Through the creation of this app, I aim to showcase the relationship between mental health data and demographic, food, and physical health data, and to encourage future NHANES analysts/researchers to dig into the causes of major depression score differences between groups.

## Tab 1: Data Exploration
![Data Exploration Tab](images/tab1_screenshot_.png)
The purpose of the data exploration tab is to explore any potential relationships between total depression demographic, dietary, and laboratory variables against total depression scores or categorized depression levels. Through this analysis, users can identify significant differences across variable pairs via automatic Wilcoxon tests plotted on boxplots and violin plots. The analysis of two variables with depression scores can also be visualized through heatmaps in this tab.

## Tab 2: Coefficient Analysis
![Coefficient Analysis Tab](images/tab2_screenshot.png)
The coefficient analysis tab provides insights into the relationships between different variables and depression score/categories through the visualization of coefficient estimates from linear regression analyses. By selecting different variables and changing the reference groups, users can interpret how specific subgroups (ex. different education levels or socioeconomic statuses) contribute to variations in depression scores.

## Tab 3: Dimensionality Reduction
![Dimensionality Reduction Tab](images/tab3_screenshot.png)
This dimensionality reduction tab allows users to explore complex relationships in the NHANES dataset through various dimensionality reduction techniques, inlucidng PCA (Principal Component Analysis), MCA (Multiple Correspondence Analysis), and FAMD (Factor Analysis using Mixed Data). These procedures allow for the highlighting of clusters and patterns within demographic, dietary, and laboratory variables. Additionally, the loadings heatmap and density plots allows for a deeper analyses of variable distributions across clusters.

## Tab 4: Depression Score Prediction and Model Comparison
![Score Prediction Tab](images/tab4_screenshot.png)
This tab provides predictive modeling tools to estimate depression scores based on demographic, dietary, and laboratory variables. Users can select from a wide variety of prediction algorithms, including linear regression, tree-based models (random forest, decision trees, and gradient boosting machines), and other machine learning algorithms (support vector machines and neural networks). This tab also includes model comparison visualizations, such as residual plots and predicted vs. actual score comparisons between model types.

## Dependencies
The primary packages utilized in this app are {shiny}, {RNHANES}, {tidyverse}, and {tidymodels}. The {RNHANES} package allows for the direct downloading of the required NHANES data for the analysis, so no extra data setup steps are required for the user. Here is a full list of the dependencies for this app:
```r
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
```

## Data Acknowledgment
This application uses data from the National Health and Nutrition Examination Survey (NHANES), conducted by the National Center for Health Statistics (NCHS). The NHANES dataset provides valuable insights into the health and nutritional status of adults and children in the United States. I acknowledge and thank the NCHS for making this dataset available to the public.

## Future Enhancements
I am currently still in the process of further enhancing and developing this app. I would like to optimize data processing with more reactive caching to make the app run faster. I also would like to continue building upon the aesthetic elements of the app by incorporating more CSS code.

