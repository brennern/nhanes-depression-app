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
library(tibble)
library(rstatix)
library(shinyBS)
library(memoise)
library(ranger)
library(future)
library(promises)
library(future.callr)
#####################

plan(callr)

### FUNCTIONS ###

get_significant_comparisons <- function(data_type, data, plot_var, method = "wilcox.test", alpha = 0.05) {

  if (data_type == "Demographic") {
    comparisons_pvalues <- compare_means(
      formula = as.formula(paste("Total_Depression_Score", "~", plot_var)),
      data = data,
      method = method)
  } else {
    comparisons_pvalues <- compare_means(
      formula = as.formula(paste(plot_var, "~", "Depression_Category")),
      data = data,
      method = method)
  }

  significant_comparisons <- comparisons_pvalues |> 
    filter(p < alpha) |> 
    select(group1, group2) |> 
    apply(1, as.list) |> 
    lapply(unlist)
  
  return(significant_comparisons)
}

create_default_boxplot <- function(data, comparisons) {
  ggboxplot(data,
            x = "Race",
            y = "Total_Depression_Score",
            add = "jitter",
            color = "Race",
            add.params = list(alpha = 0.2, position = position_jitter(width = 100))) +
    stat_compare_means(method = "wilcox.test",
                       comparisons = comparisons,
                       label = "p.signif") +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
      axis.title = element_text(size = rel(1.5)),
      legend.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1.25))
    )
}

create_boxplot <- function(data, data_type, plot_var, comparisons) {
  if (data_type == "Demographic") {
    ggboxplot(data,
              x = plot_var,
              y = "Total_Depression_Score",
              add = "jitter",
              color = plot_var,
              add.params = list(alpha = 0.2)) +
      stat_compare_means(method = "wilcox.test",
                         comparisons = comparisons,
                         label = "p.signif") +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
        axis.title = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.25))
      )
  } else {
    ggboxplot(data,
              x = "Depression_Category",
              y = plot_var,
              add = "jitter",
              color = "Depression_Category",
              add.params = list(alpha = 0.2)) +
      stat_compare_means(method = "wilcox.test",
                         comparisons = comparisons,
                         label = "p.signif") +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
        axis.title = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.25))
      )
  }
}

create_violinplot <- function(data, data_type, plot_var, comparisons) {
  if (data_type == "Demographic") {
    ggviolin(data,
             x = plot_var,
             y = "Total_Depression_Score",
             add = "jitter",
             color = plot_var,
             add.params = list(alpha = 0.2)) +
      stat_compare_means(method = "wilcox.test",
                         comparisons = comparisons,
                         label = "p.signif") +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
        axis.title = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.25))
      )
  } else {
    ggviolin(data,
             x = "Depression_Category",
             y = plot_var,
             add = "jitter",
             color = "Depression_Category",
             add.params = list(alpha = 0.2)) +
      stat_compare_means(method = "wilcox.test",
                         comparisons = comparisons,
                         label = "p.signif") +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
        axis.title = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.25))
      )
  }
}

create_heatmap <- function(data, data_type, plot_var, plot_var2) {
  # Use dynamic variable names correctly in `aggregate`
  agg_data <- data %>%
    group_by(!!sym(plot_var), !!sym(plot_var2)) %>%
    summarize(Mean_Depression_Score = mean(Total_Depression_Score, na.rm = TRUE), .groups = "drop")
  
  ggplot(agg_data, aes_string(x = plot_var, y = plot_var2, fill = "Mean_Depression_Score")) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C", name = "Mean Depression Score") +
    theme_minimal() +
    labs(x = plot_var, y = plot_var2, fill = "Mean Depression Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5))
          )
}

########################### TAB 2

create_default_coefficient_plot <- function(data) {
  
  model <- lm(Total_Depression_Score ~ Race + Education + SES + Gender + Age,
              data = data)
  
  data <- data |> 
    mutate(
      Race = fct_relevel(Race, "Mexican American"),
      SES = fct_relevel(SES, "Low"),
      Education = fct_relevel(Education, "Less than 9th grade"),
      Gender = fct_relevel(Gender, "Male"),
      Age = fct_relevel(Age, "Twenties")
    )
  
  lm_coefficients <- broom::tidy(model, conf.int = TRUE)
  
  lm_coefficients <- lm_coefficients |> 
    filter(!is.na(term))
  
  #lm_coefficients$term <- factor(lm_coefficients$term, 
                                 #levels = c("ridreth3Mexican American", "ridreth3NH Black", "ridreth3NH Asian", "ridreth3Other Race including Multi-Racial", "SESHigh SES", "SESLow SES", "dmdeduc29-11th grade", "dmdeduc2High school graduate/GED or equivalent", "dmdeduc2Some college or AA degree", "dmdeduc2College graduate or above", "(Intercept)"))

  max_estimate <- max(abs(lm_coefficients$estimate), na.rm = TRUE) + 2
  
  ggplot(lm_coefficients, aes(x = term, y = estimate)) +
    geom_bar(stat = "identity", fill = "white", color = "black") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    coord_flip() +
    geom_text(aes(label = paste0("p = ", round(p.value, 3))), 
              nudge_y = 4,
              hjust = 1,
              size = 5) +  # Add p-values as text labels
    labs(title = "Coefficient Plot: Effect of Variables on Depression Score", 
         x = "Predictor Variables", 
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = rel(1.5)),
      axis.title = element_text(size = rel(1.5))
    )
}

create_coefficient_plot <- function(data, formula, factor_levels) {
  model <- lm(formula,
              data = data)
  
  lm_coefficients <- broom::tidy(model, conf.int = TRUE)
  
  lm_coefficients <- lm_coefficients |> 
    filter(!is.na(term))
  
  lm_coefficients$term <- factor(lm_coefficients$term, 
                                 levels = factor_levels)
  
  max_estimate <- max(abs(lm_coefficients$estimate), na.rm = TRUE) + 2
  
  ggplot(lm_coefficients, aes(x = term, y = estimate)) +
    geom_bar(stat = "identity", fill = "white", color = "black") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    coord_flip() +
    geom_text(aes(label = paste0("p = ", round(p.value, 3))), 
              nudge_y = max_estimate - 4,
              hjust = 1,
              size = 5) +  # Add p-values as text labels
    labs(title = "Coefficient Plot: Effect of Variables on Depression Score", 
         x = "Predictor Variables", 
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = rel(1.5)),
      axis.title = element_text(size = rel(1.5))
    )
}

generate_factor_levels_demo <- function(data, ref_race, ref_ses, ref_education, ref_gender, ref_age) {
  levels_race <- paste0("Race", c(ref_race, setdiff(levels(data$Race), ref_race)))
  levels_ses <- paste0("SES", c(ref_ses, setdiff(levels(data$SES), ref_ses)))
  levels_education <- paste0("Education", c(ref_education, setdiff(levels(data$Education), ref_education)))
  levels_gender <- paste0("Gender", c(ref_gender, setdiff(levels(data$Gender), ref_gender)))
  levels_age <- paste0("Age", c(ref_age, setdiff(levels(data$Age), ref_age)))
  
  
  factor_levels <- c(levels_race, levels_ses, levels_education, levels_gender, levels_age, "(Intercept)")
  
  return(factor_levels)
}

################# TAB 3 GRAPHS #################

create_default_cluster <- memoise(function(data) {
    
    demo_data <- data |> 
      select(Depression_Category, Race, Education, SES) |> 
      drop_na() |> 
      select(-Depression_Category)
    
    result <- MCA(demo_data, graph=FALSE, ncp = 3)
    res.hpc2 <- HCPC(result, graph = FALSE)
    
    #great convex hulls
    fviz_cluster(res.hpc2,
                 repel = T,            # Avoid label overlapping
                 geom = "point", #plot only points
                 show.clust.cent = TRUE, # Show cluster centers
                 palette = "npg",         # Color palette see ?ggpubr::ggpar
                 ggtheme = theme_minimal(),
                 main = "MCA Cluster Graph"
    )
  
})

create_default_plotly <- memoise(function(data) {
    
    demo_data <- data |> 
      select(Depression_Category, Race, Education, SES) |> 
      drop_na() |> 
      select(-Depression_Category)
    
    demo_data2 <- data |> 
      select(Depression_Category, Race, Education, SES) |> 
      drop_na()
    
    result <- MCA(demo_data, graph=FALSE, ncp = 3)
    mca_data <- as.data.frame(result$ind$coord[, 1:3])
    res.hpc2 <- HCPC(result, graph = FALSE)
    
    mca_data$Cluster <- as.factor(res.hpc2$data.clust$clust)
    mca_data$Depression_Category <- demo_data2$Depression_Category
    
    plot_ly(data=mca_data, x=~`Dim 1`, y=~`Dim 2`, z=~`Dim 3`, type="scatter3d", mode="markers", color=~Cluster, colors = "Spectral")
  
})

create_default_loading_heatmap <- memoise(function(data) {
  demo_data <- data |> 
    select(Depression_Category, Race, Education, SES) |> 
    drop_na() |> 
    select(-Depression_Category)
  
  result <- MCA(demo_data, graph=FALSE, ncp = 3)
  loadings <- as.data.frame(result$var$coord[, 1:3])
  loadings$Variable <- rownames(loadings)
  loadings_melt <- melt(loadings, id = "Variable")
  
  ggplot(loadings_melt, aes(x = Variable, y = variable, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C") +
    labs(x = "Variables", y = "Category Levels", fill = "Loading", title = "Heatmap of MCA Loadings") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          title = element_text(size = rel(1.4))
    )
})

create_default_density_plots <- memoise(function(data) {
  
    demo_data <- data |> 
      select(Depression_Category, Race, Education, SES) |> 
      drop_na()
    
    result <- MCA(demo_data, graph=FALSE, ncp = 3)
    mca_individuals <- as.data.frame(result$ind$coord[, 1:3])
    mca_individuals$Category <- demo_data$Race
    
    mca_long <- mca_individuals |> 
      pivot_longer(cols = starts_with("Dim"), names_to = "Dimension", values_to = "Score")
    
    plot <- ggplot(mca_long, aes(x = Score, fill = Category)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~ Dimension, scales = "fixed") +
      labs(x = "MCA Score", y = "Density", title = "Density Plot of MCA") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.25)),
            legend.title = element_text(size = rel(1.25)),
            legend.text = element_text(size = rel(1.25))
      )
    
    ggplotly(plot) %>%
      layout(legend = list(title = list(text = "Race")))
  
})

create_mca_cluster <- memoise(function(data, choice) {
    
    demo_data <- data |> 
      select(Depression_Category, Race, Education, SES) |> 
      drop_na() |> 
      select(-Depression_Category)
    
    result <- MCA(demo_data, graph=FALSE, ncp = choice)
    res.hpc2 <- HCPC(result, graph = FALSE)
    
    #great convex hulls
    fviz_cluster(res.hpc2,
                 repel = T,            # Avoid label overlapping
                 geom= "point", #plot only points
                 show.clust.cent = TRUE, # Show cluster centers
                 palette = "npg",         # Color palette see ?ggpubr::ggpar
                 ggtheme = theme_minimal(),
                 main = "MCA Cluster Graph"
    )
  
})

create_pca_cluster <- memoise(function(data_type, data, choice) {

    if (data_type == "Dietary") {
      analysis_data <- data |> 
        select(Depression_Category, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
        drop_na() |> 
        select(-Depression_Category)
      
      ref_dat <- data |> 
        select(Depression_Category, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
        drop_na()
      
    } else if (data_type == "Laboratory") {
      analysis_data <- data |> 
        select(Depression_Category, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
        drop_na() |> 
        select(-Depression_Category)
      
      ref_dat <- data |> 
        select(Depression_Category, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
        drop_na()
    }
    
    scaled_data <- scale(analysis_data) #make the entire dataset on the same scale, subtract mean and divide by SD
    fit <- prcomp(scaled_data, scale=TRUE) #conduct the PCA
    
    comp <- data.frame(fit$x[,1:3])
    comp$type <- ref_dat$Depression_Category
    
    result <- PCA(analysis_data, scale.unit=TRUE, ncp = choice, graph=T)
    res.hpc2 <- HCPC(result, graph =FALSE)
    
    #great convex hulls
    fviz_cluster(res.hpc2,
                 repel = T,            # Avoid label overlapping
                 geom= "point", #plot only points
                 show.clust.cent = TRUE, # Show cluster centers
                 palette = "npg",         # Color palette see ?ggpubr::ggpar
                 ggtheme = theme_minimal(),
                 main = "PCA Cluster Graph"
    )
  
})

create_famd_cluster <- memoise(function(data, choice) {
    
    famd_data <- data |> 
      select(Depression_Category, Race, Education, SES, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
      drop_na() |> 
      select(-Depression_Category)
    
    result <- FAMD(famd_data, graph=FALSE, ncp = choice)
    res.hpc2 <- HCPC(result, graph = FALSE)
    
    #great convex hulls
    fviz_cluster(res.hpc2,
                 repel = T,            # Avoid label overlapping
                 geom= "point", #plot only points
                 show.clust.cent = TRUE, # Show cluster centers
                 palette = "npg",         # Color palette see ?ggpubr::ggpar
                 ggtheme = theme_minimal(),
                 main = "FAMD Cluster Graph"
    )

})

create_mca_plotly <- memoise(function(data, choice, color) {
    
    demo_data <- data |> 
      select(Depression_Category, Race, Education, SES) |> 
      drop_na() |> 
      select(-Depression_Category)
    
    demo_data2 <- data |> 
      select(Depression_Category, Race, Education, SES) |> 
      drop_na()
    
    result <- MCA(demo_data, graph=FALSE, ncp = choice)
    mca_data <- as.data.frame(result$ind$coord[, 1:3])
    res.hpc2 <- HCPC(result, graph = FALSE)
    
    mca_data$Cluster <- as.factor(res.hpc2$data.clust$clust)
    mca_data$Depression_Category <- demo_data2$Depression_Category
    
    plot_ly(data=mca_data, x=~`Dim 1`, y=~`Dim 2`, z=~`Dim 3`, type="scatter3d", mode="markers", color=~get(color), colors = "Spectral")

})

create_pca_plotly <- memoise(function(data_type, data, choice, color) {

    if (data_type == "Dietary") {
      analysis_data <- data |> 
        select(Depression_Category, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
        drop_na() |> 
        select(-Depression_Category)
      
      ref_dat <- data |> 
        select(Depression_Category, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
        drop_na()
      
    } else if (data_type == "Laboratory") {
      analysis_data <- data |> 
        select(Depression_Category, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
        drop_na() |> 
        select(-Depression_Category)
      
      ref_dat <- data |> 
        select(Depression_Category, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
        drop_na()
    }
    
    scaled_data<-scale(analysis_data) #make the entire dataset on the same scale, subtract mean and divide by SD
    fit <- prcomp(scaled_data, scale=TRUE) #conduct the PCA
    
    comp <- data.frame(fit$x[,1:3])
    comp$type<-ref_dat$Depression_Category
    
    
    result <- PCA(analysis_data, scale.unit=TRUE, ncp = choice, graph=FALSE)
    
    res.hpc2 <- HCPC(result, graph =FALSE)
    
    ref_dat$Cluster <- as.factor(res.hpc2$data.clust$clust)
    
    plot_ly(data=ref_dat, x=~comp$PC1, y=~comp$PC2, z=~comp$PC3, type="scatter3d", mode="markers", color=~get(color), colors = "Spectral")

})

create_famd_plotly <- memoise(function(data, choice, color) {

    famd_data <- data |> 
      select(Depression_Category, Race, Education, SES, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
      drop_na() |> 
      select(-Depression_Category)
    
    famd_data2 <- data |> 
      select(Depression_Category, Race, Education, SES, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
      drop_na()
    
    result <- FAMD(famd_data, graph=FALSE, ncp = choice)
    famd_data <- as.data.frame(result$ind$coord[, 1:3])
    res.hpc2 <- HCPC(result, graph = FALSE)
    famd_data$Cluster <- as.factor(res.hpc2$data.clust$clust)
    famd_data$Depression_Category <- famd_data2$Depression_Category
    
    plot_ly(data=famd_data, x=~Dim.1, y=~Dim.2, z=~Dim.3, type="scatter3d", mode="markers", color=~get(color), colors = "Spectral")

})

create_pca_loading_heatmap <- memoise(function(data_type, data) {
  if (data_type == "Dietary") {
    analysis_data <- data |> 
      select(Depression_Category, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
      drop_na() |> 
      select(-Depression_Category)
  } else if (data_type == "Laboratory") {
    analysis_data <- data |> 
      select(Depression_Category, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
      drop_na() |> 
      select(-Depression_Category)
  }
  
  scaled_data <- scale(analysis_data) #make the entire dataset on the same scale, subtract mean and divide by SD
  fit <- prcomp(scaled_data, scale=TRUE) #conduct the PCA
  
  loadings <- as.data.frame(fit$rotation[, 1:3])
  loadings$Variable <- rownames(loadings)
  loadings_melt <- melt(loadings, id = "Variable")
  
  # Plot heatmap
  ggplot(loadings_melt, aes(x = Variable, y = variable, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C") +
    labs(x = "Variables", y = "Principal Components", fill = "Loadings", title = "Heatmap of PCA Loadings") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          title = element_text(size = rel(1.4))
    )
  
})

create_mca_loading_heatmap <- memoise(function(data, choice) {
  demo_data <- data |> 
    select(Depression_Category, Race, Education, SES) |> 
    drop_na() |> 
    select(-Depression_Category)
  
  result <- MCA(demo_data, graph=FALSE, ncp = choice)
  loadings <- as.data.frame(result$var$coord[, 1:3])
  loadings$Variable <- rownames(loadings)
  loadings_melt <- melt(loadings, id = "Variable")
  
  ggplot(loadings_melt, aes(x = Variable, y = variable, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C") +
    labs(x = "Dimension 1", y = "Category Levels", fill = "Loading", title = "Heatmap of MCA Loadings") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          title = element_text(size = rel(1.4))
    )
})

create_famd_loading_heatmap <- memoise(function(data, choice) {
  famd_data <- data |> 
    select(Depression_Category, Race, Education, SES, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
    drop_na() |> 
    select(-Depression_Category)
  
  result <- FAMD(famd_data, graph=FALSE, ncp = choice)
  loadings <- as.data.frame(result$var$coord[, 1:3])
  loadings$Variable <- rownames(loadings)
  loadings_melt <- melt(loadings, id = "Variable")
  
  ggplot(loadings_melt, aes(x = Variable, y = variable, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C") +
    labs(x = "Dimension 1", y = "Category Levels", fill = "Loading", title = "Heatmap of FAMD Loadings") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          title = element_text(size = rel(1.4))
    )
})

create_pca_density_plots <- memoise(function(data_type, data) {
    
    if (data_type == "Dietary") {
      analysis_data <- data |> 
        select(Depression_Category, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
        drop_na() |> 
        select(-Depression_Category)
    } else if (data_type == "Laboratory") {
      analysis_data <- data |> 
        select(Depression_Category, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
        drop_na() |> 
        select(-Depression_Category)
    }
    
    scaled_data <- scale(analysis_data) #make the entire dataset on the same scale, subtract mean and divide by SD
    fit <- prcomp(scaled_data, scale=TRUE) #conduct the PCA
    
    # Get scores for the first 3 components
    scores <- as.data.frame(fit$x[, 1:3])
    
    # Reshape the scores data to long format for plotting
    scores_long <- melt(scores, variable.name = "Component", value.name = "Score")
    
    # Plot density for each component
    ggplot(scores_long, aes(x = Score, fill = Component)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~ Component, scales = "fixed") +
      labs(x = "Principal Component Score", y = "Density") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.25)),
            legend.title = element_text(size = rel(1.25)),
            legend.text = element_text(size = rel(1.25))
      ) +
      ggtitle("Density Plot of Principal Components")
  
})

create_mca_density_plots <- memoise(function(data, choice, density_var) {
    
    demo_data <- data |> 
      select(Depression_Category, Race, Education, SES) |> 
      drop_na()
    
    result <- MCA(demo_data, graph=FALSE, ncp = choice)
    mca_individuals <- as.data.frame(result$ind$coord[, 1:3])
    mca_individuals$Category <- demo_data |> pull(density_var)
    
    mca_long <- mca_individuals |> 
      pivot_longer(cols = starts_with("Dim"), names_to = "Dimension", values_to = "Score")
    
    plot <- ggplot(mca_long, aes(x = Score, fill = Category)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~ Dimension, scales = "fixed") +
      labs(x = "MCA Score", y = "Density") +
      theme_minimal() +
      ggtitle("Density Plot of MCA") +
      theme(axis.text.y = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.25)),
            legend.title = element_text(size = rel(1.25)),
            legend.text = element_text(size = rel(1.25))
      )
    
    ggplotly(plot) %>%
      layout(legend = list(title = list(text = density_var)))
  
})

create_famd_density_plots <- memoise(function(data, choice, density_var) {

    famd_data <- data |> 
      select(Depression_Category, Race, Education, SES, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids, Protein, TotalCalories, Fiber, Cholesterol, Sodium) |> 
      drop_na() |> 
      select(-Depression_Category)
    
    result <- FAMD(famd_data, graph=FALSE, ncp = 5)
    famd_individuals <- as.data.frame(result$ind$coord[, 1:3])
    famd_individuals$Category <- famd_data |> pull(density_var)
    
    famd_long <- famd_individuals |> 
      pivot_longer(cols = starts_with("Dim"), names_to = "Dimension", values_to = "Score")
    
    plot <- ggplot(famd_long, aes(x = Score, fill = Category)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~ Dimension, scales = "fixed") +
      labs(x = "FAMD Score", y = "Density") +
      theme_minimal() +
      ggtitle("Density Plot of FAMD") +
      theme(axis.text.y = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.25)),
            legend.title = element_text(size = rel(1.25)),
            legend.text = element_text(size = rel(1.25))
      )
    
    ggplotly(plot) %>%
      layout(legend = list(title = list(text = density_var)))
  
})

#############################################################################################

default_develop_prediction_model <- memoise(function(data) {
  
  # Select features and target
  data_split <- data |> 
    select(Total_Depression_Score, Race, Education, SES, Gender, Age, Protein, TotalCalories, Fiber, Cholesterol, Sodium, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
    filter(!is.na(Total_Depression_Score)) |> 
    initial_split(prop = 0.75)
  
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Recipe for pre-processing with imputation
  recipe <- recipe(Total_Depression_Score ~ ., data = train_data) |> 
    step_dummy(all_nominal_predictors()) |>  # Convert categorical variables to dummies
    step_normalize(all_numeric_predictors()) |> # Normalize numeric predictors
    step_impute_median(all_numeric_predictors()) |> # Impute missing numeric values
    step_impute_mode(all_nominal_predictors())   # Impute missing categorical values
  
  # Build the model
  lm_model <- linear_reg() |> 
    set_engine("lm") |> 
    set_mode("regression")
  
  # Workflow
  lm_workflow <- workflow() |> 
    add_recipe(recipe) |> 
    add_model(lm_model)
  
  # Fit the Model
  lm_fit <- lm_workflow |> 
    fit(data = train_data)
  
  # Return the fitted model
  lm_fit
  
})


develop_prediction_model <- memoise(function(data, model_type) {
  
  # Select features and target
  data_split <- data |> 
    select(Total_Depression_Score, Race, Education, SES, Gender, Age, Protein, TotalCalories, Fiber, Cholesterol, Sodium, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
    filter(!is.na(Total_Depression_Score)) |> 
    initial_split(prop = 0.75)
  
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Recipe for pre-processing with imputation
  recipe <- recipe(Total_Depression_Score ~ ., data = train_data) |> 
    step_dummy(all_nominal_predictors()) |>  # Convert categorical variables to dummies
    step_normalize(all_numeric_predictors()) |> # Normalize numeric predictors
    step_impute_median(all_numeric_predictors()) |> # Impute missing numeric values
    step_impute_mode(all_nominal_predictors())   # Impute missing categorical values
  
  lm_model <- switch(
    model_type,
    "Linear Model" = linear_reg() |> 
      set_engine("lm") |> 
      set_mode("regression"),
    "Random Forest" = rand_forest() |> 
      set_engine("ranger") |> 
      set_mode("regression"),
    "Decision Tree" = decision_tree(cost_complexity = 0.001) |> 
      set_engine("rpart") |> 
      set_mode("regression"),
    "Support Vector Machine" = svm_linear() |> 
      set_engine("kernlab") |> 
      set_mode("regression"),
    "Gradient Boosting Machine" = boost_tree() |> 
      set_engine("xgboost") |> 
      set_mode("regression"),
    "Neural Network" = mlp() |> 
      set_engine("nnet") |> 
      set_mode("regression")
  )
  
  # Workflow
  lm_workflow <- workflow() |> 
    add_recipe(recipe) |> 
    add_model(lm_model)
  
  # Fit the Model
  lm_fit <- lm_workflow |> 
    fit(data = train_data)
  
  # Return the fitted model
  lm_fit

})

create_residual_data <- memoise(function(data) {
  
  models <- list(
    "Linear Model" = develop_prediction_model(data, "Linear Model"),
    "Random Forest" = develop_prediction_model(data, "Random Forest"),
    "Decision Tree" = develop_prediction_model(data, "Decision Tree"),
    "Support Vector Machine" = develop_prediction_model(data, "Support Vector Machine"),
    "Gradient Boosting Machine" = develop_prediction_model(data, "Gradient Boosting Machine"),
    "Neural Network" = develop_prediction_model(data, "Neural Network")
  )
  
  # Select features and target
  data_split <- data |> 
    select(Total_Depression_Score, Race, Education, SES, Gender, Age, Protein, TotalCalories, Fiber, Cholesterol, Sodium, Vitamin_D, White_Blood_Cells, Glucose, GLA_Omega_6, Copper, Saturated_Fatty_Acids) |> 
    filter(!is.na(Total_Depression_Score)) |> 
    initial_split(prop = 0.75)
  
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  residuals_data <- map_df(names(models), function(model_name) {
    model <- models[[model_name]]
    
    # Try to generate predictions and residuals, with the if-else logic for missing .resid column
    predictions <- augment(model, test_data)
    
    # Check if .resid column exists, and calculate residuals accordingly
    if (".resid" %in% colnames(predictions)) {
      residuals <- predictions$.resid
    } else {
      residuals <- predictions$Total_Depression_Score - predictions$.pred
    }
    
    # Create a data frame with residuals and model name
    data.frame(
      Model = model_name,
      Predicted = predictions$.pred,
      Residual = residuals
    )
    
  })
  
  residuals_data
  
})

default_create_model_performance_plot <- memoise(function(data, model) {
  predictions <- augment(model, data)  # add predictions and residuals
  
  ggplot(predictions, aes(.pred, .resid)) +
    geom_point(alpha = 0.8, color = "dodgerblue2", shape = 18) +
    geom_smooth(se = FALSE, color = "red3") +
    labs(
      x = "Fitted Values",
      y = "Residuals",
      title = "Residual Plot"
    ) +
    theme_classic() +
    theme(
      title = element_text(size = rel(1.5)),
      axis.text.x = element_text(size = rel(1.5)),
      axis.text.y = element_text(size = rel(1.5)),
      axis.title = element_text(size = rel(1.5)),
      legend.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1.25))
    )
})

create_model_performance_plot <- memoise(function(data, model) {
  predictions <- augment(model, data)  # add predictions and residuals
  
  if ("resid" %in% colnames(predictions)) {
    ggplot(predictions, aes(.pred, .resid)) +
      geom_point(alpha = 0.8, color = "dodgerblue2", shape = 18) +
      geom_smooth(se = FALSE, color = "red3") +
      labs(
        x = "Fitted Values",
        y = "Residuals",
        title = "Residual Plot"
      ) +
      theme_classic() +
      theme(
        title = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.25))
      )
  } else {
  ggplot(predictions, aes(.pred, Total_Depression_Score - .pred)) +
    geom_point(alpha = 0.8, color = "dodgerblue2", shape = 18) +
    geom_smooth(se = FALSE, color = "red3") +
    labs(
      x = "Fitted Values",
      y = "Residuals",
      title = "Residual Plot"
      ) +
    theme_classic() +
    theme(
      title = element_text(size = rel(1.5)),
      axis.text.x = element_text(size = rel(1.5)),
      axis.text.y = element_text(size = rel(1.5)),
      axis.title = element_text(size = rel(1.5)),
      legend.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1.25))
    )
  }
})

create_predicted_vs_actual_plot <- memoise(function(data, model) {
  predictions <- augment(model, data)
  
  ggplot(predictions, aes(.pred, Total_Depression_Score)) +
    geom_point(alpha = 0.8, color = "darkgreen", shape = 18) +
    geom_abline(slope = 1, intercept = 0, color = "magenta", linetype = "dashed") +
    labs(
      x = "Predicted Score",
      y = "Actual Score",
      title = "Predicted vs Actual Depression Scores"
      ) +
    theme_classic() +
    theme(
      title = element_text(size = rel(1.5)),
      axis.text.x = element_text(size = rel(1.5)),
      axis.text.y = element_text(size = rel(1.5)),
      axis.title = element_text(size = rel(1.5)),
      legend.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1.25))
    )
})

create_overlapping_residual_plot <- memoise(function(residuals_data) {
  r <- ggplot(residuals_data, aes(x = Predicted, y = Residual, color = Model)) +
    geom_point(alpha = 0.5) +
    scale_color_viridis_d() +
    geom_smooth(se = FALSE, linetype = "dashed") +
    labs(title = "Overlapping Residual Plot for Model Comparison",
         x = "Predicted Values",
         y = "Residuals") +
    theme_minimal()
  
  ggplotly(r) %>%
    layout(legend = list(title = list(text = "Model")))
})

create_facet_wrap_overlapping_residual_plot <- memoise(function(residuals_data) {
  ggplot(residuals_data, aes(x = Predicted, y = Residual, color = Model)) +
    geom_point(alpha = 0.2, size = 1.5) +
    geom_smooth(se = FALSE, size = 1.2) +
    scale_color_viridis_d() +
    labs(title = "Overlapping Residual Plot for Model Comparison",
         x = "Predicted Values",
         y = "Residuals") +
    facet_wrap(~ Model, scales = "free_y", ncol = 3) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)
    )
})

### LISTS ###

demo_formula <- Total_Depression_Score ~ Race + Education + SES + Gender + Age
diet_formula <- Total_Depression_Score ~ Protein + Fiber + Cholesterol + Sodium + TotalCalories
lab_formula <- Total_Depression_Score ~ Vitamin_D + White_Blood_Cells + Glucose + GLA_Omega_6 + Copper + Saturated_Fatty_Acids

demo_levels <- c("RaceNH White", "RaceMexican American", "RaceNH Black", "RaceNH Asian", 
                 "RaceOther Race including Multi-Racial", "SESHigh SES", 
                 "SESLow SES", "Education9-11th grade", "EducationHigh school graduate/GED or equivalent", 
                 "EducationSome college or AA degree", "EducationCollege graduate or above", "(Intercept)")

diet_levels <- c("Sodium", "Protein", "Fiber", 
                 "Cholesterol", "TotalCalories", "(Intercept)")

lab_levels <- c("Vitamin_D", "White_Blood_Cells", "Glucose", 
                 "GLA_Omega_6", "Copper", "Saturated_Fatty_Acids", "(Intercept)")

default_test_data <- tibble::tibble(
  Race = factor("Mexican American", levels = c("Mexican American", "Other Hispanic", "NH White", "NH Black", "NH Asian", "Other Race including Multi-Racial")),
  Education = factor("Less than 9th grade", levels = c("Less than 9th grade", "9-11th grade", "High school graduate/GED or equivalent", "Some college or AA degree", "College graduate or above")),
  SES = factor("Low SES", levels = c("Low SES", "Middle SES", "High SES")),
  Gender = factor("Male", levels = c("Male", "Female")),
  Age = factor("Twenties", levels = c("Twenties", "Thirties", "Fourties", "Fifties", "Sixties", "Seventies and 80")),
  Protein = 70,
  TotalCalories = 2000,
  Fiber = 15,
  Cholesterol = 15,
  Sodium = 3000,
  Vitamin_D = 60,
  White_Blood_Cells = 8,
  Glucose = 100,
  GLA_Omega_6 = 50,
  Copper = 125,
  Saturated_Fatty_Acids = 2500
)
