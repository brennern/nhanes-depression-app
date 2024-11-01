### FUNCTIONS ###

create_boxplot <- function(data, data_type, plot_var, comparisons) {
  if (data_type == "Demographic") {
    ggboxplot(data,
              x = plot_var,
              y = "dep_total_score",
              add = "jitter",
              color = plot_var,
              add.params = list(alpha = 0.2)) +
      stat_compare_means(method = "t.test",
                         comparisons = comparisons) +
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
      stat_compare_means(method = "t.test",
                         comparisons = comparisons) +
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
             y = "dep_total_score",
             add = "jitter",
             color = plot_var,
             add.params = list(alpha = 0.2)) +
      stat_compare_means(method = "t.test",
                         comparisons = comparisons) +
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
      stat_compare_means(method = "t.test",
                         comparisons = comparisons) +
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
    summarize(dep_total_score = mean(dep_total_score, na.rm = TRUE), .groups = "drop")
  
  ggplot(agg_data, aes_string(x = plot_var, y = plot_var2, fill = "dep_total_score")) +
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

generate_factor_levels_demo <- function(data, ref_race, ref_ses, ref_education, ref_gender, ref_age) {
  levels_race <- paste0("Race", c(ref_race, setdiff(levels(data$Race), ref_race)))
  levels_ses <- paste0("SES", c(ref_ses, setdiff(levels(data$SES), ref_ses)))
  levels_education <- paste0("Education", c(ref_education, setdiff(levels(data$Education), ref_education)))
  levels_gender <- paste0("Gender", c(ref_gender, setdiff(levels(data$Gender), ref_gender)))
  levels_age <- paste0("Age", c(ref_age, setdiff(levels(data$Age), ref_age)))
  
  
  factor_levels <- c(levels_race, levels_ses, levels_education, levels_gender, levels_age, "(Intercept)")
  
  return(factor_levels)
}

create_mca_cluster <- function(data, choice) {
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
               main = "Factor map"
  )
}

create_pca_cluster <- function(data, choice) {
  diet_data <- data |> 
    select(Depression_Category, dr1tprot, dr1tsugr, dr1tcarb, dr1tfibe, dr1tchol, dr1tsodi, dr1tmois, dr1tkcal, dr1tsfat, dr1tmfat, dr1tpfat, dr1talco, dr1ts100) |> 
    drop_na() |> 
    select(-Depression_Category)
  
  dat2 <- data |> 
    select(Depression_Category, dr1tprot, dr1tsugr, dr1tcarb, dr1tfibe, dr1tchol, dr1tsodi, dr1tmois, dr1tkcal, dr1tsfat, dr1tmfat, dr1tpfat, dr1talco, dr1ts100) |> 
    drop_na()
  
  scaled_data <- scale(diet_data) #make the entire dataset on the same scale, subtract mean and divide by SD
  fit <- prcomp(scaled_data, scale=TRUE) #conduct the PCA
  
  comp <- data.frame(fit$x[,1:3])
  comp$type <- dat2$Depression_Category
  
  result <- PCA(diet_data, scale.unit=TRUE, ncp = choice, graph=T)
  res.hpc2 <- HCPC(result, graph =FALSE)
  
  #great convex hulls
  fviz_cluster(res.hpc2,
               repel = T,            # Avoid label overlapping
               geom= "point", #plot only points
               show.clust.cent = TRUE, # Show cluster centers
               palette = "npg",         # Color palette see ?ggpubr::ggpar
               ggtheme = theme_minimal(),
               main = "Factor map"
  )
}

create_famd_cluster <- function(data, choice) {
  famd_data <- data |> 
    select(Depression_Category, Race, Education, SES, lbxvidms, lbxwbcsi, lbxglu, lbxgla, lbxscu, lbxpm1, dr1tprot, dr1tsugr, dr1tcarb, dr1tfibe, dr1tchol, dr1tsodi, dr1tmois, dr1tkcal, dr1tsfat, dr1tmfat, dr1tpfat, dr1talco, dr1ts100) |> 
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
               main = "Factor map"
  )
}

create_mca_plotly <- function(data, choice, color) {
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
  
  plot_ly(data=mca_data, x=~`Dim 1`, y=~`Dim 2`, z=~`Dim 3`, type="scatter3d", mode="markers", color=~get(color))
}

create_pca_plotly <- function(data, choice, color) {
  diet_data <- data |> 
    select(Depression_Category, dr1tprot, dr1tsugr, dr1tcarb, dr1tfibe, dr1tchol, dr1tsodi, dr1tmois, dr1tkcal, dr1tsfat, dr1tmfat, dr1tpfat, dr1talco, dr1ts100) |> 
    drop_na() |> 
    select(-Depression_Category)
  
  dat2 <- data |> 
    select(Depression_Category, dr1tprot, dr1tsugr, dr1tcarb, dr1tfibe, dr1tchol, dr1tsodi, dr1tmois, dr1tkcal, dr1tsfat, dr1tmfat, dr1tpfat, dr1talco, dr1ts100) |> 
    drop_na()
  
  scaled_data<-scale(diet_data) #make the entire dataset on the same scale, subtract mean and divide by SD
  fit <- prcomp(scaled_data, scale=TRUE) #conduct the PCA
  
  comp <- data.frame(fit$x[,1:3])
  comp$type<-dat2$Depression_Category
  
  
  result <- PCA(diet_data, scale.unit=TRUE, ncp = choice, graph=FALSE)
  
  res.hpc2 <- HCPC(result, graph =FALSE)
  
  dat2$Cluster <- as.factor(res.hpc2$data.clust$clust)
  
  plot_ly(data=dat2, x=~comp$PC1, y=~comp$PC2, z=~comp$PC3, type="scatter3d", mode="markers", color=~get(color))
}

create_famd_plotly <- function(data, choice, color) {
  famd_data <- data |> 
    select(Depression_Category, Race, Education, SES, lbxvidms, lbxwbcsi, lbxglu, lbxgla, lbxscu, lbxpm1, dr1tprot, dr1tsugr, dr1tcarb, dr1tfibe, dr1tchol, dr1tsodi, dr1tmois, dr1tkcal, dr1tsfat, dr1tmfat, dr1tpfat, dr1talco, dr1ts100) |> 
    drop_na() |> 
    select(-Depression_Category)
  
  famd_data2 <- data |> 
    select(Depression_Category, Race, Education, SES, lbxvidms, lbxwbcsi, lbxglu, lbxgla, lbxscu, lbxpm1, dr1tprot, dr1tsugr, dr1tcarb, dr1tfibe, dr1tchol, dr1tsodi, dr1tmois, dr1tkcal, dr1tsfat, dr1tmfat, dr1tpfat, dr1talco, dr1ts100) |> 
    drop_na()
  
  result <- FAMD(famd_data, graph=FALSE, ncp = choice)
  famd_data <- as.data.frame(result$ind$coord[, 1:3])
  res.hpc2 <- HCPC(result, graph = FALSE)
  famd_data$Cluster <- as.factor(res.hpc2$data.clust$clust)
  famd_data$Depression_Category <- famd_data2$Depression_Category
  
  plot_ly(data=famd_data, x=~Dim.1, y=~Dim.2, z=~Dim.3, type="scatter3d", mode="markers", color=~get(color))
}

develop_prediction_model <- function(data) {
  # Select features and target
  data_split <- data |> 
    select(dep_total_score, Race, Education, SES) |> 
    initial_split(prop = 0.75)
  
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Recipe for pre-processing with imputation
  recipe <- recipe(dep_total_score ~ ., data = train_data) |> 
    step_dummy(all_nominal_predictors()) |>  # Convert categorical variables to dummies
    step_normalize(all_numeric_predictors())  # Normalize numeric predictors
  
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

}

### COMPARISONS AND LISTS ###
race_comparisons = list(
  c("Mexican American", "NH Asian"),
  c("Other Hispanic", "NH Asian"),
  c("NH White", "NH Asian"),
  c("NH Black", "NH Asian"),
  c("NH Asian", "Other Race including Multi-Racial")
)

education_comparisons = list(
  c("Less than 9th grade", "High school graduate/GED or equivalent"),
  c("Less than 9th grade", "Some college or AA degree"),
  c("Less than 9th grade", "College graduate or above"),
  c("9-11th grade", "High school graduate/GED or equivalent"),
  c("9-11th grade", "Some college or AA degree"),
  c("9-11th grade", "College graduate or above"),
  c("High school graduate/GED or equivalent", "College graduate or above"),
  c("Some college or AA degree", "College graduate or above")
)

ses_comparisons = list(
  c("Low SES", "Middle SES"),
  c("Low SES", "High SES"),
  c("Middle SES", "High SES")
)

gender_comparisons = list(
  c("Male", "Female")
)

age_comparisons = list(
  c("Twenties", "Thirties"),
  c("Twenties", "Fourties"),
  c("Twenties", "Fifties"),
  c("Twenties", "Sixties"),
  c("Twenties", "Seventies and 80")
)

depression_comparisons = list(
  c("None or Minimal Depression", "Mild Depression"),
  c("None or Minimal Depression", "Moderate Depression"),
  c("None or Minimal Depression", "Moderately Severe Depression"),
  c("None or Minimal Depression", "Severe Depression")
)

demo_formula <- dep_total_score ~ Race + Education + SES + Gender + Age
diet_formula <- dep_total_score ~ Protein + Fiber + Cholesterol + Sodium + TotalCalories

demo_levels <- c("RaceNH White", "RaceMexican American", "RaceNH Black", "RaceNH Asian", 
                 "RaceOther Race including Multi-Racial", "SESHigh SES", 
                 "SESLow SES", "Education9-11th grade", "EducationHigh school graduate/GED or equivalent", 
                 "EducationSome college or AA degree", "EducationCollege graduate or above", "(Intercept)")

diet_levels <- c("Sodium", "Protein", "Fiber", 
                 "Cholesterol", "TotalCalories", "(Intercept)")


