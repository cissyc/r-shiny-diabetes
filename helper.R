#' Cissy Chan
#' helper file for rshiny dashboard, pima indians diabetes analysis
#' November 2016

library(mlbench)
library(stats)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(ROCR)
library(dplyr)

# - get data
data(PimaIndiansDiabetes)
df_data <- data.frame(PimaIndiansDiabetes)
levels(df_data$diabetes) <- c("negative", "positive")

set.seed(3)
sample_index <- sample(1:nrow(df_data), 0.1*nrow(df_data), FALSE)
df_test <- df_data[sample_index, ]
df_train <- df_data[-sample_index, ]

# - description of each attribute
data_attribute_desc <- list(
  "pregnant" = "Number of times pregnant",
  "glucose" = "Plasma glucose concentration",
  "pressure" = "Diastolic blood pressure",
  "triceps" = "Triceps skin fold thickness",
  "insulin" = "2-Hour serum insulin",
  "mass" = "Body mass index",
  "pedigree" = "Diabetes pedigree function",
  "age" = "Age"
)
# - unit of measurement in each attribute
data_attribute_unit <- list(
  "pregnant" = "number of times",
  "glucose" = "glucose concentration",
  "pressure" = "mm Hg",
  "triceps" = "mm",
  "insulin" = "mu U/ml",
  "mass" = "weight in kg/(height in m)^2",
  "pedigree" = "pedigree function",
  "age" = "years"
)

# - data imputation
pressure_model <- stats::glm(pressure ~ pregnant + glucose + pedigree + age, data = df_data[df_data$pressure != 0, ])
pressure_predict <- stats::predict(pressure_model, newdata = df_data[, c("pregnant", "glucose", "pedigree", "age")], type = "response")

triceps_model <- stats::glm(triceps ~ pregnant + glucose + pedigree + age, data = df_data[df_data$triceps != 0, ])
triceps_predict <- stats::predict(triceps_model, newdata = df_data[, c("pregnant", "glucose", "pedigree", "age")], type = "response")

mass_model <- stats::glm(mass ~ pregnant + glucose + pedigree + age, data = df_train[df_train$mass != 0, ])
mass_predict <- stats::predict(mass_model, newdata = df_data[, c("pregnant", "glucose", "pedigree", "age")], type = "response")

# - save the old data set for comparison
df_data_old <- df_data

df_data <- cbind(df_data, pressure_predict, triceps_predict, mass_predict) %>%
  dplyr::mutate(
    pressure = ifelse(pressure == 0, pressure_predict, pressure),
    triceps = ifelse(triceps == 0, triceps_predict, triceps),
    mass = ifelse(mass == 0, mass_predict, mass)
  ) %>%
  dplyr::select(
    -pressure_predict,
    -triceps_predict,
    -mass_predict
  )

# - melt into required format for facet distribution plots, old and new
df_melted_old <- reshape2::melt(df_data_old, id.vars = "diabetes")
df_melted <- reshape2::melt(df_data, id.vars = "diabetes")


#' box chart
#' 
#' 
get_summary_plot_box <- function(attribute) {
  
  # - make sure attribute passed is actually in the data
  supported_attributes <- colnames(df_data)
  if (!(attribute %in% supported_attributes)) { stop(paste0("get_summary_plot_box: invalid attribute")) }
  
  # - define collapsible box with plots
  out_box <- shinydashboard::box(
    # - add a little CSS to make the title font smaller
    title = tags$p(style = "font-size: 16px;", data_attribute_desc[[attribute]]),
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 4,
    status = "primary",
    plotOutput(paste0("summary_plot_", attribute), height = 200)
  )
  
  # - return to ui
  return(out_box)
  
}


#' get_summary_output
#' 
#' 
get_summary_output <- function(input, type, attribute) {
  
  # - make sure parameters passed are valie
  supported_types <- c("plot")
  supported_attributes <- colnames(df_data)
  if (!(type %in% supported_types)) { stop(paste0("get_summary_output: invalid type")) }
  if (!(attribute %in% supported_attributes)) { stop(paste0("get_summary_output: invalid attribute")) }
  
  if (type == "plot") {
    
    # - set up new input data
    new_data <- data.frame(
      pregnant = input$pregnant,
      glucose = input$glucose,
      pressure = input$pressure,
      triceps = input$triceps,
      insulin = input$insulin,
      mass = input$mass,
      pedigree = input$pedigree,
      age = input$age
    )
    
    new_data <- data.frame(new_values = t(new_data))
    new_data$variable <- rownames(new_data)
    
    new_data_merged <- base::merge(df_melted, new_data, by = "variable") 
    
    # - plot density of existing data set in a facet wrap, overlay vline to mark new input data
    #    out_plot <- ggplot(data = new_data_merged,
    #                   aes(x = value,
    #                       group = diabetes,
    #                       colour = diabetes)) + 
    #      geom_density() + 
    #      facet_wrap(~ variable, scales = "free") +
    #      geom_vline(aes(xintercept = new_values)) +
    #      scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "black")) +
    #      theme(
    #        plot.background = element_blank(),
    #        legend.background = element_blank())
    
    # - plot density of existing data set individually given attribute, overlay vline to mark new input data
    out_plot <- ggplot(data = dplyr::filter(new_data_merged, variable == attribute),
                       aes(x = value,
                           group = diabetes,
                           colour = diabetes)) + 
      geom_density() +
      geom_vline(aes(xintercept = new_values)) +
      xlab(label = data_attribute_unit[[attribute]]) +
      scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "black")) +
      theme(
        plot.background = element_blank(),
        legend.position = "none")
    
    # - return output to server
    return(out_plot)
  }
  
}

#' get_logit_output
#' 
#' 
get_logit_output <- function(input, type) {
  
  # - get logit coefficients; perform model selection by minimising the akaike information criteria (AIC)
  logit_model <- stats::glm(diabetes ~ .*., data = df_data, family = "binomial")
  logit_model_AIC <- MASS::stepAIC(logit_model, direction = "backward", trace = 0)
  
  # - set up new input data
  new_data <- data.frame(
    pregnant = input$pregnant,
    glucose = input$glucose,
    pressure = input$pressure,
    triceps = input$triceps,
    insulin = input$insulin,
    mass = input$mass,
    pedigree = input$pedigree,
    age = input$age
  )
  
  # - get regression classification, use model after minimising AIC
  logit_predict <- stats::predict(logit_model_AIC, newdata = new_data, type = "response")
  
  # - if retrieving probability of diabetes, return text
  if (type == "probability") {
    
    logit_predict_rounded <- round(logit_predict * 100, 1)
    
    # - return text output to server
    #return(paste0("Probability of diabetes: ", logit_predict_rounded, "% \n"))
    return(logit_predict_rounded)
    
  } else if (type == "plot") {
    
    # - get cut-off point
    cutoff_optimal <- logit_diagnostics$new_cutoff
    
    # - else if want to retrieve plot, return scatter plot given two input variables to compare
    logit_predict_out <- as.factor(ifelse(logit_predict <- cutoff_optimal, "predicted_negative", "predicted_positive"))
    dt_logit_predict <- rbind(df_data, data.frame(new_data, diabetes = logit_predict_out))
    
    # - plot data and prediction
    out_plot <- ggplot(data = dt_logit_predict, 
                       aes(x = get(input$x_var),
                           y = get(input$y_var),
                           shape = diabetes,
                           size = diabetes,
                           colour = diabetes)) +
      xlab(label = input$x_var) +
      ylab(label = input$y_var) + 
      geom_point() +
      scale_shape_manual(values = c(1, 1, 17, 17)) +
      scale_size_manual(values = c(2, 2, 4, 4)) +
      scale_colour_manual(name = "diabetes", values = c("chartreuse3", "red", "chartreuse4", "red1")) +
      theme(
        plot.background = element_blank(),
        legend.background = element_blank())
    
    
    # - return plot to server
    return(out_plot)
    
  } else if (type == "model_coefficients") {
    
    model_coefficients <- data.frame(
      VARIABLE = names(logit_model_AIC$coefficients),
      COEFFICIENT = logit_model_AIC$coefficients,
      row.names = NULL)
    
    return(model_coefficients)
  }
  
}

# - initialise model diagnostics list
logit_diagnostics <- list()

# - out-of-sample prediction
logit_model_train <- stats::glm(diabetes ~ .*., data = df_train, family = "binomial")
logit_model_train_AIC <- MASS::stepAIC(logit_model_train, direction = "backward", trace = 0)
logit_predict_train <- stats::predict(logit_model_train_AIC, newdata = df_test[, 1:8], type = "response")
logit_predict_train_factor <- as.factor(ifelse(round(logit_predict_train, 0) == 0, "predicted_negative", "predicted_positive"))


df_melted_old <- df_melted_old %>%
  dplyr::filter(
    variable %in% c("pressure", "triceps", "mass")
  ) %>%
  dplyr::mutate(
    variable = paste0(variable, "_old")
  )

df_melted_new <- df_melted %>%
  dplyr::filter(
    variable %in% c("pressure", "triceps", "mass")
  ) %>%
  dplyr::mutate(
    variable = paste0(variable, "_new")
  )

df_melted_comparison <- rbind(df_melted_old, df_melted_new)
order <- unique(df_melted_comparison$variable)
df_melted_comparison$variable <- factor(df_melted_comparison$variable, levels=order)

# - save
logit_diagnostics$imputation_comparison <- ggplot(
  data = df_melted_comparison,
  aes(x = value,
      group = diabetes,
      colour = diabetes)) + 
  geom_density() + 
  facet_wrap(~ variable, scales = "free", nrow = 2) +
  scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "black")) +
  theme(
    plot.background = element_blank(),
    legend.background = element_blank())

# - save
logit_diagnostics$confusion_matrix <- as.data.frame.matrix(table(
  model_prediction = logit_predict_train_factor,
  actual_class = df_test$diabetes
))

# - plot ROC curve for false positive rate vs true positive rate
rocr_predict <- ROCR::prediction(logit_predict_train, df_test[, 9]=="positive")
roc_perf <- ROCR::performance(rocr_predict, measure = "tpr", x.measure = "fpr")
roc_perf_data <- data.frame(x = roc_perf@x.values[[1]], y = roc_perf@y.values[[1]])

auc_perf <- ROCR::performance(rocr_predict, measure = "auc")
auc <- auc_perf@y.values[[1]]

# - save
logit_diagnostics$ROC <- ggplot(
  data = roc_perf_data, aes(x = x, y = y)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "ROC Curve") +
  xlab(label = "False Positive Rate") +
  ylab(label = "True Positive Rate") +
  geom_text(aes(x = 1, y = 0, hjust = 1, vjust = 0, label = paste(sep = "", "AUC = ", round(auc, 4))), 
            colour="black", size = 4) +
  coord_fixed() +
  theme(
    plot.background = element_blank(),
    legend.background = element_blank())

# - give a cost and find optimal cut-off point
cost_perf <- ROCR::performance(rocr_predict, measure = "cost", cost.fp = 1, cost.fn = 3)
cutoff_optimal <- cost_perf@x.values[[1]][which.min(cost_perf@y.values[[1]])]

# - save
logit_diagnostics$new_cutoff <- cutoff_optimal

# - confusion matrix with new cut-off point
logit_predict_train_factor_new <- ifelse(logit_predict_train <= cutoff_optimal, "predicted_negative", "predicted_positive")

# - save
logit_diagnostics$confusion_matrix_new_cutoff <- as.data.frame.matrix(table(
  model_prediction = logit_predict_train_factor_new,
  actual_class = df_test$diabetes
))












