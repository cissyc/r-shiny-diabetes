#' Cissy Chan
#' server file for rshiny dashboard, pima indians diabetes analysis
#' November 2016

library(shiny)
library(markdown)
library(rmarkdown)
library(knitr)

# - source helper file that generate plots and outputs
source("helper.R")

function(input, output) {
  
  # - summary plots comparing input attributes with the rest of data set
  # - can probably put this into a function??
  output$summary_plot_pregnant <- renderPlot({
    get_summary_output(input, type="plot", attribute="pregnant")
  }, bg="transparent")
  
  output$summary_plot_glucose <- renderPlot({
    get_summary_output(input, type="plot", attribute="glucose")
  }, bg="transparent")
  
  output$summary_plot_pressure <- renderPlot({
    get_summary_output(input, type="plot", attribute="pressure")
  }, bg="transparent")
  
  output$summary_plot_triceps <- renderPlot({
    get_summary_output(input, type="plot", attribute="triceps")
  }, bg="transparent")
  
  output$summary_plot_insulin <- renderPlot({
    get_summary_output(input, type="plot", attribute="insulin")
  }, bg="transparent")
  
  output$summary_plot_mass <- renderPlot({
    get_summary_output(input, type="plot", attribute="mass")
  }, bg="transparent")
  
  output$summary_plot_pedigree <- renderPlot({
    get_summary_output(input, type="plot", attribute="pedigree")
  }, bg="transparent")
  
  output$summary_plot_age <- renderPlot({
    get_summary_output(input, type="plot", attribute="age")
  }, bg="transparent")
  
  # - output text showing predicted probability of diabetes given input data
  output$logit_probability <- renderText({
    paste0("Based on binomial logistic regression of the data set, ",
           "a new entry with the given input attributes is estimated to have ",
           get_logit_output(input, type="probability"), "% probability of a positive ",
           "diabetes outcome.")
  })
  
  # - output scatter plot comparing two attributes, diabetes outcome, and predicted output of
  #   input data
  output$logit_plot <- renderPlot({
    get_logit_output(input, type="plot")
  }, bg="transparent")
  
  # - model diagnostics outputs
  output$model_coefficients <- renderTable({
    get_logit_output(input, type="model_coefficients")
  }, digits=4, bg="transparent")
  
  
  output$imputation_comparison <- renderPlot({
    get_logit_diagnostics(type="imputation_comparison")
  }, bg="transparent")
  
  output$confusion_matrix <- renderTable({
    get_logit_diagnostics(type="confusion_matrix")
  }, rownames = TRUE, bg="transparent")
  
  output$ROC <- renderPlot({
    get_logit_diagnostics(type="ROC")
  }, bg="transparent")
  
  output$new_cutoff <- renderText({
    paste0("The optimal cut-off point given the relative costs is ", round(get_logit_diagnostics(type="new_cutoff") * 100, 2), "%.")
  })
  
  output$confusion_matrix_new_cutoff <- renderTable({
    get_logit_diagnostics(type="confusion_matrix_new_cutoff")
  }, rownames = TRUE, bg="transparent")
  
  output$model_notes_markdown <- renderUI({
    HTML(markdown::markdownToHTML(knitr::knit('model_notes.Rmd', quiet = TRUE), fragment.only=TRUE))
  })
}

