#' Cissy Chan
#' server file for rshiny dashboard, pima indians diabetes analysis
#' November 2016

library(shiny)

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
}

