#' Cissy Chan
#' server file for rshiny dashboard, pima indians diabetes analysis
#' November 2016

library(shiny)

# - source helper file that generate plots and outputs
source("helper.R")

function(input, output) {
  
  # - summary plot comparing input attributes with the rest of data set
  output$summary_plot <- renderPlot({
    get_summary_output(input, type="plot")
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