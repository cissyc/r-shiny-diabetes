
function(input, output) {
  
  # - source helper file that generate plots and outputs
  source("helper.R")
  
  # - summary plot comparing input attributes with the rest of data set
  output$summary_plot <- renderPlot({
    get_summary_output(input, type="plot")
  })
  
  # - output text showing predicted probability of diabetes given input data
  output$logit_probability <- renderText({
    get_logit_output(input, type="probability")
  })
  
  # - output scatter plot comparing two attributes, diabetes outcome, and predicted output of
  #   input data
  output$logit_plot <- renderPlot({
    get_logit_output(input, type="plot")
  })
}