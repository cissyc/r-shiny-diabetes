
function(input, output) {
  
  source("helper.R")
  
  output$summary_plot <- renderPlot({
    get_summary_output(input, type="plot")
  })
  
  output$logit_probability <- renderText({
    get_logit_output(input, type="probability")
  })
  
  output$logit_plot <- renderPlot({
    get_logit_output(input, type="plot")
  })
}