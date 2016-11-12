require(mlbench)
require(stats)
require(ggplot2)

# - get data
data(PimaIndiansDiabetes)
df_data <- PimaIndiansDiabetes
levels(df_data$diabetes) <- c("negative", "positive")

# - melt into required format for facet distribution plots
df_melted <- reshape2::melt(df_data, id.vars="diabetes")

# - get logit coefficients
logit_model <- stats::glm(diabetes ~ ., data=df_data, family="binomial")

#' get_summary_output
#' 
#' 
get_summary_output <- function(input, type) {
  
  if (type == "plot") {
    
    # - set up new input data
    new_data <- data.frame(
      pregnant=input$pregnant,
      glucose=input$glucose,
      pressure=input$pressure,
      triceps=input$triceps,
      insulin=input$insulin,
      mass=input$mass,
      pedigree=input$pedigree,
      age=input$age
    )
    
    new_data <- data.frame(new_values=t(new_data))
    new_data$variable <- rownames(new_data)
    
    new_data_merged <- merge(df_melted, new_data, by.x="variable") 
    
    # - plot density of existing data set, overlay vline to mark new input data
    plot <- ggplot(data=new_data_merged,
                   aes(x=value,
                       group=diabetes,
                       colour=diabetes)) + 
      geom_density() + 
      facet_wrap(~ variable, scales = "free") +
      geom_vline(aes(xintercept=new_values)) +
      scale_colour_manual(name="diabetes",values=c("chartreuse3", "red", "black"))
    
    # - return output to server
    return(plot)
  }
  
}

#' get_logit_output
#' 
#' 
get_logit_output <- function(input, type) {
  
  # - set up new input data
  new_data <- data.frame(
    pregnant=input$pregnant,
    glucose=input$glucose,
    pressure=input$pressure,
    triceps=input$triceps,
    insulin=input$insulin,
    mass=input$mass,
    pedigree=input$pedigree,
    age=input$age
  )
  
  # - get regression classification
  logit_predict <- stats::predict(logit_model, newdata=new_data, type="response")
  
  # - if retrieving probability of diabetes, return text
  if (type == "probability") {
    
    logit_predict_rounded <- round(logit_predict * 100, 1)
    
    # - return text output to server
    return(paste0("Probability of diabetes: ", logit_predict_rounded, "% \n"))
    
  } else if (type == "plot") {
    
    # - else if want to retrieve plot, return scatter plot given two input variables to compare
    logit_predict_out <- as.factor(ifelse(round(logit_predict) == 0, "predicted_negative", "predicted_positive"))
    dt_logit_predict <- rbind(df_data, data.frame(new_data, diabetes=logit_predict_out))
    
    # - plot data and prediction
    plot <- ggplot(data=dt_logit_predict, 
                   aes(x=get(input$x_var),
                       y=get(input$y_var),
                       shape=diabetes,
                       size=diabetes,
                       colour=diabetes)) + 
      geom_point() +
      scale_shape_manual(values=c(1, 1, 17, 17)) +
      scale_size_manual(values=c(2, 2, 4, 4)) +
      scale_colour_manual(name="diabetes",values=c("chartreuse3", "red", "chartreuse4", "red1"))
    
    # - return plot to server
    return(plot)
  }

}
