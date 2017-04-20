#' Cissy Chan
#' ui file for rshiny dashboard, pima indians diabetes analysis
#' November 2016

library(shiny)
library(shinydashboard)
library(shinyjs)

# - source the helper file that generates plots and outputs
source("helper.R")

# - put this here so drop down knows which headers to take
data(PimaIndiansDiabetes)
data_headers <- colnames(PimaIndiansDiabetes)
data_desc <-  c("Number of times pregnant",
                "Plasma glucose concentration",
                "Diastolic blood pressure (mm Hg)",
                "Triceps skin fold thickness (mm)",
                "2-Hour serum insulin (mu U/ml)",
                "Body mass index (weight in kg/(height in m)^2)",
                "Diabetes pedigree function",
                "Age (years)")

# - dashboard starts here
dashboardPage(
  
  dashboardHeader(
    title = "Diabetes Regression",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    
    # - side bar menu with input and output tabs
    sidebarMenu(
      
      # - userdata input
      menuItem("Input data", icon = icon("pencil"),
               numericInput(data_headers[1], data_desc[1], value=0, min=0),
               numericInput(data_headers[2], data_desc[2], value=110, min=0),
               numericInput(data_headers[3], data_desc[3], value=70, min=0, max=150),
               numericInput(data_headers[4], data_desc[4], value=20, min=0),
               numericInput(data_headers[5], data_desc[5], value=70, min=0),
               numericInput(data_headers[6], data_desc[6], value=30, min=0),
               numericInput(data_headers[7], data_desc[7], value=0.4, min=0),
               numericInput(data_headers[8], data_desc[8], value=30, min=0, max=120)
      ),
      menuItem("Comparison with population", tabName = "comparison", icon = icon("bar-chart")),
      menuItem("Diabetes likelihood", tabName = "diabetes", icon = icon("heartbeat")),
      menuItem("Model Diagnostics", tabName = "model", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    
    # - a set of tabs for outputs and further analysis
    mainPanel(
      
      
      box(width = 12, solidHeader = TRUE,
          paste0("This app explores the Pima Indians Diabetes data set. ",
                 "Enter inputs in the \"Input data\" tab on the sidebar to the left to compare their ",
                 "relation to the existing data set, and their corresponding probability of a positive ",
                 "diabetes result, based on regression on the existing data.")),
      hr(),
      tabItems(
        
        # - first tab: comparison of input data with existing data to show where it stands
        tabItem(tabName = "comparison",
                
                
                fluidPage(
                  
                  includeCSS("styles.css"),
                  
                  #tags$div(class = "content-wdrapper", style = "min-height: 4000px;"),
                  
                  br(),
                  # - description for this tab
                  paste0("The charts below show the probability density distribution of ",
                         "each attribute in the existing data set, split by each diabetes outcome."),
                  br(),
                  paste0("- Green line: density corresponding to a negative result (i.e. no diabetes)"),
                  br(),
                  paste0("- Red line: density corresponding to a positive result"),
                  br(),
                  paste0("- Black line: given input attributes"),
                  hr(),
                  
                  # - rows of density plots here
                  fluidRow(
                    get_summary_plot_box(attribute = data_headers[1]),
                    get_summary_plot_box(attribute = data_headers[2]),
                    get_summary_plot_box(attribute = data_headers[3])
                  ),
                  fluidRow(
                    get_summary_plot_box(attribute = data_headers[4]),
                    get_summary_plot_box(attribute = data_headers[5]),
                    get_summary_plot_box(attribute = data_headers[6])
                  ),
                  fluidRow(
                    get_summary_plot_box(attribute = data_headers[7]),
                    get_summary_plot_box(attribute = data_headers[8])
                  )
                  
                )
        ), 
        
        # - second tab: using logistic regression to calculate probabiltiy of diabetes
        #   given input attributes
        tabItem(tabName = "diabetes",
                fluidPage(
                  br(),
                  # - line of text showing predicted probability
                  textOutput("logit_probability"),
                  br(),
                  # - description for scatter plot
                  paste0("The following plot shows two attributes on the axes, ",
                         "with green circular data points corresponding to a negative result ",
                         "(i.e. no diabetes), red circular points corresponding to a positive result ",
                         "and the triangle corresponding to given input attributes."),
                  br(),
                  br(),
                  paste0("Adjust the attributes on each axes using the drop-down selections below"),
                  
                  # - scatter plot with two attributes of existing data, new data, 
                  #   and predicted outcome
                  fluidRow(
                    hr(),
                    plotOutput("logit_plot")
                  ),
                  # - input panel at the bottom to let the user select which attributes
                  #   to compare; default glucose and mass
                  fluidRow(
                    hr(),
                    column(3,
                           h4("Variables to compare: ")
                    ),
                    column(4, offset = 1,
                           selectInput('x_var',
                                       'X Variable',
                                       choices=data_headers,
                                       selected="glucose")
                    ),
                    column(4,
                           selectInput('y_var',
                                       'Y Variable',
                                       choices=data_headers,
                                       selected="mass")
                    )
                  )
                  
                  
                )),
        # - third tab: model statistics
        tabItem(tabName = "model",
                fluidPage(
                  uiOutput('model_notes_markdown')
                )
        )
      )
    )
  )
)





