#' Cissy Chan
#' ui file for rshiny dashboard, pima indians diabetes analysis
#' November 2016

library(shiny)
library(shinydashboard)
library(mlbench)

# - put this here so drop down knows which headers to take
data(PimaIndiansDiabetes)
data_headers <- colnames(PimaIndiansDiabetes)


dashboardPage(
  
  dashboardHeader(
    title = "Pima Indians Diabetes Analysis",
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 320,
    
    # - side bar menu with input and output tabs
    sidebarMenu(
      
      # - userdata input
      menuItem("Input data", icon = icon("pencil"),
               numericInput("pregnant", "Number of times pregnant", value=0, min=0),
               numericInput("glucose","Plasma glucose concentration", value=110, min=0),
               numericInput("pressure", "Diastolic blood pressure (mm Hg)", value=70, min=0, max=150),
               numericInput("triceps", "Triceps skin fold thickness (mm)", value=20, min=0),
               numericInput("insulin", "2-Hour serum insulin (mu U/ml)", value=70, min=0),
               numericInput("mass", "Body mass index (weight in kg/(height in m)^2)", value=30, min=0),
               numericInput("pedigree", "Diabetes pedigree function", value=0.4, min=0),
               numericInput("age", "Age (years)", value=30, min=0, max=120)
      ),
      menuItem("Comparison with population", tabName = "comparison", icon = icon("bar-chart")),
      menuItem("Diabetes likelihood", tabName = "diabetes", icon = icon("heartbeat")),
      menuItem("Model Info", tabName = "model", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    
    # - a set of tabs for outputs and further analysis
    mainPanel(
      
      
      box(width = 12, solidHeader = TRUE,
          paste0("This app explores the Pima Indians Diabetes data. ",
                 "Enter inputs in the \"Input data\" tab on the sidebar to the left to compare their ",
                 "relation to the existing data set, and their corresponding probability of a positive ",
                 "diabetes result, based on regression on the existing data.")),
      hr(),
      tabItems(
        
        # - first tab: comparison of input data with existing data to show where it stands
        tabItem(tabName = "comparison",
                fluidPage(
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
                  plotOutput("summary_plot")
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
                    column(2,
                           h4("Variables to compare: ")
                    ),
                    column(3, offset = 1,
                           selectInput('x_var',
                                       'X Variable',
                                       choices=data_headers,
                                       selected="glucose")
                    ),
                    column(3,
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
                  br(),
                  "On its way: model stats, ROC curve, accuracy, error rates")
        )
      )
    )
  )
)





