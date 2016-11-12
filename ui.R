
# - put this here so drop down knows which headers to take
require(mlbench)
data(PimaIndiansDiabetes)
data_headers <- colnames(PimaIndiansDiabetes)

fluidPage(
  
  # - app title
  titlePanel("Pima Indians Diabetes Analysis"),
  
  # - left side bar for data input
  sidebarLayout(
    sidebarPanel(
      numericInput("pregnant", "Number of times pregnant", value=0, min=0),
      numericInput("glucose","Plasma glucose concentration", value=110, min=0),
      numericInput("pressure", "Diastolic blood pressure (mm Hg)", value=70, min=0, max=150),
      numericInput("triceps", "Triceps skin fold thickness (mm)", value=20, min=0),
      numericInput("insulin", "2-Hour serum insulin (mu U/ml)", value=70, min=0),
      numericInput("mass", "Body mass index (weight in kg/(height in m)^2)", value=30, min=0),
      numericInput("pedigree", "Diabetes pedigree function", value=0.4, min=0),
      numericInput("age", "Age (years)", value=30, min=0, max=120)
    ),
    
    # - a set of tabs for outputs and further analysis
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  # - first tab: comparison of input data with existing data to show where it stands
                  tabPanel("Comparison with population",
                           hr(),
                           plotOutput("summary_plot")), 
                  
                  # - second tab: using logistic regression to calculate probabiltiy of diabetes
                  #   given input attributes
                  tabPanel("Diabetes likelihood",
                           fluidPage(
                             hr(),
                             # - line of text showing predicted probability
                             textOutput('logit_probability'),
                             hr(),
                             # - scatter plot with two attributes of existing data, new data, 
                             #   and predicted outcome
                             plotOutput('logit_plot'),
                             hr(),
                             # - input panel at the bottom to let the user select which attributes
                             #   to compare; default glucose and mass
                             fluidRow(
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
                  tabPanel("Model Info", "some stats stuff, ROC curve, accuracy, error rates etc")
      )
    )
  )
)




