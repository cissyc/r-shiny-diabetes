

fluidPage(
  
  # Application title
  titlePanel("Enter details"),
  
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
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Comparison with population",
                           hr(),
                           plotOutput("summary_plot")), 
                  
                  tabPanel("Diabetes likelihood",
                           fluidPage(
                             hr(),
                             textOutput('logit_probability'),
                             hr(),
                             plotOutput('logit_plot'),
                             hr(),
                             fluidRow(
                               column(3,
                                      h4("Variables to compare: ")
                               ),
                               column(4, offset = 1,
                                      selectInput('x_var',
                                                  'X Variable',
                                                  choices=names(df_data),
                                                  selected="glucose")
                               ),
                               column(4,
                                      selectInput('y_var',
                                                  'Y Variable',
                                                  choices=names(df_data),
                                                  selected="mass")
                               )
                             )
                           )
                           ),
                  tabPanel("Model Info", "some stats stuff, ROC curve, accuracy, error rates etc")
      )
    )
  )
)




