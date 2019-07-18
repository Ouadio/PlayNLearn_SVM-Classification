

################################# USER INTERFACE USING SHINY (V2.0) #####################################


library(shiny)

shinyUI(fluidPage(
  
  titlePanel("SVM for Classification : an interactive App"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("complexity", "Complexity of the seperating hyperplan", 
                  choices = c(linear = "1",
                              polynomial_degree2 = "2",
                              non_linear_poly = "3",
                              non_linear_sin = "4"), 
                  selected = "1"),
      
      
      sliderInput("cost",
                  "Cost :",
                  min = 1,
                  max = 10,
                  value = 1,step = 1), 
      
      sliderInput("gamma",
                   "Gamma :",
                   min = -4,
                   max = 2,
                   value = -4,step = 0.5),
      

      sliderInput("degree",
                  "Polynomial Degree :",
                  min = 2,
                  max = 8,
                  value = 3,step = 1),
      
                   
       selectInput("kernel", "SVM Kernel", choices = c(linear = "linear",
                                                                       polynomial = "polynomial",
                                                                       radial = "radial"),
                   selected = "linear"),
      
       radioButtons("mode", "Visualization Theme", choices = c(mode1 = "T", mode2 = "F"), selected="T")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type  = "tabs", 
                  tabPanel("Labeled Toy Data plot",plotOutput("dataPlot")),
                  tabPanel("SVM Class Predictions", plotOutput("svmPlot"))
    )
  )
)))
