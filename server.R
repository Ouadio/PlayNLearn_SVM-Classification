  

################################# CONTROLLER   LAYER  USING SHINY (V1 .0) #####################################


library(shiny)
source("./model.R")

shinyServer(function(input, output) {
   
  output$dataPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    allOfIt = generateData(N = 300, complexite = input$complexity)
    plotData(allOfIt, left = 2, right = 8)
    
  })

  reactiveSVMPlot <- eventReactive(input$showSVM, { 
    myData = generateData(N = 300, complexite = input$complexity, show = FALSE)
    
    fitSVMReg(data = myData, cost = input$cost, ker = input$kernel, gamma = 10**(input$gamma), input$degree,mode = input$mode)
    
    
  })
  
  output$svmPlot = renderPlot({
    reactiveSVMPlot()
    
    
    
  })
  
  
  
})
