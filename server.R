  

################################# CONTROLLER   LAYER  USING SHINY (V2.0) #####################################


library(shiny)
source("./svmRegul.R")

shinyServer(function(input, output) {
   
  output$dataPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    allOfIt = generateData(N = 600, complexite = input$complexity)
    plotData(allOfIt, left = 2, right = 8)
    
  })

  
  output$svmPlot = renderPlot({
    myData = generateData(N = 600, complexite = input$complexity, show = FALSE)
    fitSVMReg(data = myData, cost = input$cost, ker = input$kernel, gamma = 10**(input$gamma), input$degree,mode1 = input$mode)
    
    
  })
  
  
  
})
