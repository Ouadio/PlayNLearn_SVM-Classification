  

################################# BUSINESS LOGIC   (V1.0) #####################################


library(e1071)
library(caret)


myplotSVM = function (x, data, formula = NULL, fill = TRUE, grid = 50, slice = list(), 
          symbolPalette = palette(), svSymbol = "x", dataSymbol = "o",Title, 
          ...) 
{
  if (x$type < 3) {
    if (is.null(formula) && ncol(data) == 3) {
      formula <- formula(delete.response(terms(x)))
      formula[2:3] <- formula[[2]][2:3]
    }
    if (is.null(formula)) 
      stop("missing formula.")
    if (fill) {
      sub <- model.frame(formula, data)
      xr <- seq(min(sub[, 2]), max(sub[, 2]), length = grid)
      yr <- seq(min(sub[, 1]), max(sub[, 1]), length = grid)
      l <- length(slice)
      if (l < ncol(data) - 3) {
        slnames <- names(slice)
        slice <- c(slice, rep(list(0), ncol(data) - 3 - 
                                l))
        names <- labels(delete.response(terms(x)))
        names(slice) <- c(slnames, names[!names %in% 
                                           c(colnames(sub), slnames)])
      }
      for (i in names(which(sapply(data, is.factor)))) if (!is.factor(slice[[i]])) {
        levs <- levels(data[[i]])
        lev <- if (is.character(slice[[i]])) 
          slice[[i]]
        else levs[1]
        fac <- factor(lev, levels = levs)
        if (is.na(fac)) 
          stop(paste("Level", dQuote(lev), "could not be found in factor", 
                     sQuote(i)))
        slice[[i]] <- fac
      }
      lis <- c(list(yr), list(xr), slice)
      names(lis)[1:2] <- colnames(sub)
      new <- expand.grid(lis)[, labels(terms(x))]
      preds <- predict(x, new)
      filled.contour(xr, yr, matrix(as.numeric(preds), 
                                    nrow = length(xr), byrow = TRUE), plot.axes = {
                                      axis(1)
                                      axis(2)
                                      colind <- as.numeric(model.response(model.frame(x, 
                                                                                      data)))
                                      dat1 <- data[-x$index, ]
                                      dat2 <- data[x$index, ]
                                      coltmp1 <- symbolPalette[colind[-x$index]]
                                      coltmp2 <- symbolPalette[colind[x$index]]
                                      points(formula, data = dat1, pch = dataSymbol, 
                                             col = coltmp1)
                                      points(formula, data = dat2, pch = svSymbol, 
                                             col = coltmp2)
                                    }, levels = 1:(length(levels(preds)) + 1), key.axes = axis(4, 
                                                                                               1:(length(levels(preds))) + 0.5, labels = levels(preds), 
                                                                                               las = 3), plot.title = title(main = Title, 
                                                                                                                            xlab = names(lis)[2], ylab = names(lis)[1]), 
                     ...)
    }
    else {
      plot(formula, data = data, type = "n", ...)
      colind <- as.numeric(model.response(model.frame(x, 
                                                      data)))
      dat1 <- data[-x$index, ]
      dat2 <- data[x$index, ]
      coltmp1 <- symbolPalette[colind[-x$index]]
      coltmp2 <- symbolPalette[colind[x$index]]
      points(formula, data = dat1, pch = dataSymbol, col = coltmp1)
      points(formula, data = dat2, pch = svSymbol, col = coltmp2)
      invisible()
    }
  }
}


generateData = function(N, complexite = 1, seed = 2019, show = FALSE){

  set.seed(seed)
  x = runif(N, 0,10)
  y = runif(N,-5,15)
  
  #'Generating Train & Test Data (2D independent variables + 2 class target variable) 
  #' - separating hyperplane complexities : linear, polynomial, non linear with a polynomial function, non linear with a sinus function

  
  if (complexite == 1)
    pos = y>2*x-1.5
  else
    if (complexite == 2)
      pos = y>0.2*x**2-1.2*x+4
    else
      if (complexite == 3)
        pos = (0.2*(x-3)**2+0.1*(y-1)**2)<3 | x>9-0.1*y
      else
        pos = (0.2*(sin(2*x)+3)**2+0.1*(y-2)**2)<4 | x>9-0.1*y
  myData = data.frame(x=x,y=y,label = as.factor(pos))
  if (show){
    plot(myData$x, myData$y, col=myData$label, pch=19, xlab = "x", ylab = "y")
    abline(v=c(2,7), lty=2)
  }
  return(myData)
}



fitSVMReg = function(data = myData, cost = 2, ker = "linear", gamma = 0.01, degree = 3,right = 7, left = 2, mode = T){
  inTrain = data$x>right | data$x<left
  trainData = data[inTrain,]
  testData = data[!inTrain,]
  
  if(ker == "linear"){
    fitSVM<-svm(formula = label~.,data = trainData, kernel = ker, cost = cost, scale = T)
  }
  else{
    if(ker =='polynomial'){
      fitSVM<-svm(formula = label~.,data = trainData, kernel = ker, gamma = gamma,cost = cost, degree = degree,scale = T)
    }
  else{
    fitSVM<-svm(formula = label~.,data = trainData, kernel = ker, gamma = gamma,cost = cost,scale = T)
    
  }}
    
  
  predsTr = predict(fitSVM,newdata = trainData)
  predsTs = predict(fitSVM, newdata = testData)
  
  trIsCorrect = predsTr==trainData$label
  tsIsCorrect = predsTs==testData$label
  
  confMatrixTr = confusionMatrix(predsTr, trainData$label)
  confMatrixTs = confusionMatrix(predsTs, testData$label)
  
  correctTr = round(as.numeric(confMatrixTr$overall[1]), digits = 3)*100
  correctTs = round(as.numeric(confMatrixTs$overall[1]), digits = 3)*100
  
  if(mode){
    plotData(data = data, right = right, left = left)
    points(trainData$x[trIsCorrect], trainData$y[trIsCorrect], cex = 1.8, col="green3")
    points(testData$x[tsIsCorrect], testData$y[tsIsCorrect], cex = 1.8, col="green3")
    title(paste("Train : ",as.character(correctTr) ,"% | Test : ",as.character(correctTs),"%"))
    
  }
  else{
    myplotSVM(fitSVM, data = data,formula = y~x, col=c("purple","skyblue"), pch=20,
         dataSymbol = 19, Title = paste("Train : ",as.character(correctTr) ,"% | Test : ",as.character(correctTs),"%"))
    abline(v=c(right, left), lty=2)
  }
    

    
}


plotData = function(data = myData, right, left){
  inTrain = data$x>right | data$x<left
  trainData = data[inTrain,]
  testData = data[!inTrain,]
  plot(data$x,data$y, type="n", xlab = "x", ylab = "y", xlim = c(min(data$x), max(data$x)+3))
  points(trainData$x, trainData$y, col= trainData$label, pch=19)
  points(testData$x, testData$y, col=testData$label, pch=17)
  legend(floor(max(data$x))+1.5, median(data$y)+2, legend=c("Train", "Test"), pch=c(19, 17),  cex=1.5)
  abline(v=c(left,right), lty=2, lwd = 2, col = 'blue')
}




