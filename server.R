#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

##install.packages("MASS")
library(MASS)
##install.packages("neuralnet")
library(neuralnet)
##install.packages("caret")
library(caret)
##install.packages("randomForest")
library(randomForest)

##devtools::install_github('rstudio/DT', force=TRUE)

lin <- function(Entry1,Entry2,Entry3) {
  res <- Entry1+Entry2+Entry3
  return(res)
}

sq <- function(Entry1,Entry2,Entry3) {
  res <- Entry1*Entry2 + Entry1*Entry3 + Entry2*Entry3
  return(res)
}

cub <- function(Entry1,Entry2,Entry3) {
  res <- Entry1*Entry2*Entry3
  return(res)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
   output$table <- renderDataTable({
    react <- input$Nb_layers+input$Nb_neurons+input$N_data+input$noise
    r <- input$relation
    
    noise <- 5
    n <- input$N_data
    m <- 10
    data <- data.frame(Entry1 = rnorm(n,m,noise),Entry2 = rnorm(n,m,noise),Entry3 = rnorm(n,m,noise)) 
    
    if (input$relation == "lin"){
    data$Output <- lin(data$Entry1,data$Entry2,data$Entry3) + rnorm(n,0,input$noise*200)
    } else if (input$relation == "sq") {
      data$Output <- lin(data$Entry1,data$Entry2,data$Entry3) + sq(data$Entry1,data$Entry2,data$Entry3) +  rnorm(n,0,input$noise*200)
    } else if (input$relation == "cub") {
      data$Output <- lin(data$Entry1,data$Entry2,data$Entry3) + sq(data$Entry1,data$Entry2,data$Entry3) + cub(data$Entry1,data$Entry2,data$Entry3) + rnorm(n,0,input$noise*200)
    }
    
    ## Scale
    maxValue <- apply(data,2,max)
    minValue <- apply(data,2,min)
    
    ## SCALE BEFORE NEURAL NET !!!!
    ##data <- as.data.frame(scale(data,center=minValue,scale=maxValue-minValue))
    data <- round(data,3)
    
    save(data,file="data.Rdata")
    
    head(data,3)
    
  })
   
  output$distPlot1 <- renderPlot({
    react <- input$Nb_layers+input$Nb_neurons+input$N_data+input$noise
    r <- input$relation
    
    load("data.Rdata")
    
    ## Scale
    maxValue <- apply(data,2,max)
    minValue <- apply(data,2,min)
    
    data <- as.data.frame(scale(data,center=minValue,scale=maxValue-minValue))
    
    ############################################################################################
    
    ## Divide into training and test
    ind <- sample(1:nrow(data),0.8*nrow(data))
    trainDF <- data[ind,]
    testDF <- data[-ind,]
    
    ###############################################
    
    form=as.formula("Output ~ Entry1+Entry2+Entry3")
    c <- c(input$Nb_neurons)
    for (i in 1:input$Nb_layers){c <- c(c,input$Nb_neurons)}
    neuralModel <- neuralnet(formula=form,hidden=c,linear.output=T,data=trainDF,lifesign = "full")
    save(neuralModel,file="neuralModel.Rdata")
    
    ## Predict for test data
    predictions <- compute(neuralModel,testDF[,1:3])
    ##str(predictions)
    
    predictions <- predictions$net.result*(max(testDF$Output)-min(testDF$Output))+min(testDF$Output)
    actualValues <- (testDF$Output)*(max(testDF$Output)-min(testDF$Output))+min(testDF$Output)
    
    RMSE_NN <- (sum((predictions-actualValues)^2)/nrow(testDF))^0.5
    RMSE_NN <- round(RMSE_NN,3)
    save(RMSE_NN,file="RMSE_NN.Rdata")
    ##RMSE_NN
    ##save(RMSE_NN,file="RMSE_NN.Rdata")
    
    ##plot(neuralModel)
    ##p_NN <- plot(predictions,actualValues)
    
    predictions_nn <- predictions
    actualValues_nn <- actualValues
    
 
    ###############################################
    
    Model_lm <- train(form=form,data=trainDF,method="lm")
    predictions <- predict(Model_lm,newdata=testDF)
    
    predictions <- predictions*(max(testDF$Output)-min(testDF$Output))+min(testDF$Output)
    actualValues <- (testDF$Output)*(max(testDF$Output)-min(testDF$Output))+min(testDF$Output)
    
    RMSE_lm <- (sum((predictions-actualValues)^2)/nrow(testDF))^0.5
    RMSE_lm <- round(RMSE_lm,3)
    save(RMSE_lm,file="RMSE_lm.Rdata")
    ##RMSE_lm
    
    predictions_lm <- predictions
    actualValues_lm <- actualValues
    ##p_lm <- plot(predictions,actualValues)
    

    ###############################################
    
    Model_rf <- randomForest(formula=form,data=trainDF)
    predictions <- predict(Model_rf,newdata=testDF)
    
    predictions <- predictions*(max(testDF$Output)-min(testDF$Output))+min(testDF$Output)
    actualValues <- (testDF$Output)*(max(testDF$Output)-min(testDF$Output))+min(testDF$Output)
    
    RMSE_rf <- (sum((predictions-actualValues)^2)/nrow(testDF))^0.5
    RMSE_rf <- round(RMSE_rf,3)
    save(RMSE_rf,file="RMSE_rf.Rdata")
    ##RMSE_rf
    
    predictions_rf <- predictions
    actualValues_rf <- actualValues
    
    main_lm <- paste("Linear Regression: RMSE =",RMSE_lm)
    main_rf <- paste("Random Forests: RMSE =",RMSE_rf)
    main_NN <- paste("Neural Network: RMSE =",RMSE_NN)
    
    par(mfrow=c(1,3),ps = 20, cex = 1, cex.main = 1)
    plot(predictions_lm,actualValues_lm, main=main_lm,xlab="Predictions",ylab="Actual")
    plot(predictions_rf,actualValues_rf, main=main_rf,xlab="Predictions",ylab="Actual")
    plot(predictions_nn,actualValues_nn, main=main_NN,xlab="Predictions",ylab="Actual")
    
  })
  
  output$distPlot4 <- renderPlot({
    react <- input$Nb_layers+input$Nb_neurons+input$N_data+input$noise
    r <- input$relation
    
    load("neuralModel.Rdata")
    par(mfrow=c(1,1),mai=c(2, 2, 2, 2))
    plot(neuralModel, main="Illustration of the Neural Network")
    
  })
  
  output$distPlot5 <- renderPlot({
    react <- input$Nb_layers+input$Nb_neurons+input$N_data+input$noise
    r <- input$relation
    
    load("RMSE_lm.Rdata")
    load("RMSE_rf.Rdata")
    load("RMSE_NN.Rdata")
    v <- c(RMSE_lm,RMSE_rf,RMSE_NN)
    col <- c("orange","orange","orange")
    col[which(v==max(v))] <- "red"
    col[which(v==min(v))] <- "green"
    ##v <- data.frame(RMSE=v,col=col)
    names(v) <- c("Linear Regression","Random Forests","Neural Network")
    par(mfrow=c(1,1),ps = 20, cex = 1, cex.main = 1,mai=c(2, 1, 1, 1))
    barplot(v,ylab="RMSE", col=col)
    
  })
  

})
