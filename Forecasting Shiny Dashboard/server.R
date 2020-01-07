#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(tibble)
library(dplyr)
library(stringr)
library(DT)
library(xlsx)
library(kernlab)
library(e1071)
library(caret)
library(shinyjs)
library(forecast)
library(shinyWidgets)
library(ggplot2)
library(ggridges)
library(plotly)
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
#options(DT.options = list(autoWidth = TRUE, pageLength = 5, language = list(search = 'Filter:')))
options(DT.fillContainer = T)
options(shiny.reactlog = TRUE)



# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  # Interactive Data Frame Created from User File Input
  d2f <- reactive({
    typeoffile <- input$dataType
    if (typeoffile == "CSV") {
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      req(input$file)
      tryCatch({
        d2f <- read.csv(
          input$file$datapath,
          header = TRUE,
          sep = ",",
          quote = "\""
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
    } else {
      #browser()
      req(input$file1)
      inFile <- input$file1
      readxl::read_excel(inFile$datapath, 1)
    }
    
  })
  
  
  
  # User Uploaded Data Table Dispaly
  
  output$table1 <- DT::renderDataTable({
    df <- d2f()
    
    if (input$disp == "head") {
      return(datatable(head(df), options = list(
        scrollX = TRUE, scrollY = 300
      )))
    }
    else if (input$disp == "tail") {
      return(datatable(tail(df), options = list(
        scrollX = TRUE, scrollY = 300
      )))
    }
    else {
      return(datatable(df, options = list(
        scrollX = TRUE, scrollY = 300
      )))
    }
    
  })
  
  #Updating Predict Columns Filter as Per Uploaded Data
  
  observe({
    updateSelectInput(
      session = session,
      inputId = "predict",
      label = NULL,
      choices = names(d2f())
    )
    
  })
  
  #Updating Independent Columns Filter as Per Previous Predict Column Selection
  
  observe({
    input$predict
    df <- d2f()
    
    finalchoices <- names(df[,!(names(df) %in% c(input$predict))])
    #print(finalchoices)
    updateCheckboxGroupInput(
      session = session,
      inputId = "x_variables",
      label = NULL,
      choices = finalchoices,
      inline = TRUE
    )
    
  })
  
  #Predection Dataframe
  
  pred_df <- reactive({
    if (is.null(input$x_variables) == TRUE ||
        is.null(input$predict) == TRUE) {
      df2 <- NULL
    } else {
      pred_df <- as.data.frame(d2f())
      df2 <-
        subset(pred_df, select = c(input$predict, input$x_variables))
    }
  })
  
  #Split
  
  indxTrain <- reactive({
    df1 <- pred_df()
    
    # df1 <- data.frame(sapply(df1, function(x) as.numeric(as.character(x))))
    
    indxTrain <- createDataPartition(y = df1[, 1], p = 0.7, list = FALSE)
  })
  
  #Training data
  training <- reactive({
    df1 <- pred_df()
    
    # df1 <- data.frame(sapply(df1, function(x) as.numeric(as.character(x))))
    
    training <- df1[indxTrain(), ]
  })
  
  #Testing data
  testing <- reactive({
    df1 <- pred_df()
    
    # df1 <- data.frame(sapply(df1, function(x) as.numeric(as.character(x))))
    
    testing <- df1[-indxTrain(), ]
  })
  
  #model
  
  model <- reactive({
    trctrl <-
      trainControl(method = "repeatedcv",
                   number = 10,
                   repeats = 3)
    
    #lm and SVM
    
    if (input$MLType == "R") {
      G <- "lm"
      model <-
        lm(as.formula(paste(
          input$predict,
          " ~ ",
          paste(input$x_variables, collapse = "+")
        )), data = training())
      # model <- train(as.formula(paste(input$predict," ~ ",paste(input$x_variables,collapse="+"))),
      #                data = training(), method = G,
      #                trControl=trctrl,
      #                preProcess = c("center", "scale"),
      #                tuneLength = 10)
    }
    else
    {
      model <-
        train(
          as.formula(paste(
            input$predict,
            " ~ ",
            paste(input$x_variables, collapse = "+")
          )),
          data = training(),
          method = "svmLinear",
          trControl = trctrl,
          preProcess = c("center", "scale"),
          tuneLength = 10
        )
    }
    
  })
  
  # User Select Data Table For Prediction Analysis
  output$table2 <- DT::renderDataTable({
    datatable(pred_df(), options = list(scrollX = TRUE, scrollY = 300))
    
  })
  
  # Test predicted values
  test_pred <- reactive({
    m <- model()
    test_pred <- predict(m, newdata = testing())
  })
  
  # Actual and Test predicted values
  actuals_preds <- reactive({
    a <- testing()[, 1]
    b <- data.frame(actuals = a)
    
    c <- test_pred()
    d <- data.frame(predicteds = c)
    
    actuals_preds <- data.frame(cbind(actuals =b,predicteds =d))
    
  })
  
  # Accuracy Percentage
  output$accuracypercent <- renderValueBox({
    m <- model()
    if (input$MLType == "R"){
      a <- round(summary(m)$adj.r.squared,digits=2)
      b <- "Model Fitness"
    }else{
      #a <- round(m$results[2],digits=2)
      op <- confusionMatrix(test_pred(), testing()[, 1])
      
      a <- round(op$overall[1],digits=2)
      b <- "Accuracy"
      
    }
    
    valueBox(
      paste0(100 * a, "%"), b, icon = icon("tachometer-alt", lib = "font-awesome"),
      color = "yellow"
    )
    })
  
  # Accuracy Metrics
  output$accuracy <- renderPrint({
    # browser()
    # fit.lm <- lm (input$predict ~ ., data = df2)
    # fit.lm <- lm(as.formula(paste(input$predict," ~ ",paste(input$x_variables,collapse="+"))),data=df2)
    # m <- model()
    # test_pred <- predict(m, newdata = testing())
    
    if (input$MLType == "R") {
      # actuals_preds <- data.frame(cbind(actuals=testing()[,1], predicteds=test_pred()))
      
      # actuals_preds
      accuracy(actuals_preds()$actuals, actuals_preds()$predicteds)
      
      # correlation_accuracy <- cor(actuals_preds)
      
      # correlation_accuracy
    }
    else {
      op <- confusionMatrix(test_pred(), testing()[, 1])
      
      op$overall
    }
    
  })
  
  # Summary output
  output$summary <- renderPrint({
    m <- model()
    if (input$MLType == 'R') {
      summary(m)
    } else{
      m
    }
  })
  
  # Graph Output
  
   output$plot <- renderPlotly({
     a <- actuals_preds()
     if(input$MLType == "R"){
      
       plot_ly(a, y = ~predicteds, name = 'Predicted', type = 'scatter', mode = 'lines+markers') %>%
         add_trace(y = ~actuals, name = 'Actual', mode = 'lines+markers') %>%
         layout(title = "Actual VS Predicted",
                xaxis = list(title = "Records"),
                yaxis = list (title = "Values"))
       
     # plot(a$predicteds,type = "o",col = "red",
     #      xlab = "Records",
     #      ylab = "Values",
     #      main = "Actual VS Predicted")
     # lines(a$actuals, type = "o", col = "blue")
     # legend("topleft",c("Predicted","Actual"),fill=c("red","blue"))
     
   
     } else{
       a <- actuals_preds()
       
       d <- a %>% group_by(predicteds) %>% summarise(counts = n())
       d <- data.frame(d)
       d <- d %>% rename(Category = predicteds,predicted = counts )
       
       e <- a %>% group_by(actuals) %>% summarise(counts = n())
       f <- data.frame(e)
       f <- f %>% rename(Category = actuals,actuals = counts )
       
       fd <- left_join(d,f, by = "Category" )
       
       fd <- data.frame(fd)
       
       
       plot_ly(fd, x = ~Category, y = ~predicted, type = 'bar', name = 'Predicted') %>%
             add_trace(y = ~actuals, name = 'Actual') %>%
             layout(title = "Actual VS Predicted",
                    xaxis = list(title = "Category"),yaxis = list(title = 'Count'),
                    barmode = 'group')
       
       
   }
   })
  
   # Tab 3 Content
  
  # Dynamic Number Inputs for Whatif Prediction
  output$numinputs <- renderUI({
    
    df2 <- subset(pred_df(), select = c(input$x_variables))
    # df2 <- as.data.frame(df[, !(names(df) %in% c(input$predict))])
    nam_col <- names(df2)
    numIndividuals <- as.integer(ncol(df2))
    if (numIndividuals == 1) {
      numericInput(nam_col, nam_col, value = 0)
    }
    else{
      lapply(1:numIndividuals, function(i) {
        column(width = 4,
               numericInput(nam_col[i], nam_col[i], value = 0))
      })
    }
  })
  
  userdf <- reactive({
    df <- subset(pred_df(), select = c(input$x_variables))
    # print(is.null(df))
    nam_col <- names(df)
    # print(nam_col)
    num <- as.integer(ncol(df))
    #num <- as.integer(10)
    userdf <- data.frame(lapply(1:num, function(i) {
      input[[nam_col[i]]]
    }))
    # print(ncol(userdf))
    # print(ncol(df))
    if (ncol(userdf) > 0) {
      names(userdf) <- names(df)
    }
    # print(head(df))
    # datatable(userdf, options = list(scrollX = TRUE,scrollY = 300))
    userdf
  })
  
  # output$table4 <- DT::renderDataTable({
  #   datatable(userdf(), options = list(scrollX = TRUE, scrollY = 120))
  # })
  
  
  output$pred <- renderPrint({
    p <- predict(model(), newdata = userdf())
    names(p)<-paste(input$predict)
    p
  })
  
  # Interactive Data Frame Created from User File Input
  u2f <- reactive({
    typeoffile <- input$uploaddatatype
    if (typeoffile == "CSV") {
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      req(input$file2)
      tryCatch({
        u2f <- read.csv(
          input$file2$datapath,
          header = TRUE,
          sep = ",",
          quote = "\""
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
    } else {
      #browser()
      req(input$file3)
      inFile <- input$file3
      readxl::read_excel(inFile$datapath, 1)
    }
    
  })
  
  
  
  # User Uploaded Data Table Dispaly
  
  output$uploadtable <- DT::renderDataTable({
    df <- u2f()
    
    if (input$uploaddisp == "head") {
      return(datatable(head(df), options = list(
        scrollX = TRUE, scrollY = 300
      )))
    }
    else if (input$uploaddisp == "tail") {
      return(datatable(tail(df), options = list(
        scrollX = TRUE, scrollY = 300
      )))
    }
    else {
      return(datatable(df, options = list(
        scrollX = TRUE, scrollY = 300
      )))
    }
    
  })
  
  # Output for Multi-input file
  multipred <- reactive({
    p <- predict(model(), newdata = u2f())
    p <- data.frame(p)
    names(p)<-paste(input$predict)
    p <- data.frame(cbind(u2f(),p))
    p
  })
  
  # Output for Multi-input file
  output$multipred <- renderPrint({
    multipred()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Predicted", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(multipred(), file, row.names = FALSE)
    }
    
    
  )
  
  
  
})
