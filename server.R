#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$description <- renderPrint({
    str(dat)
  })
  
  output$Summary <- renderUI({
    #summary(dat)
    view(dfSummary(dat), omit.headings = FALSE, method = 'render', footnote = NA)
  })
  
  output$RawData <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(dat))
  })
  
  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$Variables, collapse = " + ")))
    vcd::mosaic(formula, data = dat[factorVariables],
                main = "Discrete Variables", shade = TRUE, legend = TRUE)
  })
  
  output$ggPairs <- renderPlot({
    GGally::ggpairs(data = dat[input$Pairs], title = "Pairs of Covid data")
    #      GGally::ggpairs(data = dat[input$Pairs],  mapping = ggplot2::aes(colour = sample(input$Pairs, 1)), title = "Pairs of Covid data")
  })
  
  output$Corrgram <- renderPlot({input$Pairs
    corrgram(dat[input$Correlation], 
             order = input$Group, 
             abs = input$abs, 
             cor.method = input$CorrMeth,
             text.panel = panel.txt,
             main = "Correlation of Covid data")
  })
  
  output$Missing <- renderPlot({
    vis_miss(dat[input$MissingInput], cluster = input$Cluster)
  })
  
  output$MissingVariables <- renderPlot({
    #gg_miss_upset(dat[input$MissingInput])
    gg_miss_var(dat)
  })
  
  output$MissingCase <- renderPlot({
    #gg_miss_upset(dat[input$MissingInput])
    gg_miss_case(dat)
  })
  
  output$MissingUpset <- renderPlot({
    #gg_miss_upset(dat[input$MissingInput])
    gg_miss_upset(dat)
  })
  
  
  output$Boxplot <- renderPlot({
    data <- as.matrix(dat[input$box])
    data <- scale(data, center = input$standardise, scale = input$standardise)
    boxplot(data, use.cols = TRUE, notch = FALSE, varwidth = FALSE,
            horizontal = FALSE, outline = input$outliers,
            col = brewer.pal(n = dim(dat)[2], name = "RdBu"),
            range = input$range, main = "Boxplots of Covid data")
    # coef = input$range
    # limits <- boxplot.stats(x = data[input$box], coef = coef)$stats
    # data$label <- ifelse(data[input$box] < limits[1] | data[input$box] > limits[5], rownames(data), NA)
    # # 
    # ggplot(data, mapping = aes(x = Residual, y = 0, label = label),) +
    #   geom_boxplot(coef = coef) +
    #   ggrepel::geom_text_repel() +
    #   labs(title = paste("Boxplot using", coef, "as IQR Multplier")) +
    #   theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    # 
  })
  
  # output$Boxplot <- renderPlot({
  #   data <- as.matrix(dat[input$box])
  #   data <- scale(data, center = input$standardise, scale = input$standardise)
  #   boxplot(data, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
  #           horizontal = FALSE, outline = input$outliers, 
  #           col = brewer.pal(n = dim(dat)[2], name = "RdBu"),
  #           range = input$range, main = "Boxplots of Covid data")
  # })
  
  output$RisingValue <- renderPlot({
    cols <- c(input$RisingInput)
    d <- dat[cols]  # select the definitely continuous columns
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col] #sort each column in ascending order
    }
    d <- scale(x = d, center = TRUE, scale = TRUE)  # scale so they can be graphed with a shared Y axis
    mypalette <- rainbow(ncol(d))
    matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile", ylab = "Values", lty = 1, lwd = 2, col = mypalette, main = "Rising Value Chart")
    legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 2, col = mypalette, ncol = round(ncol(d)^0.3))
  })
  
  # Interactive Time Series plot
  output$TimeSeries <- renderPlotly({
    autoplotly(ts(dat[input$TimeSeriesInput]))
  })
  
  output$Spectra <- renderPlot({
    matplot(t(dat[input$SpectraInput]), type = "l", ylab = "-log(R)", xaxt = "n", main = "Covid Data spectra")
    ind <- pretty(seq(from = 900, to = 1700, by = 2))
    ind <- ind[ind >= 900 & ind <= 1700]
    ind <- (ind - 898) / 2
    axis(1, ind, colnames(dat[input$SpectraInput])[ind])
  })
  pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
  
  output$remVariables <- renderText({
    threshold <- 50
    cRatio <- apply(X = dat, MARGIN = 2, FUN = pMiss) # run pMiss for each column of the data frame
    print(paste(colnames(dat)[cRatio >= threshold], collapse = ","))
  })
  
  output$remObservations <- renderText({
    threshold <- 50
    rRatio <- apply(X = dat, MARGIN = 1, FUN = pMiss)  # run pMiss for each row of the data frame
    print(paste(head(rownames(dat)[rRatio >= threshold], n = 50), collapse = ", "))
    
  })
  
  ############################
  ## cleanded data section ##
  ############################
  
  output$cdescription <- renderPrint({
    cdat <<- updateData()
    str(cdat)
  })
  
  output$cSummary <- renderUI({
    view(dfSummary(cdat), omit.headings = FALSE, method = 'render', footnote = NA)
  })
  
  output$cRawData <- DT::renderDataTable({
    cdat <<- updateData()
    DT::datatable(data = as.data.frame(cdat))
  })
  
  output$VariableMissingness <- renderPrint({
    cdat <<- updateData()
    
    threshold <- input$sliVariableMissing
    cRatio <<- apply(X = cdat, MARGIN = 2, FUN = pMiss)
    
    if(length(cdat[cRatio >= threshold]) > 0){
      print(colnames(cdat[cRatio >= threshold]))}
    else{print("NULL")}
  })
  
  output$ObservationMissingness <- renderPrint({
    cdat <<- updateData()
    
    threshold <- input$sliObservationMissing
    rRatio <- apply(X = cdat, MARGIN = 1, FUN = pMiss)
    
    #head(rownames(cdat[rRatio >= threshold]))
    if(length(rownames(cdat[rRatio >= threshold,])) > 0){
      print(rownames(cdat[rRatio >= threshold,]))}
    else{print("NULL")}
  })
  
  output$MissingCorrelation <- renderPlot({input$Pairs
    cdat <<- updateData()
    m <- is.na(cdat) + 0 # this is a trick that transforms Logical to Binary
    cm <- colMeans(m)
    m <- m[, cm > 0 & cm < 1, drop = FALSE] #remove none-missing or all-missing variables
    corrgram(m, 
             order = input$Group, 
             abs = input$abs, 
             cor.method = input$CorrMeth,
             text.panel = panel.txt,
             main = "Missing Value Correlation")
  })
  
  output$Missingness <- renderPlot({
    cdat <<- updateData()
    cdat$MISSINGNESS <- apply(X = is.na(cdat), MARGIN = 1, FUN = sum)
    tree <- train(MISSINGNESS ~ ., data = cdat, method = "rpart", na.action = na.rpart)
    rpart.plot(tree$finalModel, main = "TUNED: Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)
  })
  
  
  output$correlationPairs <- renderPlot({
    cdat <<- updateData()
    GGally::ggpairs(data = cdat[input$PairsVariables],  mapping = ggplot2::aes(colour = cdat$GOVERNMENT), title = "Correlation of Covid Data")
  })
  
  output$VariableImportance <- renderPrint({
    cdat.rf <- randomForest(DEATHRATE ~ . -COUNTRY, data=cdat, ntree=1000, keep.forest=FALSE,importance=TRUE, na.action=na.exclude)
    importance(cdat.rf)
  })
  
  output$variableImportanceGB <- renderPlot({
    cdat.rf <- randomForest(DEATHRATE ~ .-COUNTRY, data=cdat, ntree=1000, keep.forest=FALSE,importance=TRUE, na.action=na.exclude)
    varImpPlot(cdat.rf)
  })
  
  
  output$SplitTrainData <- DT::renderDataTable({
    # set.seed(10)
    # subIndex <- caret::createDataPartition(y = cdat$DEATHRATE, p = 0.7, list = TRUE)
    # train.cdat <<- cdat[unlist(subIndex),]
    # test.cdat <<- cdat[-unlist(subIndex),]
    cdat <<- updateData()
    DT::datatable(data = train.cdat)
    
    #DT::datatable(data = as.data.frame(cdat$COUNTRY[unlist(subIndex)]))
  })
  
  output$SplitTestData <- DT::renderDataTable({
    DT::datatable(data = test.cdat)
  })
  
  output$ModelPredict <- renderPrint({
    #print(predicted.Deathrate)
    cdat <<- updateData()
    rec <- recipes::recipe(DEATHRATE ~., data = cdat) %>%
      update_role("COUNTRY", new_role = "id") %>% #id is not a predictor
      step_knnimpute(all_predictors(), neighbours = 5) %>%
      step_center(all_numeric(), -has_role("outcome")) %>%
      step_scale(all_numeric(), -has_role("outcome")) %>%
      step_dummy(all_nominal())
    
    mymodel <- caret::train(rec, data = train.cdat, method = "glmnet")
    test.cdat$DEATHRATEHAT <<- predict(mymodel, newdata = test.cdat)
    test.cdat$Residual <<- test.cdat$DEATHRATE - test.cdat$DEATHRATEHAT
    print(test.cdat$DEATHRATEHAT)
  })
  
  output$ModelOutliers <- renderPlot({
    #cdat <<- updateData()
    updateModelOutliers()
  })
  
  output$ResidualsHistPlot <- renderPlot({
    #cdat <<- updateData()
    ggplot(test.cdat) +
      geom_histogram(aes(x = Residual), binwidth = 0.3) +
      labs(title = "Histogram of residuals showing no outliers")
  })
  
  output$ResidualsBoxPlot <- renderPlot({
    coef <- input$IQRrange#2.2
    
    limits <- boxplot.stats(x = test.cdat$Residual, coef = coef)$stats
    test.cdat$label <- ifelse(test.cdat$Residual < limits[1] | test.cdat$Residual > limits[5], rownames(test.cdat), NA)
    
    ggplot(test.cdat, mapping = aes(x = Residual, y = 0, label = label),) +
      geom_boxplot(coef = coef) +
      ggrepel::geom_text_repel() +
      labs(title = paste("Residuals Boxplot using", coef, "as IQR Multplier")) +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$ModelInfo <- renderPrint({
    if(is.null(test.cdat$DEATHRATEHAT)){
      print("Please run the Model first by going to the Model & Predict tab.") 
    }
    else{
      print(getModelAccuracy())
    }
  })
  
  updateData <- reactive({
    ndat <- bcdat
    if(input$chkRemoveMissingVariables) {
      cRatio <- apply(X = ndat, MARGIN = 2, FUN = pMiss) # run pMiss for each column of the data frame
      ndat <- ndat[, cRatio < input$sliVariableMissing]
    }
    
    if(input$chkRemoveMissingObservations){
      rRatio <- apply(X = ndat, MARGIN = 1, FUN = pMiss)  # run pMiss for each row of the data frame
      #head(rownames(cdat)[rRatio >= threshold])
      ndat <- ndat[rRatio < input$sliObservationMissing,]
    }
    
    # create shadow variable if it is an informed variable 
    #if(!is.nullinput$sipInformedMissing){
    #cdat$VAXRATE_shadow <- as.numeric(is.na(cdat$VAXRATE)) # create a shadow variable
    #cdat$VAXRATE[is.na(cdat$VAXRATE)] <- 0 #Assign missing to zero
    #}
    
    #train and test split
    set.seed(10)
    subIndex <- caret::createDataPartition(y = ndat$DEATHRATE, p = 0.7, list = TRUE)
    train.cdat <<- ndat[unlist(subIndex),]
    test.cdat <<- ndat[-unlist(subIndex),]
    
    ndat
  })
  
  # get the list of changes made
  getModelAccuracy <- reactive({
    
    postResample(pred = test.cdat$DEATHRATEHAT, obs = test.cdat$DEATHRATE)
  })  
  
  updateModelOutliers <- reactive ({
    ActualPredicted = range(c(test.cdat$DEATHRATE, test.cdat$DEATHRATEHAT))
    ggplot(test.cdat) +
      geom_point(mapping = aes(x = DEATHRATEHAT, y = DEATHRATE)) +
      geom_abline(slope = 1, col = "blue") +
      labs(title = "Model with dependent X & Y outliers", y = "actual", x = "predicted") +
      coord_fixed(ratio = 1, xlim = ActualPredicted, ylim = ActualPredicted, expand = TRUE)
  })
  
})
