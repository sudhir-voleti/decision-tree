###########################################################
#         Regression Tree App (server)              #
###########################################################
library(shiny)
library(rpart)
library(pastecs)
library(dplyr)
library(Hmisc)
library("hydroGOF")
require(party)
require(partykit)
library(visNetwork)

shinyServer(function(input, output,session) {
  
  #------------------------------------------------#
  
  readdata <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })

  pred.readdata <- reactive({
    if (is.null(input$filep)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })

  # sample dataset
  output$sample_data <- DT::renderDataTable(head(readdata()))
    
  # Select variables:
  output$yvarselect <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    
    selectInput("yAttr", "Select Y variable",
                colnames(readdata()), colnames(readdata())[1])
    
  })
  
  output$xvarselect <- renderUI({
    if (identical(readdata(), '') || identical(readdata(),data.frame())) return(NULL)
    #varSelectInput("selVar",label = "Select Variables",data = Dataset(),multiple = TRUE,selectize = TRUE,selected = colnames(Dataset()))
    selectInput("xAttr", label = "Select X variables",multiple = TRUE,
                   selectize = TRUE,
                   selected = setdiff(colnames(readdata()),input$yAttr),choices = setdiff(colnames(readdata()),input$yAttr)
                   )#, setdiff(colnames(readdata()),input$yAttr))
    
  })
  
  #---Model summary tab-4-----#
  
  
  output$split_summ <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    leaf_nodes4train <- fit.rt()$model$where 
    #leaf_nodes4train[1:8]
    train1 = data.frame(train_data(), leaf_node = leaf_nodes4train)
    head(train1)  # display full train1 as html table
  })
  
  
  output$mod_sum <- renderPrint({
    if (is.null(input$file)) {return(NULL)}
    as.party(fit.rt()$model)
    })
  
  
  
  #-------------------------#
  
  
  #-------results plot----#
  output$results_plot <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    cptabl = fit.rt()$model$cptable
    plot(x = seq(1:nrow(cptabl)), y = cptabl[,1], type = "b", col = "red", xlab = "num_splits", ylab = "Complexity_parm")
    
  }
  )
 
  #-------------------------#
  
  
  
  
  
 readdata.temp = reactive({
   mydata = readdata()[,c(input$yAttr,input$xAttr)]
 })
  
  data_fr_str <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      data_frame_str(readdata())
    }
    
    }) # get structure of uploaded dataset
    
  output$fxvarselect <- renderUI({
    if (is.null(input$file)||identical(readdata.temp(), '') || identical(readdata.temp(),data.frame())) return(NULL)
    
    cond_df <- data_fr_str() %>% filter((class=="numeric"| class=="integer") & unique_value_count<7)
    cols <- cond_df$variable
    
    selectInput("fxAttr", 
                  label="Select factor variable in Data set",
                   multiple = TRUE,
                   selectize = TRUE,
                   selected =  cols,
                   choices=names(readdata()) )
    
  })
  
  
  Dataset = reactive({
    mydata = readdata()[,c(input$yAttr,input$xAttr)]
    
    if (length(input$fxAttr) >= 1){
      for (j in 1:length(input$fxAttr)){
        mydata[,input$fxAttr[j]] = as.factor(mydata[,input$fxAttr[j]])
      }
    }
    return(mydata)
    
  })
  
  
  Dataset.Predict <- reactive({
    fxc = setdiff(input$fxAttr, input$yAttr)
    mydata = pred.readdata()[,c(input$xAttr)]
    
    if (length(fxc) >= 1){
      for (j in 1:length(fxc)){
        mydata[,fxc[j]] = as.factor(mydata[,fxc[j]])
      }
    }
    return(mydata)
  })
  
  # a = c('a','b','c')
  # b = ('c')
  # setdiff(a,b)
    #------------------------------------------------#
  
  out = reactive({
    data = Dataset()
    Dimensions = dim(data)
    Head = head(data)
    Tail = tail(data)
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = class(data[,i])
      Class = c(Class, c1)
    }
    
    nu = which(Class %in% c("numeric","integer"))
    fa = which(Class %in% c("factor","character"))
    nu.data = data[,nu] 
    fa.data = data[,fa] 
    Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
    
    a = seq(from = 0, to=200,by = 4)
    j = length(which(a < ncol(nu.data)))
    out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j)
    return(out)
  })
  
  output$summarydata = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[1:2]
    }
  })
  
  testsample =  reactive({
  set.seed(5898)
  sample(1:nrow(Dataset()), round(nrow(Dataset())*((input$sample)/100)))
         })

  train_data = reactive({
      Dataset()[-testsample(),]
  })
  
  test_data = reactive({
    Dataset()[testsample(),]
  })
  
  
  
  #------------------------------------------------#
  fit.rt = reactive({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
  x = setdiff(colnames(Dataset()), input$Attr)
  y = input$yAttr
  # formula1 = 
  
  
  
  ## mean predictions
  
  if (class(train_data()[,c(input$yAttr)]) == "factor"){
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                  method="class",   # use "class" for classification trees
                data=train_data())
  pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
  val = predict(pr, newdata = test_data(),type="response")
  
  imp = round(fit.rt$variable.importance/sum(fit.rt$variable.importance),2)
  
  } else {
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                  method="anova",   # use "class" for classification trees
                  data=train_data())
  pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
   val = predict(pr, newdata = test_data())
   imp = round(fit.rt$variable.importance/sum(fit.rt$variable.importance),2)
  }
  
  out = list(model = fit.rt, validation = val, imp = imp)
    })

  output$validation = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      y = test_data()[,input$yAttr]
      yhat = fit.rt()$validation
    confusion_matrix = table(y,yhat)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))*100
    out = list(Confusion_matrix_of_Validation = confusion_matrix, Accuracy_of_Validation = accuracy)
    } else {
    dft = data.frame(scale(data.frame(y = test_data()[,input$yAttr], yhat = fit.rt()$validation)))
    mse.y = mse(dft$y,dft$yhat)
    out = list(Mean_Square_Error_of_Standardized_Response_in_Validation = mse.y)
    } 
    out
       })

  
  #------------------------------------------------#
  output$results = renderPrint({
    
    if (is.null(input$file)) {return(NULL)}
     printcp(fit.rt()$model) # display the results
    # formula.mod()
  })
  
  #-----------------------------------------------#
  
 
  
  
  #------------------------------------------------#
  output$summary = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    as.party(fit.rt()$model) # detailed summary of splits  
  })
  
  
  #------------------------------------------------#
  output$imp = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    fit.rt()$imp
  })
  
  output$var_imp_plot <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    varImp_plot(fit.rt()$model)
  })
  #------------------------------------------------#
  output$plot1 = renderPlot({
    
    if (is.null(input$file)) {return(NULL)}
    
    plotcp(fit.rt()$model) # visualize cross-validation results   
  })
  
  
  #------------------------------------------------#
  output$plot2 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    title1 = paste("Decision Nodes for", input$yAttr)
    
    fit.rt1 = fit.rt()$model
    fit.rt1$frame$yval = as.numeric(rownames(fit.rt()$model$frame))
    
    # create attractive postcript plot of tree 
    post(fit.rt1, 
         # file = "tree2.ps", 
         filename = "",   # will print to console
         use.n = FALSE,
         compress = TRUE,
         title = title1) 
    
  })
  
  output$plot3 = renderVisNetwork({
    if (is.null(input$file)) {return(NULL)}
    
    
    visTree(fit.rt()$model, main = paste("Decision Tree for", input$yAttr), width = "100%")
     
    

  })
  
  
  #------------------------------------------------#
  nodes1 =  reactive({
    
  tree_nodes = as.data.frame(fit.rt()$model$where)
  colnames(tree_nodes) <- "node_number"
  # tree_nodes %>% head()
    
  a0 = as.numeric(rownames(fit.rt()$model$frame)); a0
  a1 = seq(1:nrow(fit.rt()$model$frame)); a1 
  a2 = as.vector(fit.rt()$model$where)
  node_num = a2
  for (i1 in 1:nrow(tree_nodes)){
    node_num[i1] = a0[a2[i1]]
  }
  
  tree_nodes1 <- fit.rt()$model$where %>% as.data.frame() %>% 
  cbind(node_num) %>% dplyr::select("node_num")
  tree_nodes1
  
  })

  output$nodesout = renderPrint({
    head(nodes1(),15)
  })
  
  output$downloadData3 <- downloadHandler(
    filename = function() { "Nodes Info.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      dft = data.frame(row_numer = row.names(nodes1()), nodes1())
      write.csv(dft, file, row.names=F, col.names=F)
    }
  )
  
  prediction = reactive({
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val = predict(pr, newdata = Dataset.Predict(),type="response")
      
    } 
    else {
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val = predict(pr, newdata = Dataset.Predict())
      
    }
    
    out = data.frame(Yhat = val, pred.readdata())
    return(out)    
    
  })
  
  output$prediction =  renderPrint({
    if (is.null(input$filep)) {return(NULL)}
    head(prediction(),10)
  })
  
  #------------------------------------------------#
  output$downloadData1 <- downloadHandler(
    filename = function() { "Predicted Data.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      write.csv(prediction(), file, row.names=F, col.names=F)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() { "beer data.csv" },
    content = function(file) {
      write.csv(read.csv("data/beer data.csv"), file, row.names=F, col.names=F)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "beer data - prediction sample.csv" },
    content = function(file) {
      write.csv(read.csv("data/beer data - prediction sample.csv"), file, row.names=F, col.names=F)
    }
  )
  
  
  
  
  })