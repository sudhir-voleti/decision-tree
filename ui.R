###########################################################
#           Regression Tree App (ui)                #
###########################################################
library("shiny")
# library("randomForest")

shinyUI(
  fluidPage(
    
    # titlePanel("Regression Tree"),
    titlePanel(title=div(img(src="logo.png",align='right'),"Decision Tree App")),
    
    sidebarLayout(
      
      sidebarPanel(
        # Upload data:
        h4(p(" Data Input")),
        fileInput("file", "Upload Model Training data in csv"),
        sliderInput('sample','Validation Sample Proportion',10,50,30),
        # h4(p("Select Response Variable")),
        sliderInput('cp','Complexity Parameter',0,1,0.01),
        fileInput("filep", "Upload Prediction data in csv"),
        htmlOutput("yvarselect"),
        htmlOutput("xvarselect"),
        htmlOutput("fxvarselect")
      ),   # end of sidebar panel
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Overview",
                             h4(p("How to use this application")),
                             p("This application requires a data input from the user. To do so, click on the Browse (left side-bar panel) and upload the csv file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have a csv file, first convert your data into csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                             p("Once the file is uploaded successfully, variables in the data file will reflect on the left-side Data Selection Panel. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just delete that variable and it will be dropped from the model.
                            ",align="justify"),
                             p('You can also adjust the complexity parameter in regression tree model. Default value of complexity parameter is "0.01". You can adjust the validation sample proportion from the slider in left sidebar panel. Validation sample will be selected from the input data set. If you have a similar data set on which you want to make the prediction based on regression tree, You can upload that data set in left side bar panel. Please note that prediction data should have all explanatory variables similar to model data.',align="justify"),
                             br()
                    ),
                    tabPanel("Data Download",
                             
                             h4(p("Download Sample Input Files")),
                             # br(),
                             downloadButton('downloadData03', 'Download US insurance premiums CSV file.'),
                             br(),
                             br(),
                             downloadButton('downloadHR_predictions', 'Download HR Analytics prediction input file.'),
                             br(),
                             br(),
                             downloadButton('downloadHR_train', 'Download HR analytics training input file.'),   
                             br(),
                             br(),
                             downloadButton('downloadTelecomPrediction', 'Download Telco Customer input input file.'),
                             br(),
                             br(),
                             downloadButton('downloadTelecomTrain', 'Download Telco Customer training input file.'),
                             br(),
                             br(),
                             downloadButton('downloadHousingTrain', 'Download Housing Data training input file.'),
                             br(),
                             br(),
                             downloadButton('downloadHousingPredict', 'Download Housing Data Prediction input file.'),
                             br(),
                             br(),
                             downloadButton('downloadData', 'Download model training input file.'),
                             br(),
                             br(),
                             downloadButton('downloadData2', 'Download prediction input file.'),
                             br()
                      
                    ),
                    tabPanel("Data Summary",
                             h4(p("Uploaded Data (Top-5 rows)")),
                             DT::dataTableOutput("sample_data"),
                             hr(),
                             h4(p("Summary report")),
                             verbatimTextOutput('summarydata')),
                    tabPanel("Model Summary",
                             h4('Model Result Summary'),
                             verbatimTextOutput("results"),
                             plotOutput('results_plot',height = 400, width = 600),
                             h4('Variable importance'),
                             verbatimTextOutput('imp'),
                             plotOutput('var_imp_plot',height = 400, width = 600),
                             
                    ),
                    
                    
                    
                    tabPanel("Decision Tree",
                             # h4('Visualize cross-validation results'),
                             # plotOutput("plot1",height = 600, width = 850),
                             # h4('Regression Tree'),
                             visNetwork::visNetworkOutput("plot3",height = 600, width = 850),
                             hr(),
                             h4('Detailed summary of splits'),
                             verbatimTextOutput("mod_sum"),
                             h4('Leaf node for each data point'),
                             DT::dataTableOutput("split_summ")
                    ),
                    
                    # tabPanel("Node labels",
                    #          plotOutput("plot2",height = 600, width = 850),
                    #          h4("First 15 rows node number from model training data"),
                    #          verbatimTextOutput("nodesout"),
                    #          br(),
                    #          h4("Download nodes data from model training data"),
                    #          br(),
                    #          downloadButton('downloadData3','Download nodes data (Works only in browser)')
                    #          ),
                    # tabPanel("Random Forest",verbatimTextOutput('rfimp')),
                    
                    tabPanel("Model Performance",
                             
                             h4('Validation Result Summary'),
                             #verbatimTextOutput("validation"),
                             verbatimTextOutput("validation1"),
                             plotOutput("validation0"),
                             plotOutput('plot_pred')
                             # h4('Detailed summary of splits'),
                             # verbatimTextOutput("summary")
                    ),    
                    tabPanel("Prediction",br(),
                             h4("First 10 rows of predicted data"),
                             p('"Yhat" column is the predicted value.'),
                             dataTableOutput('prediction'),
                             h4("Download Predicted data"),
                             downloadButton('downloadData1', 'Download Predicted data (Works only in browser)')
                    )
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI

