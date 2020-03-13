


library(shiny)
library(stringi)
library(Rstox)
library(stringi)
library(data.table)
library(DT)
library(tools)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(dplyr)



ui <- fluidPage(
  
  #title menu
  titlePanel( 
    fluidRow( 
      column(4, "NMDechosounderBrowser"),
      column(4, offset = 0, img(height = 50, width = 50, src = "E:/Arbeid/Koding/NMDataBrowser/Logo/sea2data.png"))
    )),
  
  
  
  
  sidebarPanel(
    selectInput(inputId = 'timeseries',label = 'TimeSeries',choices = names(getNMDinfo('cs'))),
    
    
    selectInput(inputId = 'years',label = 'Year',choices = '-'),
    
    
    selectInput(inputId = 'vessel',label = 'vessel',choices = '-'),
    
    
    actionButton("officialbutton", "Set official"),
    
    
    actionButton("downloadbutton", "Download official data"),
    br()
    
    
  )
  ,mainPanel(
    tabsetPanel(
      id = 'Test',
      tabPanel("Overall", DT::dataTableOutput("MyOverallTable")),
      tabPanel("EK", DT::dataTableOutput("mytable2")),
      tabPanel("Work/snap", DT::dataTableOutput("mytable3")),
      tabPanel("lsss", DT::dataTableOutput("mytable4"))
    )
  )
  
  # * OUTOUT()
)






server <- function(input, output,session) {
  
  #Function to fix checkbox 
  shinyInput <- function(FUN,id,num,checked) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,value = checked[i],width=1))
    }
    inputs
  }
  
  
  rowSelect_EK <- reactive({
    rows=names(input)[grepl(pattern = "EKrows_",names(input))]
    paste(unlist(lapply(rows,function(i){
      if(input[[i]]==T){
        return(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
      }
    })))
  })
  

  ######################################################
  #Set initial values
  ######################################################
  values <- reactiveValues()
  values[['NMDinfo']] <- getNMDinfo('cs')
  values[['Source']]<- '//ces.imr.no/mea/2018_Redus/echosounderViewer'
  
  load(file='//ces.imr.no/mea/2018_Redus/echosounderViewer/official_paths/paths.RDa')
  
  values[['official_paths']]<-official_paths
  
  ####
  # This needs to be more secure
  ###
  # values[['psw']]<- read.csv2(file='//ces.imr.no/mea/2018_Redus/echosounderViewer/admin/admin.txt',sep=';',header = T)
  
  # 
  # dir_source <- '//ces.imr.no/mea/2018_Redus/echosounderViewer'
  # 
  # load(file='//ces.imr.no/mea/2018_Redus/echosounderViewer/official_paths/paths.RDa')
  # 
  # 
  # 
  # print(values[['NMDinfo']])
  
  
  
  
  
  ######################################################
  #ObservEvent: 
  #     Set what to when pressing the official button
  ######################################################
  observeEvent(input$officialbutton, {
    
    showModal(modalDialog(
      inputId = "loginDialog",
      title = "Autenticate",
      textInput("userName", "User Name:"),
      passwordInput("password", "Password:"),
      br(),
      actionButton("Login", "Log in"),
      easyClose = TRUE
    ))
    
  })
  
  
  
  ######################################################
  #Function to do something when accessing official management
  ######################################################
  runOfficialManagement <- function(){
    
    #open dialog window for selecting official data
    showModal(modalDialog(
      
      #How to display
      inputId = "setOfficialDialog",
      title = "Set official data paths",
      size='l',
      
      actionButton("savebutton", "Save official"),
      
      mainPanel(
        tabsetPanel(
          id = 'officialTab',
          tabPanel("EK", DT::dataTableOutput("officialtableEK")),
          tabPanel("Work/snap", DT::dataTableOutput("officialtableWORK")),
          tabPanel("lsss", DT::dataTableOutput("officialtableLSSS"))
        )),
      br(),
      easyClose = TRUE
    ))
    
    
    sub_data <- values[['NMDinfo']][input$timeseries]
    
    #grab mapped files
    nmdfiles <- values[['NMDfiles']]
    
    
    #Filter the nmdfiles
    nmdfiles<-nmdfiles[file_ext(nmdfiles$files)=='raw',]
    
    
    #Get all folders with raw files
    nmdfiles$raw_folders <- (lapply(nmdfiles$files, function(x) (dirname(as.character(x)))))
    
    #Reorder for display        
    nmdfiles1<-unique(nmdfiles[c('Year',"Cruise",'raw_folders')])
    
    #Stack into table
    tableoffEK <- data.table(nmdfiles1)
    
    #bugfix        
    tableoffEK$raw_folders<-gsub("\\", "/", tableoffEK$raw_folders, fixed=TRUE)
    
    #Grab official info
    off_path <- values[['official_paths']][values[['official_paths']]$TimeSeries==input$timeseries,]
    off_path<-off_path[off_path$Link=='RAW']
    
    
    #Stack if official in table
    tableoffEK$Official <- FALSE
    tableoffEK$Official[tableoffEK$raw_folders%in%off_path$Path]<-TRUE
    
    #Reorder for display
    tableoffEK<-tableoffEK[,c('Official','Year','Cruise','raw_folders')]
    
    #Filter for testing!!!!!!!!
    tableoffEK<-tableoffEK[tableoffEK$Year%in%c('2019','2018'),]
    
    values[['tableoffEK']]<-tableoffEK


    output$officialtableEK <- DT::renderDataTable({
      #Display table with checkbox buttons
      DT::datatable(cbind(Pick=shinyInput(checkboxInput,"EKrows_",nrow(tableoffEK),checked=tableoffEK$Official), tableoffEK),
                    options = list(orderClasses = TRUE,
                                   lengthMenu = c(5, 25, 50),
                                   pageLength = 25 ,

                                   drawCallback= JS(
                                     'function(settings) {
                                   Shiny.bindAll(this.api().table().node());}')
                    ),selection='none',escape=F)
    })


  }
  
  
  
  ######################################################
  #ObservEvent: 
  #     Set what to when pressing save button
  ######################################################
  observeEvent(input$savebutton,{
    
    ####NEED to fix bug that is related to user experience
    
    
    official_paths<-(values[['official_paths']][ !((official_paths$TimeSeries%in%input$timeseries)&(official_paths$Year%in%values[['tableoffEK']][!as.integer(rowSelect_EK()),]$Year
    )&(official_paths$Cruise%in%values[['tableoffEK']][!as.integer(rowSelect_EK()),]$Cruise
    )&(official_paths$Link=='RAW')&(official_paths$Path%in%values[['tableoffEK']][!as.integer(rowSelect_EK()),]$raw_folders)), ])
    
    
    values[['official_paths']]<-official_paths
    
    new_off <-data.table(TimeSeries=rep(input$timeseries,length(rowSelect_EK())),
                         Year=values[['tableoffEK']][as.integer(rowSelect_EK()),]$Year,
                         Cruise=values[['tableoffEK']][as.integer(rowSelect_EK()),]$Cruise,
                         Link=rep('RAW',length(rowSelect_EK())),
                         Path=values[['tableoffEK']][as.integer(rowSelect_EK()),]$raw_folders)
    
    
    official_paths<-as.data.table(unique(rbind(values[['official_paths']],new_off)))
    
    
    values[['official_paths']]<-official_paths
    
    save(official_paths,file='//ces.imr.no/mea/2018_Redus/echosounderViewer/official_paths/paths.RDa')
    
    #Remove login
    removeModal()
    
  })
  
  
  
  ######################################################
  #ObservEvent: 
  #     Set what to when entering password
  ######################################################
  observeEvent(input$Login,{
    
    #Filter out usernames and password
    sub_psw<- (values[['psw']][values[['psw']]$timeseries==input$timeseries,])
    sub_psw<-sub_psw[sub_psw$username==input$userName,]
    sub_psw<-sub_psw[sub_psw$password==input$password,]
    
    
    #Check if there is atleast one username for this. 
    #If so continue
    if(is.null(sub_psw)){
      
      #Remove login
      removeModal()
      
      runOfficialManagement()
      
      # input$
      }else{
        
        #remove old modal dialog window
        removeModal()
        
        
        #Open a new dialog window
        showModal(modalDialog(
          inputId = "loginDialog",
          title = "Autenticate: Wrong username og password for this timeseries",
          textInput("userName", "User Name:"),
          passwordInput("password", "Password:"),
          br(),
          actionButton("Login", "Log in"),
          easyClose = TRUE
        ))
        
      }
    
    })
  
  
  
  ######################################################
  #ObservEvent: 
  #     Set what to do when a timeseries is selected
  ######################################################
  observeEvent(input$timeseries,{
                 
                 
                 #load mapped data paths
                 load(file.path(values[['Source']],'mapped_paths',paste0(input$timeseries,'.RDa')))
                 
                 #Store data from mapping
                 values[['NMDfiles']]<-NMDfiles
                 
                 #Grab number of years
                 values[['survey_years']]<-as.character(values[['NMDinfo']][names(values[['NMDinfo']])==input$timeseries][[1]]$Year)
                 
                 #Grab year range
                 values[['all_years']]<-as.character(c(min(as.integer(values[['survey_years']])):max(as.integer(values[['survey_years']]))))
                 
                   
                 #Update the selection of years
                 updateSelectInput(session, "years",
                                   label = paste("Number of years", length(unique(values[['survey_years']]))),
                                   choices = values[['survey_years']],
                                   selected = tail(values[['survey_years']], 1))


                 #Store the survey overall table
                 values[['OVerall_table']]<-values[['NMDinfo']][names(values[['NMDinfo']])==input$timeseries][[1]]

                 #Fix missing years
                 missing_year <- data.table('Year'=values[['all_years']][!values[['all_years']]%in%unique(values[['OVerall_table']]$Year)])
                 missing_year$code <- NA
                 missing_year$Cruise <- NA
                 missing_year$ShipName <- NA
                 values[['OVerall_table']]<-rbind(values[['OVerall_table']],missing_year)
                 

                 #Merge with info from mapped files
                 values[['OVerall_table']]<-merge(values[['OVerall_table']],unique(values[['NMDfiles']][c('Year','CruiseFolder','Cruise')]),by=c('Year','Cruise'),all=T)

                 
                 #Process to see how many raw files in cruise folders
                 num_raw <- c()
                 for( i in 1:nrow(values[['OVerall_table']])){
                   data_subset <- subset(NMDfiles , Year==values[['OVerall_table']][i,]$Year & CruiseFolder==values[['OVerall_table']][i,]$CruiseFolder)
                   num_raw <- c(num_raw,sum(stri_detect_fixed(data_subset$files,'.raw')))
                 }
                 values[['OVerall_table']]$RAW <- num_raw


                 #Add numer of raw files to table
                 values[['OVerall_table']]<-values[['OVerall_table']][ ,colnames(values[['OVerall_table']]) %in% c('Year','Cruise','ShipName','CruiseFolder','RAW'), with=FALSE]


                 #Display table in GUI
                 output$MyOverallTable <- DT::renderDataTable({
                   DT::datatable(values[['OVerall_table']])})
               })
  
  
  
  ######################################################
  #ObservEvent: 
  #     Set what to do when a years is selected
  ######################################################
  observeEvent(input$years, {
    
    y<-values[['NMDinfo']][names(values[['NMDinfo']])==input$timeseries][[1]]
    values[['Vessel']]<-paste(y[y$Year==input$years,]$ShipName,y[y$Year==input$years,]$Cruise,sep=',')
    
    #Update list of avaliable vessels
    updateSelectInput(session, "vessel",
                      label = paste("Number of vessels", length(values[['Vessel']])),
                      choices = values[['Vessel']],
                      selected = tail(values[['Vessel']], 1))
  })
  
  
  
  
  
  ######################################################
  #ObservEvent: 
  #     Set what to do when a vessel is selected
  ######################################################
  observeEvent(input$vessel, {
    
    
    #######################################
    #     Make table for folders with .raw files
    #######################################
    
    #Filter the nmdfiles
    nmdfiles<-values[['NMDfiles']][values[['NMDfiles']]$Year==input$years,]
    
    #Filter out all .raw files
    nmdfiles<-nmdfiles[nmdfiles$Cruise==as.integer(strsplit(input$vessel,split = ',')[[1]][2]),]
    nmdfiles<-nmdfiles[file_ext(nmdfiles$files)=='raw',]

    #Get all folders with raw files
    raw_folders <- unique(lapply(nmdfiles$files, function(x) (dirname(as.character(x)))))

    #Bug fix
    raw_folders<-gsub("\\", "/", raw_folders, fixed=TRUE)

    #Stack into table
    table1 <- data.table('Survey' = input$vessel,'Raw_Paths'=raw_folders)
    
    #Grab official info
    off_path <- values[['official_paths']][values[['official_paths']]$TimeSeries==input$timeseries,]
    off_path<-off_path[off_path$Year == input$years]
    off_path<-off_path[off_path$Cruise == as.integer(strsplit(input$vessel,split = ',')[[1]][2])]
    off_path<-off_path[off_path$Link =='RAW']

    #Stack if official in table
    table1$Official <- table1$Raw_Paths%in% off_path$Path

    #update table output
    output$mytable2 <- DT::renderDataTable({
      DT::datatable(table1)})

    
    
    



    #######################################
    #     Make table for Work folders
    #######################################
    table3 <- data.table('WORK_folders'=list.dirs(file.path(values[['Source']],'work',input$timeseries,input$years,as.integer(strsplit(input$vessel,split = ',')[[1]][2]),'WORK'),full.names = T,recursive = F))

    #Grab official info
    off_path <- values[['official_paths']][values[['official_paths']]$TimeSeries==input$timeseries,]
    off_path<-off_path[off_path$Year == input$years]
    off_path<-off_path[off_path$Cruise == as.integer(strsplit(input$vessel,split = ',')[[1]][2])]
    table3$Official <- table3$WORK_folders%in% off_path$PathWork

    output$mytable3 <- DT::renderDataTable({
      DT::datatable(table3)})







    #######################################
    #     Make table for lsss files
    #######################################
    table4 <- data.table('LSSS_files'=list.files(file.path(values[['Source']],'work',input$timeseries,input$years,as.integer(strsplit(input$vessel,split = ',')[[1]][2]),'LSSS'),full.names = T))

    #Grab official info
    off_path <- values[['official_paths']][values[['official_paths']]$TimeSeries==input$timeseries,]
    off_path<-off_path[off_path$Year == input$years]
    off_path<-off_path[off_path$Cruise == as.integer(strsplit(input$vessel,split = ',')[[1]][2])]
    table4$Official <- table4$LSSS_files%in% off_path$lsss_file

    output$mytable4 <- DT::renderDataTable({
      DT::datatable(table4)})
    
  })
  
  
  
  
  
}




shinyApp(ui = ui, server = server)