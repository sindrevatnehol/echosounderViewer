


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
  

  ######################################################
  #Set initial values
  ######################################################
  values <- reactiveValues()
  values[['NMDinfo']] <- getNMDinfo('cs')
  values[['Source']]<- '//ces.imr.no/mea/2018_Redus/echosounderViewer'
  
  
  # data.table(Timeseries<-)
  
  # 
  # dir_source <- '//ces.imr.no/mea/2018_Redus/echosounderViewer'
  # 
  # load(file='//ces.imr.no/mea/2018_Redus/echosounderViewer/official_paths/paths.RDa')
  # 
  # psw <- read.csv2(file='//ces.imr.no/mea/2018_Redus/echosounderViewer/admin/admin.txt',sep=';',header = T)
  # 
  # 
  # print(values[['NMDinfo']])
  
  
  
  
  
  ######################################################
  #ObservEvent: 
  #     Set what to do when a timeseries is selected
  ######################################################
  observeEvent(input$timeseries, 
               {
                 
                 
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
                   print(i)
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
  
  
  
  #Case when year is selected
  observeEvent(input$years, {
    
    y<-values[['NMDinfo']][names(values[['NMDinfo']])==input$timeseries][[1]]
    values[['Vessel']]<-paste(y[y$Year==input$years,]$ShipName,y[y$Year==input$years,]$Cruise,sep=',')
    
    #Update list of avaliable vessels
    updateSelectInput(session, "vessel",
                      label = paste("Number of vessels", length(values[['Vessel']])),
                      choices = values[['Vessel']],
                      selected = tail(values[['Vessel']], 1))
  })
  
  
  
  
  
  
}




shinyApp(ui = ui, server = server)