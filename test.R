library(shiny)
df <- read.csv('/Users/yokk/Desktop/2ndcar/2ndhandcar.csv', sep = ",") #load csv file from local
df$model <- tolower(df$model) #lowercase text in df$model
drops <- c("X...") #remove index column
df<-df[ , !(names(df) %in% drops)]#remove index column


ui<- fluidPage(
  theme = "2ndhandcar.css",
  # Fluidrow() divided app into rows # new row
    # column( width,offset ) 
      ## Width::no matter how the screen wide it will distributed into 12
      ## Width:: therefore, number of widthe will be t 1-12
      ## offset :: the location of the column
  (h1("CAR PRICE IN THB")),
  sidebarLayout(sidebarPanel( wellPanel(uiOutput('selectmake'),
                   uiOutput('selectmodel'),
                   uiOutput("selecttransmission"),
                    uiOutput("yearrange"),
                    uiOutput("mileagerange"),
                   downloadButton("downloadfile", "Download")
                   )#closed wellPanel
                  ) #closed sidebarPanel
                ,mainPanel(
                  tabsetPanel(
                    tabPanel("Data Table",dataTableOutput("printtable"))
                    ,tabPanel("Plot", plotOutput("printplot"))
                    )#closed tabsrtPanel
                  )#closed mainPanel
            )#closed sidebarLayout
)#closed fluidPage

#####
server<-function(input,output){
  ############## render drop downlist for select make#################
  output$selectmake<-renderUI(selectInput("selectmake","Choose Brand",
                                          choices=sort(c("All",as.character(unique(df$make)))),
                                          selected = "All"))
  
  ############## render drop downlist for select model#################
  output$selectmodel <-renderUI(
    
    if (input$selectmake=="All"){
      selectInput("selectmodel","Choose Model", choices = c("All",sort(as.character(unique(df$model)))))
    } 
    else if (input$selectmake =="Toyota"){
      sdata<-df[df$make==input$selectmake,]
      selectInput("selectmodel","Choose Model", choices = c("All",sort(as.character(unique(sdata$model)))))
    }
    else if(input$selectmake =="Mercedes-Benz"){
      sdata<-df[df$make==input$selectmake,]
      selectInput("selectmodel","Choose Model", choices = c("All",sort(as.character(unique(sdata$model)))))
      
    })        
  ############## render radiobutton for select transmission#################
  output$selecttransmission <-renderUI(radioButtons("selecttransmission","Transmission"
                                                    , choices = c("All",as.character(unique(df$transmission)))))
  
  ############## render slider for select year& mileage#################
  #### min value is the minimum year in dataframe
  #### max value is the maximum year in dataframe
  #### value is an initial value of range
  #### steps the gap between interval
  output$yearrange <-renderUI(sliderInput("yearrange","Year", min=min(df$year),max=max(df$year)
                                          ,value = df$year,step = 1,sep=''))
  
  output$mileagerange<-renderUI(sliderInput("mileagerange", "Mileage", min=min(df$mileage),max=max(df$mileage)
                                            ,value = df$mileage, step=500))
  
  ############## render Text to summarise the chosen value#################
  #### reactive is to reactive evrytime selected value changes
  #### then call the rendertext
  formulartext<-reactive({
    paste("Showing ", input$selectmake," Model ", input$selectmodel," Transmission ", input$selecttransmission)
  })
  output$printtext<-renderText({formulartext()})
  
  ############## all selected value will be save in ans#################
  ans<-reactive({
    if(input$selectmake !="All"){
      df<-df[df$make==input$selectmake,]
    }
    if(input$selectmodel !="All"){
      df<-df[df$model==input$selectmodel,]
    }
    if(input$selecttransmission !="All"){
      df<-df[df$transmission==input$selecttransmission,]
    }
    if(!is.null(input$yearrange)){
      df<-df[df$year>=input$yearrange[1]&df$year<=input$yearrange[2],]
    }
    
    if(!is.null(input$mileagerange)){
      df<-df[df$mileage>=input$mileagerange[1]&df$mileage<=input$mileagerange[2],]
    }
    df})
  
  output$printtable <- renderDataTable({
    ans()# called allvalue got from ans()
  })
  
  ############how to download file from datatable
  
  ### Downloadable csv of selected dataset ----
  output$downloadfile <- downloadHandler(
    filename = function() {
      paste(input$selectmake,'_',Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ans(),file)
    }
  )
  
}

shinyApp(ui,server)