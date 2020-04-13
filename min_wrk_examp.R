library(shiny)
library(rhandsontable)

############################# UI #############################

ui = shinyUI(fluidPage(
  selectInput("tab", "Chose table: ", choices = list("table1", "table2")),
  dateInput("week", "chose date:",
            value = Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
            format = "DD, dd. M. yyyy",
            language = "sl",
            weekstart = 1),
  fluidRow(wellPanel(
    column(6,
           rHandsontableOutput("hot"),
           actionButton(inputId="enter",label="Shrani urnik")
    ),

    column(6,
           textOutput("title"),
           tableOutput("tabela")
    )))
))

########################## SEREVER ###########################

server=function(input,output, session){

  tab_change <- reactiveVal(FALSE)

  zac_tedna <-  reactive(input$week - as.numeric(format(input$week, "%u")) + 1) # postavi na začetek tedna (+1, ne +8)
  observe(
    updateDateInput(session, "week", value = zac_tedna())
  )

   file_name <- reactive(paste(input$tab, format(input$week, "-%d-%b"), ".csv", sep = ""))

   observe( if(!file.exists(file_name())) {
     tdn <- seq(Sys.Date()+1, by = "day", length.out = 7)
     df <-data.frame(day = weekdays(tdn+1),
                     datum = as.character(tdn, "%e. %b. %Y"),
                     beginning = as.numeric(rep(NA, 7)),
                     ending = as.numeric(rep(NA, 7)),
                     hours = as.numeric(rep(NA, 7)))

     write.csv2(df, file_name(), row.names = FALSE)
     df
   })

   rw <- reactiveFileReader(1000, session, file_name, read.csv2)
   react_week <- reactive({
     df <- as.data.frame(rw())
     df[3:5] <-  sapply(3:5, function (i) as.numeric(df[, i]))
     df
     })

  output$week <- renderTable(
    react_week())

  output$title <- renderText(input$tab)

  output$tabela <- renderTable(react_week())

  observeEvent(list(input$tab, input$week),
               tab_change(TRUE))


  # Calculation of columns from https://stackoverflow.com/questions/44074184/reactive-calculate-columns-in-rhandsontable-in-shiny-rstudio
  for_week <- reactive({

    datacopy <- NULL

    #For initial data upltabd
    if(isolate(tab_change()) || is.null(input$hot)) {
      datacopy <- react_week()
    }
    else {
      datacopy <- hot_to_r(input$hot)
    }

    #If there is change in data
    if(!is.null(input$hot$changes$changes)){

      # row.no <- as.numeric(unlist(input$hot$changes$changes)[1])
      col.no <- as.numeric(unlist(input$hot$changes$changes)[2])
      new.val <- unlist(input$hot$changes$changes)[4]

      #If the changed value is prihod or odhod
      if(col.no == 2 || col.no == 3){
        datacopy[, 5] <- as.numeric(datacopy[, 4]) - as.numeric(datacopy[, 3])
      }

    }

    tab_change(FALSE)

    datacopy

  })

  output$hot <- renderRHandsontable({
    rhandsontable(for_week())
  })

  observeEvent(input$enter, write.csv2(hot_to_r(input$hot), file_name(), row.names = FALSE))

}

shinyApp(ui = ui, server = server)

