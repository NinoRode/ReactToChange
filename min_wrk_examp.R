library(shiny)
library(rhandsontable)

data <- list (
  table1 = data.frame( beginning = as.numeric(rep(8, 4)),
                ending = as.numeric(rep(15, 4))),

table2 = data.frame( beginning = as.numeric(rep(9, 4)),
                ending = as.numeric(rep(17, 4)))
)

data[["table1"]]$hours <- data[["table1"]]$ending - data[["table1"]]$beginning
data[["table2"]]$hours <- data[["table2"]]$ending - data[["table2"]]$beginning

############################# UI #############################

ui = shinyUI(fluidPage(
  selectInput("tab", "Chose table: ", choices = list("table1", "table2")),
  fluidRow(wellPanel(
    column(6,
           rHandsontableOutput("hot"),
           actionButton(inputId="enter",label="Save")
    ),

    column(6,
           textOutput("title"),
           tableOutput("tabela")
    )))
))

########################## SEREVER ###########################

server=function(input,output, session){

  tab_change <- reactiveVal(FALSE)


   # rw <- reactivePoll(1000, session, file_name, read.csv2)
   react_week <- reactive({
     df <- data[[input$tab]]
     })

  output$title <- renderText(input$tab)

  output$tabela <- renderTable(react_week())

  observeEvent(input$tab,
               {tab_change(TRUE)
               })


  # Calculation of columns
  for_week <- eventReactive(list(input$tab, input$hot$changes$changes), {

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

      col.no <- as.numeric(unlist(input$hot$changes$changes)[2])
      new.val <- unlist(input$hot$changes$changes)[4]

      #If the changed value is prihod or odhod
      if(col.no == 0 || col.no == 1){
        datacopy[, 3] <- as.numeric(datacopy[, 2]) - as.numeric(datacopy[, 1])
      }

    }

    tab_change(FALSE)
    datacopy

  })

  output$hot <- renderRHandsontable(
    rhandsontable(for_week())
  )

  observeEvent(input$enter, {
    data[[input$tab]] <<- hot_to_r(input$hot)
    output$tabela <- renderTable( data[[input$tab]])
  })

}

shinyApp(ui = ui, server = server)

