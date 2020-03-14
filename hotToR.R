library(shiny)
library(rhandsontable)
library(data.table)

ui = shinyUI(fluidPage(
  fluidRow(wellPanel(
    rHandsontableOutput("hot"),
    actionButton(inputId="enter",label="enter")
  ))
))


server=function(input,output){

  zac_tedna <-  Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 1
  mes_df <- data.frame("datum" = seq(zac_tedna, by = "day", length.out = 7))
  mes_df$dan <- weekdays( mes_df$datum)
  mes_df$zacetek <- c(rep(8, 5), NA, NA)
  mes_df$konec <- c(rep(14, 5), NA, NA)
  mes_df$odsotnost <- factor(c(rep("-", 5), "SO", "NE"), levels = c("-", "SO", "NE", "P", "D", "B", "ID", "DD", "IZ"),
                             labels = c("-", "sobota", "nedelja", "praznik", "dopust", "bolniška", "izredni dopust", "dodatni dopust", "izobraževanje"),
                             ordered = TRUE)
  mes_df$delovni_cas <-  mes_df$konec -  mes_df$zacetek



  teden <- reactive({

    datacopy <- NULL

    #For initial data upload
    if(is.null(input$hot)){
      datacopy <- mes_df
      datacopy=data.table(datacopy)

    }else{
      datacopy = hot_to_r(input$hot)

      #If there is change in data
      if(!is.null(input$hot$changes$changes)){

        row.no <- unlist(input$hot$changes$changes)[1]
        col.no <- unlist(input$hot$changes$changes)[2]
        new.val <- unlist(input$hot$changes$changes)[4]
        #If the changed value is mpg or cyl
        if(col.no == 2 || col.no == 3){
          datacopy[(row.no+1), 6] = datacopy[(row.no+1), 4] - datacopy[(row.no+1), 3]
        }
      }
    }

    datacopy

  })

  output$hot=renderRHandsontable({

    rhandsontable(teden())

  })
  observeEvent(input$enter, {

    mes_df <- hot_to_r(input$hot)
    print(mes_df)
  })
}


shinyApp(ui = ui, server = server)

