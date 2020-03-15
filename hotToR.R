library(shiny)
library(rhandsontable)
library(data.table)

ui = shinyUI(fluidPage(
  selectInput("OA", "Izberi asistentko:", choices = list("Lucija Metelko", "Ana Ljubi")),
  dateInput("teden", "Izberi teden:",
            value = Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
            format = "DD, dd. M. yyyy",
            language = "sl",
            weekstart = 1),
  fluidRow(wellPanel(
    rHandsontableOutput("hot"),
    actionButton(inputId="enter",label="Shrani urnik")
  ))
))


server=function(input,output){

  zac_tedna <-  Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8
  mes_df <- data.frame("dat" = seq(zac_tedna, by = "day", length.out = 7))
  mes_df$dan <- weekdays( mes_df$dat)
  mes_df$datum <- as.character(mes_df$dat, "%e. %b. %Y")
  mes_df$zacetek <- c(rep(8, 5), NA, NA)
  mes_df$konec <- c(rep(14, 5), NA, NA)
  mes_df$odsotnost <- factor(c(rep("-", 5), "SO", "NE"), levels = c("-", "SO", "NE", "P", "D", "B", "ID", "DD", "IZ"),
                             labels = c("-", "sobota", "nedelja", "praznik", "dopust", "bolniška", "izredni dopust", "dodatni dopust", "izobraževanje"),
                             ordered = TRUE)
  mes_df$delovni_cas <-  mes_df$konec -  mes_df$zacetek



  # Calculation of columns from https://stackoverflow.com/questions/44074184/reactive-calculate-columns-in-rhandsontable-in-shiny-rstudio
  za_teden <- reactive({

    datacopy <- NULL

    #For initial data upload
    if(is.null(input$hot)){
      datacopy <- mes_df[, -1]
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

    rhandsontable(za_teden(), dateFormat = "L")

  })
  observeEvent(input$enter, {

    mes_df <- cbind(mes_df$dat, hot_to_r(input$hot))
    names(mes_df)[1] <- "dat"
    ime <- isolate(unlist(strsplit(input$OA, " ")))
    save_dir <- gsub(" ", "", paste(getwd(), "/", ime[1], "_", ime[2], "_", isolate(input$teden)))
    write.csv2(mes_df, save_dir)
  })
}


shinyApp(ui = ui, server = server)

