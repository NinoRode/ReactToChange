library(shiny)
library(rhandsontable)
library(openxlsx)
library(data.table)

saveXcllWb <- function(OA, mes_df, file_name) {
  meseca <- c("januarja", "februarja", "marca", "aprila", "maja", "junija", "julija", "avgusta", "septrembra", "oktobra", "novembra", "decembra")

  ## set default border Colour and style

  wb <- createWorkbook()
  options("openxlsx.borderColour" = "#4F80BD")
  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.halign" = "center")
  options("openxlsx.borderStyle" = "thin")
  modifyBaseFont(wb, fontSize = 10, fontName = "Arial Narrow")

  tabHeadStyle <- createStyle(halign = "center", borderStyle = "thin", textDecoration = "bold",
                              border = "bottom")
  tabStyle <- createStyle(halign = "center")
  tabFootStyle <- createStyle(halign = "center", borderStyle = "thin", textDecoration = "bold",
                              border = "top")
  infoStyle <- createStyle(textDecoration = "bold")

  addWorksheet(wb, sheetName = paste(OA, Sys.Date()))

  writeData(wb, sheet = 1, x = t(c("Uporabnik:", "Matjaž Metelko")),
            startCol = 1,
            startRow = 1,
            colNames = FALSE, rowNames = FALSE
  )


  writeData(wb, sheet = 1, x = "Urnik dela:",
            startCol = 2,
            startRow = 3,
            colNames = FALSE, rowNames = FALSE
  )

  writeData(wb, sheet = 1, x = c("od: ", "do: "),
            startCol = 3,
            startRow = 3,
            colNames = FALSE, rowNames = TRUE
  )

  od_dne <- paste(format(mes_df$dat[1], "%d."), meseca[as.numeric(format(mes_df$dat[1], "%m"))])
  do_dne <- paste(format(mes_df$dat[7], "%d."), meseca[as.numeric(format(mes_df$dat[7], "%m"))])
  writeData(wb, sheet = 1, x = c(od_dne, do_dne),
            startCol = 4,
            startRow = 3,
            colNames = FALSE, rowNames = TRUE
  )


  writeData(wb, sheet = 1, x = c(format(mes_df$dat[1], "%Y"), format(mes_df$dat[7], "%Y")),
            startCol = 5,
            startRow = 3,
            colNames = FALSE, rowNames = TRUE
  )

  writeData(wb, sheet = 1, x = t(c("za:", OA)),
            startCol = 2,
            startRow = 6,
            colNames = FALSE, rowNames = TRUE
  )

  writeDataTable(wb, sheet = 1, x = mes_df[-1],
                 startCol = 1,
                 startRow = 8,
                 colNames = TRUE, rowNames = FALSE,
                 withFilter = FALSE,
                 firstColumn = TRUE
  )

  writeData(wb, sheet = 1, x = t(c("Skupaj:", "", "", sum(mes_df$ure, na.rm = TRUE))),
            startCol = 2,
            startRow = 16,
            colNames = FALSE, rowNames = TRUE
  )

  addStyle(wb, sheet = 1, infoStyle, rows = 1:6, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, sheet = 1, tabHeadStyle, rows = 8, cols = 1:6)
  addStyle(wb, sheet = 1, tabFootStyle, rows = 16, cols = 1:6)

  # setColWidths(wb, sheet = 1, cols = 1:6, widths = "auto")
  setColWidths(wb, sheet = 1, cols = 1:6, widths = c(11, 14, 10, 10, 10, 10))


  saveWorkbook(wb, file = file_name, overwrite = TRUE)
}

saveXcllWb(OA, mes_df)

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
  mes_df$prihod <- c(rep(8, 5), NA, NA)
  mes_df$odhod <- c(rep(14, 5), NA, NA)
  mes_df$ure <-  mes_df$odhod -  mes_df$prihod
  mes_df$ure[is.na(mes_df$ure)] <-  0
  mes_df$odsotnost <- factor(c(rep("-", 5), "SO", "NE"), levels = c("-", "SO", "NE", "P", "D", "B", "ID", "DD", "IZ"),
                             labels = c("-", "sobota", "nedelja", "praznik", "dopust", "bolniška", "izredni dopust", "dodatni dopust", "izobraževanje"),
                             ordered = TRUE)
  ne_dela <- c("dopust", "bolniška", "izredni dopust", "dodatni dopust", "izobraževanje")


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

        row.no <- as.numeric(unlist(input$hot$changes$changes)[1])
        col.no <- as.numeric(unlist(input$hot$changes$changes)[2])
        new.val <- unlist(input$hot$changes$changes)[4]
        #If the changed value is mpg or cyl
        if(is.numeric(new.val) && (col.no == 2 || col.no == 3)){
          datacopy[(row.no+1), 5] <- datacopy[(row.no+1), 4] - datacopy[(row.no+1), 3]
        }
        else {
          datacopy[(row.no+1), 5] <- NA
        }

        if(new.val %in% ne_dela) {
          datacopy[(row.no+1), 4] <- NA
          datacopy[(row.no+1), 3] <- NA
        }
      }
    }

    datacopy

  })

  output$hot=renderRHandsontable({

    rhandsontable(za_teden())

  })
  observeEvent(input$enter, {

    mes_df <- cbind(mes_df$dat, hot_to_r(input$hot))
    names(mes_df)[1] <- "dat"
    ime <- isolate(unlist(strsplit(input$OA, " ")))
    file_name <- gsub(" ", "", paste(getwd(), "/", ime[1], "_", ime[2], "_", isolate(input$teden)))

    saveXcllWb(isolate(input$OA), mes_df, file_name)

  })
}


shinyApp(ui = ui, server = server)

