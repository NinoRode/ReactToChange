library(shiny)
library(rhandsontable)

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
  mes_df[is.na(mes_df)] <- ""

  output$hot=renderRHandsontable(hot_col(rhandsontable(mes_df, readOnly=F), "odsotnost", allowInvalid = FALSE))

  observeEvent(input$enter, {
    mes_df=hot_to_r(input$hot)
    print(mes_df)
  })
}


shinyApp(ui = ui, server = server)

