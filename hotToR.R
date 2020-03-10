library(shiny)
library(rhandsontable)

ui = shinyUI(fluidPage(
  fluidRow(wellPanel(
    rHandsontableOutput("hot"),
    actionButton(inputId="enter",label="enter")
  ))
))


server=function(input,output){

  DF=data.frame(Code=c(1,2,3),Amount=c(NA,NA,NA))
  DF[is.na(DF)] <- ""

  output$hot=renderRHandsontable(rhandsontable(DF,readOnly=F))

  observeEvent(input$enter, {
    DF=hot_to_r(input$hot)
    DF[DF == ""] <- NA
    print(DF)
  })
}

shinyApp(ui = ui, server = server)

