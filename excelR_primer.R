library(shiny)
library(excelR)

shinyApp(
  ui = fluidPage(excelOutput("table")),
  server = function(input, output, session) {
    output$table <-
      renderExcel(excelTable(data = head(iris)))
  }
)

shinyApp(
  ui = fluidPage(excelOutput("table")),
  server = function(input, output, session) {
    output$table <-
      renderExcel(excelTable(data = head(iris)))
    observeEvent(input$table,{
      table_data <- excel_to_R(input$table)
      if(!is.null(table_data)){
        print(table_data)
      }

    })
  }
)
