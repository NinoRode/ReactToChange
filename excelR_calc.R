library(shiny)
library(excelR)

wb_df <- data.frame(DAT = rep(NaN, 7), dan = character(7), ODSOT = character(7),
                    noc00_06z = rep(NaN, 7),  noc00_06k = rep(NaN, 7), 
                    ur00_06 = rep(NaN, 7),  dop06_16z = rep(NaN, 7), 
                    dop06_16k = rep(NaN, 7), ur06_16 = rep(NaN, 7),
                    pop16_22z = rep(NaN, 7), pop16_22k = rep(NaN, 7), 
                    ur16_22 = rep(NaN, 7), noc22_24z = rep(NaN, 7),
                    noc22_24k = rep(NaN, 7), ur22_24 = rep(NaN, 7)
                    )

# row.names(wb_df) <- c("ponedeljek", "torek","sreda", "četrtek", "petek", "sobota", "nedelja")
row.names(wb_df) <- c("Po", "To","Sr", "Če", "Pe", "So", "Ne")
wb_df$dan <- c("ponedeljek", "torek","sreda", "četrtek", "petek", "sobota", "nedelja")

# df <- data.frame(beginning = c(NaN, NaN, NaN), ending = c(NaN, NaN, NaN))
# df$calculate = df$ending - df$beginning

shinyApp(
  ui = fluidPage(excelOutput("table")),
  server = function(input, output, session) {
    output$table <-
      renderExcel(excelTable(data = wb_df))
    observeEvent(input$table,{
      df <- excel_to_R(input$table)
      
      wb_df[4:15] <- sapply(df[4:15], as.numeric)

      wb_df$ur00_06 <- wb_df$noc00_06k - wb_df$noc00_06z
      wb_df$ur06_16 <- wb_df$dop06_16k - wb_df$dop06_16z
      wb_df$ur16_22 <- wb_df$pop16_22k - wb_df$pop16_22z
      wb_df$ur22_24 <- wb_df$noc22_24k - wb_df$noc22_24z
      
      print(wb_df)

      output$table <-
        renderExcel(excelTable(data = wb_df))
    })
  }
)

