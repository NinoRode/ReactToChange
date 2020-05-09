library(shiny)
library(rhandsontable)
# library(openxlsx)

############################# UI #############################

ui = shinyUI(
  fluidPage(
    fluidRow(
      column(4, allign = "center",
             # JS to calculate dimensions of thw window and display ratio
             # from: https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
             tags$head(tags$script('
                                var dimension = 0;
                                $(document).on("shiny:connected", function(e) {
                                    dimension = window.innerWidth
                                    Shiny.setInputValue("dimension", dimension, {priority: "event"});
                                });
                                $(window).resize(function(e) {
                                    dimension = window.innerWidth
                                   Shiny.setInputValue("dimension", dimension, {priority: "event"});
                                });
                            ')),


             textOutput("title"),
             verbatimTextOutput("dimension_display"),
      ),
      column(4, allign = "center",
             selectInput("OA", NULL, choices = list("Lucija Metelko", "Ana Ljubi"), width = "100%"),
      ),
      column(4, allign = "center",
             dateInput("teden", NULL,
                       value = Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
                       format = "DD, dd. M. yyyy",
                       language = "sl",
                       weekstart = 1,
                       width = "100%",
                       daysofweekdisabled = c(0, 2:6))
      )
    ),

    fluidRow(wellPanel(
      column(8, allign = "center",
             rHandsontableOutput("hot"),
             fluidRow(
               column(6, allign = "center",
                      actionButton(inputId="enter",label="Shrani urnik", width = "100%")
               ),
               column(6, allign = "center",
                      actionButton(inputId="do_report",label="Pripravi listo prisotnosti", width = "100%"),
               ))

      ),

      column(4,
             p("Ure:"),
             textOutput("sum_w_hours"),
             textOutput("sum_P_hours"),
             textOutput("sum_D_hours"),
             textOutput("sum_B_hours"),
             textOutput("sum_all_hours")
      )))
  ))

########################## SEREVER ###########################

server=function(input,output, session){

  display_dimensions <- reactive(input$dimension)
  output$dimension_display <- renderText(display_dimensions())

  default_df <- data.frame(dat = as.character(Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8, "%d. %b. %Y"),
                           pon_prihod = 0, pon_odhod = 0, pon_opomba = "prosto",
                           tor_prihod = 0, tor_odhod = 0, tor_opomba = "prosto",
                           sre_prihod = 0, sre_odhod = 0, sre_opomba = "prosto",
                           čet_prihod = 0, čet_odhod = 0, čet_opomba = "prosto",
                           pet_prihod = 0, pet_odhod = 0, pet_opomba = "prosto",
                           sob_prihod = 0, sob_odhod = 0, sob_opomba = "sobota",
                           ned_prihod = 0, ned_odhod = 0, ned_opomba = "nedelja",
                           stringsAsFactors = FALSE)

  build_teden <- function(df, display_width) {
    # Builds the table for display
    zac_tedna <- as.Date(df$dat,  "%d. %b. %Y")

    tdn <- seq(zac_tedna, by = "day", length.out = 7)
    teden_df <- data.frame("datum" = as.character(tdn, "%e. %b %Y"))
    if (display_width < 620) {
      teden_df$datum <- as.character(tdn, "%e. %m. ")
    }
    m <- list(df[grepl("prihod", names(df))], df[grepl("odhod", names(df))], df[grepl("opomba", names(df))])
    teden_df$prihod <- unlist(m[1])
    teden_df$prihod <- as.numeric(teden_df$prihod)

    teden_df$odhod <- unlist(m[2])
    teden_df$odhod <- as.numeric(teden_df$odhod)

    teden_df$ure <- teden_df$odhod - teden_df$prihod

    teden_df$opomba <- factor(unlist(m[3]),
                              levels = c("-", "prosto", "bolniška", "dopust",
                                         "sobota", "nedelja", "praznik",
                                         "izobraževanje"))
    rownames(teden_df) <- c("Po", "To", "Sr", "Če", "Pe", "So", "Ne")
    return(teden_df)
  }

  output$title <- renderText("Tabela za OA:")
  output$tabela <- renderText("Naslov Tabele")

  output$sum_w_hours <- renderText(c("delo: ", 0))
  output$sum_P_hours <- renderText(c("praznik: ", 0))
  output$sum_D_hours <- renderText(c("dopust: ", 0))
  output$sum_B_hours <- renderText(c("bolnioška: ", 0))
  output$sum_all_hours <-  renderText(c("Skupno", 0))

  za_teden <- reactive(build_teden(default_df, display_dimensions()))

  col_widths = c(56, 43, 45, 45, 106)


  hott <- reactive(hot_validate_numeric(
    hot_col(
      hot_col(
        hot_cols(
          rhandsontable(za_teden(), stretchH = "all"),
          colWidths = col_widths),
        col = c(1, 4), halign = "htRight", readOnly = TRUE),
      col = 5, halign = "htRight"),
    col = c(2, 3), min = 0, max = 24))

  output$hot <- renderRHandsontable({
    hott()
  })

  onSessionEnded(stopApp)
}


shinyApp(ui = ui, server = server)

