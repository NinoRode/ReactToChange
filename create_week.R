
library(poorman)
library(rhandsontable)

create_week_table <- function(beginning_date) {
  #' Create a table for a week
  #' @output table

  # week_df <- data.frame(datum = character(), odsot = character(),
  #                       jutro00_06z = numeric(), jutro00_06k = numeric(), jutro_ur00_06 = numeric(),
  #                       dopoldan06_16z = numeric(), dopoldan06_16k = numeric(), dopoldan_ur06_16 = numeric(),
  #                       popoldan16_22z = numeric(), popoldan16_22k = numeric(), popoldan_ur16_22 = numeric(),
  #                       vecer22_24z = numeric(), vecer22_24k = numeric(), vecer_ur22_24 = numeric()
  # )[1:7, ]

  week_df <- data.frame(datum = character(), odsot = character(),
                        noc0z = numeric(), noc0k = numeric(), noc0_ur = numeric(),
                        jutroz = numeric(), jutrok = numeric(), jutro_ur = numeric(),
                        dopoldanz = numeric(), dopoldank = numeric(), dopoldan_ur = numeric(),
                        kosiloz = numeric(), kosilok = numeric(), kosilo_ur = numeric(),
                        popoldan1z = numeric(), popoldan1k = numeric(), popoldan1_ur = numeric(),
                        popoldan2z = numeric(), popoldan2k = numeric(), popoldan2_ur = numeric(),
                        vecerz = numeric(), vecerk = numeric(), vecer_ur = numeric(),
                        noc1z = numeric(), noc1k = numeric(), noc1_ur = numeric()
  )[1:7, ]

  week_df$datum <- as.character(seq(as.Date(beginning_date, "%d. %b. %Y"), by = "day", length.out = 7), "%e. %b. %Y")
  week_df$odsot <- "-"

  return(week_df)

}

beginning_date <- as.character(Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 1, "%d. %b. %Y")
create_week_table(beginning_date)

###########################......................................................
#   prepis v %>% obliko   #
###########################......................................................
#
# hott <- reactive(hot_validate_numeric(
#   hot_col(
#     hot_col(
#       hot_cols(
#         rhandsontable(za_teden()),
#         colWidths = c(100, 100, 60, 60, 60, 120)),
#       col = c(1, 2, 5), halign = "htRight", readOnly = TRUE),
#     col = 6, halign = "htRight"),
#   col = c(3, 4), min = 0, max = 24))
#
# output$hot <- renderRHandsontable({
#   hott()
# })

hott <- rhandsontable(create_week_table(Sys.Date())) %>%
        # hot_cols(  colWidths = c(100, 100, 60, 60, 60, 120)) %>%
        hot_col(col = c(1, 2, 5), halign = "htRight", readOnly = TRUE) %>%
        hot_col(col = 6, halign = "htRight") %>%
        hot_validate_numeric(col = c(3, 4), min = 0, max = 24)

hott

