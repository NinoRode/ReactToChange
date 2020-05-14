library(shiny)
library(shinyBS)
library(shinyWidgets)
library(rhandsontable)
library(openxlsx)
library(RSQLite)
library(tidyxl)

# library(reactlog)

# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)

EasterDate <- function (year) {
  #' "A New York correspondent" to the journal Nature in 1876 algorithm,
  #' also called "Meeus/Jones/Butcher" algorithm
  #' from https://wiki2.org/en/Computus
  #'
  #' @year the year for which the Easter is to be computed.
  #'
  #' Returns a date of Easter (of class "Date")

  Y <- as.integer(year)
  a <- Y %% 19
  b <- Y %/% 100
  c <- Y %% 100

  d <- b %/% 4
  e <- b %% 4
  f <- (b + 8) %/% 25

  g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4
  k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451

  month <- (h + l - 7 * m + 114) %/% 31
  day <- ((h + l - 7 * m + 114) %% 31) + 1

  return(as.Date(sprintf("%d. %d. %d", day, month, Y), "%d. %m. %Y"))
}

holidaysRS <- function() {
  easter <- EasterDate(as.integer(format(Sys.Date(), "%Y")))

  holidays <-sort(c(as.Date(c("1.1", "2.1", "8.2", "27.4", "1.5", "2.5", "31.5", "25.6",
                              "15.8", "1.11", "25.12", "26.12"), "%d.%m"),
                    easter, easter + 1))
  holidays
  return(holidays)
}

# holidaysRS()

flatten_teden <- function(df) {
  ldf <- as.list(df[, c(3, 4, 6)])
  # Transpose list of lists from https://stackoverflow.com/questions/16179197/transpose-a-list-of-lists
  n <- length(ldf[[1]])
  vr <- lapply(1:n, function(i) lapply(ldf, "[[", i))

  # Concatenate lists to list from https://stackoverflow.com/questions/36665492/how-to-combine-two-lists-in-r
  skup_l <- list()
  n <- length(vr)
  names(vr) <- c("pon", "tor", "sre", "čet", "pet", "sob", "ned")
  skup_l <- lapply(1:n, function(i) do.call(c, list(skup_l, vr[i])))
  skup_df <- as.data.frame(skup_l, stringsAsFactors = FALSE)
  names(skup_df) <- sapply(names(skup_df), function(x) gsub(".", "_", x, fixed = TRUE))

  return(skup_df)
}

# build_teden <- function(df) {
#   # Builds the table for display
#   zac_tedna <- as.Date(df$dat,  "%d. %b. %Y")
#
#   tdn <- seq(zac_tedna, by = "day", length.out = 7)
#   teden_df <- data.frame("dan" = weekdays(tdn))
#   teden_df$datum <- as.character(tdn, "%e. %b. %Y")
#
#   m <- list(df[grepl("prihod", names(df))], df[grepl("odhod", names(df))], df[grepl("opomba", names(df))])
#   teden_df$prihod <- unlist(m[1])
#   teden_df$prihod <- as.numeric(teden_df$prihod)
#
#   teden_df$odhod <- unlist(m[2])
#   teden_df$odhod <- as.numeric(teden_df$odhod)
#
#   teden_df$ure <- teden_df$odhod - teden_df$prihod
#   # teden_df$ure[is.na(teden_df$ure)] <-  0
#
#   teden_df$opomba <- factor(unlist(m[3]),
#                             levels = c("-", "prosto", "bolniška", "dopust",
#                                        "sobota", "nedelja", "praznik",
#                                        "izobraževanje"))
#   return(teden_df)
# }

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

gsubAll <- function(string, to_replace_with, sep = "") {
  n <- ncol(to_replace_with)
  for (x in 1:n) {
    string <- gsub(to_replace_with[1, x], to_replace_with[2, x], string)
  }
  return(string)
}

createTable <- function(db, df, table_name, key) {
  n <- length(df)
  columns <- paste(sapply(1:n, function(x) paste(names(df[x]), typeof(df[[x]]))), collapse = ", ")
  substitutions <- matrix(c("integer", "TEXT", "double", "INTEGER", "character", "TEXT"), 2, 3)
  columns <- gsubAll(columns, substitutions, " ")

  # prepare the create table
  query <- sprintf(
    "CREATE TABLE %s (%s, UNIQUE (%s)) ",
    table_name,
    columns,
    key)

  # Submit the update query
  dbExecute(db, query)
}

replaceData <- function(db, table, df) {
  query <- sprintf(
    "REPLACE INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(df), collapse = ", "),
    paste(df, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbExecute(db, query)
}

readData <- function(db, table, what = "*", crit = "", sel_val = "") {
  # Prepare the querry
  if (crit != "") crit <- sprintf("WHERE %s = '%s'", crit, sel_val)
  query <- sprintf("SELECT %s FROM %s %s", what, table, crit)
  # Submit the query
  df <- dbGetQuery(db, query)
  return(df)
}

saveXcllWb <- function(OA, teden_df, file_name = NULL) {

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
            colNames = FALSE, rowNames = FALSE
  )

  prvi <- as.Date(teden_df$dat[1],  "%d. %b. %Y")
  zadnji <- as.Date(teden_df$dat[7],  "%d. %b. %Y")
  od_dne <- paste(format(prvi, "%d. "), meseca[as.numeric(format(prvi, "%m"))])
  do_dne <- paste(format(zadnji, "%d. "), meseca[as.numeric(format(zadnji, "%m"))])
  writeData(wb, sheet = 1, x = c(od_dne, do_dne),
            startCol = 4,
            startRow = 3,
            colNames = FALSE, rowNames = FALSE
  )


  writeData(wb, sheet = 1, x = c(format(prvi, "%Y"), format(zadnji, "%Y")),
            startCol = 5,
            startRow = 3,
            colNames = FALSE, rowNames = FALSE
  )

  writeData(wb, sheet = 1, x = t(c("za:", OA)),
            startCol = 3,
            startRow = 6,
            colNames = FALSE, rowNames = FALSE
  )

  writeDataTable(wb, sheet = 1, x = teden_df,
                 startCol = 1,
                 startRow = 8,
                 colNames = TRUE, rowNames = FALSE,
                 withFilter = FALSE,
                 firstColumn = TRUE
  )

  writeData(wb, sheet = 1, x = t(c("Skupaj:", "", "", sum(teden_df$ure, na.rm = TRUE))),
            startCol = 2,
            startRow = 16,
            colNames = FALSE, rowNames = FALSE
  )

  addStyle(wb, sheet = 1, infoStyle, rows = 1:6, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, sheet = 1, tabHeadStyle, rows = 8, cols = 1:6)
  addStyle(wb, sheet = 1, tabStyle, rows = 9:15, cols = 2:6, gridExpand = TRUE)
  addStyle(wb, sheet = 1, tabFootStyle, rows = 16, cols = 1:6)

  setColWidths(wb, sheet = 1, cols = 1:6, widths = c(11, 14, 10, 10, 10, 10))

  if(!is.null(file_name)){
    saveWorkbook(wb, file = file_name, overwrite = TRUE)
  }
}

saveXcllRprt<- function(OA, mesec, rep_df, xl_name) {

  #Prepare data frame for the report
  rep_df <- rep_df[order(as.Date(rep_df$dat, "%d. %b. %Y")), ]
  rep_num <- length(rep_df$dat)
  ted_df <- data.frame()
  for (i in 1:rep_num) {
    ted_df <- rbind(build_teden(rep_df[i, ], isolate(display_dimensions())), ted_df)
  }
  mes <- seq.Date(as.Date(paste("1.", mesec, as.character(Sys.Date(), "%Y")), "%d. %B %Y"), by = "month", length.out = 2)

  ted_df <- ted_df[(mes[1] <= as.Date(ted_df$datum,  "%e. %b. %Y") &
                     as.Date(ted_df$datum,  "%e. %b. %Y") < mes[2]), ]

  ted_df$datum <- as.Date(ted_df$datum,  "%e. %b. %Y")
  ted_df <- ted_df[order(ted_df$datum), ]

  day_num <- length(ted_df$prihod)

  wb_df <- data.frame(DAT = numeric(), ODSOT = character(),
                      noc00_06z = numeric(), noc00_06k = numeric(), ur00_06 = numeric(),
                      dop06_16z = numeric(), dop06_16k = numeric(), ur06_16 = numeric(),
                      pop16_22z = numeric(), pop16_22k = numeric(), ur16_22 = numeric(),
                      noc22_24z = numeric(), noc22_24k = numeric(), ur22_24 = numeric()
  )[1:day_num, ]

  # wb_df$DAT <- as.numeric(as.character(as.Date(ted_df$datum, "%e. %b. %Y"), "%d"))
  wb_df$DAT <- c(1:length(wb_df$DAT))

  wb_df$ODSOT <- as.character(ted_df$opomba)
  wb_df$ODSOT[wb_df$ODSOT == "dopust"] <- "D"
  wb_df$ODSOT[wb_df$ODSOT == "bolniška"] <- "B"
  wb_df$ODSOT[wb_df$ODSOT == "praznik"] <- "P"

  wb_df$noc00_06z <- 0
  wb_df$dop06_16z <- 6
  wb_df$pop16_22z <- 16
  wb_df$noc22_24z <- 22

  wb_df$noc00_06k <- 6
  wb_df$dop06_16k <- 16
  wb_df$pop16_22k <- 22
  wb_df$noc22_24k <- 24

  mask_time <- matrix( nrow = length( wb_df$noc00_06z), ncol = length(wb_df))

  mask_time [, 3] <- ifelse(ted_df$prihod < 6, 1, 0)
  mask_time [, 6] <- ifelse(ted_df$prihod < 16 ,1, 0)
  mask_time [, 9] <- ifelse(ted_df$prihod < 22, 1, 0)
  mask_time [, 12] <- 1

  mask_time [, 3] <-  mask_time [, 3]
  mask_time [, 6] <- ifelse(ted_df$odhod <= 6, 0, 1 * mask_time [, 6])
  mask_time [, 9] <- ifelse(ted_df$odhod <= 16, 0, 1 * mask_time [, 9])
  mask_time [, 12] <- ifelse(ted_df$odhod <= 22, 0, 1 * mask_time [, 12])

  mask_time [, 4] <- 1
  mask_time [, 7] <- ifelse(ted_df$odhod <= 6, 0, 1)
  mask_time [, 10] <- ifelse(ted_df$odhod <= 16, 0, 1)
  mask_time [, 13] <- ifelse(ted_df$odhod <= 22, 0, 1)

  mask_time [, 4] <- ifelse(ted_df$prihod < 6, 1 * mask_time [, 4], 0)
  mask_time [, 7] <- ifelse(ted_df$prihod < 16, 1 * mask_time [, 7], 0)
  mask_time [, 10] <- ifelse(ted_df$prihod < 22, 1 * mask_time [, 10], 0)
  mask_time [, 13] <- mask_time [, 13]

  mask_time <- mask_time[, 3:14]
  mask_time[is.na(mask_time)] <-0

  wb_m <- as.matrix(wb_df[3:14])
  wb_m[is.na(wb_m)] <- 0

  wb_df <- cbind(wb_df[1:2], as.data.frame(mask_time * wb_m))

  wb_df$noc00_06z <- ifelse(ted_df$prihod < 6,  ted_df$prihod, wb_df$noc00_06z)
  wb_df$dop06_16z <- ifelse(ted_df$prihod >= 6 & ted_df$prihod < 16,  ted_df$prihod, wb_df$dop06_16z)
  wb_df$pop16_22z <- ifelse(ted_df$prihod >= 16 & ted_df$prihod < 22,  ted_df$prihod, wb_df$pop16_22z)
  wb_df$noc22_24z <- ifelse(ted_df$prihod >= 22,  ted_df$prihod, wb_df$noc22_24z)

  wb_df$noc00_06k <- ifelse(ted_df$odhod <= 6,  ted_df$odhod, wb_df$noc00_06k)
  wb_df$dop06_16k <- ifelse(ted_df$odhod > 6 & ted_df$odhod <= 16,  ted_df$odhod, wb_df$dop06_16k)
  wb_df$pop16_22k <- ifelse(ted_df$odhod > 16 & ted_df$odhod <= 22,  ted_df$odhod,  wb_df$pop16_22k)
  wb_df$noc22_24k <- ifelse(ted_df$odhod > 22,  ted_df$odhod, wb_df$noc22_24k)

  wb_df$ur00_06 <- wb_df$noc00_06k - wb_df$noc00_06z
  wb_df$ur06_16 <- wb_df$dop06_16k - wb_df$dop06_16z
  wb_df$ur16_22 <- wb_df$pop16_22k - wb_df$pop16_22z
  wb_df$ur22_24 <- wb_df$noc22_24k - wb_df$noc22_24z

  # Prepare a workbook

  rep_wb <- loadWorkbook("/home/nino/Dokumenti/Matjaz/OA/PRISOTNOST ASISTENTI 2020.xlsx")
  meseci <- paste(toupper(format(ISOdate(2020, 1:12, 1), "%B")), as.character(Sys.Date(), "%Y"))

  this_month <- paste(toupper(mesec), as.character(Sys.Date(), "%Y"))

  lapply(as.list(meseci), function (x) {
    if (x != this_month) removeWorksheet(rep_wb, x)
  })

  # Wipe the old data
  wipe <- matrix(rep("X", day_num * (length(wb_df) - 2)), nrow = day_num)

  writeData(rep_wb, sheet = 1, x = wipe,
            startCol = 4,
            startRow = 18,
            colNames = FALSE, rowNames = FALSE
  )

  # saveWorkbook(rep_wb, xl_name, overwrite = TRUE)
  #
  # loadWorkbook(xl_name)

  #Fill in the report with data and formulae

  writeData(rep_wb, sheet = 1,
            x = isolate(OA),
            startCol = 6,
            startRow = 1,
            colNames = FALSE, rowNames = FALSE
  )

  writeData(rep_wb, sheet = 1,
            x = "Matjaž Metelko",
            startCol = 4,
            startRow = 2,
            colNames = FALSE, rowNames = FALSE
  )

  writeData(rep_wb, sheet = 1, x = wb_df$ODSOT,
                 startCol = 3,
                 startRow = 18,
                 colNames = FALSE, rowNames = FALSE
  )

  writeData(rep_wb, sheet = 1, x = wb_df$noc00_06z,
                 startCol = 4,
                 startRow = 18,
                 colNames = FALSE, rowNames = FALSE
  )

  writeData(rep_wb, sheet = 1, x = wb_df$noc00_06k,
            startCol = 5,
            startRow = 18,
            colNames = FALSE, rowNames = FALSE,
            withFilter = FALSE,
  )

  dif_formula <- sprintf("=%s-%s", paste("E", (1:day_num) + 17, sep = ""), paste("D", (1:day_num) + 17, sep = ""))
  writeFormula(rep_wb, sheet = 1, x = dif_formula,
            startCol = 6,
            startRow = 18
  )

  writeData(rep_wb, sheet = 1, x = wb_df$dop06_16z,
                 startCol = 7,
                 startRow = 18,
                 colNames = FALSE, rowNames = FALSE,
                 withFilter = FALSE,
  )

  writeData(rep_wb, sheet = 1, x = wb_df$dop06_16k,
                 startCol = 8,
                 startRow = 18,
                 colNames = FALSE, rowNames = FALSE,
                 withFilter = FALSE,
  )

  dif_formula <- sprintf("=%s-%s", paste("H", (1:day_num) + 17, sep = ""),  paste("G", (1:day_num) + 17, sep = ""))
  writeFormula(rep_wb, sheet = 1, x = dif_formula,
               startCol = 9,
               startRow = 18
  )

  writeData(rep_wb, sheet = 1, x = wb_df$pop16_22z,
                 startCol = 10,
                 startRow = 18,
                 colNames = FALSE, rowNames = FALSE,
                 withFilter = FALSE,
  )

  writeData(rep_wb, sheet = 1, x = wb_df$pop16_22k,
                 startCol = 11,
                 startRow = 18,
                 colNames = FALSE, rowNames = FALSE,
                 withFilter = FALSE,
  )

  dif_formula <- sprintf("=%s-%s", paste("K", (1:day_num) + 17, sep = ""),  paste("J", (1:day_num) + 17, sep = ""))
  writeFormula(rep_wb, sheet = 1, x = dif_formula,
               startCol = 12,
               startRow = 18
  )
  writeData(rep_wb, sheet = 1, x = wb_df$noc22_24z,
                 startCol = 13,
                 startRow = 18,
                 colNames = FALSE, rowNames = FALSE,
                 withFilter = FALSE,
  )

  writeData(rep_wb, sheet = 1, x =  wb_df$noc22_24k,
                 startCol = 14,
                 startRow = 18,
                 colNames = FALSE, rowNames = FALSE,
                 withFilter = FALSE,
  )

  dif_formula <- sprintf("=%s-%s", paste("N", (1:day_num) + 17, sep = ""),  paste("M", (1:day_num) + 17, sep = ""))
  writeFormula(rep_wb, sheet = 1, x = dif_formula,
               startCol = 15,
               startRow = 18
  )

  ##################################### tidyxl  #####################################
  base_wb <- xlsx_cells("/home/nino/Dokumenti/Matjaz/OA/PRISOTNOST ASISTENTI 2020.xlsx",
                        sheets = this_month, include_blank_cells = FALSE)

  # fmting <- xlsx_formats("/home/nino/Dokumenti/Matjaz/OA/PRISOTNOST ASISTENTI 2020.xlsx", check_filetype = TRUE)

  rep_formula <-base_wb[!is.na(base_wb$formula), ]

  # rep_text <-base_wb[!is.na(base_wb$character), c("row", "col", "character")]

  # Barva ozadja celic: PRAZNIK: #92D050 SOBOTA, NEDELJA: #FFFF00, PRAZNO (ZAŠČITENO) #DDD9C3 ali #C0C0C0
  # Barva besedila POMEMBNO #FF0000 ali #FC1621

  write_the_cell <- function(wb, base, pos) {
    vars <- names(base)

    if ("col" %in% vars &&
        "row" %in% vars &&
        sum(which(c("formula", "numeric", "character", "logical", "date", "error", "blank") %in% vars)) > 0) {

      if (is.na(base$formula[pos])) {
        to_write <- switch (base$data_type[pos],
                            numeric = base$numeric[pos],
                            character = base$character[pos],
                            logical = base$logical[pos],
                            date = base$date[pos],
                            error= base$error[pos],
                            blank = ""
        )
        writeData(wb, sheet = this_month, x = to_write,
                  startCol = base$col[pos],
                  startRow = base$row[pos],
                  colNames = FALSE, rowNames = FALSE
        )
      }
      else {
        to_write <- paste("=", base$formula[pos], sep = "")
        writeFormula(wb, sheet = this_month, x = to_write,
                     startCol = base$col[pos],
                     startRow = base$row[pos]
        )
      }
    }
    else {
      print("inappropriate data")
    }
  }

  n <- length(rep_formula$col)
  lapply(1:n, function (x) write_the_cell(rep_wb, rep_formula, x))

  ##################################### tidyxl  #####################################

    # Fill in the leave data
  # First find where the table begins
  start_is_here <- 45 + which("OPRAVLJENE URE" == read.xlsx(
    rep_wb,
    sheet = 1,
    startRow = 46,
    colNames = FALSE,
    cols = 2,
    skipEmptyRows = FALSE
  ))

  # print(sum(wb_df$ODSOT == "P"))
  writeData(rep_wb, sheet = 1,
            x = sum(wb_df$ODSOT == "P"),
            startCol = 8,
            startRow = start_is_here + 1,
            colNames = FALSE, rowNames = FALSE
  )

  writeData(rep_wb, sheet = 1,
            x = sum(wb_df$ODSOT == "D"),
            startCol = 8,
            startRow = start_is_here + 2,
            colNames = FALSE, rowNames = FALSE
  )

  writeData(rep_wb, sheet = 1,
            x = sum(wb_df$ODSOT == "B"),
            startCol = 8,
            startRow = start_is_here + 3,
            colNames = FALSE, rowNames = FALSE
  )

  saveWorkbook(rep_wb, xl_name, overwrite = TRUE)
}

default_df <- data.frame(dat = as.character(Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8, "%d. %b. %Y"),
                         pon_prihod = NA, pon_odhod = NA, pon_opomba = "prosto",
                         tor_prihod = NA, tor_odhod = NA, tor_opomba = "prosto",
                         sre_prihod = NA, sre_odhod = NA, sre_opomba = "prosto",
                         čet_prihod = NA, čet_odhod = NA, čet_opomba = "prosto",
                         pet_prihod = NA, pet_odhod = NA, pet_opomba = "prosto",
                         sob_prihod = NA, sob_odhod = NA, sob_opomba = "sobota",
                         ned_prihod = NA, ned_odhod = NA, ned_opomba = "nedelja",
                         stringsAsFactors = FALSE)

############################# UI #############################
#
# ui = shinyUI(fluidPage(
#   fluidRow(
#     column(8, offset = 2, allign = "center",
#            h2(textOutput("title")))),
#   fluidRow(
#     h3(column(6, offset = 4, allign = "center",
#            selectInput("OA", "Izberi asistentko:", choices = list("Lucija Metelko", "Ana Ljubi"))))),
#   fluidRow(
#     column(6,
#            dateInput("teden", "Izberi teden:",
#                      value = Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
#                      format = "DD, dd. M. yyyy",
#                      language = "sl",
#                      weekstart = 1),
#            selectInput("izbor", "ali Izberi teden iz baze:",
#                        choices = as.character(
#                          Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
#                          "%d. %b. %Y"))
#     ),
#     column(6,
#            actionButton(inputId="do_report",label="Pripravi listo prisotnosti"),
#            selectInput(inputId="report",label="Za:",
#                        choices = as.list(format(ISOdate(2020, 1:12, 1), "%B")),
#                        selected = format(Sys.Date(), "%B"))
#     )
#   ),
#   fluidRow(wellPanel(
#     column(6,
#            rHandsontableOutput("hot"),
#            textOutput("sum_w_hours"),
#            textOutput("sum_P_hours"),
#            textOutput("sum_D_hours"),
#            textOutput("sum_B_hours"),
#            textOutput("sum_all_hours")
#     ),
#
#     column(6,
#            tableOutput("tabela"),
#     ))),
#
#   fluidRow(wellPanel(
#     column(6,
#            actionButton(inputId="enter",label="Shrani urnik")
#     )
#   ))
# ))

############################# UI #############################

ui = shinyUI(
  fluidPage(title = "Urniki dela za osebne asistente",
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
                      actionButton(inputId="enter",label="Shrani urnik", width = "100%"),
                      bsModal("urnik", "Shrani urnik", "enter",
                              h4(textOutput("title_urnik")),
                              selectInput("izbor", "Shrani v taden:",    ############ preglej: samo datum mora spremeniti ###########
                                          choices = as.character(
                                            Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
                                            "%d. %b. %Y")),
                              actionButton(inputId="really_save", label="Shrani"),
                              size = "small")
               ),
               column(6, allign = "center",
                      actionButton(inputId="do_report",label="Pripravi listo prisotnosti", width = "100%"),
                      bsModal("prisotnost", "Lista prisotnosti", "do_report",
                              h4(textOutput("title_report")),
                              selectInput(inputId="report",label="za mesec:",
                                          choices = as.list(format(ISOdate(2020, 1:12, 1), "%B")),
                                          selected = format(Sys.Date(), "%B")),
                              actionButton(inputId="really_do_report", label="Pripravi"),
                              size = "small")
               )
             )
      ),

      column(4,
             p("Ure:"),
             textOutput("sum_w_hours"),
             textOutput("sum_P_hours"),
             textOutput("sum_D_hours"),
             textOutput("sum_B_hours"),
             textOutput("sum_all_hours")
      )
    ))
  )
)

#------------------------------------------------------------#

########################## SEREVER ###########################

server=function(input,output, session){

  options(warn = -1)

  ne_delovni <- c("prosto", "praznik", "dopust", "sobota", "nedelja", "izobraževanje")
  OA_change <- reactiveVal(FALSE)

  zac_tedna <-  reactive(input$teden - as.numeric(format(input$teden, "%u")) + 1) # postavi na začetek tedna (+1, ne +8)
  observe(
    updateDateInput(session, "teden", value = zac_tedna())
    )

  # Prepare empty default data frame (this will go out of server)

  table_name <- reactive(paste(unlist(strsplit(input$OA, " ")), collapse = ""))
  output$title <- renderText(c("Tabela za OA: ", input$OA, ", v tednu od ", as.character(input$teden, "%d. %b. %Y")))
  output$title_report <- renderText(c("Poročilo o prisotnosti za OA:\n", input$OA))
  output$tabela <- renderText(table_name())

  sql_name <- paste(getwd(), "/", "Matjaz_Metelko.sqlite", sep = "")

  getDates <- function(sql_name, table_name) {
    urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)
    ch <- readData(urnik_db, table_name, what = "dat")

    dbDisconnect(urnik_db)

    ch <- unlist(ch)
    ch <- as.Date(ch, "%d. %b. %Y")
    ch <- as.list(ch[order(ch, decreasing = TRUE)])
    ch <- lapply(ch, function(c) as.character(c, "%d. %b. %Y"))
    names(ch) <- NULL
    return(ch)
  }

  observe({
    ch <- getDates(sql_name, table_name())
    updateSelectInput(session, "izbor", choices = ch)
  })

  observeEvent(input$izbor, {
    val <-as.Date(isolate(input$izbor), "%d. %b. %Y")
    updateDateInput(session, "teden", value = val)
    })

  observeEvent(input$copy, {
    zac_tedna <- (isolate(input$copy) - as.numeric(format(isolate(input$copy), "%u")) + 1) # postavi na začetek tedna (+1, ne +8)
    updateDateInput(session, "teden", value = zac_tedna())

    copy_df <- flat_df()
    copy_df$dat <- as.character(zac_tedna)

    urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)

    replaceData(urnik_db, table_name(), copy_df)
    dbDisconnect(urnik_db)

    updateDateInput(session, "teden", value = zac_tedna())
  })

  observeEvent(input$really_do_report, {
    date_rec <- unlist(getDates(sql_name, table_name()))
    num_rec <- length(date_rec)
    val <- sapply(1:num_rec, function (i) {
      as.Date(date_rec[i], "%d. %b. %Y")
    })
    val <- as.Date(val, "1970-01-01")
    begin_month <- as.Date(paste("1.", isolate(input$report), "2020"),  "%d. %B %Y")
    report_month <- seq.Date(begin_month, by = "month", length.out = 2)
    report_month[1] <- report_month[1] - as.numeric(format(report_month[1], "%u"))
    report_month[2] <- report_month[2] - 1
    for_report <-date_rec[(report_month[1] < val & val < report_month[2] & !is.na(val))]

    rep_df <- data.frame()

    urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)

    for(d in for_report) {
      rep_df <- (rbind(readData(urnik_db, table_name(), what ="*", crit = "dat", sel_val = as.character(d, "%d. %b. %Y")), rep_df))
    }
    rep_df <- as.data.frame(rep_df)

    dbDisconnect(urnik_db)

    ime <- (paste(unlist(strsplit(isolate(input$OA), " ")), collapse = ""))

    xl_name <- paste(getwd(), "/", ime, "_", isolate(input$report), as.character(format(Sys.Date(), "%y")), ".xlsx", sep = "")

    saveXcllRprt(isolate(input$OA), isolate(input$report), rep_df, xl_name)

  })

  flat_df <- reactive({
    urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)

    # zt <- as.character(zac_tedna(), "%d. %b. %Y")

    if(!dbExistsTable(urnik_db, table_name())) {
      createTable(urnik_db, default_df,  table_name(), "dat")
      replaceData(urnik_db, table_name(), default_df)
    }

    fl_df <- readData(urnik_db, table_name(), what ="*", crit = "dat", sel_val = as.character(zac_tedna(), "%d. %b. %Y"))

    dbDisconnect(urnik_db)

    if (nrow(fl_df) == 0)
      fl_df <- default_df

    fl_df
  })

  display_dimensions <- reactive(input$dimension)

  teden_df <- reactive({
    build_teden(flat_df(), isolate(display_dimensions()))
  })

  output$tabela <- renderTable(teden_df())

  observeEvent(list(input$OA, input$teden),
               OA_change(TRUE))

  # Calculation of columns from https://stackoverflow.com/questions/44074184/reactive-calculate-columns-in-rhandsontable-in-shiny-rstudio
  za_teden <- eventReactive(list(input$OA, input$teden, input$hot$changes$changes), {

    datacopy <- NULL

    #For initial data upload
    if(isolate(OA_change()) || is.null(input$hot)) {
      datacopy <- teden_df()
    }
    else {
      datacopy <- hot_to_r(input$hot)
    }

    #If there is change in data
    if(!is.null(input$hot$changes$changes)){
      if(is.null(input$hot$changes$changes[[1]][[3]]))
        len.chg <- 3
      else
        len.chg <- 4
      change.in.table <- matrix(unlist(input$hot$changes$changes), nrow = len.chg)
      row.no <- as.numeric(change.in.table[1, ])
      col.no <- as.numeric(change.in.table[2, ])
      if(len.chg ==4) {
      old.val <- change.in.table[3, ]
      new.val <- change.in.table[4, ]
      }
      else {
        old.val <- NULL
        new.val <- change.in.table[3, ]
      }

      posit <- length(change.in.table) %/% len.chg

      # If nonworking day change work hours
      ifelse(new.val[posit] %in% ne_delovni,
             {
               datacopy[(row.no+1), 4] <- NA
               datacopy[(row.no+1), 3] <- NA
             },
             {
               identity(datacopy[(row.no+1), 4])
               identity(datacopy[(row.no+1), 3])
             }
      )
      #If the changed value is prihod or odhod

      if(is.numeric(new.val) && (col.no == 2 || col.no == 3))
        datacopy[(row.no+1), 5] <- "-"

      datacopy[, 4] <- datacopy[, 3] - datacopy[, 2]
    }

    OA_change(FALSE)

    datacopy

  })

  col_widths = c(100, 43, 45, 45, 106)
  if (isolate(display_dimensions()) < 620) col_widths[1] <- 56

    hott <- reactive(hot_validate_numeric(
                      hot_col(
                        hot_col(
                          hot_cols(
                            rhandsontable(za_teden(), stretchH = "all"),
                          colWidths = col_widths),
                        col = c(1, 4), halign = "htRight", readOnly = TRUE),
                      col = 5, halign = "htCenter"),
                    col = c(2, 3), min = 0, max = 24))

  output$hot <- renderRHandsontable({
    hott()
  })

  observe({
  w_hours <- sum(za_teden()[, 4], na.rm = TRUE)
  P_hours <- sum(za_teden()[, 5] == "praznik", na.rm = TRUE) * 8
  D_hours <- sum(za_teden()[, 5] == "dopust", na.rm = TRUE) * 8
  B_hours <- sum(za_teden()[, 5] == "bolniška", na.rm = TRUE) * 8

  all_hours <- w_hours + P_hours + D_hours + B_hours

  output$sum_w_hours <- renderText(c("Število delovnih ur ta teden: ", w_hours))
  output$sum_P_hours <- renderText(c("Število prazničnih ur ta teden: ", P_hours))
  output$sum_D_hours <- renderText(c("Število ur iz dopusta ta teden: ", D_hours))
  output$sum_B_hours <- renderText(c("Število ur bolnioške odsotnosti ta teden: ", B_hours))
  output$sum_all_hours <-  renderText(c("Skupno število ur ta teden", all_hours))
  })

  observeEvent(input$really_save, {

    sav_teden_df <-hot_to_r(input$hot)
    sav_teden_df$opomba <- as.character(sav_teden_df$opomba)
    tdf <- flatten_teden(sav_teden_df)
    fl_df <- cbind(dat = as.character(zac_tedna(), "%d. %b. %Y"), tdf)
    fl_df$dat <- as.character(fl_df$dat)

    ime <- reactive(paste(unlist(strsplit(input$OA, " ")), collapse = ""))

    # table_name <- reactive(paste(ime(), collapse = ""))
    xl_name <- paste(getwd(), "/", ime(), "_", input$teden, ".xlsx", sep = "")
    sql_name <- paste(getwd(), "/", "Matjaz_Metelko.sqlite", sep = "")


    saveXcllWb(isolate(input$OA), sav_teden_df, xl_name)

    urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)

    if(!dbExistsTable(urnik_db, ime()))
      createTable(urnik_db, fl_df,  ime(), "dat")

    replaceData(urnik_db, ime(), fl_df)

    output$tabela <- renderTable(sav_teden_df)

    dbDisconnect(urnik_db)

  })

  observeEvent(input$copy, {
               teden_copy <- teden_df()
  })

  # To do this you should use reactive values Prestavi dogajanje v reactiveValues
  # teden_df <- eventReactive(input$paste,
  #              teden_copy
  # )

  ############## commented out 24.4.
  # observeEvent(input$report, {
  #   # tu pride izdelava poročila
  # })

  onSessionEnded(stopApp)
}

# app <- shinyApp(ui = ui, server = server)
#
# runApp(app)
#
#
# reactlogShow()

shinyApp(ui = ui, server = server)

