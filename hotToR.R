library(shiny)
library(rhandsontable)
library(openxlsx)
library(data.table)
library(RSQLite)

flatten_teden <- function(df, lead_txt, which_vars) {
  ldf <- as.list(df[, which_vars])
  # Transpose list of lists from https://stackoverflow.com/questions/16179197/transpose-a-list-of-lists
  n <- length(ldf[[1]])
  vr <- lapply(1:n, function(i) lapply(ldf, "[[", i))

  # Concatenate lists to list from https://stackoverflow.com/questions/36665492/how-to-combine-two-lists-in-r
  skup_l <- list()
  n <- length(vr)
  names(vr) <- c("pon", "tor", "sre", "cet", "pet", "sob", "ned")
  skup_l <- lapply(1:n, function(i) do.call(c, list(skup_l, vr[i])))
  skup_df <- as.data.frame(skup_l, stringsAsFactors = FALSE)
  names(skup_df) <- sapply(names(skup_df), function(x) gsub(".", "_", x, fixed = TRUE))
  return(skup_df)
}

build_teden <- function(df = NULL) {
  if (is.null(df)) {
    tdn <- Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8
  } else {
    tdn <- as.Date(df$dat,  "%d. %b. %Y")
  }
  teden_df <- data.frame("dat" = seq(tdn, by = "day", length.out = 7))
  teden_df$dan <- weekdays( teden_df$dat)
  teden_df$datum <- as.character(teden_df$dat, "%e. %b. %Y")

  if (is.null(df)) {
    teden_df$prihod <- c(rep(8, 5), NA, NA)
    teden_df$odhod <- c(rep(16, 5), NA, NA)
  } else {
    m <- list(df[grepl("prihod", names(df))], df[grepl("odhod", names(df))], df[grepl("opomba", names(df))])
    teden_df$prihod <-unlist(m[1])
    teden_df$odhod <- unlist(m[2])
  }
  teden_df$ure <- teden_df$odhod - teden_df$prihod
  teden_df$ure[is.na(teden_df$ure)] <-  0
  teden_df$opomba <- factor(c(rep("-", 5), "sobota", "nedelja"),
                            levels = c("-", "počitek", "bolniška", "dopust",
                                       "sobota", "nedelja", "praznik",
                                       "izobraževanje"))
  if (!is.null(df)) {
    teden_df$opomba <- factor(unlist(m[3]))
  }
  return(teden_df)
}

# Priprava podatkov tedna za vnos v SQLite bazo
zac_tedna <-  Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8

teden_df <- build_teden()
teden_df$opomba <- as.character(teden_df$opomba)
tdf <- flatten_teden(teden_df, 2, c(4, 5, 7))
fl_df <- cbind(dat = as.character(zac_tedna, "%d. %b. %Y"), tdf)
fl_df$dat <- as.character(fl_df$dat)


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

saveData <- function(db, table, df) {
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(df), collapse = ", "),
    paste(df, collapse = "', '")
  )
  # Submit the update query and disconnect
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

readData <- function(db, table, crit = "", sel_val = "") {
  # Prepare the querry
  if (crit != "") crit <- sprintf("WHERE %s = '%s'", crit, sel_val)
  query <- sprintf("SELECT * FROM %s %s", table, crit)
  # Submit the query
  print(query)
  df <- dbGetQuery(db, query)
  return(df)
}

##########################################
##           VKLJUČI V SERVER           ##
##########################################

OA <- "Lucija Metelko"

# zac_tedna <-  Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8

# ime <- unlist(strsplit(OA, " "))
# file_name <- paste(getwd(), "/", ime[1], "_", ime[2], sep = "")
# sql_name <- paste(file_name, ".sqlite", sep = "")
#
# zt <- as.character(zac_tedna, "%d. %b. %Y")
#
# urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)
#
# # fl_df <- buildTeden()
# createTable(urnik_db, fl_df,  "teden", "dat")
# replaceData(urnik_db, "teden", fl_df)
#
# n_db <- readData(urnik_db, "teden", "dat", zt)
# n_db$pon_prihod <- 13
# replaceData(urnik_db, "teden", n_db)

##########################################

saveXcllWb <- function(OA, mes_df, file_name = NULL) {

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

  od_dne <- paste(format(mes_df$dat[1], "%d."), meseca[as.numeric(format(mes_df$dat[1], "%m"))])
  do_dne <- paste(format(mes_df$dat[7], "%d."), meseca[as.numeric(format(mes_df$dat[7], "%m"))])
  writeData(wb, sheet = 1, x = c(od_dne, do_dne),
            startCol = 4,
            startRow = 3,
            colNames = FALSE, rowNames = FALSE
  )


  writeData(wb, sheet = 1, x = c(format(mes_df$dat[1], "%Y"), format(mes_df$dat[7], "%Y")),
            startCol = 5,
            startRow = 3,
            colNames = FALSE, rowNames = FALSE
  )

  writeData(wb, sheet = 1, x = t(c("za:", OA)),
            startCol = 3,
            startRow = 6,
            colNames = FALSE, rowNames = FALSE
  )

  writeDataTable(wb, sheet = 1, x = mes_df[, -1],
                 startCol = 1,
                 startRow = 8,
                 colNames = TRUE, rowNames = FALSE,
                 withFilter = FALSE,
                 firstColumn = TRUE
  )

  writeData(wb, sheet = 1, x = t(c("Skupaj:", "", "", "", sum(mes_df$ure, na.rm = TRUE))),
            startCol = 2,
            startRow = 16,
            colNames = FALSE, rowNames = FALSE
  )

  addStyle(wb, sheet = 1, infoStyle, rows = 1:6, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, sheet = 1, tabHeadStyle, rows = 8, cols = 1:6)
  addStyle(wb, sheet = 1, tabFootStyle, rows = 16, cols = 1:6)

  setColWidths(wb, sheet = 1, cols = 1:6, widths = c(11, 14, 10, 10, 10, 10))

  if(!is.null(file_name)){
    saveWorkbook(wb, file = file_name, overwrite = TRUE)
  }
}

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

  prepare_df <- function(last_data, zac_tedna = NULL) {
    if (file.exists(last_data)) {
      mes_df <- read.csv2(last_data)
      mes_df$dat = seq(zac_tedna, by = "day", length.out = 7)
    }
    else {
      mes_df <- data.frame("dat" = seq(zac_tedna, by = "day", length.out = 7))
      mes_df$dan <- weekdays( mes_df$dat)
      mes_df$datum <- as.character(mes_df$dat, "%e. %b. %Y")
      mes_df$prihod <- c(rep(8, 5), NA, NA)
      mes_df$odhod <- c(rep(16, 5), NA, NA)
      mes_df$ure <-  mes_df$odhod -  mes_df$prihod
      mes_df$ure[is.na(mes_df$ure)] <-  0
      mes_df$opomba <- factor(c(rep("-", 5), "sobota", "nedelja"),
                                levels = c("-", "počitek", "bolniška", "dopust",
                                           "sobota", "nedelja", "praznik",
                                           "izobraževanje"))
    }
    return(mes_df)
  }

  ne_dela <- c("počitek", "sobota", "nedelja", "dopust", "izredni dopust", "dodatni dopust", "izobraževanje")

  ime <- isolate(unlist(strsplit(input$OA, " ")))
  file_name <- gsub(" ", "", paste(getwd(), "/", ime[1], "_", ime[2], "_", "last.csv"))
  mes_df <- prepare_df(file_name, zac_tedna)

  # Calculation of columns from https://stackoverflow.com/questions/44074184/reactive-calculate-columns-in-rhandsontable-in-shiny-rstudio
  za_teden <- reactive({

    datacopy <- NULL

    #For initial data upload
    if(is.null(input$hot)){
      datacopy <- mes_df[, -1]
      datacopy=data.table(datacopy)

    } else {
      datacopy = hot_to_r(input$hot)

      #If there is change in data
      if(!is.null(input$hot$changes$changes)){

        row.no <- as.numeric(unlist(input$hot$changes$changes)[1])
        col.no <- as.numeric(unlist(input$hot$changes$changes)[2])
        new.val <- unlist(input$hot$changes$changes)[4]

        # If nonworking day change work hours
        if(new.val %in% ne_dela) {
          datacopy[(row.no+1), 4] <- NA
          datacopy[(row.no+1), 3] <- NA
        }
        #If the changed value is prihod or odhod

        if(is.numeric(new.val) && (col.no == 2 || col.no == 3))
          datacopy[(row.no+1), 6] <- "-"

        datacopy[, 5] <- datacopy[, 4] - datacopy[, 3]
        # else {
        #   datacopy[(row.no+1), 5] <- 0
        # }
      }
    }

    datacopy

  })

  hott <- reactive(rhandsontable(za_teden()))
  output$hot=renderRHandsontable({

    # rhandsontable(za_teden())

    hot_validate_numeric(hott(), col = c(3, 4), min = 0, max = 24)

  })
  observeEvent(input$enter, {

    mes_df <- cbind(mes_df$dat, hot_to_r(input$hot))
    names(mes_df)[1] <- "dat"

    ime <- isolate(unlist(strsplit(input$OA, " ")))
    # file_name <- gsub(" ", "", paste(getwd(), "/", ime[1], "_", ime[2], "_", isolate(input$teden)))
    file_name <- paste(getwd(), "/", ime[1], "_", ime[2], sep = "")
    xl_name <- paste(file_name,  "_", isolate(input$teden), ".xlsx", sep = "")
    sql_name <- paste(file_name, ".sqlite", sep = "")


    saveXcllWb(isolate(input$OA), mes_df, xl_name)

    zt <- as.character(zac_tedna, "%d. %b. %Y")

    urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)

    if(!dbExistsTable(urnik_db, "teden"))
      createTable(urnik_db, fl_df,  "teden", "dat")

    replaceData(urnik_db, "teden", fl_df)

    # n_db <- readData(urnik_db, "teden", "dat", zt)
    # n_db$pon_prihod <- 13
    # replaceData(urnik_db, "teden", n_db)
    # system2("cp", args = c(" -f", csv_name, paste(ime[1], "_", ime[2], "_", "last.csv", sep = "")))
  })
}


shinyApp(ui = ui, server = server)

