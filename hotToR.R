library(shiny)
library(reactlog)
library(rhandsontable)
library(openxlsx)
library(RSQLite)

# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)


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

build_teden <- function(df) {
  # Builds the table for display
  zac_tedna <- as.Date(df$dat,  "%d. %b. %Y")

  tdn <- seq(zac_tedna, by = "day", length.out = 7)
  teden_df <- data.frame("dan" = weekdays(tdn))
  teden_df$datum <- as.character(tdn, "%e. %b. %Y")

  m <- list(df[grepl("prihod", names(df))], df[grepl("odhod", names(df))], df[grepl("opomba", names(df))])
  teden_df$prihod <-unlist(m[1])
  teden_df$prihod <- as.numeric(teden_df$prihod)

  teden_df$odhod <- unlist(m[2])
  teden_df$odhod <- as.numeric(teden_df$odhod)

  teden_df$ure <- teden_df$odhod - teden_df$prihod
  # teden_df$ure[is.na(teden_df$ure)] <-  0

  teden_df$opomba <- factor(unlist(m[3]),
                            levels = c("-", "prosto", "bolniška", "dopust",
                                       "sobota", "nedelja", "praznik",
                                       "izobraževanje"))
  return(teden_df)
}

# prepare_flat <- function(zt) {
#   # Priprava podatkov tedna za vnos v SQLite bazo
#   teden_df <- build_teden()
#   teden_df$opomba <- as.character(teden_df$opomba)
#   tdf <- flatten_teden(teden_df)                            # flatten reši argumentov
#   fl_df <- cbind(dat = zt, tdf)
#   fl_df$dat <- as.character(fl_df$dat, "%d. %b. %Y")
#
#   return(fl_df)
# }

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

ui = shinyUI(fluidPage(
  fluidRow(
    column(8, offset = 2, allign = "center",
           h2(textOutput("title")))),
  fluidRow(
    h3(column(6, offset = 4, allign = "center",
           selectInput("OA", "Izberi asistentko:", choices = list("Lucija Metelko", "Ana Ljubi"))))),
  fluidRow(
    column(6,
           dateInput("teden", "Izberi teden:",
                     value = Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
                     format = "DD, dd. M. yyyy",
                     language = "sl",
                     weekstart = 1),
    ),
    column(6,
           selectInput(inputId="report",label="Pripravi listo prisotnosti za:",
                       choices = as.list(format(ISOdate(2020, 1:12, 1), "%B")),
                       selected = format(Sys.Date(), "%B"))
    )
  ),
  fluidRow(wellPanel(
    column(6,
           rHandsontableOutput("hot"),
    ),

    column(6,
           tableOutput("tabela"),
    ))),

  fluidRow(wellPanel(
    column(6,
           actionButton(inputId="enter",label="Shrani urnik")
    ),

    column(3,
           selectInput("izbor", "Prikaži shranjene tedne",
                       choices = as.character(
                         Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
                         "%d. %b. %Y"))
    ),
    column(3,
          dateInput(inputId="copy", label="Kopiraj urnik v:",
                    value = Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
                    format = "DD, dd. M. yyyy",
                    language = "sl",
                    weekstart = 1))
  ))
))

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

  observeEvent(input$report, {
    date_rec <- unlist(getDates(sql_name, table_name()))
    num_rec <- length(date_rec)
    val <- sapply(1:num_rec, function (i) {
      as.Date(date_rec[i], "%d. %b. %Y")
    })
    val <- as.Date(val, "1970-01-01")
    begin_month <- as.Date(paste("1.", isolate(input$report), "2020"),  "%d. %B %Y")
    report_month <- seq.Date(begin_month, by = "month", length.out = 2) # REPORT MOINTH JE NAROBE?
    report_month[1] <- report_month[1] - as.numeric(format(report_month[1], "%u"))
    report_month[2] <- report_month[2] - 1
    for_report <-date_rec[(report_month[1] < val & val < report_month[2] & !is.na(val))]
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

  teden_df <- reactive({
    build_teden(flat_df())
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
        datacopy[(row.no+1), 6] <- "-"

      datacopy[, 5] <- datacopy[, 4] - datacopy[, 3]
    }


    datacopy

  })


    hott <- reactive(hot_validate_numeric(
                      hot_col(
                        hot_col(
                          hot_cols(
                            rhandsontable(za_teden()),
                          colWidths = c(100, 100, 60, 60, 60, 120)),
                        col = c(1, 2, 5), halign = "htRight", readOnly = TRUE),
                      col = 6, halign = "htRight"),
                    col = c(3, 4), min = 0, max = 24))

  output$hot <- renderRHandsontable({
    hott()
  })

  observeEvent(input$enter, {

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

  observeEvent(input$report, {
    # tu pride izdelava poročila
  })

  onSessionEnded(stopApp)
}

# app <- shinyApp(ui = ui, server = server)
#
# runApp(app)
#
#
# reactlogShow()

shinyApp(ui = ui, server = server)

