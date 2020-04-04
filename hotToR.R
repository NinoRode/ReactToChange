library(shiny)
library(rhandsontable)
library(openxlsx)
library(RSQLite)

#########################################################################################################
#    TO DO:
# Osnova je fl_df. fl_df ora biti reative, odvisen od OA in datuma
#       pri vsaki spremembi pogleda, ali je za ta datum in tega OA že bazi, če ni, ga pa nar'di
#
# Iz fl_df gradiš teden, iz tedna hot
# hot obdeluješ in ga spraviš z gubom (tu narediš nov fl_df)
#########################################################################################################

flatten_teden <- function(df) {
  ldf <- as.list(df[, c(3, 4, 6)])
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
  # Builds the table for display
  # If no data frame is provided, the table is built from sratch,
  # else it is built from the data frame

  if (is.null(df) || nrow(df) == 0) {
    zac_tedna <- Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8
  } else {
    zac_tedna <- as.Date(df$dat,  "%d. %b. %Y")
  }

  tdn <- seq(zac_tedna, by = "day", length.out = 7)
  teden_df <- data.frame("dan" = weekdays(tdn))
  teden_df$datum <- as.character(tdn, "%e. %b. %Y")

  if (is.null(df) || nrow(df) == 0) {
    teden_df$prihod <- c(rep(8, 5), NA, NA)
    teden_df$odhod <- c(rep(16, 5), NA, NA)
  } else {
    m <- list(df[grepl("prihod", names(df))], df[grepl("odhod", names(df))], df[grepl("opomba", names(df))])
    teden_df$prihod <-unlist(m[1])
    teden_df$prihod <- as.numeric(teden_df$prihod)

    teden_df$odhod <- unlist(m[2])
    teden_df$odhod <- as.numeric(teden_df$odhod)
  }

  teden_df$ure <- teden_df$odhod - teden_df$prihod
  # teden_df$ure[is.na(teden_df$ure)] <-  0

  if (is.null(df) || nrow(df) == 0) {
    teden_df$opomba <- factor(c(rep("-", 5), "sobota", "nedelja"),
                            levels = c("-", "prosto", "bolniška", "dopust",
                                       "sobota", "nedelja", "praznik",
                                       "izobraževanje"))
  } else {
    teden_df$opomba <- factor(unlist(m[3]),
                              levels = c("-", "prosto", "bolniška", "dopust",
                                         "sobota", "nedelja", "praznik",
                                         "izobraževanje"))
  }

  return(teden_df)
}

# zac_tedna <-  Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8      # Spravi v input in ga uporabljaj od tam

prepare_flat <- function(zt) {
  # Priprava podatkov tedna za vnos v SQLite bazo
  teden_df <- build_teden()
  teden_df$opomba <- as.character(teden_df$opomba)
  tdf <- flatten_teden(teden_df)                            # flatten reši argumentov
  fl_df <- cbind(dat = zt, tdf)
  fl_df$dat <- as.character(fl_df$dat)

  return(fl_df)
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

############################# UI #############################

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

########################## SEREVER ###########################

server=function(input,output){

  # options(warn = -1)

  ne_delovni <- c("prosto", "sobota", "nedelja", "dopust", "izredni dopust", "dodatni dopust", "izobraževanje")

  zac_tedna <-  reactive(input$teden - as.numeric(format(input$teden, "%u")) + 1) # postavi na začetek tedna (+1, ne +8)

  table_name <- reactive(paste(unlist(strsplit(input$OA, " ")), collapse = ""))
  sql_name <- paste(getwd(), "/", "Matjaz_Metelko.sqlite", sep = "")

  flat_df <- reactive({
    urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)

    zt <- as.character(zac_tedna(), "%d. %b. %Y")
    # tn <- isolate(table_name())

    if(!dbExistsTable(urnik_db, table_name())) {
      fl_df <- prepare_flat(zt)
      createTable(urnik_db, fl_df,  table_name(), "dat")
      replaceData(urnik_db, table_name(), fl_df)
    }

    flat_df <- readData(urnik_db, table_name(), "dat", zt) ############### make flat_df reative or not

    dbDisconnect(urnik_db)

    flat_df
  })

  teden_df <- reactive(build_teden(flat_df()))

  # Calculation of columns from https://stackoverflow.com/questions/44074184/reactive-calculate-columns-in-rhandsontable-in-shiny-rstudio
  za_teden <- reactive({

    datacopy <- NULL

    #For initial data upload
    if(is.null(input$hot)) {
      datacopy <- teden_df()
      }
    else {
      datacopy = hot_to_r(input$hot)

      #If there is change in data
      if(!is.null(input$hot$changes$changes)){

        row.no <- as.numeric(unlist(input$hot$changes$changes)[1])
        col.no <- as.numeric(unlist(input$hot$changes$changes)[2])
        new.val <- unlist(input$hot$changes$changes)[4]

        # If nonworking day change work hours
        if(new.val %in% ne_delovni) {
          datacopy[(row.no+1), 4] <- NA
          datacopy[(row.no+1), 3] <- NA
        }
        #If the changed value is prihod or odhod

        if(is.numeric(new.val) && (col.no == 2 || col.no == 3))
          datacopy[(row.no+1), 6] <- "-"

        datacopy[, 5] <- datacopy[, 4] - datacopy[, 3]
      }
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

  output$hot=renderRHandsontable({
    hott()
  })

  observeEvent(input$enter, {

    teden_df <-hot_to_r(input$hot)
    teden_df$opomba <- as.character(teden_df$opomba)
    tdf <- flatten_teden(teden_df)
    fl_df <- cbind(dat = as.character(zac_tedna(), "%d. %b. %Y"), tdf)
    fl_df$dat <- as.character(fl_df$dat)

    ime <- isolate(unlist(strsplit(input$OA, " ")))
    # file_name <- gsub(" ", "", paste(getwd(), "/", ime[1], "_", ime[2], "_", isolate(input$teden)))
    table_name <- paste(ime, collapse = "")
    xl_name <- paste(getwd(), "/", ime[1], "_", ime[2], "_", isolate(input$teden), ".xlsx", sep = "")
    sql_name <- paste(getwd(), "/", "Matjaz_Metelko.sqlite", sep = "")


    saveXcllWb(isolate(input$OA), teden_df, xl_name)

    zt <- as.character(zac_tedna(), "%d. %b. %Y")

    urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)

    if(!dbExistsTable(urnik_db, table_name))
      createTable(urnik_db, fl_df,  table_name, "dat")

    replaceData(urnik_db, table_name, fl_df)

  })
}


shinyApp(ui = ui, server = server)

