library(shiny)
library(rhandsontable)
library(openxlsx)
library(data.table)

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

build_teden <- function(df) {
  teden_df <- data.frame("dat" = seq(as.Date(df$dat,  "%d. %b. %Y"), by = "day", length.out = 7))
  teden_df$dan <- weekdays( teden_df$dat)
  teden_df$datum <- as.character(teden_df$dat, "%e. %b. %Y")
  m <- list(df[grepl("prihod", names(df))], df[grepl("odhod", names(df))], df[grepl("opomba", names(df))])
  teden_df$prihod <-unlist(m[1])
  teden_df$odhod <- unlist(m[2])
  teden_df$ure <- teden_df$odhod - teden_df$prihod
  teden_df$ure[is.na(teden_df$ure)] <-  0
  teden_df$opomba <- factor(unlist(m[3]), levels = c("-", "počitek", "sobota", "nedelja", "praznik", "dopust", "bolniška", "izredni dopust", "dodatni dopust", "izobraževanje"),
                            ordered = TRUE)
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
  if (!dbExistsTable(urnik_db, table_name)) {
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

    # Submit the update query and disconnect
    dbExecute(db, query)
  }
}

# Construct the update query by looping over the data fields
query <- sprintf(
  "INSERT INTO %s (%s) VALUES ('%s')",
  table,
  paste(names(df), collapse = ", "),
  paste(df, collapse = "', '")
)

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

readData <- function(db, table, crit, sel_val) {
  query <- sprintf("SELECT * FROM %s WHERE %s = '%s'", table, crit, sel_val)
  # Submit the fetch query and disconnect
  print(query)
  df <- dbGetQuery(db, query)
  return(df)
}

loadData <- function(db) {
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  df <- dbGetQuery(db, query)
  return(df)
}

# OA <- "Lucija Metelko"
#
# zac_tedna <-  Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8

prepareTeden <- function(){
  teden_df <- data.frame("dat" = seq(zac_tedna, by = "day", length.out = 7))
  teden_df$dan <- weekdays( teden_df$dat)
  teden_df$datum <- as.character(teden_df$dat, "%e. %b. %Y")
  teden_df$prihod <- c(rep(8, 5), NA, NA)
  teden_df$odhod <- c(rep(16, 5), NA, NA)
  teden_df$ure <-  teden_df$odhod -  teden_df$prihod
  teden_df$ure[is.na(teden_df$ure)] <-  0
  teden_df$opomba <- factor(c(rep("-", 5), "SO", "NE"), levels = c("-", "PD", "SO", "NE", "P", "D", "B", "ID", "DD", "IZ"),
                            labels = c("-", "počitek", "sobota", "nedelja", "praznik", "dopust", "bolniška", "izredni dopust", "dodatni dopust", "izobraževanje"),
                            ordered = TRUE)
  teden_df$opomba <- as.character(teden_df$opomba)
  tdf <- flatten_teden(teden_df, 2, c(4, 5, 7))
  tdf
  fl_df <- cbind(dat = as.character(zac_tedna, "%d. %b. %Y"), tdf)
  fl_df$dat <- as.character(fl_df$dat)
  return(fl_df)
}

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
      mes_df$opombe <- factor(c(rep("-", 5), "SO", "NE"), levels = c("-", "PD", "SO", "NE", "P", "D", "B", "ID", "DD", "IZ"),
                                 labels = c("-", "počitek", "sobota", "nedelja", "praznik", "dopust", "bolniška", "izredni dopust", "dodatni dopust", "izobraževanje"),
                                 ordered = TRUE)
    }
    return(mes_df)
  }

  ne_dela <- c("dopust", "izredni dopust", "dodatni dopust", "izobraževanje")

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

    }else{
      datacopy = hot_to_r(input$hot)

      #If there is change in data
      if(!is.null(input$hot$changes$changes)){

        row.no <- as.numeric(unlist(input$hot$changes$changes)[1])
        col.no <- as.numeric(unlist(input$hot$changes$changes)[2])
        new.val <- unlist(input$hot$changes$changes)[4]
        #If the changed value is mpg or cyl
        if(is.numeric(new.val) && (col.no == 2 || col.no == 3)){
          datacopy[(row.no+1), 5] <- datacopy[(row.no+1), 4] - datacopy[(row.no+1), 3]
        }
        else {
          datacopy[(row.no+1), 5] <- NA
        }

        if(new.val %in% ne_dela) {
          datacopy[(row.no+1), 4] <- NA
          datacopy[(row.no+1), 3] <- NA
        }
      }
    }

    datacopy

  })

  output$hot=renderRHandsontable({

    rhandsontable(za_teden())

  })
  observeEvent(input$enter, {

    mes_df <- cbind(mes_df$dat, hot_to_r(input$hot))
    names(mes_df)[1] <- "dat"

    ime <- isolate(unlist(strsplit(input$OA, " ")))
    file_name <- gsub(" ", "", paste(getwd(), "/", ime[1], "_", ime[2], "_", isolate(input$teden)))
    xl_name <- paste(file_name, ".xlsx", sep = "")
    csv_name <- paste(file_name, ".csv", sep = "")

    saveXcllWb(isolate(input$OA), mes_df, xl_name)

    write.csv2(mes_df, csv_name)
    system2("cp", args = c(" -f", csv_name, paste(ime[1], "_", ime[2], "_", "last.csv", sep = "")))
  })
}


shinyApp(ui = ui, server = server)

