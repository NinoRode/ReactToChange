library(RSQLite)
# library(glue)

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
  # Connect to the database
  # db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(df), collapse = ", "),
    paste(df, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbExecute(db, query)
  # dbDisconnect(db)
}

replaceData <- function(db, table, df) {
  # Connect to the database
  # db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "REPLACE INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(df), collapse = ", "),
    paste(df, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbExecute(db, query)
  # dbDisconnect(db)
}

readData <- function(db, table, crit, sel_val) {
  # Connect to the database
  # db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s WHERE %s = '%s'", table, crit, sel_val)
  # Submit the fetch query and disconnect
  print(query)
  df <- dbGetQuery(db, query)
  # dbDisconnect(db)
  return(df)
}

loadData <- function(db) {
  # Connect to the database
  # db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  df <- dbGetQuery(db, query)
  # dbDisconnect(db)
  return(df)
}

lepi <- function(vr = NULL) {
  if(!is.null(vr)){
    skup_n <- paste(vr, collapse = ", ")
    return(skup_n)
  }
}

OA <- "Lucija Metelko"

zac_tedna <-  Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8

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

ime <- unlist(strsplit(OA, " "))
file_name <- paste(getwd(), "/", ime[1], "_", ime[2], sep = "")
sql_name <- paste(file_name, ".sqlite", sep = "")

zt <- as.character(zac_tedna, "%d. %b. %Y")

urnik_db <- dbConnect(RSQLite::SQLite(), sql_name)

fl_df <- prepareTeden()
createTable(urnik_db, fl_df,  "teden", "dat")
saveData(urnik_db, "teden", fl_df)

n_db <- readData(urnik_db, "teden", "dat", zt)
n_db$pon_prihod <- 13
replaceData(urnik_db, "teden", n_db)

