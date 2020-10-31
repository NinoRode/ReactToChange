#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(openxlsx)
library(RSQLite)
library(tidyxl)

# Database manipulation

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
    # Prepare the query
    if (crit != "") crit <- sprintf("WHERE %s = '%s'", crit, sel_val)
    query <- sprintf("SELECT %s FROM %s %s", what, table, crit)
    # Submit the query
    df <- dbGetQuery(db, query)
    return(df)
}

workDB <- function(userName, assistName, toDo = NULL, ...) {
    sql_name <- paste0("./", userName)
    table <- assistName
    db <- dbConnect(RSQLite::SQLite(), sql_name)
    actions <- list(...)
    i = 1
    while (i <= length(actions)) {
        switch(actions[i],
               read = readData(db, table),
               replace = {replaceData(db, table, actions[i+1])
                   i = i + 1},
               find = {readData(db, table, actions[i+1], actions[i+2])
                   i = i + 2},
               newtable = {createTable(db, actions[i+1], table, actions[i+2])
                   i = i + 2},
               print(c("neznano: ", actions[i]))
        )
        i = i + 1
    }
}

# Define UI for application that prepares the schedule for the presonal assistant
ui <- fluidPage(

    # Application title
    titlePanel("Urniki za OA"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
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
                       selectInput("izbor", "ali Izberi teden iz baze:",
                                   choices = as.character(
                                       Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 8,
                                       "%d. %b. %Y"))
                ),
                column(6,
                       actionButton(inputId="do_report",label="Pripravi listo prisotnosti"),
                       selectInput(inputId="report",label="Za:",
                                   choices = as.list(format(ISOdate(2020, 1:12, 1), "%B")),
                                   selected = format(Sys.Date(), "%B"))
                )
            ),
            fluidRow(wellPanel(
                column(6,
                       rHandsontableOutput("hot"),
                       textOutput("sum_w_hours"),
                       textOutput("sum_P_hours"),
                       textOutput("sum_D_hours"),
                       textOutput("sum_B_hours"),
                       textOutput("sum_all_hours")
                ),

                column(6,
                       tableOutput("tabela"),
                ))),

            fluidRow(wellPanel(
                column(6,
                       actionButton(inputId="enter",label="Shrani urnik")
                )
            ))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application
shinyApp(ui = ui, server = server)
