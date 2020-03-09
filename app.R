rm(list = ls())
library(shiny)
library(shinysky)
library(readxl)

server <- shinyServer(function(input, output, session) {

    # OAsistent <- reactive(list("Lucija Metelko", "Ana Ljubi"))

    read_a_file <- function(a_file){
        mesec_z <- colnames(read_xlsx(a_file, range = "D3:D3", col_types = "numeric"))
        mesec_k <- colnames(read_xlsx(a_file, range = "D4:D4"))

        df <- read_xlsx(a_file, range = "A9:E15",
                        col_names = c("datum", "dan", "za", "ko", "st_ur"),
                        col_types = c("numeric", "text", "text", "text", "numeric")
        )
        df$zacetek <- suppressWarnings(ifelse(is.na(as.numeric(df$za)), NA, round(as.numeric(df$za) * 24)))
        df$konec <- suppressWarnings(round(as.numeric(df$ko) * 24))
        df$odsotnost <- suppressWarnings(ifelse(is.na(as.numeric(df$za)), df$za, NA))
        df$st_ur[is.na(df$st_ur)] <- 0
        df <- df[, -3:-4]
        df$mesec <- substr(ifelse(df$datum < df$datum[1], mesec_k, mesec_z), 1, 3)
        return(df)
    }

    # Initiate your table
    # mes_df <- data.frame("datum" = numeric(), "dan" = character(), "st_ur" = numeric(), "zacetek" = numeric(), "konec" = numeric(), "odsotnost" = character(), "mesec" = character())
    # wrk_dir <- "LucijaMetelko-feb-20/"
    # # while(wrk_dir == "") wrk_dir <- readline("Kateri direktorij?")
    #
    # directory <- paste("/home/nino/Dokumenti/Matjaz/OA", wrk_dir, sep = "/")
    #
    # setwd(directory)
    # mes_df <- read_a_file("10-16feb20LucijaMetelko.xlsx")
    zac_tedna <-  Sys.Date() - as.numeric(format(Sys.Date(), "%u")) + 1
    mes_df <- data.frame("datum" = seq(zac_tedna, by = "day", length.out = 7))
    mes_df$dan <- weekdays( mes_df$datum)
    mes_df$zacetek <- c(rep(8, 5), NA, NA)
    mes_df$konec <- c(rep(14, 5), NA, NA)
    mes_df$odsotnost <- factor(c(rep("-", 5), "SO", "NE"), levels = c("-", "SO", "NE", "P", "D", "B", "ID", "DD", "IZ"),
                               labels = c("-", "sobota", "nedelja", "praznik", "dopust", "bolniška", "izredni dopust", "dodatni dopust", "izobraževanje"),
                               ordered = TRUE)
    mes_df$delovni_cas <-  mes_df$konec -  mes_df$zacetek

    previous <- reactive({mes_df})

    Trigger_orders <- reactive({
        if(is.null(input$hotable1)){return(previous())}
        else if(!identical(previous(),input$hotable1)){
            # hot.to.df function will convert your updated table into the dataframe
            previous <- as.data.frame(hot.to.df(input$hotable1))
            previous
        }
    })
    output$hotable1 <- renderHotable({Trigger_orders()}, readOnly = F)
    # You can see the changes you made
})

ui <- fluidPage(
    selectInput("OA", "Izberi asistentko:", choices = list("Lucija Metelko", "Ana Ljubi")),
    dateInput("teden", "Izberi teden:",
              format = "DD, dd. M. yyyy",
              language = "sl",
              weekstart = 1),
    hotable("hotable1"))
shinyApp(ui, server)
