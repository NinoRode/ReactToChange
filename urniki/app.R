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

# Define UI for application that draws a histogram
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

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
