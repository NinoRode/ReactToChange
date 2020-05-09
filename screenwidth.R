library(shiny)
# From https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$script('
                                var dimension = 0;
                                $(document).on("shiny:connected", function(e) {
                                    dimension = window.innerWidth;
                                    Math.random()
                                    Shiny.setInputValue("dimension", dimension, {priority: "event"});
                                });
                                $(window).resize(function(e) {
                                    dimension = window.innerWidth;
                                    Math.random()
                                    Shiny.setInputValue("dimension", dimension, {priority: "event"});
                                });
                            ')),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("dimension_display"),
      plotOutput("distPlot")
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

print(isolate(input$dimension))

  output$dimension_display <- renderText({
    input$dimension
  })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  onSessionEnded(stopApp)
})

# Run the application
shinyApp(ui = ui, server = server)
