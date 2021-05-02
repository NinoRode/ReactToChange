library(shiny)

library(rhandsontable)

editTable <- function(DF, outdir=getwd(), outfilename="table"){
  #' From http://stla.github.io/stlapblog/posts/shiny_editTable.html
  #' Shiny app allowing to edit a data frame and to save the result 
  #' in a file than can be loaded in R.
  ui <- shinyUI(fluidPage(
    
    titlePanel("Edit and save a table"),
    sidebarLayout(
      sidebarPanel(
        helpText("Shiny app based on an example given in the rhandsontable package.", 
                 "Right-click on the table to delete/insert rows.", 
                 "Double-click on a cell to edit"),
        
        wellPanel(
          h3("Table options"),
          radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
        ),
        br(), 
        
        wellPanel(
          h3("Save table"), 
          div(class='row', 
              div(class="col-sm-6", 
                  actionButton("save", "Save")),
              div(class="col-sm-6",
                  radioButtons("fileType", "File type", c("ASCII", "RDS")))
          )
        )
        
      ),
      
      mainPanel(
        wellPanel(
          uiOutput("message", inline=TRUE)
        ),
        
        actionButton("cancel", "Cancel last action"),
        br(), br(), 
        
        rHandsontableOutput("hot"),
        br(),
        
        wellPanel(
          h3("Add a column"),
          div(class='row', 
              div(class="col-sm-5", 
                  uiOutput("ui_newcolname"),
                  actionButton("addcolumn", "Add")),
              div(class="col-sm-4", 
                  radioButtons("newcolumntype", "Type", c("integer", "double", "character"))),
              div(class="col-sm-3")
          )
        )
        
      )
    )
  ))
  
  server <- shinyServer(function(input, output) {
    
    values <- reactiveValues()
    
    ## Handsontable
    observe({
      if (!is.null(input$hot)) {
        values[["previous"]] <- isolate(values[["DF"]])
        DF = hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
    })
    
    output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
    })
    
    ## Save 
    observeEvent(input$save, {
      fileType <- isolate(input$fileType)
      finalDF <- isolate(values[["DF"]])
      if(fileType == "ASCII"){
        dput(finalDF, file=file.path(outdir, sprintf("%s.txt", outfilename)))
      }
      else{
        saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
      }
    }
    )
    
    ## Cancel last action    
    observeEvent(input$cancel, {
      if(!is.null(isolate(values[["previous"]]))) values[["DF"]] <- isolate(values[["previous"]])
    })
    
    ## Add column
    output$ui_newcolname <- renderUI({
      textInput("newcolumnname", "Name", sprintf("newco l%s", 1+ncol(values[["DF"]])))
    })
    observeEvent(input$addcolumn, {
      DF <- isolate(values[["DF"]])
      values[["previous"]] <- DF
      newcolumn <- eval(parse(text=sprintf('%s(nrow(DF))', isolate(input$newcolumntype))))
      values[["DF"]] <- setNames(cbind(DF, newcolumn, stringsAsFactors=FALSE), c(names(DF), isolate(input$newcolumnname)))
    })
    
    ## Message
    output$message <- renderUI({
      if(input$save==0){
        helpText(sprintf("This table will be saved in folder \"%s\" once you press the Save button.", outdir))
      }else{
        outfile <- ifelse(isolate(input$fileType)=="ASCII", "table.txt", "table.rds")
        fun <- ifelse(isolate(input$fileType)=="ASCII", "dget", "readRDS")
        list(helpText(sprintf("File saved: \"%s\".", file.path(outdir, outfile))),
             helpText(sprintf("Type %s(\"%s\") to get it.", fun, outfile)))
      }
    })
    
  })
  
  ## run app 
  runApp(list(ui=ui, server=server))
  return(invisible())
}

editTable(head(iris))

###############################################################################
# From https://swechhya.github.io/excelR/
#
# Make function from it
# Make a Shiny module
###############################################################################

library(excelR)

editJExcTable <- function(DF=head(iris), outdir=getwd(), outfilename="table"){
  
  shinyApp(
    ui = fluidPage(
      excelOutput("table"),
      tableOutput("selectedData")),
    server = function(input, output, session) {
      output$table <-
        renderExcel(excelTable(data = DF))
      observeEvent(input$table,{
        table_data <- excel_to_R(input$table)
        if(!is.null(table_data)){
          output$selectedData <- renderTable(table_data)
          # print(table_data)
        }
        
      })
    }
  )
} 

editJExcTable()

selectInJExcTable <- function(DF=head(iris), outdir=getwd(), outfilename="table"){
  
  shinyApp(
    
    ui = fluidPage(tags$h6("Excel Table:"),
                   excelOutput("table", height = 175),
                   tags$h6("Selected Data:"),
                   tableOutput("selectedData"),
    ),
    
    server = function(input, output, session) {
      
      output$table <- excelR::renderExcel(excelTable(data = DF, getSelectedData = TRUE))
      
      # Print the selected data in table
      observeEvent(input$table,{
        output$selectedData <- renderTable(get_selected_data(input$table))
      })
    }
  )
  
}

selectInJExcTable()
  
