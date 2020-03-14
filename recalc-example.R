# From https://stackoverflow.com/questions/44074184/reactive-calculate-columns-in-rhandsontable-in-shiny-rstudio

library(shiny)
library(datasets)
library(rhandsontable)
library(data.table)

ui=fluidPage(

  rHandsontableOutput("table1")

)

server=function(input, output, session) {

  mt=reactive({

    datacopy <- NULL

    #For initial data upload
    if(is.null(input$table1)){
      datacopy= mtcars[, names(mtcars) %in% c("mpg" , "cyl" , "disp")]
      datacopy=data.table(datacopy)

    }else{
      datacopy = hot_to_r(input$table1)

      #If there is change in data
      if(!is.null(input$table1$changes$changes)){

        row.no <- unlist(input$table1$changes$changes)[1]
        col.no <- unlist(input$table1$changes$changes)[2]
        new.val <- unlist(input$table1$changes$changes)[4]
        #If the changed value is mpg or cyl
        if(col.no == 0 || col.no == 1){
          datacopy[(row.no+1), 3] = datacopy[(row.no+1), 1]/datacopy[(row.no+1), 2]
        }else{
          datacopy[(row.no+1), 2] = datacopy[(row.no+1), 1]/datacopy[(row.no+1), 3]
        }

      }

    }

    datacopy

  })


  output$table1=renderRHandsontable({

    rhandsontable(mt())

  })

}



shinyApp(ui,server)
