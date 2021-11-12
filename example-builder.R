
library(shiny.forms)
library(shiny)
#example

test <- ShinyFormBuilder$new("test_id")
shinyApp(ui = fluidPage(
  tags$head(
    tags$style(HTML("
                    .ShinyForm-break {
                      line-height: 100%;
                    }
                    .ShinyForm-Preview-Container {
                      padding: 15px;
                    }
                    .ShinyForm-Container {
                      padding: 10px;
                    }
                    .ShinyForm-Menu {
                      padding: 5px;
                    }
                    /*.ShinyForm-Element:hover {
                      background-color: #7682FF;
                      opacity: .5;
                    }*/
                    .ShinyForm-Element-selected {
                      border: 2px dotted grey;
                    }
                    .ShinyForm-Column {
                      border: .5px solid grey;
                      padding: 25px;
                      border-radius: 15px;
                    }
                    .ShinyForm-selected,
                    .ShinyForm-Element.ShinyForm-selected,
                    .ShinyForm-Column.ShinyForm-selected {
                      border: 5px solid red;
                      opacity: .5;
                    }
                    /*.ShinyForm-Column-selected:hover {
                      background-color: #FF7676;
                      opacity: .5;
                    }*/
                    "))
  ),
  test$ui(),
  actionButton(".browse", "Browse")), function(input, output, session){

    p <- test$reactive()

    test$call()

    observeEvent(input$.browse, {
      browser()

      return(NULL)
    })
  })
