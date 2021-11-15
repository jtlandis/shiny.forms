
library(shiny.forms)
library(shiny)
#example

test <- ShinyFormBuilder$new("test_id")
shinyApp(ui = fluidPage(
  test$ui(),
  actionButton(".browse", "Browse")), function(input, output, session){

    p <- test$reactive()

    test$call()

    observeEvent(input$.browse, {
      browser()

      return(NULL)
    })
  })
