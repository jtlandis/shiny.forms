

ui <- fluidPage(
  fluidRow(style = "background-color:#FF0000;",
           "First Row",
    div(style = "background-color:#00FF00",id = "Form_Container", class = "FormContainer",
             "Second Row",
      column(12, id = "colID-1",
             "First Column 12",
             actionButton("addButton", NULL),
             column(6, "column in 12 - 6", id = "colID-1-1"),
             column(6, " column in 12 - 6", id = "colID-1-2")),
      column(4,
             "Next Column set 4"),
      column(8, "Next Column set 8")
    ),
    fluidRow()
  )
)

ui <- fluidPage(
  fluidRow(
           column(6, "6-width\n6-width", style = "background-color:#FF0000;height:50px;"),
           column(3, "3-width"),
           column(3, "3-width"),
           column(3, "3-width"))
)

shinyApp(ui, server = function(input, output, session){})
textIn1 <- R6TextInput$new("New1", "Sample Name")
textIn2 <- R6TextInput$new("New2", "User")
l <- list(textIn1, textIn2)
shinyApp(ui = fluidPage(map(l, ~.x$ui),
                        textOutput("View")), server = function(input, output, session){
  Out <- map(l, ~.x$call())
  output$View <- renderText({
    vals <- map(Out, ~.x$value())
    reduce(vals, ~str_c(.x, .y, sep = "\n"))
    }
    )
})

shinyApp(ShinyFormBuilder$new("id")$ui, function(input,output,session){
})
