ui <- fluidPage(
  tags$head(
    tags$style(HTML(
    '.outer, .inner, #ShinyForm-Builder-Container, .item {
      border: 2px solid;
      padding: 5px;
      margin: 5px;'
  ))),
  h1("GeeksforGeeks"),
  br(),
  title = "How to Check if an element is a child of a parent using JavaScript?",
  fluidRow(
    column(6, id = "sorting",
           div(class = "item", "item 1"),
           div(class = "item", "item 2")),
    column(6, id = "sorting",
           div(class = "item", "item 3"),
           div(class = "item", "item 4"))
  ),
  sortable_js("sorting"),
  div(class = "outer", id = "Outer-id", "Outer div element",
      div(class = "inner", "Inner div element")),
  div(id = "ShinyForm-Builder-Container",
      p("This is the start of the container"),
      div(id = "Some-id", "this element has some id"), 
      verbatimTextOutput("click_id"))
)

shinyApp(ui, server = function(input, output, session){
  output$click_id <- renderPrint(input$ShinyForm_clicked_id)
})


# script_defer <- function(...){
#   tagAppendAttributes(
#     tags$script(
#       ...
#     ),
#     defer = NA
#   )
# }
# 
# function withinShinyForm(target) {
#   parent = document.querySelector(".ShinyForm-Builder-Container");
#   return parent.contains(target);
# }
# 
# $(".ShinyForm-Builder-Container").on("click", function(e){
#   if(e.target.id.length == 0) { return } // No ID, return
#   Shiny.setInputValue("ShinyForm_clicked_id", e.target.id, {priority: "event"} )
# })
