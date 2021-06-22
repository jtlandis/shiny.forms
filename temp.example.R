ui <- fluidPage(
  tags$head(
    tags$style(HTML(
    '.outer, .inner, #ShinyForm-Builder-Container {
      border: 2px solid;
      padding: 5px;
      margin: 5px;'
  ))),
  tags$head(script_defer(HTML(
    '$("#ShinyForm-Builder-Container").on("click", function(e){
  if(e.target.id.length == 0) { return } // No ID, return
  if(e.target.id == "ShinyForm-Builder-Container") { return }
  //parent = document.querySelector("#ShinyForm-Builder-Container");
  //if(!parent.contains(e.target)) { return }
  Shiny.setInputValue("ShinyForm_clicked_id", e.target.id, {priority: "event"} )
});'
  ))),
  h1("GeeksforGeeks"),
  br(),
  title = "How to Check if an element is a child of a parent using JavaScript?",
  div(class = "outer", id = "Outer-id", "Outer div element",
      div(class = "inner", "Inner div element")),
  div(id = "ShinyForm-Builder-Container",
      p("This is the start of the container"),
      div(id = "Some-id", "this element has some id"), 
      verbatimTextOutput("click_id")),
  #tags$script()
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