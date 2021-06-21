
updateMy_Col <- function(id, width = 6, session){
  m <- list(
    id = id,
    width = width
  )
  print(m)
  session$sendCustomMessage("handler1", m)
}
Col_UI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      6,
      id = ns("My_Col"),
      class = "My_Col"
    ),
    numericInput(ns("width"), "New Width", value = 6, min = 1, max = 12)
  )
  
}

Col_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      observe({
        updateMy_Col(id = ns("My_Col"), width = input$width, session = session)
      })
    }
  )
}

header_ui <- tags$head(
  tags$style(HTML(
    ".My_Col {
              border: .5px solid grey;
              padding: 25px;
              border-radius: 15px;
            }
      " )),
  tags$script(HTML("Shiny.addCustomMessageHandler('handler1', CustomJSMethod);
    function CustomJSMethod(message) {
      console.log(message.id) ;
      let col = $('#' + message.id)[0] ;
      col.className = col.className.replace(/(col-..-)([0-9]+)/, '$1' + message.width);
    }"))
)  

ui <- fluidPage(
  header_ui,
  br(),
  actionButton('addButton', '', icon = icon('plus'))
)

server <- function(input,output, session){
  observeEvent(input$addButton, {
    i <- sprintf('%04d', input$addButton)
    id <- sprintf('test%s', i)
    insertUI(
      selector = '#addButton',
      where = "beforeBegin",
      ui = Col_UI(id)
    )
    Col_Server(id)
  })
}

shinyApp(ui, server)


nested_module_UI <- function(id) {
  ns <- NS(id)
  tagList(
    p("This is a new module with id ", id),
    actionButton(ns('addButton'), '', icon = icon('plus'))
  )
}

nested_module_Sever <- function(id){
  moduleServer(id,
               module = function(input, output, session){
                 ns <- session$ns
                 observeEvent(input$addButton, {
                   i <- sprintf('%04d', input$addButton)
                   id <- sprintf('test%s', i)
                   insertUI(
                     selector = paste0('#',ns('addButton')),
                     where = "beforeBegin",
                     ui = Col_UI(ns(id))
                   )
                   Col_Server(id)
                 }, ignoreInit = T)
               })
}

ui2 <- fluidPage(header_ui,
                 nested_module_UI("T1"),
                 #nested_module_UI("T2")
                 )

server2 <- function(input, output, session){
  nested_module_Sever("T1")
 # nested_module_Sever("T2")
}


shinyApp(ui2, server2)
