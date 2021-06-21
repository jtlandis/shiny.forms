library(shiny)

ShinyModule <- R6::R6Class("ShinyModule",
                           public = list(
                             id = NULL,
                             initialize = function(id){
                               private$reactiveDep <- function(x) NULL
                               self$id <- id
                             },
                             call = function(){
                               moduleServer(self$id,
                                            private$server)
                             },
                             reactive = function() {
                               if(is.null(private$reactiveExpr)) {
                                 private$reactiveDep <- reactiveVal(0L)
                                 private$reactiveExpr <- reactive({
                                   private$reactiveDep()
                                   self
                                 })
                               }
                               private$reactiveExpr
                             },
                             invalidate = function(){
                               private$count <- private$count + 1L
                               cat("ReactiveDep:", private$count, "\n")
                               private$reactiveDep(private$count)
                               invisible()
                             }
                           ),
                           active = list(
                             ui = function(value){
                               if(missing(value)){
                                 private$.ui()
                               } else {
                                 stop("`$ui` is read only.")
                               }
                             }
                           ),
                           private = list(
                             .ui = function(){
                               ns <- NS(self$id)
                               tagList()
                             },
                             server = function(input, output, session){
                               
                             },
                             reactiveDep = NULL,
                             reactiveExpr = NULL,
                             
                             count = 0L
                           ))
ShinyFormColumn <- R6::R6Class("ShinyFromColumn",
                               inherit = ShinyModule,
                               public = list(
                                 initialize = function(id, width){
                                   super$initialize(id)
                                   self$width <- width
                                 },
                                 width = NULL,
                                 server2 = function(input, output, session){
                                   ns <- NS(self$id)
                                   observe({
                                     updateShinyFormColumn(id = ns("ShinyForm-Column"), width = input$width, session = session)
                                   })
                                 }
                               ),
                               active = list(
                                 edit = function(value){
                                   if(missing(value)){
                                     private$.edit()
                                   } else {
                                     stop("`$edit` is read only.")
                                   }
                                 }
                               ),
                               private = list(
                                 .ui = function(){
                                   ns <- NS(self$id)
                                   tagList(
                                     column(
                                       self$width,
                                       id = ns("ShinyForm-Column"),
                                       `data-rank-id` = paste0(ns("ShinyForm-Column"),'-',self$width),
                                       class = "ShinyForm-Column"
                                     ),
                                     sortable_js(ns("ShinyForm-Column"),
                                                 options = sortable_options(
                                                   #onSort = sortable_js_capture_input(ns(paste0("Preview_Sortable_Order-", index))) #may change how we get order later.
                                                 ))
                                   )
                                   
                                 },
                                 .edit = function(){
                                   ns <- NS(self$id)
                                   tagList(
                                     numericInput(ns("width"), label = "New Width", 
                                                  value = self$width, min = 1, max = 12)
                                   )
                                 },
                                 server = function(input, output, session){
                                   ns <- NS(self$id)
                                   observe({
                                     updateShinyFormColumn(id = ns("ShinyForm-Column"), width = input$width, session = session)
                                   })
                                 }
                               ))


ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      ".ShinyForm-Column {
                      border: .5px solid grey;
                      padding: 25px;
                      border-radius: 15px;
                    }
      "
    )),
    tags$script(HTML("Shiny.addCustomMessageHandler('updateShinyFormColumn', updateShinyFormColumn);
    function updateShinyFormColumn(message) {
      console.log(message.id) ;
      let col = $('#' + message.id)[0] ;
      col.className = col.className.replace(/(col-..-)([0-9]+)/, '$1' + message.width);
    }"))
  ),
  br(),
  actionButton('addButton', '', icon = icon('plus'))
)

server <- function(input, output, session){
  observeEvent(input$addButton, {
    i <- sprintf('%04d', input$addButton)
    id <- sprintf('test%s', i)
    col <- ShinyFormColumn$new(id, 6)
    insertUI(
      selector = '#addButton',
      where = "beforeBegin",
      ui = tagList(
        col$ui, col$edit
      )
    )
    col$call()
  })
}

shinyApp(ui, server)

ModTest <- R6::R6Class("ModTest", inherit = ShinyModule,
                       private = list(
                         .ui = function(){
                           ns <- NS(self$id)
                           tagList(
                             br(),
                             actionButton(ns('addButton'), '', icon = icon('plus'))
                           )
                         },
                         server = function(input, output, session){
                           ns <- NS(self$id)
                           observeEvent(input$addButton, {
                             i <- sprintf('%04d', input$addButton)
                             id <- sprintf('test%s', i)
                             col <- ShinyFormColumn$new(ns(id), 6)
                             insertUI(
                               selector = paste0('#',ns('addButton')),
                               where = "beforeBegin",
                               ui = tagList(
                                 col$ui, col$edit
                               )
                             )
                             col$call()
                           })
                         }
                       ))

.mod <- ModTest$new("Test")
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      ".ShinyForm-Column {
                      border: .5px solid grey;
                      padding: 25px;
                      border-radius: 15px;
                    }
      "
    )),
    tags$script(HTML("Shiny.addCustomMessageHandler('updateShinyFormColumn', updateShinyFormColumn);
    function updateShinyFormColumn(message) {
      console.log(message.id) ;
      let col = $('#' + message.id)[0] ;
      col.className = col.className.replace(/(col-..-)([0-9]+)/, '$1' + message.width);
    }"))
  ),
  .mod$ui
)
server <- function(input, output, session){
  .mod$call()
}
shinyApp(ui, server)
