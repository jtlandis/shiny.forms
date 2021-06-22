library(shiny)
"%||%" <- function(a,b) if(is.null(a)) b else a
remove_shiny_inputs <- function(id, .input) {
  impl <- .subset2(.input, "impl")
  lgl <- id %in% impl$.values$keys()
  if(any(!lgl)) warn(glue("The following `id`s were not found in shiny server input and cannot be removed : ", glue_collapse(glue("`{id[!lgl]}`"), sep = ", ", last = ", and ")))
  to_rm <- id[lgl]
  invisible(
    lapply(to_rm, function(i) {
      impl$.values$remove(i)
    })
  )
}
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
                             ui = function(id = self$id){
                               ns <- NS(id)
                               tagList()
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
                           private = list(
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
                                 ui = function(id = self$id){
                                   ns <- NS(id)
                                   div(class = ns("ShinyForm-Column-Container"),
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
                                 edit = function(id = self$id){
                                   ns <- NS(id)
                                   tagList(
                                     numericInput(ns("width"), label = "New Width", 
                                                  value = self$width, min = 1, max = 12),
                                     actionButton(ns('rm'), '', icon = icon('minus'))
                                   )
                                 },
                                 remove = function(input = NULL, session = NULL){
                                   session <- session %||% getDefaultReactiveDomain()
                                   ns <- session$ns
                                   removeUI(glue(".shiny-input-container:has(#{ns('width')})"))
                                   removeUI(glue(".{ns('ShinyForm-Column-Container')}:has(#{ns('ShinyForm-Column')})"))
                                   removeUI(glue("#{ns('rm')}"))
                                   if(!is.null(input)){
                                     remove_shiny_inputs(c(ns('rm'), ns('width')), input)
                                   }
                                 }
                               ),
                               private = list(
                                 finalizer = function(){
                                   session <- getDefaultReactiveDomain()
                                   if(!is.null(session)){
                                     ns <- session$ns
                                     self$remove()
                                   }
                                 },
                                 server = function(input, output, session){
                                   ns <- session$ns
                                   observe({
                                     updateShinyFormColumn(id = ns("ShinyForm-Column"), width = input$width, session = session)
                                   })
                                   
                                   observeEvent(input$rm, {
                                     self$remove(input, session)
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
      let col = $('#' + message.id);
      if(col.length !=0){
        col = col[0];
        col.className = col.className.replace(/(col-..-)([0-9]+)/, '$1' + message.width);
      }
    }"))
  ),
  br(),
  actionButton('addButton', '', icon = icon('plus')),
  actionButton('browse', "Browse Env")
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
        col$ui(), col$edit()
      )
    )
    col$call()
  })
  observeEvent(input$browse, {
    browser()
  })
}

shinyApp(ui, server)

ModTest <- R6::R6Class("ModTest", inherit = ShinyModule,
                       public = list(
                         cols = list(),
                         push_col = function(id){
                           self$cols[[private$count]] <- ShinyFormColumn$new(id, 6)
                         },
                         ui = function(id = self$id){
                           ns <- NS(id)
                           tagList(
                             br(),
                             actionButton(ns('addButton'), '', icon = icon('plus'))
                           )
                         }
                       ),
                       private = list(
                         server = function(input, output, session){
                           s <- self$reactive()
                           ns <- session$ns
                           observeEvent(input$addButton, {
                             self$invalidate()
                             i <- sprintf('%04d', input$addButton)
                             id <- sprintf('test%s', i)
                             self$push_col(id)
                             insertUI(
                               selector = paste0('#',ns('addButton')),
                               where = "beforeBegin",
                               ui = tagList(
                                 self$cols[[private$count]]$ui(ns(self$cols[[private$count]]$id)), self$cols[[private$count]]$edit(ns(self$cols[[private$count]]$id))
                               )
                             )
                             self$cols[[private$count]]$call()
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
  .mod$ui(),
  actionButton('browse', "Browse Env")
)
server <- function(input, output, session){
  .mod$call()
  observeEvent(input$browse,{
    browser()
  })
}
shinyApp(ui, server)
