

Timer <- R6::R6Class("Timer",
                     inherit = ShinyModule,
                     public = list(
                       initialize = function(id){
                         super$initialize(id)
                       },
                       timestamp = NULL
                     ),
                     private = list(
                       .ui = function(){
                         ns <- NS(self$id)
                         
                         tagList(
                           fluidRow(
                             actionButton(ns("gettime"), "click me"),
                             textOutput(ns("texttime"))
                           )
                         )
                       },
                       server = function(input, output, session){
                         
                         s <- self$reactive()
                         
                         observeEvent(input$gettime,{
                           self$timestamp <- Sys.time()
                           self$invalidate()
                         })
                         
                         output$texttime <- renderText(as.character(s()$timestamp))
                       }
                     ))

time <- Timer$new("mytimer")

shinyApp(time$ui, function(input, output, session){
  time$call()
})


insertColumnUI <- function(id, input, output, session){
  moduleServer(
    id,
    function(input, output, session){
      
    }
  )
}

