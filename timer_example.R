

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
                           #Store a normal value
                           self$timestamp <- Sys.time()
                           #indicate that something has changed
                           self$invalidate()
                         })

                         output$texttime <- renderText(as.character(s()$timestamp))
                       }
                     ))

time <- Timer$new("mytimer")

shinyApp(time$ui, function(input, output, session){
  time$call()
})


