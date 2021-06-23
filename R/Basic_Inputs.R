
R6Input <- R6::R6Class("R6Input",
                       inherit = ShinyModule,
                       public = list(
                         label = NULL,
                         selected = FALSE,
                         initialize = function(id, label = NULL){
                           super$initialize(id)
                           self$label <- label
                         },
                         ui = function(id = self$id) {
                           stop("Cannot use R6Input `$ui`")
                         },
                         edit = function(id = self$id) {
                           stop("Cannot use R6Input `$edit`")
                         },
                         preview = function(){
                           div(class = ifelse(self$selected,
                                              "ShinyForm-Element ShinyForm_selected",
                                              "ShinyForm-Element"), 
                               `data-rank-id` = self$id,
                               self$ui())
                         }
                       ),
                       private = list(
                         server = function(input, output, session){
                           value <- reactive(input$user_input)
                           return(list(value = value))
                         }
                       ))

R6TextInput <- R6::R6Class("R6TextInput",
                              inherit = R6Input,
                              public = list(
                                ui = function(id = self$id){
                                  ns <- NS(id)
                                  tagList(
                                    textInput(ns("user_input"), label = self$label, value = "")
                                  )
                                }
                              ))

R6SelectInput <- R6::R6Class("R6SelectInput",
                             inherit = R6Input,
                             public = list(
                               choices = NULL,
                               initialize = function(id, label = NULL, choices){
                                 super$initialize(id, label)
                                 self$choices <- choices
                               }),
                             private = list(
                               .ui = function(){
                                 ns <- NS(self$id)
                                 tagList(selectInput(ns("user_input"),
                                                     label = self$label,
                                                     choices = self$choices))
                               }
                             )
                             )

R6NumericInput <- R6::R6Class("R6NumericInput",
                              inherit = R6Input,
                              public = list(
                                min = NA,
                                max = NA,
                                initialize = function(id, label = NULL, min = NA, max = NA){
                                  super$initialize(id, label)
                                  self$min <- min
                                  self$max <- max
                                }
                              ),
                              private = list(
                                .ui = function(){
                                  ns <- NS(self$id)
                                  tagList(
                                    numericInput(ns("user_input"),
                                                 label = self$label,
                                                 min = self$min,
                                                 max = self$max)
                                  )
                                }
                              ))
