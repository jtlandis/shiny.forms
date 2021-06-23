
R6Input <- R6::R6Class("R6Input",
                       inherit = ShinyModule,
                       public = list(
                         label = NULL,
                         default = NULL,
                         initialize = function(id, label = NULL, default = NULL){
                           super$initialize(id)
                           self$label <- label 
                           self$default <- default
                         },
                         ui = function(id = self$id) {
                           stop("Cannot use R6Input `$ui`")
                         },
                         edit = function(id = self$id) {
                           stop("Cannot use R6Input `$edit`")
                         },
                         edit_mod = function(input, output, session) {
                           stop("Cannot use R6Input `$edit_mod`")
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
                                    textInput(ns("user_input"), label = self$label, value = self$default)
                                  )
                                },
                                edit = function(id = self$id){
                                  ns <- NS(id)
                                  tagList(
                                    textInput(ns('label'), label = "Set Label", value = self$label),
                                    textInput(ns('default'), label = "Set Default", value = self$default)
                                  )
                                },
                                edit_mod = function(input, output, session){
                                  ns <- session$ns
                                  observe({
                                    self$label <- input$label
                                    self$default <- input$default
                                    updateTextInput(session = session,
                                                    inputId = "user_input",
                                                    label = self$label, 
                                                    value = self$default)
                                  })
                                },
                                remove = function(input, ns = NULL){
                                  ns <- ns %||% getDefaultReactiveDomain()$ns
                                  removeUI(glue(".shiny-input-container:has(#{ns('label')})"))
                                  removeUI(glue(".shiny-input-container:has(#{ns('default')})"))
                                  removeUI(glue(".shiny-input-container:has(#{ns('user_input')})"))
                                  if(!is.null(input)){
                                    remove_shiny_inputs(c(ns('label'),ns('default'), ns('user_input')), input)
                                  }
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
