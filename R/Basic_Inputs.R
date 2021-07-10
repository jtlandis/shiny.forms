empty_on_0str <- function(x) {
  if(is.null(x)&&(length(x)==0||(is.character(x)&&x==""))) return(character())
  x
}
empty2null <- function(x){
  if(is.character(x)&&x=="") return(NULL)
  x
}
R6Input <- R6::R6Class("R6Input",
                       inherit = ShinyModule,
                       public = list(
                         selector = function(ns = NULL){
                           ns <- ns %||% getDefaultReactiveDomain()
                           glue(".shiny-input-container:has(#{ns('user_input')})")
                         },
                         inner_id = 'user_input',
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
                           ns <- session$ns
                           value <- reactive(input$user_input)
                           return(list(value = value))
                         }
                       ))

ShinyFormColumn <- R6::R6Class("ShinyFromColumn",
                               inherit = R6Input,
                               public = list(
                                 selector = function(ns){
                                   ns <- ns %||% getDefaultReactiveDomain()$ns
                                   id <- self$inner_id
                                   glue(".{ns(paste0(id,'-Container'))}:has(#{ns(id)})")
                                 },
                                 inner_id = "ShinyForm-Column",
                                 initialize = function(id, width){
                                   super$initialize(id)
                                   self$width <- width
                                 },
                                 width = NULL,
                                 ui = function(id = self$id){
                                   ns <- NS(id)
                                   div(class = ns(glue("{self$inner_id}-Container")),
                                       h3(self$id, class = 'SFC-label', hidden = NA),
                                       column(
                                         self$width,
                                         id = ns(self$inner_id),
                                         `data-rank-id` = paste0(ns(self$inner_id),'-',self$width),
                                         class = "ShinyForm-Column"
                                       ),
                                       sortable_js(ns(self$inner_id))
                                   )
                                   
                                 },
                                 edit = function(id = self$id){
                                   ns <- NS(id)
                                   tagList(
                                     numericInput(ns("width"), label = "New Width", 
                                                  value = self$width, min = 1, max = 12)
                                   )
                                 },
                                 edit_mod = function(input, output, session){
                                   ns <- session$ns
                                   observe({
                                     req(input$width)
                                     self$width <- input$width
                                     updateShinyFormColumn(id = ns("ShinyForm-Column"), width = input$width, session = session)
                                   })
                                 },
                                 remove = function(input, ns = NULL){
                                   ns <- ns %||% getDefaultReactiveDomain()$ns
                                   removeUI(glue(".shiny-input-container:has(#{ns('width')})"))
                                   removeUI(self$selector(ns))
                                   if(!is.null(input)){
                                     remove_shiny_inputs(ns('width'), input)
                                   }
                                 }
                               ))

R6TextInput <- R6::R6Class("R6TextInput",
                              inherit = R6Input,
                              public = list(
                                initialize = function(id, label = NULL, value = NULL){
                                  super$initialize(id)
                                  self$label <- label
                                  self$default <- value
                                },
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
                                    self$label <- empty2null(input$label)
                                    self$default <- empty_on_0str(input$default)
                                    updateTextInput(session = session,
                                                    inputId = "user_input",
                                                    label = self$label, 
                                                    value = self$default)
                                    if(is.null(self$label)){
                                      addClass(id = 'user_input-label', 'shiny-label-null')
                                    }
                                  })
                                },
                                remove = function(input, ns = NULL){
                                  ns <- ns %||% getDefaultReactiveDomain()$ns
                                  removeUI(glue(".shiny-input-container:has(#{ns('label')})"))
                                  removeUI(glue(".shiny-input-container:has(#{ns('default')})"))
                                  removeUI(self$selector(ns))
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
