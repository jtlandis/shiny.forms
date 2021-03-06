
R6Input <- R6::R6Class("R6Input",
                       inherit = ShinyModule,
                       public = list(
                         name = "",
                         selector = function(ns = NULL){
                           ns <- ns %||% getDefaultReactiveDomain()
                           glue(".shiny-input-container:has(#{ns('user_input')})")
                         },
                         inner_id = 'user_input',
                         label = NULL,
                         default = NULL,
                         initialize = function(id, name = "", label = NULL, default = NULL){
                           super$initialize(id)
                           self$name <- name
                           self$label <- label
                           self$default <- default
                         },
                         finalize = function(){
                           cat(glue("** Removing <{class(self)[1L]}> with id '{self$id}' from memory **"),"\n")
                         },
                         ui = function(id = self$id) {
                           eval(self$get_call(id))
                         },
                         edit = function(id = self$id) {
                           stop("Cannot use R6Input `$edit`")
                         },
                         edit_mod = function(input, output, session) {
                           stop("Cannot use R6Input `$edit_mod`")
                         },
                         get_call = function(id = self$id) {
                           stop("Cannot use R6Input `$get_call`")
                         },
                         preview = function(){
                           div(class = ifelse(self$selected,
                                              "ShinyForm-Element ShinyForm_selected",
                                              "ShinyForm-Element"),
                               `data-rank-id` = self$id,
                               self$ui())
                         },
                         print = function(){
                           fields <- formals(self$initialize)
                           fields <- map(setNames(nm = names(fields)), ~ self[[.x]])
                           cat("<", class(self)[1L], ">\n", sep = "")
                           names <- paste0(" * ", format(names(fields), justify = "right"), " : ")
                           values <- format(map_chr(fields, format), justify = "left")
                           cat(paste(names, values, collapse = "\n"), "\n", sep = "")
                           return(invisible(self))
                         }
                       ),
                       private = list(
                         server = function(input, output, session){
                           ns <- session$ns
                           value <- reactive(input$user_input)
                           return(list(value = value))
                         },
                         .obs = list()
                       ))

ShinyFormColumn <- R6::R6Class("ShinyFormColumn",
                               inherit = R6Input,
                               public = list(
                                 selector = function(ns){
                                   ns <- ns %||% getDefaultReactiveDomain()$ns
                                   id <- self$inner_id
                                   glue("#{ns(paste0(id,'-Container'))}:has(#{ns(id)})")
                                 },
                                 inner_id = "ShinyForm-Column",
                                 initialize = function(id,  width){
                                   super$initialize(id = id)
                                   self$width <- width
                                 },
                                 width = NULL,
                                 ui = function(id = self$id){
                                   ns <- NS(id)
                                   div(id = ns(glue("{self$inner_id}-Container")),
                                       class = "ShinyForm-Column-Container",
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
                                     sliderInput(ns("width"), label = "New Width:", value = self$width, min = 1L, max = 12L, step = 1L, ticks = F)
                                   )
                                 },
                                 edit_mod = function(input, output, session){
                                   ns <- session$ns
                                   private$.obs[[1L]] <- observe({
                                     req(input$width)
                                     self$width <- input$width
                                     updateShinyFormColumn(id = "ShinyForm-Column", width = input$width, session = session)
                                     cat(glue("{self$id} has changed!"),"\n")
                                   })
                                 },
                                 remove = function(input, ns = NULL){
                                   ns <- ns %||% getDefaultReactiveDomain()$ns
                                   removeUI(glue(".shiny-input-container:has(#{ns('width')})"), immediate = T)
                                   removeUI(self$selector(ns), immediate = T)
                                   if(!is.null(input)){
                                     remove_shiny_inputs(ns('width'), input)
                                   }
                                   private$.obs[[1L]]$destroy()
                                 },
                                 get_call = function(id = self$id) {
                                   ns <- NS(self$id)
                                   call("column", width = self$width, id = ns(self$inner_id))
                                 }
                               ))

R6TextInput <- R6::R6Class("R6TextInput",
                              inherit = R6Input,
                              public = list(
                                initialize = function(id, name = NULL, label = NULL, default = NULL){
                                  super$initialize(id, name = name, label = label, default = default)
                                },
                                get_call = function(id = self$id) {
                                  ns <- NS(id)
                                  call("textInput",inputId = ns("user_input"), label = self$label, value = self$default)
                                },
                                edit = function(id = self$id){
                                  ns <- NS(id)
                                  tagList(
                                    textInput(ns('name'), label = "Output Name", value = self$name),
                                    textInput(ns('label'), label = "Set Label", value = self$label),
                                    textInput(ns('default'), label = "Set Default", value = self$default)
                                  )
                                },
                                edit_mod = function(input, output, session){
                                  ns <- session$ns
                                  private$.obs[[1L]] <- observe({
                                    validate(
                                      need(!is.null(input$label)|!is.null(input$default),
                                           "Nothing has been Edited")
                                      )

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

                                  private$.obs[[2L]] <- observe({
                                    validate(
                                      need(!is.null(input$name), "Nothing has been Edited")
                                    )
                                    self$name <- input$name
                                  })
                                },
                                remove = function(input, ns = NULL){
                                  ns <- ns %||% getDefaultReactiveDomain()$ns
                                  removeUI(glue(".shiny-input-container:has(#{ns('label')})"), immediate = T)
                                  removeUI(glue(".shiny-input-container:has(#{ns('default')})"), immediate = T)
                                  removeUI(self$selector(ns), immediate = T)
                                  if(!is.null(input)){
                                    remove_shiny_inputs(c(ns('label'), ns('default'), ns('name'), ns('user_input')), input)
                                  }
                                  map(private$.obs, ~.x$destroy())
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
