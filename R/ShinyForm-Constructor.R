
#' @include ShinyModule-Inputs.R

SFConstructor <- R6::R6Class("ShinyFromConstructor",
                           inherit = ShinyModule,
                           public = list(
                             initialize = function(id){
                               super$initialize(id)
                               if(!inherits(private$R6,"R6ClassGenerator")) stop("ShinyFormConstructor can only carry a `R6ClassGenerator` object")
                             },
                             ui = function(id = self$id){
                               ns <- NS(id)
                               tagList(
                                 actionButton(ns("init"), self$id, width = '100%', class = "btn-primary")
                               )
                             },
                             call = function(){
                               # (layout, selected)
                               # iwalk(list(layout = layout, selected = selected),
                               #       function(x, y, fun){ assign(y, x, envir = environment(fun))}, fun = private$server)
                               moduleServer(self$id,
                                            private$server)
                             },
                             layout = NULL
                           ),
                           private = list(
                             R6 = NULL,
                             index = function() {
                               objs <- self$layout$objects[type==self$id,][["obj"]]
                               if (length(objs)==0L) return(1L)
                               indices <- as.integer(str_extract(map_chr(objs, ~.x$id), "[0-9]+$"))
                               i_min <- min(indices)
                               if (i_min!=1L) return(1L)
                               indices <- indices[order(indices)]
                               .diff <- diff(indices)
                               if (any(.diff>1L))
                                 return(which(.diff>1L)[1L] + 1L)
                               else
                                 return(max(indices) + 1L)

                             },
                             counter = 0L,
                             increment = function(){
                               private$counter <- private$counter + 1L
                               invisible(self)
                             },
                             server = function(input, output, session){
                               ns <- session$ns

                               val <- reactive({
                                 input$insert
                                 private$increment()
                                 private$update()
                                 private$make()
                               })
                               return(list(value = val, insert = reactive({input$insert}), id = self$id))
                             },
                             update = function(){},
                             make = function(){
                               eval.parent(quote(private$R6$new(glue("{private$index()}"))))
                             }
                           ))


SFC_Column <- R6::R6Class("SFC_Column",
                          inherit = SFConstructor,
                          public = list(
                            ui = function(id = self$id){
                              ns <- NS(id)
                              tagList(
                                sliderInput(ns("width"), label = "Column Width", min = 1L, max = 12L,value = self$width, step = 1L, ticks = F),
                                actionButton(ns("insert"), "OK", class = "btn-primary")
                              )
                            },
                            width = 6L
                          ),
                          private = list(
                            R6 = ShinyFormColumn,
                            update = function(){
                              eval.parent(quote({
                                self$width <- input$width
                              }))
                            },
                            make = function(){
                              eval.parent(
                                quote(private$R6$new(glue("{private$index()}"), width = self$width))
                              )
                            }
                          ))



SFC_TextInput <- R6::R6Class("SFC_TextInput",
                          inherit = SFConstructor,
                          public = list(
                            ui = function(id = self$id){
                              ns <- NS(id)
                              tagList(
                                textInput(ns('name'), label = 'Output Name:', value = self$name),
                                textInput(ns('label'), label = 'Field Label:', value = self$label),
                                textInput(ns('default'), label = 'Default Value:', value = self$default),
                                actionButton(ns("insert"), "OK", class = "btn-primary")
                              )
                            },
                            name = NULL,
                            label = NULL,
                            default = NULL
                          ),
                          private = list(
                            R6 = R6TextInput,
                            update = function(){
                              eval.parent(
                                quote({
                                  self$name <- empty2null(input$name)
                                  self$label <- empty2null(input$label)
                                  self$default <- empty2null(input$default)
                                })
                              )
                            },
                            make = function(){
                              eval.parent(
                                quote(
                                  private$R6$new(glue("{self$id}-{private$index()}"),
                                                 name = self$name,
                                                 label = self$label,
                                                 default = self$default)
                                  )
                                )
                            }
                          ))


