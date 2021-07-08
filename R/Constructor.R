

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
                                 actionBttn(ns("init"), self$id, block = T, style = 'minimal', color = 'primary')
                               )
                             },
                             call = function(){
                               # (layout, selected)
                               # iwalk(list(layout = layout, selected = selected),
                               #       function(x, y, fun){ assign(y, x, envir = environment(fun))}, fun = private$server)
                               moduleServer(self$id,
                                            private$server)
                             }
                           ),
                           private = list(
                             R6 = NULL,
                             counter = 0L,
                             increment = function(){
                               private$counter <- private$counter + 1L
                               invisible(self)
                             },
                             configure = NULL
                           ))


SFC_Column <- R6::R6Class("SFC_Column",
                          inherit = SFConstructor,
                          private = list(
                            R6 = ShinyFormColumn,
                            server = function(input, output, session){
                              ns <- session$ns
                              
                              observeEvent(input$init, {
                                showModal(
                                  modalDialog(
                                    fluidRow(
                                      numericInput(ns("width"), "Column Width", value = 6L, min = 1L, max = 12L)),
                                    footer = tagList(
                                      modalButton("Cancel"),
                                      actionButton(ns("insert"), "OK")
                                    )
                                  ))
                              })
                              observeEvent(input$insert, {
                                removeModal()
                              })
                              # observeEvent(input$insert, {
                              #   removeModal()
                              #   browser()
                              #   private$increment()
                              #   obj <- private$R6$new(private$counter, width = input$width)
                              #   parent <- selected()
                              #   layout$add_object(obj = obj,
                              #                     parent = parent,
                              #                     dom = ns(glue("{private$counter}-ShinyForm-Column")),
                              #                     type = "column")
                              #   insertUI(glue("#{parent}"),
                              #            where = "beforeEnd",
                              #            ui = obj$ui(ns(obj$id)))
                              #   obj$call() #insure things that need to be reactive are active
                              #   print(layout$objects)
                              # })
                              val <- reactive({
                                input$insert
                                private$increment()
                                private$R6$new(glue("{private$counter}"), width = isolate(input$width))
                              })
                              return(list(value = val, insert = reactive({input$insert}), clicked = reactive({input$init})))
                            }
                          ))



SFC_TextInput <- R6::R6Class("SFC_TextInput",
                          inherit = SFConstructor,
                          private = list(
                            R6 = R6TextInput,
                            server = function(input, output, session){
                              ns <- session$ns
                              
                              val <- reactive({
                                input$init
                                private$increment()
                                private$R6$new(glue("{self$id}-{private$counter}"))
                              })
                              return(list(value = val, insert = reactive(input$init), clicked = reactive({input$init})))
                            }
                          ))


