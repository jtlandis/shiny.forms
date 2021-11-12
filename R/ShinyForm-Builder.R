# library(shiny)
# library(R6)
# library(purrr)
# library(sortable)
# library(shinyWidgets)
# library(shinyjs)
# library(tidyr)
# library(dplyr)
# library(tibble)
# library(stringr)
# library(rlang)
# library(here)
# library(glue)
#
# source(here("R/Basic_Inputs.R"))
# source(here("R/tidy_tibble.R"))
# source(here("R/Constructor.R"))
#



weave_ui <- function(.l, ui){
  n <- length(.l)
  l <- vector('list',2*n-1)
  eve_index <- seq_along(.l)*2
  l[eve_index] <- .l
  odd_index <- (eve_index-1L)[-1L]
  l[odd_index] <- ui
  return(l)
}

#' @export
ShinyFormBuilder <- R6::R6Class("ShinyFormBuilder",
                                inherit = ShinyModule,
                                public = list(
                                  layout = NULL,
                                  modules = NULL,
                                  initialize = function(id){
                                    super$initialize(id)
                                    private$.tmp <- tempfile("ShinyForm", fileext = ".rds")
                                    self$layout <- ShinyLayout$new(id = id)
                                    self$modules <- list(
                                      column = SFC_Column$new('column'),
                                      textInput = SFC_TextInput$new('textInput')
                                    )
                                  },
                                  ui = function(id = self$id) {
                                    ns <- NS(id)
                                    # mods <- lapply(self$modules, function(x){x$ui(ns(x$id))})
                                    # mods <- weave_ui(mods, tagList(div()))
                                    # addmenu <- quo(dropdownButton(inputId = ns('AddElement'),
                                    #                    icon = icon('plus'),
                                    #                    status = "primary",
                                    #                    circle = F, inline = T,
                                    #                    !!!mods))
                                    tagList(
                                      useShinyjs(),
                                      fluidRow(class = "ShinyForm-Container",
                                        column(width = 8,
                                               class = "ShinyForm-Preview-Container",
                                               id = ns("ShinyForm-Sortable-Container")),
                                               sortable_js(ns("ShinyForm-Sortable-Container")),
                                        column(width = 4,
                                               class = "ShinyForm-Menu-Container",
                                               wellPanel(
                                                 tabsetPanel(type = 'pills',
                                                   tabPanel("Create",
                                                            br(),
                                                            selectInput(ns('element-options'),
                                                                        "Select An Element",
                                                                        choices = names(self$modules)),
                                                            hr(),
                                                            uiOutput(ns('init_mod'))
                                                            ),
                                                   tabPanel("Edit",
                                                            br(),
                                                            uiOutput(ns("SELECTED")),
                                                            hr(),
                                                            checkboxInput(ns('unhide_labels'),
                                                                          label = 'Show Column Labels',
                                                                          value = F),
                                                            hr(),
                                                            selectInput(ns('parent_select'),label = "New Parent:",
                                                                        choices = c('none'='none'), selected = c('none'='none')),
                                                            hr(),
                                                            actionButton(ns('mv'), label = "Move!", class = 'btn-warning', align = 'right'),
                                                            actionButton(ns('rm'), label = "Remove!", class = 'btn-danger', align = 'center')
                                                            )
                                                 ),
                                                 br(),
                                                 actionButton(ns('Save'), label = "Save", class = "btn-primary", align = 'center')
                                               ))
                                      ),
                                      tags$script(HTML(get_ShinyForm_Element("ShinyForm-Sortable-Container", ns)))
                                    )
                                  },
                                  selected_ui = NULL,
                                  selected_col = NULL
                                ),
                                private = list(
                                  .static_mods = NULL,
                                  .tmp = NULL,
                                  server = function(input, output, session){
                                    ns <- session$ns
                                    s <- self$reactive()
                                    disable('rm')
                                    disable('mv')
                                    parent_id <- reactive({
                                      selected_id <- input$ShinyForm_selected_id
                                      objects <- self$layout$objects
                                      if(is.null(selected_id)){
                                        return(ns("ShinyForm-Sortable-Container"))
                                      } else if(selected_id %in% objects$parent) {
                                        return(selected_id)
                                      } else if (selected_id %in% objects$dom) {
                                        if(objects[dom %in% .env$selected_id,]$type=="column") {
                                          return(selected_id)
                                        }
                                        return(objects[dom %in% .env$selected_id,]$parent)
                                      }
                                    })



                                    observe({
                                      selected_id <- input$ShinyForm_selected_id
                                      if(is.null(selected_id)||length(selected_id)==0){
                                        disable('rm')
                                        disable('mv')
                                      } else {
                                        enable('rm')
                                        enable('mv')
                                      }
                                    })
                                    #call each constructor object's module
                                    built_obj <- lapply(self$modules,function(x){x$call()})
                                    #If a constructor's init is clicked - close menu
                                    output$init_mod <- renderUI({
                                      mod <- self$modules[[input$`element-options`]]
                                      mod$ui(ns(mod$id))
                                    })
                                    # lapply(built_obj,
                                    #        function(mod){
                                    #          observeEvent(mod$clicked(), {
                                    #            toggleDropdownButton('AddElement', session = session)
                                    #          })
                                    #        })
                                    #Add element to page if `insert` is active
                                    lapply(names(built_obj),
                                           function(mod){
                                             observeEvent(built_obj[[mod]]$insert(), {
                                               obj <- built_obj[[mod]]$value()
                                               parent <- parent_id()
                                               self$layout$add_object(obj = obj,
                                                                      parent = parent,
                                                                      dom = ns(glue("{obj$id}-{obj$inner_id}")),
                                                                      type = mod)
                                               insertUI(glue("#{parent}"),
                                                        where = "beforeEnd",
                                                        ui = obj$ui(ns(obj$id)),
                                                        immediate = T)
                                               moduleServer(obj$id, obj$edit_mod) #insure things that need to be reactive are active
                                               print(self$layout$objects)
                                               s()$invalidate()
                                             })
                                           })

                                    observe({
                                      isolate({prev_selected <- input$parent_select})
                                      col_objs <- s()$layout$objects[type=='column',][['obj']]
                                      if (length(col_objs)>0){
                                        indexes <- map_chr(col_objs, ~.x$id)
                                        opts <- c('none', paste('Column', indexes))
                                        vals <- c('none', indexes)
                                        names(vals) <- opts
                                      } else {
                                        vals <- c('none' = 'none')
                                      }
                                      selected <-
                                        if(prev_selected %in% vals){
                                          prev_selected
                                        } else {
                                          'none'
                                      }
                                      updateSelectInput(session = session,
                                                        inputId = 'parent_select',
                                                        choices = vals,
                                                        selected = selected)
                                    })

                                    observeEvent(input$mv, {
                                      selected <- input$ShinyForm_selected_id
                                      parent_num <- input$parent_select
                                      new_parent <-
                                        if (parent_num=="none"){
                                          ns('ShinyForm-Sortable-Container')
                                        } else {
                                          self$layout$objects[map_lgl(obj, ~.x$id==.env$parent_num)&type=='column', dom, drop = T]
                                        }
                                      objs <- self$layout$objects[dom %in% .env$selected, obj, drop = T]
                                      selectors <- paste0(map_chr(objs, ~.x$selector(NS(ns(.x$id)))),collapse = ",")
                                      session$sendCustomMessage("appendParentWithSelected", list(parent = new_parent,
                                                                                                 selected_query = selectors))
                                      self$layout$objects[dom==.env$selected,parent] <- new_parent
                                      print(self$layout$objects)
                                    })

                                    clicked_element <- reactive({
                                      id <- input$ShinyForm_selected_id
                                      if(is.null(id)) return(NULL)
                                      res <- self$layout$objects[dom %in% .env$id, obj, drop = T]
                                      validate(need(length(res)==1, "There was a problem with `clicked_element`"))
                                      res[[1L]]
                                    })



                                    observeEvent(input$Layout, {

                                    })


                                    output$SELECTED <- renderUI({
                                      ele <- clicked_element()
                                      validate(need(!is.null(ele), "No UI provided"))
                                      tagList(
                                        ele$edit(ns(ele$id))
                                      )
                                    })

                                    observe({
                                      validate(need(!is.null(input$ShinyForm_selected_id), 'Select Something'))
                                      dom <- s()$layout$objects$dom
                                      m <- list(query = glue_collapse(glue("#{dom}"),sep = ","))
                                      session$sendCustomMessage('findSubElements', m)
                                    })


                                    observeEvent(input$rm,{
                                      ele_children <- input$ShinyForm_children
                                      if(!is.null(ele_children)){
                                        showModal(
                                          modalDialog(
                                            p(glue("There are {length(ele_children)} within the selected element. If you continue,",
                                                   "you will delete these elements as well. Do you still want to continue?")),
                                            # div(class = "ShinyFormModal-Container"),
                                            footer = tagList(
                                              modalButton("Cancel!"),
                                              actionButton(ns("removeThis"), "Continue!")
                                            )
                                          )
                                        )
                                      } else {
                                        ele <- clicked_element()
                                        ele_id <- input$ShinyForm_selected_id
                                        ele$remove(input, NS(ns(ele$id)))
                                        self$layout$objects <- self$layout$objects[!(dom %in% c(.env$ele_id, .env$ele_children)),]
                                        session$sendCustomMessage("unselectShinyForm", list(id = ele_id))
                                        s()$invalidate()
                                      }
                                    }, priority = 0L)

                                    observeEvent(input$removeThis,{
                                      removeModal()
                                      ele <- clicked_element()
                                      ele_id <- input$ShinyForm_selected_id
                                      ele_children <- input$ShinyForm_children
                                      ele$remove(input, NS(ns(ele$id)))
                                      self$layout$objects <- self$layout$objects[!(dom %in% c(.env$ele_id, .env$ele_children)),]
                                      session$sendCustomMessage("unselectShinyForm", list(id = ele_id))
                                      s()$invalidate()
                                    })

                                    observe({
                                      s()
                                      if(input$unhide_labels){
                                        shinyjs::show(select = 'h3.SFC-label')
                                      } else {
                                        shinyjs::hide(selector = 'h3.SFC-label')
                                      }

                                    })

                                    output$View <- renderPrint({
                                      validate(need(input$Preview_Sortable_Order, "Nothing to View"))
                                      input$Preview_Sortable_Order
                                    })

                                    init_query <- function() {
                                      # browser()
                                      session$sendCustomMessage(
                                        "orderElementIDs",
                                        list(query = glue_collapse(glue("#{self$layout$objects$dom}"),sep = ","))
                                      )
                                    }

                                    observeEvent(input$Save, init_query(), priority = 2L)
                                    observeEvent(input$rm, init_query(), priority = 2L)


                                    observe({
                                      req(input$ShinyForm_ele_ordered)
                                      ids_ord <- input$ShinyForm_ele_ordered
                                      self$layout$objects <- self$layout$objects[sort_by(dom, .env$ids_ord),]
                                      print(self$layout$objects)
                                      .tbl <- self$layout$objects
                                      .tbl$parent <- str_remove(.tbl$parent, ns(""))
                                      .tbl$dom <- str_remove(.tbl$dom, ns(""))
                                      saveRDS(.tbl, private$.tmp)
                                      cat(glue("updating: {private$.tmp}\n"))
                                    }, priority = 2L)


                                  }
                                ))



#/tmp/RtmppCF8wH/ShinyForm22dab4052a7d.rds

