library(shiny)
library(R6)
library(purrr)
library(sortable)
library(tidyr)
library(dplyr)
library(tibble)

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
                           active = list(
                             ui = function(value){
                               if(missing(value)){
                                 private$.ui()
                               } else {
                                 stop("`$ui` is read only.")
                               }
                             }
                           ),
                           private = list(
                             .ui = function(){
                               ns <- NS(self$id)
                               tagList()
                             },
                             server = function(input, output, session){
                               
                             },
                             reactiveDep = NULL,
                             reactiveExpr = NULL,
                             
                             count = 0L
                           ))

ShinyLayout <- R6::R6Class("ShinyLayout",
                           public = list(
                             initialize = function(){
                               self$push_column(12, 1L)
                             },
                             column = tibble(index = integer(),
                                              width = integer()),
                             layout = tibble(index = integer(),
                                             width = integer(),
                                             elements = list(),
                                             element_index = integer()),
                             #add_element = function(){},
                             push_element = function(ele, index = NULL) {
                               if(is.null(index)){
                                 index <- max(self$column$index)
                               }
                               if(sum(self$column$index%in%index)==0){
                                 self$push_column(12, index)
                               }
                               layout <- self$layout[self$layout$index%in%index,]
                               if(nrow(layout)==0){
                                 newindx <- 1L
                               } else {
                                 newindx <- max(layout$element_index) + 1L
                               }
                               self$layout <- dplyr::bind_rows(self$layout,
                                                               tibble(index = index,
                                                                      width = unique(self$column[self$column$index%in%index,]$width),
                                                                      elements = list(ele),
                                                                      element_index = newindx))
                               invisible(self)
                             },
                             push_column = function(size, index = NULL, where = "after") {
                               if(is.null(index)){ # if index not proved -  default to max
                                 index <- max(self$column$index)
                               }
                               if(nrow(self$column)==0){
                                 self$column <- tibble(index = index,
                                                       width = size)
                               } else {
                                 if(where == "At"){#replace width of indexed column
                                   if(sum(self$column$index%in%index)==0){
                                     self$column <- dplyr::bind_rows(self$column,
                                                                     tibble(index = index,
                                                                            size = size))
                                   } else {
                                     self$column[index,"width"] <- size
                                   }
                                 } else if(where == "Before") {
                                   logic <- self$column$index>=index
                                   .index <- self$column$index[logic]
                                   self$column[logic,"index"] <- .index + 1L
                                   self$column <- dplyr::bind_rows(self$column,
                                                                   tibble(index = index,
                                                                          width = size))
                                   logic <- self$layout$index>=index
                                   .index <- self$layout$index[logic]
                                   self$layout[logic,"index"] <- .index + 1L
                                 } else if(where == "After") {
                                   logic <- self$column$index>=(index + 1L)
                                   .index <- self$column$index[logic] 
                                   self$column[logic,"index"] <- .index + 1L
                                   self$column <- dplyr::bind_rows(self$column,
                                                                   tibble(index = index + 1L,
                                                                          width = size))
                                   logic <- self$layout$index>=(index + 1L)
                                   .index <- self$layout$index[logic]
                                   self$layout[logic,"index"] <- .index + 1L
                                 }
                                 self$column <- self$column[order(self$column$index),]
                               }
                               invisible(self)
                             }
                             
                           ))

# ShinyForm <- R6::R6Class("ShinyForm",
#                          inhert = ShinyModule,)


ShinyFormBuilder <- R6::R6Class("ShinyFormBuilder",
                                inherit = ShinyModule,
                                public = list(
                                  layout = NULL,
                                  initialize = function(id){
                                    super$initialize(id)
                                    self$layout <- ShinyLayout$new()
                                  },
                                  preview_layout = function(){
                                    if(is.null(self$layout$layout)||nrow(self$layout$layout)==0) return()
                                    df <- self$layout$layout
                                    df <- nest(group_by(df, index, width), col_ele = c(elements, element_index))
                                    fluidRow(
                                      class = "ShinyForm-Preview-Container",
                                      id = "Preview-Sortable",
                                      pmap(list(df$width, df$col_ele, df$index), function(x,y,z){
                                        column(
                                          x,
                                          class = "ShinyForm-Column",
                                          fluidRow(
                                            id = paste0("Sortable-index-", z),
                                            map(y$elements, ~.x$preview()),
                                            sortable_js(paste0("Sortable-index-", z))
                                          )
                                        )
                                      }),
                                      sortable_js("Preview-Sortable")
                                    )
                                  },
                                  selected_ui = NULL
                                ),
                                private = list(
                                  .ui =  function(){
                                    ns <- NS(self$id)
                                    
                                    tagList(
                                      fluidRow(
                                        actionButton(ns("addcolumn"), NULL, icon = icon("plus")),
                                        actionButton(ns("rmcolumn"), NULL, icon = icon("minus")),
                                        actionButton(ns("editcolumn"), NULL, icon = icon("edit")),
                                        actionButton(ns("TextInput"), NULL, icon = icon("text")),
                                        actionButton(ns("SelectInput"), NULL)
                                      ),
                                      br(),
                                      fluidRow(
                                        column(9,
                                               uiOutput(ns("PREVIEW"))),
                                        column(3,
                                               uiOutput(ns("SELECTED"))
                                               ),
                                        textOutput(ns("View"))
                                      )
                                    )
                                  },
                                  server = function(input, output, session){
                                    ns <- session$ns
                                    s <- self$reactive()
                                    
                                    # Out <- map(self$layout$layout$elements, ~.x$call())
                                    # Out <- reactive({
                                    #   s()
                                    #   validate(need(!is.null(self$layout$layout)&&
                                    #              nrow(self$layout$layout)>0, "No Elements exist"))
                                    #   browser()
                                    #   map(self$layout$layout$elements, ~.x$call())
                                    # })
                                    
                                    
                                    
                                    observeEvent(input$addcolumn, {
                                      showModal(
                                        modalDialog(
                                          numericInput(ns("NewColSize"), "Column Width", value = 6L, min = 1L, max = 12L),
                                          selectInput(ns("NewColWhich"), "Which", choices = self$layout$column$index),
                                          selectInput(ns("NewColWhere"), "Where", choices = c("After","Before"), selected = "After"),
                                          footer = tagList(
                                            modalButton("Cancel"),
                                            actionButton(ns("InsertColumn"), "OK")
                                        )
                                      ))
                                    })
                                    
                                    observeEvent(input$InsertColumn, {
                                      removeModal()
                                      self$layout$push_column(size = input$NewColSize,
                                                              index = as.numeric(input$NewColWhich),
                                                              where = input$NewColWhere)
                                      print(self$layout$column)
                                      self$invalidate()
                                    })
                                    
                                    observeEvent(input$Layout, {
                                      
                                    })
                                    
                                    observeEvent(input$TextInput, {
                                      lay <- self$layout
                                      showModal(
                                        modalDialog(
                                          textInput(ns("NewTextIn"), "Label", placeholder = "Leave blank for none"),
                                          selectInput(ns("NewColWhich"), "Which Column", choices = lay$column$index),
                                          selectInput(ns("NewEleLoc"), "At Which Element", choices = lay$layout[lay$layout$index%in%as.numeric(input$NewColWhich),]$element_index),
                                          selectInput(ns("NewColWhere"), "Where", choices = c("After","Before"), selected = "After"),
                                          footer = tagList(
                                            modalButton("Cancel"),
                                            actionButton(ns("InsertTextInput"), "OK")
                                          )
                                        ))
                                    })
                                    
                                    observeEvent(input$InsertTextInput, {
                                      removeModal()
                                      i <- sprintf("%04d", input$InsertTextInput)
                                      id <- sprintf("FormTextInput%s", i)
                                      self$layout$push_element(ele = R6TextInput$new(id, input$NewTextIn), 
                                                               index = as.numeric(input$NewColWhich))
                                      print(self$layout$layout)
                                      self$invalidate()
                                    })
                                    
                                    observeEvent(input$SelectInput, {
                                      
                                    })
                                    
                                    output$PREVIEW <- renderUI({
                                      s()$preview_layout()
                                    })
                                    
                                    output$SELECTED <- renderUI({
                                      ui <- s()$selected_ui
                                      validate(need(ui, "No UI provided"))
                                      ui
                                    })
                                    
                                    # output$View <- renderText({
                                    #   browser()
                                    #   vals <- map(Out, ~.x$value())
                                    #   reduce(vals, ~str_c(.x, .y, sep = ";"))
                                    # }
                                    # )
                                    
                                    
                                    
                                  }
                                ))


test <- ShinyFormBuilder$new("test_id")
shinyApp(ui = fluidPage(
  tags$head(
    tags$style(HTML("
                    .ShinyForm-Element:hover {
                      background-color: #7682FF;
                      opacity: .5;
                    }
                    # .ShinyForm-Column {
                    #   min-height: 20px;
                    # }
                    .ShinyForm-Column:hover {
                      background-color: #FF7676;
                      opacity: .5;
                    }
                    "))
  ),
  test$ui), function(input, output, session){
  
  p <- test$reactive()
  
  test$call()
  
})

random_html_color <- function(){
  str_c(sample(c("0","1","2","3","4","5","6","7","8","9",
                 "A","B","C","D","E","F"), 6, replace = T), collapse = "")
}
