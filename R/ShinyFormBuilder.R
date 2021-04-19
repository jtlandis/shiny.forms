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

get_ShinyForm_Element <- function(id, ns = NULL){
  selected_ele <- "ShinyForm_element_id"
  selected_col <- "ShinyForm_column_id"
  if(!is.null(ns)){
    id <- ns(id)
    selected_ele <- ns(selected_ele)
    selected_col <- ns(selected_col)
  }
  glue::glue(
    .open = "<", .close = ">", .sep = "\n",
    "
    var ShinyForm_selected_col = null;
    function ShinyColumn(el){
      if (el.classList.contains('ShinyForm-Column')) { 
        return el;
      }
      while(el && el.parentNode) {
        el = el.parentNode;
        if(el.classList.contains('ShinyForm-Column')) {
          return el;
        }
      }
      return null;
    }
    var ShinyForm_selected_ele = null;
    $('#<id>').on('click', function(e){ // clicked in the container we care about
      
      let shinyCol = ShinyColumn(e.target); // store the closest ShinyForm-Column node
      if (ShinyForm_selected_col == null) {
          ShinyForm_selected_col = shinyCol;
          ShinyForm_selected_col.classList.add('ShinyForm-Column-selected');
          Shiny.setInputValue('<selected_col>', ShinyForm_selected_col.id, {priority: 'event'});
      } else if (shinyCol.id != ShinyForm_selected_col.id) { // if not the same or is null
          ShinyForm_selected_col.classList.remove('ShinyForm-Column-selected'); //remove select class from current
          ShinyForm_selected_col = shinyCol;
          ShinyForm_selected_col.classList.add('ShinyForm-Column-selected')
          Shiny.setInputValue('<selected_col>', ShinyForm_selected_col.id, {priority: 'event'}); // we may not need priority event
      }
      if(e.target.id.length == 0) { return } // if no id -- return
      if(e.target.id == '<id>') { return } // if target is same as container -- return
      if (e.target.id != ShinyForm_selected_col.id) { //if what we clicked is not the same as the container
          if(ShinyForm_selected_ele == null) {
            ShinyForm_selected_ele = e.target;
            ShinyForm_selected_ele.classList.add('ShinyForm-Element-selected');
            Shiny.setInputValue('<selected_ele>', ShinyForm_selected_ele.id, {priority: 'event'});
          } else {
            ShinyForm_selected_ele.remove('ShinyForm-Element-selected');
            if(ShinyForm_selected_ele.id == e.target.id) {
              ShinyForm_selected_ele = null;
              Shiny.setInputValue('<selected_ele>', 'NULL', {priority: 'event'}); // tell shiny we deselected
            } else {
              ShinyForm_selected_ele = e.target;
              Shiny.setInputValue('<selected_ele>', ShinyForm_selected_ele.id, {priority: 'event'});
            }
          }
      }
    })
    ")}

sort_by <- function(x, by){
  order(match(x, by))
}
ShinyFormBuilder <- R6::R6Class("ShinyFormBuilder",
                                inherit = ShinyModule,
                                public = list(
                                  layout = NULL,
                                  initialize = function(id){
                                    super$initialize(id)
                                    self$layout <- ShinyLayout$new()
                                  },
                                  ShinyFormColumn = function(width, index) {
                                    ns <- NS(self$id)
                                    column(
                                      width,
                                      id = paste0("ShinyForm-Column-", index),
                                      `data-rank-id` = paste0("ShinyForm-Column-",index,'-',width),
                                      class = "ShinyForm-Column",
                                      fluidRow(
                                        id = paste0("Sortable-index-", index)
                                      ),
                                      sortable_js(paste0("Sortable-index-", index),
                                                  options = sortable_options(
                                                    onSort = sortable_js_capture_input(ns(paste0("Preview_Sortable_Order-", index)))
                                                  ))
                                    )
                                  },
                                  preview_layout = function(){
                                    ns <- NS(self$id)
                                    df_cols <- self$layout$column
                                    df <- self$layout$layout
                                    if(is.null(df)){
                                      df_map <- df_cols
                                      df_map$col_ele <- list(NA)
                                    } else {
                                      df <- nest(group_by(df, index, width), col_ele = c(elements, element_index))
                                      df_map <- left_join(df_cols, df, by = c("index","width"))
                                    }
                                    df <- df_map
                                    # if(is.null(self$layout$layout)||nrow(self$layout$layout)==0) return()
                                    # df <- self$layout$layout
                                    # df <- nest(group_by(df, index, width), col_ele = c(elements, element_index))
                                    tagList(
                                      fluidRow(
                                        class = "ShinyForm-Preview-Container",
                                        id = "Preview-Sortable",
                                        pmap(list(df$width, df$col_ele, df$index), function(x,y,z){
                                          column(
                                            x,
                                            id = paste0("ShinyForm-Column-", z),
                                            `data-rank-id` = paste0(z,'-',x),
                                            class = "ShinyForm-Column",
                                            fluidRow(
                                              id = paste0("Sortable-index-", z),
                                              map(y$elements, ~.x$preview())
                                            ),
                                            sortable_js(paste0("Sortable-index-", z),
                                                        options = sortable_options(
                                                          onSort = sortable_js_capture_input(ns(paste0("Preview_Sortable_Order-", z)))
                                                        ))
                                          )
                                        }),
                                      ),
                                      sortable_js("Preview-Sortable",
                                                  options = sortable_options(
                                                    onSort = sortable_js_capture_input(ns("Preview_Sortable_Order"))
                                                  ))
                                    )
                                    
                                  },
                                  selected_ui = NULL,
                                  selected_col = NULL
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
                                               fluidRow(
                                                 class = "ShinyForm-Preview-Container",
                                                 self$ShinyFormColumn(self$layout$column$width, self$layout$column$index), #initial column
                                                 id = ns("Preview-Sortable")),
                                               sortable_js(ns("Preview-Sortable"),
                                                           options = sortable_options(
                                                             onSort = sortable_js_capture_input(ns("Preview_Sortable_Order"))
                                                           ))),
                                        column(3,
                                               uiOutput(ns("SELECTED"))
                                               )
                                      ),
                                      verbatimTextOutput(ns("View")),
                                      tags$script(HTML(get_ShinyForm_Element("Preview-Sortable", ns)))
                                    )
                                  },
                                  server = function(input, output, session){
                                    ns <- session$ns
                                    s <- self$reactive()
                                    num <- reactiveVal(0L)
                                    num2 <- reactiveVal(1L)
                                    
                                    clicked_element <- reactive({
                                      req(input$ShinyForm_element_id)
                                      id <- input$ShinyForm_element_id
                                      if(id=="NULL") return(NULL)
                                      validate(need(nrow(self$layout$layout)>0, "layout needs at least one element"))
                                      lgl <- map_lgl(self$layout$layout$elements, ~paste0(.x$id,"-user_input") %in% id)
                                      wch <- which(lgl)
                                      validate(need(is_scalar_integer(wch), "multiple elements matched..."))
                                      self$layout$layout$elements[[wch]]
                                    })
                                    
                                    observe({
                                      selected <- clicked_element()
                                      if(!is.null(self$selected_ui)){
                                        self$selected_ui$selected <- FALSE #turn off selected
                                        if(is.null(selected) || self$selected_ui$id == selected$id) {
                                          self$selected_ui <- NULL
                                        } else {
                                          self$selected_ui <- selected
                                          self$selected_ui$selected <- TRUE
                                        }
                                      } else {
                                        self$selected_ui <- selected
                                        self$selected_ui$selected <- TRUE
                                      }
                                      self$invalidate()
                                    })
                                    
                                    observeEvent(input$addcolumn, {
                                      validate(need(input$ShinyForm_column_id, "Select a column First"))
                                      showModal(
                                        modalDialog(
                                          numericInput(ns("NewColSize"), "Column Width", value = 6L, min = 1L, max = 12L),
                                          #selectInput(ns("NewColWhich"), "Which", choices = self$layout$column$index),
                                          #selectInput(ns("NewColWhere"), "Where", choices = c("After","Before"), selected = "After"),
                                          footer = tagList(
                                            modalButton("Cancel"),
                                            actionButton(ns("InsertColumn"), "OK")
                                        )
                                      ))
                                    })
                                    numVal2 <- reactive({
                                      validate(need(input$InsertColumn, "Need to add a text element"))
                                      isolate({
                                        newVal <- num2() + 1L
                                        num2(newVal)
                                        num2()
                                      })
                                    })
                                    
                                    observeEvent(input$InsertColumn, {
                                      removeModal()
                                      # self$layout$push_column(size = input$NewColSize,
                                      #                         index = as.numeric(input$NewColWhich),
                                      #                         where = input$NewColWhere)
                                      insertUI(paste0("#",input$ShinyForm_column_id),
                                               where = "afterEnd",
                                               ui = self$ShinyFormColumn(input$NewColSize, numVal2()))
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
                                    
                                    
                                    numVal <- reactive({
                                      validate(need(input$InsertTextInput, "Need to add a text element"))
                                      isolate({
                                        newVal <- num() + 1L
                                        num(newVal)
                                        num()
                                        })
                                      })
                                    
                                    observeEvent(input$InsertTextInput, {
                                      removeModal()
                                      i <- sprintf("%04d", numVal())
                                      id <- sprintf("FormTextInput%s", i)
                                      self$layout$push_element(ele = R6TextInput$new(id, input$NewTextIn), 
                                                               index = as.numeric(input$NewColWhich))
                                      print(self$layout$layout)
                                      self$invalidate()
                                    })
                                    
                                    observeEvent(input$SelectInput, {
                                      
                                    })
                                    
                                    output$PREVIEW <- renderUI({
                                      validate(need(nrow(s()$layout$layout)>0||nrow(s()$layout$column)>0, "Need to Add Elements"))
                                      s()$preview_layout()
                                    })
                                    
                                    output$SELECTED <- renderUI({
                                      ui <- s()$selected_ui
                                      validate(need(!is.null(ui), "No UI provided"))
                                      ui$ui
                                    })
                                    
                                    # reactive({
                                    #   req(input$Preview_Sortable_Order)
                                    #   inval
                                    #   input$Preview_Sortable_Order
                                    # })
                                    
                                    observe({
                                      clicked_element()
                                      col_Order <- input$Preview_Sortable_Order
                                      lay <- self$layout$layout
                                      lay_isOrdered <- map_lgl(unique(lay$index), ~!is.null(input[[paste0("Preview_Sortable_Order-",.x)]]))
                                      if(any(lay_isOrdered)){
                                        lay <- lay  %>%
                                          group_by(index) %>%
                                          mutate(
                                            ele_ord = sort_by(x = map_chr(elements, ~.x$id),
                                                              by = input[[paste0("Preview_Sortable_Order-",cur_group())]])
                                          ) %>%
                                          arrange(index, ele_ord) %>% select(-ele_ord) %>% ungroup()
                                        self$layout$layout <- lay
                                      }
                                      if(!is.null(col_Order)){
                                        cols <- self$layout$column
                                        x_col <- paste0("ShinyForm-Column-",cols$index,"-",cols$width)
                                        self$layout$column <- cols[sort_by(x_col, col_Order),]
                                      }
                                    }, priority = 2)
                                    
                                    output$View <- renderPrint({
                                      validate(need(input$Preview_Sortable_Order, "Nothing to View"))
                                      input$Preview_Sortable_Order
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
                    .ShinyForm-Preview-Container {
                      padding: 15px;
                    }
                    .ShinyForm-Element:hover {
                      background-color: #7682FF;
                      opacity: .5;
                    }
                    .ShinyForm-Element-selected {
                      border: 2px dotted grey;
                    }
                    .ShinyForm-Column {
                      border: .5px solid grey;
                      padding: 25px;
                      border-radius: 15px;
                    }
                    .ShinyForm-Column-selected, 
                    .ShinyForm-Column.ShinyForm-Column-selected {
                      border: 5px solid red;
                      opacity: .5;
                    }
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


