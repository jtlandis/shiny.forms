library(shiny)
library(R6)
library(purrr)
library(sortable)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(rlang)
library(here)
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
                             ui = function(id = self$id){
                               ns <- NS(id)
                               tagList()
                             },
                             remove = function(input, session){
                               abort("ShinyModule `$remove` called. Did you mean to make your own method?")
                             }
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
                           private = list(
                             server = function(input, output, session){
                               abort("ShinyModule `$server` called. Did you mean to make your own method?")
                             },
                             reactiveDep = NULL,
                             reactiveExpr = NULL,
                             count = 0L
                           ))
source(here("R/Basic_Inputs.R"))
ShinyLayout <- R6::R6Class("ShinyLayout",
                           public = list(
                             id = NULL,
                             initialize = function(id = NULL){
                               self$id <- id
                               self$push_column(12, 1L)
                             },
                             column = tibble(index = integer(),
                                              width = integer()),
                             object = tibble(index = integer(),
                                             width = integer(),
                                             elements = list(),
                                             element_index = integer()),
                             #add_element = function(){},
                             push_element = function(ele, index, reference) {
                               
                               if(is.null(index)){
                                 index <- max(self$column$index)
                               }
                               object <- self$object[self$object$index%in%index,]
                               if(nrow(object)==0){
                                 newindx <- 1L
                               } else {
                                 newindx <- max(object$element_index) + 1L
                               }
                               self$object <- dplyr::bind_rows(self$object,
                                                               tibble(index = index,
                                                                      width = unique(self$column[self$column$index%in%index,]$width),
                                                                      elements = list(ele),
                                                                      element_index = newindx))
                               invisible(self)
                             },
                             push_column = function(size, index, where = "beforeEnd", reference = NULL) {
                               ns <- NS(self$id)
                               if(nrow(self$column)==0&&is.null(reference)){
                                 self$column <- tibble(index = index,
                                                       width = size,
                                                       where = where,
                                                       reference = ns("ShinyForm-Column-1"))
                               } else if(is.null(reference)){
                                 abort('reference must not be NULL')
                               } else{
                                 self$column <- dplyr::bind_rows(
                                   self$column,
                                   tibble(index = index,
                                          width = size,
                                          where = where,
                                          reference = reference)
                                   )
                               }
                               invisible(self)
                             }
                             
                           ))

# ShinyForm <- R6::R6Class("ShinyForm",
#                          inhert = ShinyModule,)
updateShinyFormColumn <- function(id, width = 6, session){
  m <- list(
    id = id,
    width = width
  )
  print(m)
  session$sendCustomMessage("updateShinyFormColumn", m)
}
get_ShinyForm_Element <- function(id, ns = NULL){
  selected <- "ShinyForm_selected_id"
  if(!is.null(ns)){
    id <- ns(id)
    selected <- ns(selected)
  }
  glue::glue(
    .open = "<", .close = ">", .sep = "\n",
    "
    Shiny.addCustomMessageHandler('updateShinyFormColumn', updateShinyFormColumn);
    function updateShinyFormColumn(message) {
      let col = $('#' + message.id);
      if(col.length !=0){
        col = col[0];
        col.className = col.className.replace(/(col-..-)([0-9]+)/, '$1' + message.width);
      }
    }
    var ShinyForm_selected = null;
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
    
    $('#<id>').on('click', function(e){ // clicked in the container we care about
      
      if(e.target.id.length == 0) { return } // if no id -- return
      if(e.target.id == '<id>') { return } // if target is same as container -- return
      let shinyCol = ShinyColumn(e.target); // store the closest ShinyForm-Column node
     // if (shinyCol.childElementCount != 0 && e.target.id != shinyCol.id) { //if what we clicked is not the same as the container
          if(ShinyForm_selected == null) { // if unselected
            ShinyForm_selected = e.target;
            ShinyForm_selected.classList.add('ShinyForm-selected');
            Shiny.setInputValue('<selected>', ShinyForm_selected.id, {priority: 'event'});
          } else { // if an element/column is already selected
            ShinyForm_selected.classList.remove('ShinyForm-selected');
            if(ShinyForm_selected.id == e.target.id) {
              ShinyForm_selected = null;
              Shiny.setInputValue('<selected>', null, {priority: 'event'}); // tell shiny we deselected
            } else {
              ShinyForm_selected = e.target;
              ShinyForm_selected.classList.add('ShinyForm-selected');
              Shiny.setInputValue('<selected>', ShinyForm_selected.id, {priority: 'event'});
            }
          }
          return ;
     // }
      /*if (ShinyForm_selected_col == null) {
          ShinyForm_selected_col = shinyCol;
          ShinyForm_selected_col.classList.add('ShinyForm-Column-selected');
          Shiny.setInputValue('<selected_col>', ShinyForm_selected_col.id, {priority: 'event'});
      } else if (shinyCol.id != ShinyForm_selected_col.id) { // if not the same or is null
          ShinyForm_selected_col.classList.remove('ShinyForm-Column-selected'); //remove select class from current
          ShinyForm_selected_col = shinyCol;
          ShinyForm_selected_col.classList.add('ShinyForm-Column-selected')
          Shiny.setInputValue('<selected_col>', ShinyForm_selected_col.id, {priority: 'event'}); // we may not need priority event
      } else if (shinyCol.id == ShinyForm_selected_col.id) {
          ShinyForm_selected_col.classList.remove('ShinyForm-Column-selected'); //remove select class from current
          ShinyForm_selected_col = null;
          Shiny.setInputValue('<selected_col>', null, {priority: 'event'});
      }*/
      
    })
    ")}

sort_by <- function(x, by){
  order(match(x, by))
}

ShinyFormColumn <- R6::R6Class("ShinyFromColumn",
                               inherit = ShinyModule,
                               public = list(
                                 initialize = function(id, width){
                                   super$initialize(id)
                                   self$width <- width
                                 },
                                 width = NULL,
                                 ui = function(id = self$id){
                                   ns <- NS(id)
                                   div(class = ns("ShinyForm-Column-Container"),
                                       column(
                                         self$width,
                                         id = ns("ShinyForm-Column"),
                                         `data-rank-id` = paste0(ns("ShinyForm-Column"),'-',self$width),
                                         class = "ShinyForm-Column"
                                       ),
                                       sortable_js(ns("ShinyForm-Column"),
                                                   options = sortable_options(
                                                     #onSort = sortable_js_capture_input(ns(paste0("Preview_Sortable_Order-", index))) #may change how we get order later.
                                                   ))
                                   )
                                   
                                 },
                                 edit = function(id = self$id){
                                   ns <- NS(id)
                                   tagList(
                                     numericInput(ns("width"), label = "New Width", 
                                                  value = self$width, min = 1, max = 12),
                                     actionButton(ns('rm'), '', icon = icon('minus'))
                                   )
                                 },
                                 remove = function(input = NULL, session = NULL){
                                   session <- session %||% getDefaultReactiveDomain()
                                   ns <- session$ns
                                   removeUI(glue(".shiny-input-container:has(#{ns('width')})"))
                                   removeUI(glue(".{ns('ShinyForm-Column-Container')}:has(#{ns('ShinyForm-Column')})"))
                                   removeUI(glue("#{ns('rm')}"))
                                   if(!is.null(input)){
                                     remove_shiny_inputs(c(ns('rm'), ns('width')), input)
                                   }
                                 }
                               ),
                               private = list(
                                 finalizer = function(){
                                   session <- getDefaultReactiveDomain()
                                   if(!is.null(session)){
                                     ns <- session$ns
                                     self$remove()
                                   }
                                 },
                                 server = function(input, output, session){
                                   ns <- session$ns
                                   observe({
                                     self$width <- input$width
                                     updateShinyFormColumn(id = ns("ShinyForm-Column"), width = input$width, session = session)
                                   })
                                   
                                   observeEvent(input$rm, {
                                     self$remove(input, session)
                                   })
                                   
                                 }
                               ))

ShinyFormBuilder <- R6::R6Class("ShinyFormBuilder",
                                inherit = ShinyModule,
                                public = list(
                                  layout = NULL,
                                  initialize = function(id){
                                    super$initialize(id)
                                    self$layout <- ShinyLayout$new(id = id)
                                  },
                                  ShinyFormColumn = function(width, index) {
                                    ns <- NS(self$id)
                                    column(
                                      width,
                                      id = paste0(ns("ShinyForm-Column-"), index),
                                      `data-rank-id` = paste0("ShinyForm-Column-",index,'-',width),
                                      class = "ShinyForm-Column"
                                    )
                                  },
                                  ShinyForm_Script = function(index) {
                                    ns <- NS(self$id)
                                    sortable_js(paste0(ns("ShinyForm-Column-"), index),
                                                options = sortable_options(
                                                  onSort = sortable_js_capture_input(ns(paste0("Preview_Sortable_Order-", index)))
                                                ))
                                  },
                                  preview_layout = function(){
                                    ns <- NS(self$id)
                                    df_cols <- self$layout$column
                                    df <- self$layout$object
                                    if(is.null(df)){
                                      df_map <- df_cols
                                      df_map$col_ele <- list(NA)
                                    } else {
                                      df <- nest(group_by(df, index, width), col_ele = c(elements, element_index))
                                      df_map <- left_join(df_cols, df, by = c("index","width"))
                                    }
                                    df <- df_map
                                    # if(is.null(self$layout$object)||nrow(self$layout$object)==0) return()
                                    # df <- self$layout$object
                                    # df <- nest(group_by(df, index, width), col_ele = c(elements, element_index))
                                    tagList(
                                      fluidRow(
                                        class = "ShinyForm-Preview-Container",
                                        id = "ShinyForm-Sortable-Container",
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
                                      sortable_js("ShinyForm-Sortable-Container",
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
                                      fluidRow(class = "ShinyForm-Container",
                                        column(9,
                                               fluidRow(
                                                 class = "ShinyForm-Preview-Container",
                                                 self$ShinyFormColumn(self$layout$column$width, self$layout$column$index), #initial column
                                                 id = ns("ShinyForm-Sortable-Container")),
                                               sortable_js(ns("ShinyForm-Sortable-Container"),
                                                           options = sortable_options(
                                                             onSort = sortable_js_capture_input(ns("Preview_Sortable_Order"))
                                                           ))),
                                        column(3,
                                               uiOutput(ns("SELECTED"))
                                               )
                                      ),
                                      self$ShinyForm_Script(1),
                                      verbatimTextOutput(ns("View")),
                                      tags$script(HTML(get_ShinyForm_Element("ShinyForm-Sortable-Container", ns)))
                                    )
                                  },
                                  server = function(input, output, session){
                                    ns <- session$ns
                                    s <- self$reactive()
                                    num <- reactiveVal(0L)
                                    num2 <- reactiveVal(1L)
                                    
                                    clicked_element <- reactive({
                                      id <- input$ShinyForm_selected_id
                                      if(is.null(ShinyForm_column_id())) return(NULL)
                                      validate(need(nrow(self$layout$object)>0, "layout needs at least one element"))
                                      lgl <- map_lgl(self$layout$object$elements, ~paste0(.x$id,"-user_input") %in% id)
                                      wch <- which(lgl)
                                      validate(need(is_scalar_integer(wch), "multiple elements matched..."))
                                      self$layout$object$elements[[wch]]
                                    })
                                    
                                    ShinyForm_column_id <- reactive({
                                      id <- input$ShinyForm_selected_id
                                      if(is.null(id)||str_detect(id, "ShinyForm-Column", negate = T)) return(NULL)
                                      id
                                    })
                                    
                                    
                                    observe({
                                      selected <- clicked_element()
                                      if(!is.null(self$selected_ui)){
                                        self$selected_ui$selected <- FALSE #turn off selected
                                        if(is.null(selected) || (inherits(self$selected_ui, "ShinyModule") && self$selected_ui$id == selected$id)) {
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
                                      validate(need(ShinyForm_column_id(), "Select a column First"))
                                      showModal(
                                        modalDialog(
                                          fluidRow(
                                            numericInput(ns("NewColSize"), "Column Width", value = 6L, min = 1L, max = 12L),
                                            selectInput(ns("NewColWhere"), "Where", choices = c("beforeBegin", "afterBegin",
                                                                                                "beforeEnd", "afterEnd"), selected = "beforeEnd")),
                                          #selectInput(ns("NewColWhich"), "Which", choices = self$layout$column$index),
                                          #,
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
                                      self$layout$push_column(size = input$NewColSize,
                                                              index = numVal2(),
                                                              where = input$NewColWhere,
                                                              reference = ShinyForm_column_id())
                                      insertUI(paste0("#",ShinyForm_column_id()),
                                               where = input$NewColWhere,
                                               ui = self$ShinyFormColumn(input$NewColSize, numVal2()))
                                      insertUI(paste0(".ShinyForm-Container"),
                                               where = "beforeEnd",
                                               ui = div(self$ShinyForm_Script(numVal2())))
                                      print(self$layout$column)
                                      self$invalidate()
                                    })
                                    
                                    observeEvent(input$Layout, {
                                      
                                    })
                                    
                                    observeEvent(input$TextInput, {
                                      i <- sprintf("%04d", numVal())
                                      id <- sprintf("FormTextInput%s", i)
                                      indx <- filter(self$layout$column,
                                                     reference %in% ShinyForm_column_id()) %>% pull(index)
                                      ele <- R6TextInput$new(id)
                                      self$layout$push_element(ele = ele,
                                                               index = indx,
                                                               reference = ShinyForm_column_id())
                                      insertUI(paste0("#",ShinyForm_column_id()),
                                               where = "beforeEnd",
                                               ui = ele$ui)
                                      print(self$layout$object)
                                      self$invalidate()
                                    })
                                    
                                    
                                    numVal <- reactive({
                                      validate(need(input$TextInput, "Need to add a text element"))
                                      isolate({
                                        newVal <- num() + 1L
                                        num(newVal)
                                        num()
                                        })
                                      })
                                    
                                    
                                    
                                    output$PREVIEW <- renderUI({
                                      validate(
                                        need(NULL, "disabled"),
                                        need(nrow(s()$layout$object)>0||nrow(s()$layout$column)>0, "Need to Add Elements"))
                                      s()$preview_layout()
                                    })
                                    
                                    output$SELECTED <- renderUI({
                                      validate(need(input$ShinyForm_selected_id, "Please Select an Element to Edit."))
                                      ui <- s()$selected_ui
                                      validate(need(!is.null(ui), "No UI provided"))
                                      ui$ui
                                    })
                                    
                                    observe({
                                      clicked_element()
                                      col_Order <- input$Preview_Sortable_Order
                                      lay <- self$layout$object
                                      lay_isOrdered <- map_lgl(unique(lay$index), ~!is.null(input[[paste0("Preview_Sortable_Order-",.x)]]))
                                      if(any(lay_isOrdered)){
                                        lay <- lay  %>%
                                          group_by(index) %>%
                                          mutate(
                                            ele_ord = sort_by(x = map_chr(elements, ~.x$id),
                                                              by = input[[paste0("Preview_Sortable_Order-",cur_group())]])
                                          ) %>%
                                          arrange(index, ele_ord) %>% select(-ele_ord) %>% ungroup()
                                        self$layout$object <- lay
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
                                    
                                    
                                  }
                                ))


test <- ShinyFormBuilder$new("test_id")
shinyApp(ui = fluidPage(
  tags$head(
    tags$style(HTML("
                    .ShinyForm-Preview-Container {
                      padding: 15px;
                    }
                    /*.ShinyForm-Element:hover {
                      background-color: #7682FF;
                      opacity: .5;
                    }*/
                    .ShinyForm-Element-selected {
                      border: 2px dotted grey;
                    }
                    .ShinyForm-Column {
                      border: .5px solid grey;
                      padding: 25px;
                      border-radius: 15px;
                    }
                    .ShinyForm-selected, 
                    .ShinyForm-Element.ShinyForm-selected,
                    .ShinyForm-Column.ShinyForm-selected {
                      border: 5px solid red;
                      opacity: .5;
                    }
                    /*.ShinyForm-Column-selected:hover {
                      background-color: #FF7676;
                      opacity: .5;
                    }*/
                    "))
  ),
  test$ui), function(input, output, session){
  
  p <- test$reactive()
  
  test$call()
  
})

# random_html_color <- function(){
#   str_c(sample(c("0","1","2","3","4","5","6","7","8","9",
#                  "A","B","C","D","E","F"), 6, replace = T), collapse = "")
# }


