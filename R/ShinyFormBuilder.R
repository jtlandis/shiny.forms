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
library(glue)
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
                           private = list(
                             server = function(input, output, session){
                               abort("ShinyModule `$server` called. Did you mean to make your own method?")
                             },
                             reactiveDep = NULL,
                             reactiveExpr = NULL,
                             count = 0L
                           ))
source(here("R/Basic_Inputs.R"))
"%||%" <- function(a,b) if(is.null(a)) b else a
remove_shiny_inputs <- function(id, .input) {
  impl <- .subset2(.input, "impl")
  lgl <- id %in% impl$.values$keys()
  if(any(!lgl)) warn(glue("The following `id`s were not found in shiny server input and cannot be removed : ", glue_collapse(id[!lgl]), sep = ", ", last = ", and "))
  to_rm <- id[lgl]
  invisible(
    lapply(to_rm, function(i) {
      impl$.values$remove(i)
    })
  )
}
updateShinyFormColumn <- function(id, width = 6L, session){
  m <- list(
    id = id,
    width = width
  )
  validate(need(is.integer(width), "`width` must be an Integer"))
  session$sendCustomMessage("updateShinyFormColumn", m)
}
get_ShinyForm_Element <- function(id, ns = NULL){
  selected <- "ShinyForm_selected_id"
  dom <- "ShinyForm_ele_ordered"
  if(!is.null(ns)){
    id <- ns(id)
    selected <- ns(selected)
    dom <- ns(dom)
  }
  glue::glue(
    .open = "<", .close = ">", .sep = "\n",
    "
    Shiny.addCustomMessageHandler('orderElementIDs', orderElementIDs)
    function orderElementIDs(message) {
      let ids = [];
      $(message.query).map((index,ele) =>{
        ids.push(ele.id);
        });
      console.log(ids);
      console.log('<dom>');
      Shiny.setInputValue('<dom>', ids, {priority: 'event'});
    }
    Shiny.addCustomMessageHandler('updateShinyFormColumn', updateShinyFormColumn);
    function updateShinyFormColumn(message) {
      let col = $('#' + message.id);
      if(col.length !=0){
        col = col[0];
        col.className = col.className.replace(/(col-..-)([0-9]+)/, '$1' + message.width);
      }
    }
    var ShinyForm_selected = null;
    Shiny.addCustomMessageHandler('unselectShinyForm', setShinyFormNull);
    function setShinyFormNull(message) {
      ShinyForm_selected = null;
      Shiny.setInputValue('<selected>', null, {priority: 'event'})
    }
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
                                                  value = self$width, min = 1, max = 12)
                                   )
                                 },
                                 remove = function(input, ns = NULL){
                                   ns <- ns %||% getDefaultReactiveDomain()$ns
                                   removeUI(glue(".shiny-input-container:has(#{ns('width')})"))
                                   removeUI(glue(".{ns('ShinyForm-Column-Container')}:has(#{ns('ShinyForm-Column')})"))
                                   if(!is.null(input)){
                                     remove_shiny_inputs(ns('width'), input)
                                   }
                                 }
                               ),
                               private = list(
                                 server = function(input, output, session){
                                   ns <- session$ns
                                   observe({
                                     req(input$width)
                                     self$width <- input$width
                                     updateShinyFormColumn(id = ns("ShinyForm-Column"), width = input$width, session = session)
                                   })
                                   
                                   
                                 }
                               ))

ShinyLayout <- R6::R6Class("ShinyLayout",
                           public = list(
                             id = NULL,
                             initialize = function(id = NULL){
                               self$id <- id
                             },
                             column = tibble(index = integer(),
                                             col = list(),
                                             parent = character(),
                                             dom = character()),
                             object = tibble(index = integer(),
                                             elements = list(),
                                             parent = character(),
                                             dom = character()),
                             #add_element = function(){},
                             push_element = function(ele, index, parent, dom) {
                               
                               if(is.null(index)){
                                 index <- max(self$column$index)
                               }
                               self$object <- dplyr::bind_rows(self$object,
                                                               tibble(index = index,
                                                                      elements = list(ele),
                                                                      parent = parent,
                                                                      dom = dom))
                               invisible(self)
                             },
                             push_column = function(size, index, parent = NULL, dom = NULL) {
                               # session <- session %||% getDefaultReactiveDomain() 
                               # ns <- session$ns %||% NS(self$id)
                               obj <- ShinyFormColumn$new(index, size)
                               if(is.null(parent)){
                                 abort('parent must not be NULL')
                               } else if(nrow(self$column)==0){
                                 self$column <- tibble(index = index,
                                                       col = list(obj),
                                                       parent = parent,
                                                       dom = dom)
                               } else {
                                 self$column <- dplyr::bind_rows(
                                   self$column,
                                   tibble(index = index,
                                          col = list(obj),
                                          parent = parent,
                                          dom = dom)
                                 )
                               }
                               invisible(self)
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
                                  ui = function(id = self$id) {
                                    ns <- NS(id)
                                    tagList(
                                      fluidRow(
                                        actionButton(ns("addcolumn"), NULL, icon = icon("plus")),
                                        actionButton(ns("rmcolumn"), NULL, icon = icon("minus")),
                                        actionButton(ns("editcolumn"), NULL, icon = icon("edit")),
                                        actionButton(ns("TextInput"), NULL, icon = icon("text")),
                                        actionButton(ns("SelectInput"), NULL),
                                        actionButton(ns("Save"), NULL, icon = icon("save"))
                                      ),
                                      br(),
                                      fluidRow(class = "ShinyForm-Container",
                                               column(9,
                                                      fluidRow(
                                                        class = "ShinyForm-Preview-Container",
                                                        id = ns("ShinyForm-Sortable-Container")),
                                                      sortable_js(ns("ShinyForm-Sortable-Container"),
                                                                  options = sortable_options(
                                                                    onSort = sortable_js_capture_input(ns("Preview_Sortable_Order"))
                                                                  ))),
                                               column(3,
                                                      uiOutput(ns("SELECTED"))
                                               )
                                      ),
                                      verbatimTextOutput(ns("View")),
                                      tags$script(HTML(get_ShinyForm_Element("ShinyForm-Sortable-Container", ns)))
                                    )
                                  },
                                  selected_ui = NULL,
                                  selected_col = NULL
                                ),
                                private = list(
                                  server = function(input, output, session){
                                    ns <- session$ns
                                    s <- self$reactive()
                                    num <- reactiveVal(0L)
                                    num2 <- reactiveVal(1L)
                                    
                                    clicked_element <- reactive({
                                      id <- input$ShinyForm_selected_id
                                      if(is.null(id)) return(NULL)
                                      if(str_detect(id, "ShinyForm-Column")){ #is a column
                                        validate(need(nrow(self$layout$column)>0, "layout needs at least one column"))
                                        lgl <- self$layout$column$dom %in% id
                                        wch <- which(lgl)
                                        validate(need(is_scalar_integer(wch), "multiple elements matched..."))
                                        self$layout$column$col[[wch]]
                                      } else {
                                        validate(need(nrow(self$layout$object)>0, "layout needs at least one element"))
                                        lgl <- self$layout$object$dom %in% id
                                        wch <- which(lgl)
                                        validate(need(is_scalar_integer(wch), "multiple elements matched..."))
                                        self$layout$object$elements[[wch]]
                                      }
                                    })
                                    
                                    ShinyForm_column_id <- reactive({
                                      id <- input$ShinyForm_selected_id
                                      if(is.null(id)||str_detect(id, "ShinyForm-Column", negate = T)) return(NULL)
                                      id
                                    })
                                    
                                    observeEvent(input$addcolumn, {
                                      if(nrow(self$layout$column)>0){
                                        validate(need(ShinyForm_column_id(), "Select a column First"))
                                      }
                                      showModal(
                                        modalDialog(
                                          fluidRow(
                                            numericInput(ns("NewColSize"), "Column Width", value = 6L, min = 1L, max = 12L)),
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
                                      no_cols <- nrow(self$layout$column)==0
                                      self$layout$push_column(size = input$NewColSize,
                                                              index = numVal2(),
                                                              parent = ifelse(no_cols,ns('ShinyForm-Sortable-Container'),ShinyForm_column_id()),
                                                              dom = ns(glue("{numVal2()}-ShinyForm-Column")))
                                      col_proxy <- filter(self$layout$column, index == numVal2())$col[[1]]
                                      insertUI(glue("#{ifelse(no_cols,ns('ShinyForm-Sortable-Container'),ShinyForm_column_id())}"),
                                               where = "beforeEnd",
                                               ui = col_proxy$ui(ns(col_proxy$id)))
                                      col_proxy$call() #insure things that need to be reactive are active
                                      print(self$layout$column)
                                      self$invalidate()
                                    })
                                    
                                    observeEvent(input$Layout, {
                                      
                                    })
                                    
                                    observeEvent(input$TextInput, {
                                      validate(need(!is.null(ShinyForm_column_id()), "Select a Column "))
                                      i <- sprintf("%04d", numVal())
                                      id <- sprintf("FormTextInput%s", i)
                                      indx <- self$layout$column %>%
                                        filter(dom %in% ShinyForm_column_id()) %>%
                                        pull(index)
                                      ele <- R6TextInput$new(id)
                                      self$layout$push_element(ele = ele,
                                                               index = indx,
                                                               parent = ShinyForm_column_id(),
                                                               dom = ns(glue("{id}-user_input")))
                                      insertUI(paste0("#",ShinyForm_column_id()),
                                               where = "beforeEnd",
                                               ui = ele$ui(ns(id)))
                                      moduleServer(id, ele$edit_mod)
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
                                      ele <- clicked_element()
                                      validate(need(!is.null(ele), "No UI provided"))
                                      tagList(
                                        actionButton(ns("rm"), '', icon = icon('minus')),
                                        ele$edit(ns(ele$id))
                                      )
                                    })
                                    
                                    observeEvent(input$rm,{
                                      ele <- clicked_element()
                                      ele_id <- input$ShinyForm_selected_id
                                      ele$remove(input, NS(ns(ele$id)))
                                      if(str_detect(ele_id, "ShinyForm-Column")) {
                                        self$layout$column <- filter(self$layout$column,
                                                                     !(dom %in% .env$ele_id | parent %in% .env$ele_id))
                                        if(ele_id %in% self$layout$object$parent) {
                                          self$layout$object <- filter(self$layout$object, !parent %in% .env$ele_id)
                                        }
                                      } else {
                                        self$layout$object <- filter(self$layout$object, !dom %in% .env$ele_id)
                                      }
                                      session$sendCustomMessage("unselectShinyForm", list(id = ele_id))
                                    })
                                    
                                    output$View <- renderPrint({
                                      validate(need(input$Preview_Sortable_Order, "Nothing to View"))
                                      input$Preview_Sortable_Order
                                    })
                                    
                                    observeEvent(input$Save, {
                                      tib <- bind_rows(self$layout$column,
                                                       self$layout$object)
                                      m <- list(query = glue_collapse(glue("#{tib$dom}"),sep = ","))
                                      session$sendCustomMessage("orderElementIDs", m)
                                    })
                                    
                                    observe({
                                      req(input$ShinyForm_ele_ordered)
                                      ids_ord <- input$ShinyForm_ele_ordered
                                      col_ord <- ids_ord[ids_ord %in% self$layout$column$dom]
                                      self$layout$column <- self$layout$column[sort_by(self$layout$column$dom, col_ord),]
                                      obj_ord <- ids_ord[ids_ord %in% self$layout$object$dom]
                                      self$layout$object <- self$layout$object[sort_by(self$layout$object$dom, obj_ord),]
                                      
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
  test$ui()), function(input, output, session){
  
  p <- test$reactive()
  
  test$call()
  
})

# random_html_color <- function(){
#   str_c(sample(c("0","1","2","3","4","5","6","7","8","9",
#                  "A","B","C","D","E","F"), 6, replace = T), collapse = "")
# }


