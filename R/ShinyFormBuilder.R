library(shiny)
library(R6)
library(purrr)
library(sortable)
library(shinyWidgets)
library(shinyjs)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(rlang)
library(here)
library(glue)
ShinyModule <- R6::R6Class("ShinyModule",
                           public = list(
                             #' ID that is assigned by the user.
                             id = NULL,
                             #' An ID that is used within `$ui` that is
                             #' may be of some care for the user.
                             inner_id = NULL,
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
source(here("R/tidy_tibble.R"))
source(here("R/Constructor.R"))
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
  children <- "ShinyForm_children"
  if(!is.null(ns)){
    id <- ns(id)
    selected <- ns(selected)
    dom <- ns(dom)
    children <- ns(children)
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
    Shiny.addCustomMessageHandler('findSubElements', findSubElements)
    function findSubElements(message) {
      if(ShinyForm_selected==null) {
        Shiny.setInputValue('<children>', null, {priority: 'event'}) ;
        return ;
      } 
      let ids = [];
      $('.ShinyForm-selected').find(message.query).map((index, ele) => {
        ids.push(ele.id); 
        });
      console.log(ids);
      Shiny.setInputValue('<children>', ids, {priority: 'event'});
    }
    function ShinyColumn(el){
      if (el.classList.contains('ShinyForm-Column')||el.classList.contains('ShinyForm-Preview-Container')) { 
        return el;
      }
      while(el && el.parentNode) {
        el = el.parentNode;
        if(el.classList.contains('ShinyForm-Column')||el.classList.contains('ShinyForm-Preview-Container')) {
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
                                 inner_id = "ShinyForm-Column",
                                 initialize = function(id, width){
                                   super$initialize(id)
                                   self$width <- width
                                 },
                                 width = NULL,
                                 ui = function(id = self$id){
                                   ns <- NS(id)
                                   div(class = ns(glue("{self$inner_id}-Container")),
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
                                   id <- self$inner_id
                                   removeUI(glue(".shiny-input-container:has(#{ns('width')})"))
                                   removeUI(glue(".{ns(paste0(id,'-Container'))}:has(#{ns(id)})"))
                                   if(!is.null(input)){
                                     remove_shiny_inputs(ns('width'), input)
                                   }
                                 }
                               ))

ShinyLayout <- R6::R6Class("ShinyLayout",
                           public = list(
                             id = NULL,
                             initialize = function(id = NULL){
                               self$id <- id
                             },
                             objects = tidy_tibble(
                               obj = list(),
                               parent = character(),
                               dom = character(),
                               type = character()
                             ),
                             add_object = function(obj, parent, dom, type){
                               self$objects <- 
                                 bind_rows(self$objects,
                                           tidy_tibble(obj = list(obj),
                                                       parent = parent,
                                                       dom = dom,
                                                       type = type))
                               invisible(self)
                             }
                             
                           ))

ShinyFormBuilder <- R6::R6Class("ShinyFormBuilder",
                                inherit = ShinyModule,
                                public = list(
                                  layout = NULL,
                                  modules = NULL,
                                  initialize = function(id){
                                    super$initialize(id)
                                    self$layout <- ShinyLayout$new(id = id)
                                    self$modules <- list(
                                      column = SFC_Column$new('column'),
                                      textInput = SFC_TextInput$new('textInput')
                                    )
                                  },
                                  ui = function(id = self$id) {
                                    ns <- NS(id)
                                    mods <- lapply(self$modules, function(x){x$ui(ns(x$id))})
                                    addmenu <- quo(dropdownButton(inputId = ns('AddElement'),
                                                       icon = icon('plus'),
                                                       status = "primary",
                                                       circle = F, inline = T,
                                                       !!!mods))
                                    tagList(
                                      useShinyjs(),
                                      fluidRow(
                                        eval_tidy(addmenu),
                                        actionButton(ns("rm"), NULL, icon = icon('minus'), class = 'btn-danger'),
                                        dropdownButton(inputId = ns('MoveElement'),
                                                       status = "primary",
                                                       circle = F, inline = T,
                                                       icon = icon('share-square'),
                                                       selectInput('parent_select', "Select New Parent",
                                                                   choices = c("none"), multiple = F),
                                                       actionBttn(ns('mv'), label = 'Move', style = 'material-flat', 
                                                                  block = T, color = 'warning')),
                                        actionButton(ns("Save"), NULL, icon = icon("save"), class = 'btn-primary')
                                      ),
                                      br(),
                                      fluidRow(class = "ShinyForm-Container",
                                               column(9,
                                                      fluidRow(
                                                        class = "ShinyForm-Preview-Container",
                                                        id = ns("ShinyForm-Sortable-Container")),
                                                      sortable_js(ns("ShinyForm-Sortable-Container"))),
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
                                    disable('rm')
                                    disable('MoveElement')
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
                                        disable('MoveElement')
                                      } else {
                                        enable('rm')
                                        enable('MoveElement')
                                      }
                                    })
                                    #call each constructor object's module
                                    built_obj <- lapply(self$modules,function(x){x$call()})
                                    #If a constructor's init is clicked - close menu
                                    lapply(built_obj,
                                           function(mod){
                                             observeEvent(mod$clicked(), {
                                               toggleDropdownButton('AddElement', session = session)
                                             })
                                           })
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
                                      input$ShinyForm_selected_id
                                      dom <- s()$layout$objects$dom
                                      m <- list(query = glue_collapse(glue("#{dom}"),sep = ","))
                                      session$sendCustomMessage('findSubElements', m)
                                    })
                                    
                                    
                                    continue_remove <- reactiveVal(0L)
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
                                    })
                                    
                                    output$View <- renderPrint({
                                      validate(need(input$Preview_Sortable_Order, "Nothing to View"))
                                      input$Preview_Sortable_Order
                                    })
                                    
                                    observeEvent({
                                      input$Save
                                      input$rm
                                      }, {
                                      m <- list(query = glue_collapse(glue("#{self$layout$objects$dom}"),sep = ","))
                                      session$sendCustomMessage("orderElementIDs", m)
                                    }, priority = 2L)
                                    
                                    
                                    observe({
                                      req(input$ShinyForm_ele_ordered)
                                      ids_ord <- input$ShinyForm_ele_ordered
                                      self$layout$objects <- self$layout$objects[sort_by(dom, .env$ids_ord),]
                                      print(self$layout$objects)
                                    }, priority = 2L)
                                    
                                    
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



