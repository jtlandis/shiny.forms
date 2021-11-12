
#' R6 Class that is the work horse of the shiny.forms
#' package
#'
#' This is to help modulize code in order to keep objects
#' self contained, but also convienent to read from.
ShinyModule <- R6::R6Class(
  "ShinyModule",
  public = list(
    #' @field id element id that is assigned by the user.
    #' depending on how nested the module is, this may not be
    #' the id represented on the DOM due to namespacing of
    #' \link[shiny]{moduleServer}
    id = NULL,
    #' @field inner_id an id that is used within `$ui` that is
    #' may be of some care for the user.
    inner_id = NULL,
    #' @description
    #' Create a ShinyModule object
    #' @param id Set the value of `$id` field
    initialize = function(id){
      private$reactiveDep <- function(x) NULL
      self$id <- as.character(id)
    },
    #' @description
    #' Call \link[shiny]{moduleServer} which inacts
    #' this modules `private$server` function with
    #' `$id` as the namespace.
    call = function(){
      moduleServer(self$id,
                   private$server)
    },
    #' @description
    #' generate the UI associated with this ShinyModule object.
    #' @param id the id to use as a namespace from \link[shiny]{NS}
    #' defaults to `$id` field.
    ui = function(id = self$id){
      ns <- NS(id)
      tagList()
    },
    #' @description
    #' Function to call to remove this ShinyModule's UI elements
    #' and clean up the server side input elements.
    remove = function(input, session){
      abort("ShinyModule `$remove` called. Did you mean to make your own method?")
    },
    #' @description
    #' Function that when called, will create a reactive expression that
    #' returns a reference to this ShinyModule Object. Calling this
    #' function will enable reactivity on this object. This allows
    #' users to set fields as normal values and have
    #' more control over when a field should be updated.
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
    #' @description
    #' Function used to invalidate the private reactive expression.
    #' This should only be called if `$reactive` has been called at
    #' the top of the app.
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
