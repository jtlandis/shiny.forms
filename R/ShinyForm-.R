
#' @include tidy-table.R

ShinyLayout <- R6::R6Class("ShinyLayout",
                           public = list(
                             id = NULL,
                             initialize = function(id = NULL){
                               self$id <- as.character(id)
                             },
                             objects = tidy_table(
                               obj = list(),
                               parent = character(),
                               dom = character(),
                               type = character()
                             ),
                             add_object = function(obj, parent, dom, type){
                               self$objects <-
                                 row_bind(self$objects,
                                           tidy_table(obj = list(obj),
                                                       parent = parent,
                                                       dom = dom,
                                                       type = type))
                               invisible(self)
                             }
                           ))

#takes a layout and builds it
ShinyForm <- R6::R6Class(
  "ShinyForm",
  inherit = ShinyModule,
  public = list(
    initialize = function(id, layout, file) {
      super$initialize(id)
      self$layout <- layout
      private$.file <- file
    },
    layout = NULL,
    ui = function(id = self$id) {
      ns <- NS(id)
      .call <- call2("div", id = glue("{ns(id)}-ShinyForm-Container"),
                     class = "ShinyForm-Container",
                     !!!map(self$layout[grepl("-Container$", parent),]$dom, generate_call,
                            layout = self$layout, ns = ns))
      eval(.call)
    }
  ),
  private = list(
    .file = NULL,
    server = function(input, output, session) {

      input_objs <- layout[type!="column", obj, drop = TRUE]
      mods <- map(input_objs, ~.x$call())
      val_names <- map_chr(input_objs, ~.x$name)

      value <- reactive({
        df <- map(input_objs, ~.x$value())
        names(df) <- val_names
        as_tidy_table(df)

      })

      return(list(value = value))
    }
  )

)


generate_call <- function(target, layout, ns = NULL) {
  ns <- ns %||% function(x) x
  row_i <- layout$dom==target
  if (layout$type[row_i]!="column") {
    .obj <- layout[row_i, obj, drop = TRUE][[1L]]
    return(.obj$get_call(ns(.obj$id)))
  }

  .obj <- layout$obj[[which(row_i)]]
  target_call <- .obj$get_call(ns(.obj))
  prnts <- layout$parent == target

  if (any(prnts)) {
    call_modify(target_call, !!!map(layout$dom[prnts], generate_call, layout = layout, ns = ns))
  } else {
    target_call
  }
}

# tbl <- readRDS( here::here("example_tidy_table.rds"))
# saveRDS(tbl, file = here::here("example_tidy_table.rds"))
# call2("tagList", !!!map(tbl[str_detect(parent, "-Container$"),]$dom, generate_call, tbl))
# generate_call("1-ShinyForm-Column", tbl)
