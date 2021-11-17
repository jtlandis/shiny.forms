
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

#' @export
ShinyForm <- R6::R6Class(
  "ShinyForm",
  inherit = ShinyModule,
  public = list(
    initialize = function(id, layout, dir) {
      super$initialize(id)
      self$layout <- layout
      private$.dir <- dir
    },
    print = function() {
      cat(glue("<{class(self)[1L]}> saved at {private$.loc()}
               \t {nrow(self$layout)} elements stored:"),"\n")
      for(o in self$layout$obj) {
        print(o)
        cat("\n")
      }


    },
    save = function() {
      loc <- private$.loc()
      saveRDS(object = self, file = loc)
      cat("caching:", loc, "\n")
    },
    layout = NULL,
    ui = function(id = self$id) {
      ns <- NS(id)
      .call <- call2("column", width = 12L,
                     useShinyFormsLite(),
                     id = ns('ShinyForm-Container'),
                     class = "ShinyForm-Container",
                     !!!map(self$layout[grepl("-Container$", parent),]$dom, generate_call,
                            layout = self$layout, ns = ns))
      eval(.call)
    },
    load_preview = function(id = self$id){
      ns <- NS(id)
      for (i in seq_len(nrow(self$layout))) {
        p <- self$layout[["parent"]][i]
        o <- self$layout[["obj"]][[i]]
        insertUI(selector = glue("#{p}"),
                 where = "beforeEnd",
                 ui = o$ui(ns(o$id)),
                 immediate = T)
        moduleServer(o$id, o$edit_mod)
      }
    }
  ),
  private = list(
    .loc = function() {if(length(self$id)==0) tempfile(".ShinyForm", private$.dir, ".rds") else glue("{private$.dir}/{self$id}.rds")},
    .dir = NULL,
    server = function(input, output, session) {

      input_objs <- self$layout[type!="column", obj, drop = TRUE]
      mods <- map(input_objs, ~.x$call())
      val_names <- map_chr(input_objs, ~.x$name %||% "")

      value <- reactive({
        validate(
          need(all(nchar(val_names)>0), "Some outputs are not named."),
          need(length(val_names)==length(unique(val_names)), "All output names must be unique.")
        )
        df <- map(mods, ~.x$value() %||% NA)
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
