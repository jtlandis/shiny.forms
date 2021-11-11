

#takes a layout and builds it
ShinyForm <- R6::R6Class(
  "ShinyForm",
  inherit = ShinyModule,
  initialize = function(id, layout, file) {
    super$initialize(id)
    self$layout <- layout
    private$.file <- file
  },
  ui = function(id = self$id) {
    ns <- NS(id)
  }
)


generate_call <- function(target, layout) {
  row_i <- layout$dom==target
  if (layout$type[row_i]!="column") {
    return(layout$obj[[which(row_i)]]$get_call())
  }
  
  target_call <- layout$obj[[which(row_i)]]$get_call()
  prnts <- layout$parent == target
  
  if (any(prnts)) {
    call_modify(target_call, !!!map(layout$dom[prnts], generate_call, layout = layout))
  } else {
    target_call
  }
}

tbl <- readRDS( here::here("example_tidy_table.rds"))
saveRDS(tbl, file = here::here("example_tidy_table.rds"))
call2("tagList", !!!map(tbl[str_detect(parent, "-Container$"),]$dom, generate_call, tbl))
generate_call("1-ShinyForm-Column", tbl)
