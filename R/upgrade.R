

upgrade <- function(x, ...) {
  UseMethod("upgrade")
}
upgrade.default <- function(x, ...) {
  abort(glue("No method internal S3 'upgrade' method defined for class <{glue_collapse(class(x), sep = '/')}>"), class = "shiny.form_no_upgrade")
}
upgrade.ShinyForm <- function(x, ...) {

  x$layout$obj <- map(x$layout$obj, function(.x) {.x$upgrade()})
  eval.parent(
    call_modify(
      quote(ShinyForm$new(id = self$id, layout = self$layout, dir = private$.dir)),
      ...))

}


upgrade.SFConstructor <- function(x, ...) {
  eval.parent(
    call_modify(
      expr((!!sym(class(x)[1L]))$new(id = self$id)),
      ...
    )
  )
}

upgrade.R6Input <- function(x, ...) {
  l <- map(setNames(nm =names(formals(x$initialize))), function(f) call("$", quote(self), as.name(f)))
  eval.parent(
    call_modify(
      expr((!!sym(class(x)[1L]))$new(!!!l)),
      ...))
}
