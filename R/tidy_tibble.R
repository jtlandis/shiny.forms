
#' @name tidy_tibble
#' @description An additional class to extend `tibble()` to allow
#' for NSE filtering on `i`, and tidyselect functionality on `j`.
#' This will help condense some code statements.
tidy_tibble <- function(..., .rows = NULL, .name_repair = c("check_unique", 
                                                     "unique", "universal", "minimal")) {
  x <- match.call()
  x[[1]] <- quote(tibble)
  dtbl <- eval(x)
  structure(dtbl, class = c("tidy_tbl", class(dtbl)))
}
as_tidy_tibble <- function(x) {
  x <- as_tibble(x)
  structure(x, class = c("tidy_tbl", class(x)))
}
`[.tidy_tbl` <- function(x, i, j, drop = FALSE) {
  if(!missing(i)){
    i_quo <- enquo(i)
    i <- eval_tidy(i_quo, x)
  }
  
  if(!missing(j)){
    j_quo <- enquo(j)
    j <- tidyselect::eval_select(j_quo, x)
  }
  
  NextMethod("[", x, i = i, j = j, drop = drop)
}

`[<-.tidy_tbl` <- function(x, i, j, value) {
  if(!missing(i)){
    i <- eval_tidy(enquo(i), x)
  }
  if(!missing(j)) {
    j <- eval_tidy(enquo(j), x)
  }
  NextMethod("[<-", x, i = i, j = j, value = value)
}
