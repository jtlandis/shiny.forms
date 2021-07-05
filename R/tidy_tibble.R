
# random_html_color <- function(){
#   str_c(sample(c("0","1","2","3","4","5","6","7","8","9",
#                  "A","B","C","D","E","F"), 6, replace = T), collapse = "")
# }

#' @name tidy_tibble
#' @description An additional class to extend `tibble()` to allow
#' for NSE filtering on `i`, and tidyselect functionality on `j`.
#' This will help condense some code statements.
tidy_tibble <- function(..., .rows = NULL, .name_repair = c("check_unique", 
                                                     "unique", "universal", "minimal")) {
  x <- match.call()
  x[[1]] <- quote(tibble)
  dtbl <- eval_tidy(x, env = caller_env())
  structure(dtbl, class = c("tidy_tbl", class(dtbl)))
}
as_tidy_tibble <- function(x) {
  x <- as_tibble(x)
  structure(x, class = c("tidy_tbl", class(x)))
}
`[.tidy_tbl` <- function(x, i, j, drop = FALSE) {
  i_quo <- enquo(i)
  j_quo <- enquo(j)
  n_real_args <- nargs() - !missing(drop)
  if (n_real_args <= 2L) {
    if(!missing(drop)){
      warn("`drop` argument ignored for subsetting a tibble with `x[j]`, it has an effect only for `x[i, j]`.")
      drop <- FALSE
    }
    #attempt to eval as if j
    i <- tidyselect::eval_select(i_quo, x)
  } else if (!missing(i)){
    i <- eval_tidy(i_quo, x)
  }
  
  if(!missing(j)){
    j_quo <- enquo(j)
    j <- tidyselect::eval_select(j_quo, x)
  }
  NextMethod("[")
}

`[<-.tidy_tbl` <- function(x, i, j, value) {
  i_quo <- enquo(i)
  j_quo <- enquo(j)
  n_real_args <- nargs()
  if (n_real_args <= 3L) {
    #attempt to eval as if j
    i <- tryCatch(tidyselect::eval_select(i_quo, x),
                  error = function(e){
                    eval_tidy(i_quo, x)
                  }) 
  } else if (!missing(i)){
    i <- eval_tidy(i_quo, x)
  }
  
  if(!missing(j)){
    j_quo <- enquo(j)
    j <- tidyselect::eval_select(j_quo, x)
  }
  NextMethod("[<-")
}

get_sym2str <- function(x) UseMethod("get_sym2str")
get_sym2str.default <- function(x) NULL
get_sym2str.call <- function(x) unlist(lapply(x[-1L], get_sym2str))
get_sym2str.name <- function(x) as.character(x)
get_sym2str.quosure <- function(x) get_sym2str(quo_get_expr(x))
