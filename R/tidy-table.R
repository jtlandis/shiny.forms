
# random_html_color <- function(){
#   str_c(sample(c("0","1","2","3","4","5","6","7","8","9",
#                  "A","B","C","D","E","F"), 6, replace = T), collapse = "")
# }

#' The NSE table
#'
#' @name tidy_table
#'
#' @description An additional class to extend `data.frame` to allow
#' for NSE filtering on `i`, and tidyselect functionality on `j`.
#' This will help condense some code statements.
#' @export
tidy_table <- function(...) {

  dots <- match.call(expand.dots = F)$...
  dnms <- ...names()
  if(any(.na <- is.na(dnms))) {
    dnms[.na] <- vapply(dots[.na], deparse1, character(1))
  }
  out <- list()
  env <- parent.frame()
  for (i in seq_len(...length())) {
    out[[dnms[i]]] <- eval(dots[[i]], envir = out, env)
  }


  out <- lapply(out, function(x) if (inherits(x, "data.frame")) list(x) else x)
  lgnths <- vapply(out, length, integer(1))
  max_size <- max(lgnths)
  failed_dots <- !lgnths %in% c(1L, max_size)
  if (any(failed_dots)) {
    abort(
      glue("The following expressions either need",
           " to evaluate to length 1 or {max_size}. failed columns :\n",
           glue_collapse(paste("\tlength:", lgnths[failed_dots],"  ", map_chr(dots[failed_dots], deparse_cut, width = 35)), sep = "\n"),
           "\n")
      )
  }

  len_1 <- max_size!=1L & lgnths==1L
  if (any(len_1)) {
    out[len_1] <- lapply(out[len_1], rep_len, max_size)
  }

  attr(out, "row.names") <- .set_row_names(max_size)
  attr(out, "names") <- names(dots)
  class(out) <- c("tidy_table", "data.frame")

  return(out)
}

deparse_cut <- function(x, width = 35) {
  x <- deparse(x, width)
  if (length(x)>1) {
    x <- paste0(x[1L], "...")
  }
  return(x)
}
as_tidy_table <- function(x) {
  x <- as.list(x)
  attr(x, "row.names") <- .set_row_names(length(x[[1L]]))
  class(x) <- c("tidy_table", "data.frame")
  return(x)
}

#' @export
`[.tidy_table` <- function(x, i, j, drop = FALSE) {
  i_quo <- enquo(i)
  j_quo <- enquo(j)

  n_real_args <- nargs() - !missing(drop)
  cl <- class(x)
  x <- as.list(x)

  if (n_real_args <= 2L) {
    if(!missing(drop)){
      warn("`drop` argument ignored for subsetting a tibble with `x[j]`, it has an effect only for `x[i, j]`.")
      drop <- FALSE
    }
    #attempt to eval as if j
    j <- tidyselect::eval_select(i_quo, x)
    return(as_tidy_table(x[j]))
  }

  if (!missing(i)){
    i <- eval_tidy(i_quo, x)
  } else {
    i <- NULL
  }

  if (!missing(j)) {
    j <- eval_select(j_quo, x)
  } else {
    j <- NULL
  }

  if(!is.null(j)) {
    x <- x[j]
  }
  if(!is.null(i)) {
    x <- lapply(x, `[`, i)
  }

  if (drop && length(j)==1L) {
    return(x[[1L]])
  }

  class(x) <- cl
  attr(x, "row.names") <- .set_row_names(length(x[[1L]]))
  return(x)
}

#' @export
`[<-.tidy_table` <- function(x, i, j, value) {
  i_quo <- enquo(i)
  j_quo <- enquo(j)
  cl <- class(x)
  x <- as.list(x)
  n_real_args <- nargs()
  if (n_real_args < 4) {
    abort("when assigning to a tidy_table, all i, j, and value must be in call signature")
  }

  if (length(dim(value)) == 0) {
    value <- list(value)
  }

  if (missing(j)) {
    j <- setNames(seq_along(x), names(x))
  } else {
    j <- eval_select(j_quo, x)
  }

  if (missing(i)) {
    i <- eval_tidy(i_quo, x)
  } else {
    i <- NULL
  }

  if (!(length(value)==j || length(value)==1L)) abort("replacement must be an atomic vector, data.frame with 1 column or equal columns to j selection")


  if (is.null(i)) {
    n <- length(x[[1L]])
    for (jj in seq_along(j)) {
      x[[j[jj]]] <- rep_len1(value[[jj]], n)
    }
  } else {
    n <- i_len(i)
    for (jj in seq_along(j)) {
      x[[j[jj]]][i] <- rep_len1(value[[jj]], n)
    }
  }

  class(x) <- cl
  attr(x, "row.names") <- .set_row_names(length(x[[1L]]))
  return(x)
}

#' @export
print.tidy_table <- function(x, n = 12, width = 100, min = 4, max = 10) {

  format(x, n, width)

}

i_len <- function(x) UseMethod("i_len")
i_len.default <- function(x) length(x)
i_len.logical <- function(x) sum(x)

rep_len1 <- function(x, n) {
  l <- length(x)
  if (l==1L) return(rep_len(x, n))
  if (l==n) return(x)
  abort(glue("replacement needs to be length {n} or 1"))
}

format.tidy_table <- function(x, n = 12, width = Inf) {

  .dim <- dim(x)
  .obs <- .dim[1L]
  .col <- .dim[2L]
  .nms <- names(x)
  cat("# A bable with ", .obs,
      " observation", if(.obs>1L) "s" else "",
      " and ", .col, " column",
      if(.col>1L) "s" else "", "\n", sep = "")

  if (width == Inf) {
    width <- sum(nchar(.nms)) + 10L
  }

  short_vis <- FALSE
  if (.obs <= n) {
    short_vis <- TRUE
    n <- .obs
  }

  lst <- as.list(x)

  if (short_vis) {
    data_sub <- lst
    .rows <- format(c("","", paste0(seq_len(n), ": ")), justify = "right")
  } else {
    head_n <- n %/% 2
    tail_n <- n - head_n
    tail_seq <- rev(seq_len(tail_n))
    data_sub <- lapply(lst, `[`, c(seq_len(head_n), .obs + 1L - tail_seq))
    .rows <- format(c("", "",
                      paste0(
                        format(c(seq_len(head_n),
                                 format(c(paste0("n-", tail_seq[-1L]), "n"), justify = "left")),
                               justify = "right"),
                        ":"
                      )), justify = "right")
  }

  w_avail <- width - max(nchar(.rows)) - 1L

  out <- list()

  for (i in seq_len(.col)) {
    out[[.nms[i]]] <- format(c(.nms[i], class_abbrv(data_sub[[i]]),
                               format(data_sub[[i]], justify = "right")), justify = "right")
    w_avail <- w_avail - max(nchar(out[[i]])) - 2L

    if (w_avail < 0) {
      out <- out[-length(out)]
      break
    }
  }



  body <- c(list(.rows), out)

  if (!short_vis) {
    body <- lapply(body,
                   function(x, after) {
                     format(c(x[1L:after],
                              "...", x[(after + 1L):(length(x))]),
                            justify = "centre")},
                   after = head_n + 2L)
  }

  body <- do.call("paste", c(body, list(sep = "  ", collapse = "\n")))

  cat(body, "\n")

  footer_data <- data_sub[setdiff(.nms, names(out))]
  .remain <- length(footer_data)
  if (.remain > 0) {
    footer <- paste0("# ... with ", .remain, " more variable", if(.remain>1) "s" else "", ":\n# ")
    footer <- c(footer,
                paste(names(footer_data), vapply(footer_data, class_abbrv, character(1)), collapse = ", "))
    cat(footer, "\n")
  }


  invisible(x)

}




format.list <- function(x, ..., width = 6) {
  vapply(x, class_abbrv, FUN.VALUE = character(1), width = width)
}

#' @export
class_abbrv <- function(x, ...) {
  UseMethod("class_abbrv")
}

#'@export
class_abbrv.ShinyModule <- function(x, width = 12L) paste0("<",abbreviate(class(x)[1L], minlength = width-2L),">")
#' @export
class_abbrv.default <- function(x, width = 6L) paste0("<",abbreviate(class(x)[1L], minlength = width-2L),">")
#' @export
class_abbrv.double <- function(x, ...) "<dbl>"
#' @export
class_abbrv.integer <- function(x, ...) "<int>"
#' @export
class_abbrv.factor <- function(x, ...) "<fct>"
#' @export
class_abbrv.character <- function(x, ...) "<chr>"
#' @export
class_abbrv.list <- function(x, ...) "<lst>"
#' @export





get_sym2str <- function(x) UseMethod("get_sym2str")
get_sym2str.default <- function(x) NULL
get_sym2str.call <- function(x) unlist(lapply(x[-1L], get_sym2str))
get_sym2str.name <- function(x) as.character(x)
get_sym2str.quosure <- function(x) get_sym2str(quo_get_expr(x))
