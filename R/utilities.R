


#' Remove Shiny Inputs
#'
#' @name remove_shiny_inputs
#'
#' @param id character vector of full input ids to remove
#' @param .input an input object from a shiny session
#'
#' @return
#' @export
#'
#' @examples
remove_shiny_inputs <- function(id, .input) {
  impl <- .subset2(.input, "impl")
  lgl <- id %in% impl$.values$keys()
  if(any(!lgl)) warn(glue("The following `id`s were not found in shiny server input and cannot be removed : ",
                          glue_collapse(id[!lgl]), sep = ", ", last = ", and "))
  to_rm <- id[lgl]
  invisible(
    map(to_rm, function(i) {
      impl$.values$remove(i)
    })
  )
}

sort_by <- function(x, by){
  order(match(x, by))
}

# The following are simple helpers in some `R6Input` classes
# to help edit the dom correctly when values change
empty_on_0str <- function(x) {
  if(is.null(x)&&(length(x)==0||(is.character(x)&&x==""))) return(character())
  x
}
empty2null <- function(x){
  if(is.character(x)&&x=="") return(NULL)
  x
}

#' @description
#'  my attempt at making a faster rbind function.
#'  check will be used to reorder names and ensure
#'  datatypes match between names. otherwise
#'  it will concatinate lists together without checking.
#'  Currently this runs twice as fast as `base::rbind`,
#'  but other packages equivalents that are written in
#'  C are 2-4 times faster than this version.
#' @export
row_bind <- function(..., check = TRUE) {
  #browser()
  lst <- list(...)
  .names <- unique(unlist(lapply(lst, names)))
  lst <- lapply(lst, as.list)
  if (check) {

    lst <- lapply(lst, function(df, names){
      names_fill <- setdiff(names, names(df))
      n_ <- length(df[[1]])
      if (length(names_fill)) {
        for (nm in names_fill) {
          df[[nm]] <- rep_len(NA, n_)
        }
      }
      return(df[names])
    }, names = .names)

  }

  out <- lapply(
    seq_along(lst[[1L]]),
    function(i) do.call('c', lapply(lst, `[[`, i))
  )

  names(out) <- .names
  n <- length(out[[1]])
  #m <- length(out)
  attr(out, "row.names") <- .set_row_names(n)
  class(out) <- c("tidy_table" ,"data.frame")
  return(out)


}
