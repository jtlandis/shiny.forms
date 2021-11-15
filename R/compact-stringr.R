

str_detect <- function(string, pattern, negate = FALSE, ...) {

  m <- grepl(pattern, string, ...)
  if (negate)
    !m
  else
    m
}

str_replace <- function(string, pattern, replacement, ...) {
  gsub(pattern, replacement, string, ...)
}

str_remove <- function(string, pattern, ...) {
  str_replace(string, pattern, replacement = "", ...)
}
