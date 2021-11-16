

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

str_extract <- function(string, pattern, ...) {
  res <- regexpr(pattern, string, ...)
  res[res==-1L] <- NA
  pmap_chr(list(res, string,attr(res,"match.length")), ~substr(..2, ..1, ..1 + ..3))
}
