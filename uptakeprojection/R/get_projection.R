#' Wrapper for projection functions
#'
#' @param name character to choose method, one of "rate-matched|date-matched"
#' @param ... Further arguments passed to the chosen method
#' @return output of chosen method
#' @seealso [rate_matched_method()], [date_matched_method()]
#' @export
get_projection <- function(name, ...) {
  switch(name,
    rate_matched = rate_matched_method(...),
    date_matched = date_matched_method(...)
  )
}
