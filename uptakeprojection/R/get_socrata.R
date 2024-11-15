#' Get Socrata data
#'
#' @param json_url JSON endpoint
#' @param where vector of filters like `state=AL` and `state in(AL, DC)`, or a
#'   string of filters connected by `" and "`
#' @param select vector of columns to keep, or comma-separated string of columns
#' @param limit number of rows (default `NULL` keeps all rows)
#' @param ... additional arguments passed to `RSocrata::read.socrata()`
#'
#' @details
#' `where` clauses can only use single quotes
#'
#' This function constructs "clauses" that are joined into a single
#' URL. For example, `select = c("name", "age")` creates a clause
#' `$select=name,age`. The clauses are joined, URL-encoded, and
#' passed through `read.socrata()`.
#'
#' @return `tibble` coerced from `read.socrata()`
#' @export
#'
#' @examples
#' get_socrata(
#'   "https://healthdata.gov/resource/g62h-syeh.json",
#'   where = "state=AL",
#'   select = c("date", "state", "previous_day_admission_adult_covid_confirmed"),
#'   limit = 10
#' )
get_socrata <- function(
    json_url,
    where = NULL,
    select = NULL,
    limit = NULL,
    ...) {
  clauses <- c()
  if (!is.null(select)) {
    clauses <- c(
      clauses,
      paste0("$select=", paste(select, collapse = ","))
    )
  }
  if (!is.null(where)) {
    clauses <- c(
      clauses,
      paste0("$where=", paste(where, collapse = " and "))
    )
  }
  if (!is.null(limit)) {
    stopifnot(length(limit) == 1)
    clauses <- c(clauses, paste0("$limit=", limit))
  }

  if (length(clauses) == 0) {
    url <- json_url
  } else {
    url <- paste0(json_url, "?", paste(clauses, collapse = "&"))
  }

  url %>%
    URLencode() %>%
    RSocrata::read.socrata(...) %>%
    as_tibble()
}
