#' Search For a Single Consumer's Phone Numbers
#'
#' @param key Your AccurateAppend API Key
#' @param first_name String
#' @param last_name String, required
#' @param address String
#' @param city String, required
#' @param state String, state postal abbreviation, required
#' @param postal_code String
#'
#' @return Data frame with 6 columns:
#' \describe{
#'   \item{AreaCode}{Character}
#'   \item{LineType}{Character, mobile v. landline}
#'   \item{MatchLevel}{Character, code for fidelity of match}
#'   \item{MaxValidationLevel}{Character, maximum MatchLevel possible for data provided}
#'   \item{PhoneNumber}{Character}
#'   \item{Source}{Character, code for source database}
#' }
#'
#' See \url{http://docs.accurateappend.com/docs/appendphone} for additional
#' infor including match code and source definitions.
#'
#' @export
consumer_phone <- function (
  key, first_name = NA, last_name, address = NA, city, state,
  postal_code = NA,
  sources = c("C", "D", "M", "E"),
  match_levels = c("E1", "E2", "N1", "N2", "B1", "B2")
) {
  if (missing(state) && missing(postal_code)) {
    stop("At least one of state or postal_code is required")
  }
  sources <- match.arg(sources, several.ok = TRUE)
  match_levels <- match.arg(match_levels, several.ok = TRUE)
  call_args <- list(
    "firstname" = first_name,
    "lastname" = last_name,
    "address" = address,
    "city" = city,
    "state" = state,
    "postalcode" = postal_code,
    "source" = sources,
    "matchlevel" = match_levels
  ) %>% Filter(. %>% anyNA %>% `!`, .)
  aa_api_call(key, "phone", call_args)
}

# internal api call method with retry loop
aa_api_call <- function (key, method, args) {
  always_wait <- 0
  retry_wait <- 1
  retry_count <- 0
  key <- paste0(key, "/")
  vecs <- sapply(args, length) > 1
  args[vecs] %<>% lapply(paste, collapse = ";")
  req_url <- httr::modify_url(base_url, path = c(method_urls[method], key),
                              query = args)
  res <- httr::GET(req_url,
                   httr::add_headers(`content-type` = "application/json"))
  if (httr::http_error(res)) {
    if (httr::status_code(res) %in% 400:401) {
      stop(res %>% httr::content("text") %>% jsonlite::fromJSON() %>%
             magrittr::use_series("Error"))
    }
    while (httr::status_code(res) == 500 && retry_count < 5) {
      Sys.sleep(retry_wait)
      retry_wait <- retry_wait * 2
      retry_count <- retry_count + 1
      res <- httr::GET(req_url,
                       httr::add_headers(`content-type` = "application/json"))
    }
    httr::stop_for_status(res)
  }
  res %>% httr::content("text") %>% jsonlite::fromJSON() %>%
    magrittr::use_series("Phones")
}
