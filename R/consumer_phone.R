#' Search For a Single Consumer's Phone Numbers
#'
#' @param key Your AccurateAppend API Key
#' @param first_name String
#' @param last_name String, required
#' @param address String
#' @param city String, required
#' @param state String, state postal abbreviation, required
#' @param postal_code String
#' @param sources Code(s) for databases to search, see Details
#' @param match_levels Code(s) for acceptable match fidelity, see Details
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
#' info including match code and source definitions.
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
  aa_api_call(key, "phone", call_args) %>%
    magrittr::use_series("Phones") %>%
    results_cleanup(
      rlang::quos(
        area_code = AreaCode, phone_type = LineType, match_level = MatchLevel,
        max_validation_level = MaxValidationLevel,
        phone = PhoneNumber, source = Source
      )
    ) %>%
    dplyr::mutate(phone = paste0(.data$area_code, .data$phone)) %>%
    dplyr::select(-.data$area_code)
}
