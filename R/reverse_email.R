#' Lookup name and address from e-mail
#'
#' @param key Your AccurateAppend api key
#' @param first_name String
#' @param last_name String
#'
#' @return Data frame with 6 columns:
#' \describe{
#'   \item{address}{Character, House number, street, and unit}
#'   \item{city}{Character}
#'   \item{first_name}{Character}
#'   \item{last_name}{Character}
#'   \item{zip}{Character}
#'   \item{state}{Character}
#'   \item{street_name}{Character, Street portion of address}
#' }
#'
#' See \url{http://docs.accurateappend.com/docs/email-append-reverse} for
#' additional info.
#'
#' @export
reverse_email <- function(key, email, first_name = NA, last_name = NA) {
  call_args <- list(
    "emailaddress" = email,
    "firstname" = first_name,
    "lastname" = last_name
  ) %>% Filter(. %>% anyNA %>% `!`, .)
  aa_api_call(key, "reverse_email", call_args) %>%
    magrittr::use_series("Records") %>%
    results_cleanup(
      rlang::quos(
        address = Address, city = City, first_name = FirstName,
        last_name = LastName, zip = PostalCode, state = State,
        street_name = StreetName
      )
    )
}
