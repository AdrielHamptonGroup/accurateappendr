results_cleanup <- function(res, rename_key) {
  if (!length(res)) {
    # no results? return empty df with correct structure
    return(
      rep(list(character(0)), length(rename_key)) %>%
        setNames(names(rename_key)) %>%
        dplyr::as_tibble()
    )
  }
  dplyr::rename(res, !!!rename_key)
}
