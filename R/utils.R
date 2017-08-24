results_cleanup <- function(res, rename_key) {
  if (!length(res)) {
    # no results? return empty df with correct structure
    return(
      rep(list(character(0)), length(rename_key)) %>%
        setNames(names(rename_key)) %>%
        dplyr::as_tibble()
    )
  }
  orig_names <- sapply(rename_key, as.character)[2 , ]
  missings <- !orig_names %in% names(res)
  if (any(missings)) {
    rep(NA_character_, sum(missings)) %>%
      as.list() %>%
      setNames(orig_names[missings]) %>%
      {dplyr::mutate(res, !!! .)} ->
      res
  }
  dplyr::rename(res, !!! rename_key)
}
