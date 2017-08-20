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
  res %>% httr::content("text") %>% jsonlite::fromJSON()
}
