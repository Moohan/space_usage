# Function to get a list of fullname which can be matched to usernames
get_full_names <- function() {
  if (fs::file_exists(fs::path(tempdir(), "user_full_names.rds"))) {
    full_names <- readRDS(fs::path(tempdir(), "user_full_names.rds"))
  } else {
    full_names <- stringr::str_match(
      system("getent passwd", intern = TRUE),
      "([a-z]+?\\d\\d):.+?1000:([A-Z][a-z]+?\\s.+?):"
    ) %>%
      stats::na.omit() %>%
      tibble::as_tibble(.name_repair = ~ c("string", "user", "full_name")) %>%
      dplyr::select(user, full_name) %>%
      dplyr::mutate(full_name = stringr::str_squish(full_name)) %>%
      dplyr::arrange(user)

    saveRDS(full_names, fs::path(tempdir(), "user_full_names.rds"), compress = FALSE)
  }

  return(full_names)
}
