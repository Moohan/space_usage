#' Get a list of fullname which can be matched to usernames
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
get_full_names <- function() {
  if (fs::file_exists(fs::path(tempdir(), "user_full_names.rds"))) {
    full_names <- readr::read_rds(fs::path(tempdir(), "user_full_names.rds"))
  } else {
    full_names <- stringr::str_match(
      system("getent passwd", intern = TRUE),
      "([a-z]+?\\d\\d):.+?1000:([A-Z][a-z]+?\\s.+?):"
    ) %>%
      stats::na.omit() %>%
      tibble::as_tibble(.name_repair = ~ c("string", "user", "full_name")) %>%
      dplyr::select(.data$user, .data$full_name) %>%
      dplyr::mutate(full_name = stringr::str_squish(.data$full_name)) %>%
      dplyr::arrange(.data$user)

    readr::write_rds(full_names, fs::path(tempdir(), "user_full_names.rds"))
  }

  return(full_names)
}
