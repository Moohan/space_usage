#' Get all groups or users in a given group
#'
#' @param group_name name of a group, this is usually the same as a drive name
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#' @importFrom rlang .data
get_group_membership <- function(group_name = NULL) {
  groups <- stringr::str_split_fixed(
    system("getent group", intern = TRUE),
    ":", 4
  ) %>%
    tibble::as_tibble(.name_repair = ~ c("group", "2", "3", "user")) %>%
    dplyr::select(.data$group, .data$user) %>%
    dplyr::filter(.data$user != "") %>%
    dplyr::mutate(user = stringr::str_split(.data$user, ",")) %>%
    tidyr::unnest(cols = .data$user)

  if (is.null(group_name)) {
    return(groups)
  }

  full_names <- get_full_names()

  users <- groups %>%
    dplyr::filter(.data$group %in% group_name) %>%
    dplyr::left_join(full_names, by = "user") %>%
    dplyr::arrange(.data$group, .data$full_name)

  return(users)
}
