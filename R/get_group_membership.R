#' Get all groups or users in a given group (or groups)
#'
#' @description If given no parameters it will return a list of all
#' possible groups. The first parameter `group_name`, can be a vector
#' of groups, and it will return all users (and their full names) who
#' have membership. The second parameter `group_name_exclude` can be
#' used to find users except those in the supplied group(s).
#'
#' @param group_name name of a group, this is usually the same as a drive name. Can be a vector of multiple groups.
#' @param group_name_exclude Group or groups to exclude.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
get_group_membership <- function(group_name = NULL, group_name_exclude = NULL) {
  if (fs::file_exists(fs::path(tempdir(), "groups.rds"))) {
    groups <- readr::read_rds(fs::path(tempdir(), "groups.rds"))
  } else {
    groups <- stringr::str_split_fixed(
      system("getent group", intern = TRUE),
      ":", 4
    ) %>%
      tibble::as_tibble(.name_repair = ~ c("group", "2", "3", "user")) %>%
      dplyr::filter(.data$user != "") %>%
      dplyr::select(.data$group, .data$user) %>%
      dplyr::mutate(user = stringr::str_split(.data$user, ",")) %>%
      tidyr::unnest(cols = .data$user)

    readr::write_rds(groups, fs::path(tempdir(), "groups.rds"))
  }

  if (is.null(group_name)) {
    cli::cli_inform(c("i" = "{.param group_name} was not submitted, returning an
                      alphabetised list of all possible group names."))

    group_names <- groups %>%
      dplyr::count(.data$group) %>%
      dplyr::filter(.data$n > 1) %>%
      dplyr::pull(.data$group)

    return(group_names)
  }

  if (!is.null(group_name_exclude)) {
    users_to_exclude <- groups %>%
      dplyr::filter(tolower(.data$group) %in% tolower(group_name_exclude)) %>%
      dplyr::pull(.data$user)

    groups <- groups %>%
      dplyr::filter(!(.data$user %in% users_to_exclude))
  }

  group_membership <- groups %>%
    dplyr::filter(tolower(.data$group) %in% tolower(group_name)) %>%
    dplyr::left_join(get_full_names(), by = "user") %>%
    dplyr::arrange(.data$group, .data$full_name)

  return(group_membership)
}
