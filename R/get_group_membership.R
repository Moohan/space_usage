get_group_membership <- function(group_name = NULL) {
  groups <- stringr::str_split_fixed(system("getent group", intern = TRUE),
                    ":", 4) %>%
    tibble::as_tibble(.name_repair = ~ c("group", "2", "3", "user")) %>%
    dplyr::select(group, user) %>%
    dplyr::filter(user != "") %>%
    dplyr::mutate(user = str_split(user, ",")) %>%
    tidyr::unnest(cols = user)

  if (is.null(group_name)) {
    return(groups)
  }

  full_names <- get_full_names()

  users <- groups %>%
    dplyr::filter(group %in% group_name) %>%
    dplyr::left_join(full_names, by = "user") %>%
    dplyr::arrange(group, full_name)

  return(users)
}
