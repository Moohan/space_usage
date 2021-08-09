#' Get the space usage of a given drive
#'
#' @param drive a drive name
#' @return a [tibble][tibble::tibble-package]
#' @export
#' @importFrom rlang .data
get_usage <- function(drive) {
  full_names <- get_full_names()

  usage_data <- fs::dir_info(
    path = fs::path("/conf", drive),
    type = "file",
    recurse = TRUE,
    fail = FALSE
  ) %>%
    dplyr::select(.data$path, .data$size, .data$user, tidyselect::ends_with("_time")) %>%
    dplyr::mutate(size = dplyr::case_when(
      .data$size < 0 | is.na(.data$size) ~ fs::fs_bytes(0),
      TRUE ~ size
    )) %>%
    dplyr::mutate(
      file_dir = stringr::str_remove(dirname(.data$path), fs::path("/conf", drive)),
      top_folder = stringr::str_extract(.data$file_dir, "(\\w.+?)(?=[$/])"),
      top_folder = dplyr::if_else(is.na(.data$top_folder), stringr::str_sub(.data$file_dir, 2), .data$top_folder),
      file_name = basename(.data$path),
      file_type = fs::path_ext(.data$path),
      file_type_lumped =
      # Turn into a factor but group file types which don't contribute much
      # 11 seems to produce reasonable plots but you could make this larger or smaller
        forcats::fct_lump_n(.data$file_type,
          n = 7,
          w = .data$size,
          other_level = "Other / NA"
        ) %>%
          # Group the NAs into the Other level
          forcats::fct_explicit_na("Other / NA")
    ) %>%
    dplyr::left_join(full_names, by = "user") %>%
    dplyr::arrange(dplyr::desc(.data$size))

  return(usage_data)
}
