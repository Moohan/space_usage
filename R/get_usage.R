get_usage_info <- function(drive) {

  usage_data <- fs::dir_info(
    path = fs::path(str_glue("/conf/{drive}")),
    type = "file",
    recurse = TRUE,
    fail = FALSE
  ) %>%
    dplyr::select(path, size, user, ends_with("_time")) %>%
    dplyr::mutate(size = case_when(
      size < 0 | is.na(size) ~ fs::fs_bytes(0),
      TRUE ~ size
    )) %>%
    dplyr::mutate(
      file_name = fs::basename(path),
      file_type = fs::path_ext(path) %>%
        # Turn into a factor but group file types which don't contribute much
        # 11 seems to produce reasonable plots but you could make this larger or smaller
        forcats::fct_lump_n(
          n = 11,
          w = size,
          other_level = "Other / NA"
        ) %>%
        # Group the NAs into the Other level
        forcats::fct_explicit_na("Other / NA")
    ) %>%
    dplyr::left_join(get_full_names(), by = "user") %>%
    dplyr::arrange(desc(size))

  return(usage_data)
}
