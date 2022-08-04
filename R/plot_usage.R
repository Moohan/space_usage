#' Plot usage data
#'
#' @description
#'
#' @param usage_data data provided by [get_usage()]
#' @param plot_title The title to use for the plot, by default it will try to
#' guess the drive name and use that.
#' @param by one of "user" (default), "folder" or "all" to split the plot by
#'
#' @return a [ggplot][ggplot2::ggplot2-package] plot object
#' @export
#' @seealso [get_usage()]
plot_usage <- function(usage_data, plot_title = NULL, by = c("user", "folder", "both")) {
  if (missing(by)) {
    by <- "user"
  } else {
    by <- match.arg(by)
  }

  if (is.null(plot_title)) {
    plot_title <- stringr::str_remove(
      usage_data$path[1],
      stringr::fixed(
        paste(usage_data$file_dir[1],
          usage_data$file_name[1],
          sep = "/"
        )
      )
    )
  }


  usage_data <- usage_data %>%
    dplyr::mutate(full_name = forcats::as_factor(.data$full_name) %>%
      forcats::fct_lump_prop(prop = 0.005, w = .data$size, other_level = "Other users (or NA)") %>%
      forcats::fct_explicit_na(na_level = "Other users (or NA)"))

  plot <- ggplot2::ggplot(
    usage_data,
    ggplot2::aes(y = .data$size, fill = .data$file_type_lumped)
  ) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(plot_title) +
    ggplot2::scale_fill_brewer("File Type", type = "qual") +
    ggplot2::scale_y_continuous("Space used", labels = fs::fs_bytes) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      legend.position = "top"
    )

  if (by == "user") {
    plot <- plot +
      ggplot2::geom_col(ggplot2::aes(x = .data$full_name)) +
      ggplot2::xlab("User")
  } else if (by == "folder") {
    plot <- plot +
      ggplot2::geom_col(ggplot2::aes(x = .data$top_folder)) +
      ggplot2::xlab("Top-level folder")
  } else if (by == "both") {
    plot <- plot +
      ggplot2::geom_col(ggplot2::aes(x = .data$full_name)) +
      ggplot2::xlab("User") +
      ggplot2::facet_wrap("top_folder")
  }

  return(plot)
}
