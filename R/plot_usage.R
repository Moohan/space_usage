#' Plot usage data
#'
#' @param usage_data data provided by [spaceusage::get_usage()]
#' @param drive_name a drive name
#'
#' @return a plot object from [ggplot][tibble::tibble-package]
#' @export
plot_usage <- function(usage_data, drive_name, by = "user") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed to plot data usage. Please install it.",
      call. = FALSE
    )
  }

  plot <- ggplot2::ggplot(
    usage_data,
    ggplot2::aes(y = .data$size, fill = .data$file_type_lumped)
  ) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(drive_name) +
    ggplot2::scale_fill_brewer("File Type", type = "qual") +
    ggplot2::ylab("Space used") +
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
  } else if (by == "all") {
    plot <- plot +
      ggplot2::geom_col(ggplot2::aes(x = .data$full_name)) +
      ggplot2::xlab("User") +
      ggplot2::facet_wrap("top_folder")
  }

  return(plot)
}
