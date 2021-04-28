#' Plot usage data
#'
#' @param usage_data data provided by [spaceusage::get_usage()]
#' @param drive_name a drive name
#'
#' @return a plot object from [ggplot][tibble::tibble-package]
#' @export
plot_usage <- function(usage_data, drive_name) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed to plot data usage. Please install it.",
      call. = FALSE
    )
  }

  plot <- ggplot2::ggplot(
    usage_data,
    ggplot2::aes(x = .data$full_name, y = .data$size, fill = .data$file_type)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    ) +
    ggplot2::labs(
      x = "Person",
      y = paste("Total space used -", .data$drive_name)
    ) +
    ggplot2::ggtitle(.data$drive_name) +
    ggplot2::scale_fill_brewer(palette = "Set3")

  return(plot)
}
