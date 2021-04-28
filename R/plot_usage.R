# Function to plot the data
plot_usage <- function(usage_data, drive_name) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package \"ggplot2\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

  ggplot2::ggplot(usage_data, ggplot2::aes(x = full_name, y = size, fill = file_type)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    ) +
    ggplot2::labs(
      x = "Person",
      y = paste("Total space used -", drive_name)
    ) +
    ggplot2::ggtitle(drive_name) +
    ggplot2::scale_fill_brewer(palette = "Set3")
}
