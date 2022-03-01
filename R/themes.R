my_theme <- function(position = "top_right") {
  my_theme <- ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10, face = "bold"),
                   legend.text = ggplot2::element_text(size = 8),
                   legend.title = ggplot2::element_blank(),
                   legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "cm"),
                   legend.spacing.x = ggplot2::unit(0.1, "cm"),
                   panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(),
                   strip.text = ggplot2::element_text(face = "bold",
                                                      color = "white",
                                                      size = 11),
                   strip.background = ggplot2::element_rect(fill = "black"))
  if (position == "top_right") {
    my_theme <- my_theme + ggplot2::theme(
      legend.position = c(0.99, 0.99),
      legend.justification = c(0.99, 0.99)
      )
  } else if (position == "top_left") {
    my_theme <- my_theme + ggplot2::theme(
      legend.position = c(0.01, 0.99),
      legend.justification = c(0.01, 0.99)
    )
  } else if (position == "bottom_left") {
    my_theme <- my_theme + ggplot2::theme(
      legend.position = c(0.01, 0.01),
      legend.justification = c(0.01, 0.01)
    )
  } else if (position == "bottom_right") {
     my_theme <- my_theme + ggplot2::theme(
       legend.position = c(0.99, 0.01),
       legend.justification = c(0.99, 0.01)
    )
  }
  my_theme
}
geo_theme <- function() {
  my_theme <- ggplot2::theme_bw() +
    ggplot2::theme(legend.justification = c("right", "bottom"),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10, face = "bold"),
                   legend.text = ggplot2::element_text(size = 8),
                   legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "cm"),
                   legend.spacing.x = ggplot2::unit(0.1, "cm"),
                   panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(),
                   strip.text = ggplot2::element_text(face = "bold",
                                                      color = "white",
                                                      size = 11),
                   strip.background = ggplot2::element_rect(fill = "black"))
}

modified_theme <- function() {
  modified_theme <- ggplot2::theme_bw() +
    ggplot2::theme(legend.position = c(0.99, 0.01),
                   legend.justification = c("right", "bottom"),
                   axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 14, face = "bold"),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.title = ggplot2::element_blank(),
                   legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "cm"),
                   legend.spacing.x = ggplot2::unit(0.1, "cm"),
                   panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 16, face = "bold"),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(),
                   strip.text = ggplot2::element_text(face = "bold",
                                                      color = "white",
                                                      size = 15),
                   strip.background = ggplot2::element_rect(fill = "black"))
}
