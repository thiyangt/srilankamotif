#'
#'
#'
#'
#'@export
generate_galbinduwa <- function(n = 10, size = 8) {

  library(ggplot2)

  df <- data.frame(
    x = 1:n,
    y = 0,
    shape = rep(c("circle", "diamond"), length.out = n)
  )

  ggplot(df) +
    coord_fixed() +

    # black background
    theme_void() +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA)
    ) +

    xlim(0, n + 1) +
    ylim(-1, 1) +

    # circles
    geom_point(
      data = subset(df, shape == "circle"),
      aes(x = x, y = y),
      shape = 21,
      size = size,
      fill = "white",
      color = "white",
      stroke = 1.2
    ) +

    # diamonds
    geom_point(
      data = subset(df, shape == "diamond"),
      aes(x = x, y = y),
      shape = 23,
      size = size,
      fill = "white",
      color = "white",
      stroke = 1.2
    )
}

