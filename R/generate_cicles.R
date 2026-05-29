#'
#'
#'
#'
#'
#'@export
generate_Circles <- function(n_points = 1000,
                     arms = 6,
                     turns = 5,
                     a = 0,
                     b = 0.15) {

  theta <- seq(0, turns * 2 * pi, length.out = n_points)

  df <- do.call(rbind, lapply(0:(arms - 1), function(k) {

    angle_shift <- 2 * pi * k / arms

    r <- a + b * theta

    data.frame(
      x = r * cos(theta + angle_shift),
      y = r * sin(theta + angle_shift),
      group = k
    )
  }))

  ggplot(df, aes(x, y, group = group)) +
    geom_path(color = "#8B0000", linewidth = 0.8) +
    coord_equal() +
    theme_void()
}
