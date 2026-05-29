#'
#'
#'
#'@export
generate_flower <- function(n_petals = 16,
                            petal_length = 1,
                            petal_width = 0.25) {

  theta <- seq(0, 2 * pi, length.out = 100)

  petal <- data.frame(
    x = petal_length * sin(theta),
    y = petal_width * sin(theta) * cos(theta)
  )

  petals <- do.call(
    rbind,
    lapply(seq(0, 2 * pi, length.out = n_petals + 1)[-1], function(a) {
      data.frame(
        x = petal$x * cos(a) - petal$y * sin(a),
        y = petal$x * sin(a) + petal$y * cos(a),
        group = a
      )
    })
  )

  ggplot(petals, aes(x, y, group = group)) +
    geom_polygon(
      fill = "#FFB000",
      colour = "#8B0000",
      linewidth = 0.4
    ) +
    coord_equal() +
    theme_void()
}

