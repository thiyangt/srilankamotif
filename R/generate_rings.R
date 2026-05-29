#'
#'
#'
#'
#'
#'@export
generate_rings <- function(n_rings = 8,
                     n_stripes = 36,
                     r_max = 1,
                     stripe_width = 0.08) {

  df <- do.call(rbind, lapply(1:n_rings, function(i) {

    r_inner <- (i - 1) * (r_max / n_rings)
    r_outer <- r_inner + stripe_width

    do.call(rbind, lapply(1:n_stripes, function(j) {

      theta1 <- (j - 1) * 2 * pi / n_stripes
      theta2 <- j * 2 * pi / n_stripes

      data.frame(
        x = c(
          r_inner * cos(theta1),
          r_outer * cos(theta1),
          r_outer * cos(theta2),
          r_inner * cos(theta2)
        ),
        y = c(
          r_inner * sin(theta1),
          r_outer * sin(theta1),
          r_outer * sin(theta2),
          r_inner * sin(theta2)
        ),
        ring = i,
        stripe = j
      )
    }))
  }))

  ggplot(df, aes(x, y, group = interaction(ring, stripe))) +
    geom_polygon(fill = "#D4AF37", color = "black", linewidth = 0.2) +
    coord_equal() +
    theme_void()
}
