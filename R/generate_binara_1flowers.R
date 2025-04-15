#'
#'
#'
#'
generate_binara_1flower <- function(width=0.2) {
  center <- c(0.5, 0.5)

  # Define square corners (clockwise)
  corners <- list(
    c(1, 1),
    c(0, 1),
    c(0, 0),
    c(1, 0)
  )

  draw_petal <- function(center, tip, width = 0.1, n = 100) {
    # Vector from center to tip
    dx <- tip[1] - center[1]
    dy <- tip[2] - center[2]
    len <- sqrt(dx^2 + dy^2)

    # Unit vector along direction
    ux <- dx / len
    uy <- dy / len

    # Perpendicular unit vector (normal)
    nx <- -uy
    ny <- ux

    # Symmetric petal shape using sine curve
    t <- seq(0, 1, length.out = n)
    arc_x <- center[1] + t * len * ux + sin(pi * t) * width * nx
    arc_y <- center[2] + t * len * uy + sin(pi * t) * width * ny

    # Mirror the petal for symmetry
    arc_x_mirror <- rev(center[1] + t * len * ux - sin(pi * t) * width * nx)
    arc_y_mirror <- rev(center[2] + t * len * uy - sin(pi * t) * width * ny)

    x_full <- c(arc_x, arc_x_mirror)
    y_full <- c(arc_y, arc_y_mirror)

    polygon(x_full, y_full, col = "lightpink", border = "firebrick")
  }

  # Draw plot area
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), asp = 1, axes = FALSE, xlab = "", ylab = "")
  rect(0, 0, 1, 1, lwd = 2, border = "black")

  # Draw 4 symmetric petals
  for (tip in corners) {
    draw_petal(center, tip, width = width)
  }

  # Optional: center detail
  points(center[1], center[2], pch = 16, col = "firebrick")
}

