#'
#'
#'
#'
generate_binara_3flower <- function() {
  center <- c(0.5, 0.5)

  # 4 corners of the square
  corner_tips <- list(
    c(1, 1), c(0, 1), c(0, 0), c(1, 0)
  )

  # 4 edge midpoints
  edge_tips <- list(
    c(0.5, 1), c(1, 0.5), c(0.5, 0), c(0, 0.5)
  )

  # 4 mid-diagonal midpoints (halfway between center and edge midpoints)
  mid_vertex_tips <- lapply(edge_tips, function(pt) {
    c((pt[1] + center[1]) / 2, (pt[2] + center[2]) / 2)
  })

  draw_petal <- function(center, tip, width = 0.08, n = 100, col = NA, border = "black", lwd = 2) {
    dx <- tip[1] - center[1]
    dy <- tip[2] - center[2]
    len <- sqrt(dx^2 + dy^2)
    ux <- dx / len; uy <- dy / len
    nx <- -uy; ny <- ux
    t <- seq(0, 1, length.out = n)

    arc_x <- center[1] + t * len * ux + sin(pi * t) * width * nx
    arc_y <- center[2] + t * len * uy + sin(pi * t) * width * ny
    arc_x_mirror <- rev(center[1] + t * len * ux - sin(pi * t) * width * nx)
    arc_y_mirror <- rev(center[2] + t * len * uy - sin(pi * t) * width * ny)

    x_full <- c(arc_x, arc_x_mirror)
    y_full <- c(arc_y, arc_y_mirror)

    polygon(x_full, y_full, col = col, border = border, lwd = lwd)
  }

  # Set up canvas
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), asp = 1,
       axes = FALSE, xlab = "", ylab = "")
  rect(0, 0, 1, 1, lwd = 2)

  # Draw all three flower layers
  for (tip in corner_tips)      draw_petal(center, tip, width = 0.08, col = NA, border = "black")
  for (tip in edge_tips)        draw_petal(center, tip, width = 0.08, col = NA, border = "darkblue")
  for (tip in mid_vertex_tips)  draw_petal(center, tip, width = 0.06, col = NA, border = "darkgreen")

  # Center dot
  points(center[1], center[2], pch = 16, col = "black")
}
