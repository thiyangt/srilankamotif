#'
#'
#'
#'
#'@export
generate_arimbuwa <- function(n = 10, stripe_height = 1, size = 4) {

  y <- stripe_height / 2

  plot(1:n, rep(y, n),
       pch = 21,
       bg = "white",
       col = "white",
       cex = size,
       xaxt = "n",
       yaxt = "n",
       xlab = "",
       ylab = "",
       asp = 1,
       bty = "n",
       ylim = c(0, stripe_height),
       xlim = c(0.5, n + 0.5),
       col.axis = NA,
       col.lab = NA,
       panel.first = {
         rect(par("usr")[1], par("usr")[3],
              par("usr")[2], par("usr")[4],
              col = "black", border = NA)
       })
}
