require(grDevices) # for colours
# x <- -6:16
data <- load("params.RData")
mean_x <- data["mn.x"]
mean_y <- data["mn.y"]

op <- par(mfrow = c(2, 2))
contour(x = -5:5, y = -5:5, params)
#contour(-8:8, -8:8, method = "edge", vfont = c("sans serif", "plain"))
#contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z)
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1,
        xlab = quote(x[1]), ylab = quote(x[2]))
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple",
        main = "20 levels; \"simple\" labelling method")
par(op)