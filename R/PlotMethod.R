#plot method for class "CarGrid"
plot.CarGrid = function(x,original = FALSE, ...) {
  if (original) {
    x$griddf = x$origin
    plot.CarGrid(x, original = FALSE, ...)
  }
  else {
    nrow = x$dimension[1]; ncol = x$dimension[2]
    df = x$griddf
    gridmat = matmaker(df, nrow, ncol)
    image(1:ncol, 1:nrow, z = t(gridmat), col = c("White", "Red", "Blue"),...)
  }
}

matmaker = function(griddf, r, j) {
  matdf = data.frame(row = rep(1:r, j), column = rep(1:j, each = r), dens = 0)
  bluepos = (griddf$occupy == "Blue")
  griddf$dens = 1
  griddf$dens[bluepos] = 2
  gridpos = paste(griddf$row, griddf$column)
  allpos = paste(matdf$row, matdf$column)
  matdf[(allpos %in% gridpos),]$dens = griddf$dens
  mat = matrix(matdf$dens, nrow = r, ncol = j)
  mat
}