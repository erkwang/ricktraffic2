#summary methond for class "CarGrid"
summary.CarGrid = function(object, ...) {
  sum = list()
  sum$dimention = paste("Number of rows:", object$dimension[1], 
                        "Number of columns:", object$dimension[2])
  if (length(object$density) == 1) {
    sum$density = paste("Density of Red:", object$density, 
                        "Density of Blue:", object$density)
  }
  else if (length(object$density) > 1) {
    sum$density = paste("Density of Red:", object$density[1], 
                        "Density of Blue:", object$density[2])
  }
  sum$count = paste("Number of Red Cars:",
                    nrow(object$griddf[object$griddf$occupy == "Red",]),
                    " Number of Blue Cars:",
                    nrow(object$griddf[object$griddf$occupy == "Blue",]))
  sum$time = paste("Total Number of Movements:", object$time)
  cat(unlist(sum), sep = "\n\n")
}

#return dimension of the whole grid
dim.CarGrid =
function(x) {
  x$dimension
}

#gives detailed information about how cars move between two time points
movesummary = function(cargrid, time1, time2) {
  t = sort(c(time1, time2), decreasing = FALSE)
  if (time1 > cargrid$time | time2 > cargrid$time) {
    warning("one or more time points exceed limit, maximum time point used")
    t[t > cargrid$time] = cargrid$time
  }
  if (t[1] == t[2]) {
    warning("same time points, no movement calculated")
    return(cat("Number of Cars Moved: 0", "Number of Cars Blocked: 0", sep = "\n"))
  }
  calc = movecalc(cargrid$origin, t[1], t[2], dim(cargrid))
  cat(calc, sep = "\n")
}

movecalc = function(oridf, t1, t2, griddim) {
  df1 = oridf
  for (i in 1:t1) df1 = gridmove(infodf = df1, t = i, r = griddim[1], j = griddim[2])
  df2 = df1
  for (i in (t1+1):t2) df2 = gridmove(infodf = df2, t = i, r = griddim[1], j = griddim[2])
  nomove = sum(df1$row == df2$row & df1$column == df2$column)
  move = nrow(oridf) - nomove
  return(c(paste("Number of Cars Moved:", move), paste("Number of Cars Blocked:", nomove)))
}




