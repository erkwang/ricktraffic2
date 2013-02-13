#generate the randomly seeded matrix with red and blue cars
gridseed = function(r, j, p) {
  level = c("Red", "Blue", "Empty")
  totalnum = r * j
  if (length(p) == 1 && 2*p <= 1) {
    field = sample(x = level, size = totalnum, replace = TRUE, prob = c(p, p, 1-2*p)) 
  }
  else if (length(p) == 2 && sum(p) <= 1) {
    field = sample(x = level, size = totalnum, replace = TRUE, prob = c(p, 1-sum(p)))
  }
  else if (length(p) == 3 && sum(p) == 1) {
    field = sample(x = level, totalnum, replace = TRUE, prob = p)
  }
  else {
    stop("invalid probability")
  }
  field = dataframer(field, r, j)
  field
}

#place information in the matrix into a data frame
dataframer = function(fd, rownum, colnum) {
  row = rep(1:rownum, colnum)
  column = rep(1:colnum, each = rownum)
  df = data.frame(row, column, occupy = fd)
  df = df[(df[,3] != "Empty"), ]
  df
}


#move the "cars" according to given step length and direction
mover = function(infodf, stepup = 1, stepright = 1, move, r, j) {
  oldpos = infodf
  newpos = oldpos
  if (move == "Blue") {
    newpos$row[infodf$occupy == "Blue"] = newpos$row[infodf$occupy == "Blue"] - stepup
    newpos = edgecheck(newpos, r, j)
    newpos = poscheck(new = newpos, old = oldpos, check = "Red")  
  }
  else if (move == "Red") {
    newpos$column[infodf$occupy == "Red"] = newpos$column[infodf$occupy == "Red"] + stepright
    newpos = edgecheck(newpos, r, j)
    newpos = poscheck(new = newpos, old = oldpos, check = "Blue")  
  }
  newpos
}

#check if any car is out of edge after the movement
edgecheck = function(new, maxrow, maxcol) {
  new$row[new$row < 1] = maxrow - (0 - new$row[new$row < 1])
  new$column[new$column > maxcol] = new$column[new$column > maxcol] - maxcol
  new
}

#check if there is any conflict in positions
poscheck = function(new, old, check) {
	nr = new$row[new$occupy != check]
	nc = new$column[new$occupy != check]
	cr = new$row[new$occupy == check]
	cc = new$column[new$occupy == check]
	or = old$row[old$occupy != check]
	oc = old$column[old$occupy != check]
	out = .Call("poscheck", nr, nc, cr, cc, or, oc)
	new$row[new$occupy != check] = out$row
	new$column[new$occupy != check] = out$column
	new
}

#execute move based on time
gridmove = function(infodf, t, r, j) {
  if (t == 0) {return(infodf)}
  if ((t/2 - floor(t/2)) * 2) {
    newpos = mover(infodf, move = "Blue", r = r, j = j)
  }
  else {
    newpos = mover(infodf, move = "Red", r = r, j = j)
  }
  newpos
}

#given the dimension, density and time, create the final grid
gridsim = function(nrow, ncol, density, time) {
  griddf = gridseed(r = nrow, j = ncol, p = density)
  origin = griddf
  for (i in 1:time) griddf = gridmove(infodf = griddf, t = i, r = nrow, j = ncol)
  cargrid = list(griddf = griddf, dimension = c(nrow, ncol), 
                 density = density, origin = origin, time = time)
  class(cargrid) = "CarGrid"
  cargrid
}