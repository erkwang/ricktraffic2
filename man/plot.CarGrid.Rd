\name{plot.CarGrid}
\alias{plot.CarGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plotting Method for CarGrid Objects
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This is the function for plotting a CarGrid object. The result will be a grid with red and blue cells representing cars.
}
\usage{
plot.CarGrid(x, original = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{gridobj} here~~
A CarGrid object.
}
  \item{original}{
%%     ~~Describe \code{\dots} here~~
Logical. The default is FALSE for plotting the final state of the grid. If it is TRUE, the original state plot will be produced.
}
  \item{\dots}{
Other graphical parameters to be passed. See details at plot().  
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
This function will create a plot representing the state of a given BML grid.
}

\author{
%%  ~~who you are~~
Rick Wang
}
\examples{
#creating a CarGrid object
foo = gridsim(100, 100, 0.1, 200)

#plot the final state
plot(foo)

#plot the original grid
plot(foo, original = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
