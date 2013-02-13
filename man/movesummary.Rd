\name{movesummary}
\alias{movesummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Summary for Cars Movements between Two Time Points
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function gives a brief summary of how cars have moved between two given time points for a CarGrid object.
}
\usage{
movesummary(cargrid, time1, time2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cargrid}{
%%     ~~Describe \code{cargrid} here~~
A CarGrid object.
  }
  \item{time1}{
Numeric. First time point.  
}
  \item{time2}{
Numeric. Second time point.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A print out of moved and blocked car counts will be printed out.
}
\author{
%%  ~~who you are~~
Rick Wang
}
\note{
%%  ~~further notes~~
If the difference between two time points is 1, cars of the color that should not move will also be counted as blocked. If two time points are the same, both moved and blocked counts will be 0. If any time point exceeds the maximum, counts only are considered up to the maximum.
}


\examples{
#create a CarGrid object
foo = gridsim(100, 100, 0.2, 200)

#movement between time points 10 and 20
movesummary(foo, 10, 20)

#order of time points does not matter, this will return same result
movesummary(foo, 20, 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
