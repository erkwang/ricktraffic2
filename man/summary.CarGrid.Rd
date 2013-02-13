\name{summary.CarGrid}
\alias{summary.CarGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Method for Summarizing a CarGrid object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Provide summary for a CarGrid object
}
\usage{
summary.CarGrid(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{cargrid} here~~
A CarGrid object.
}
  \item{\dots}{
  Other argument to be passed.
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
This function will print out a summary of the given CarGrid object with details such as dimensions of the grid, car density, car numbers and movement times.
}

\author{
%%  ~~who you are~~
Rick Wang
}
\note{
%%  ~~further notes~~
If interested in cars behavior between two time points, see function movesummary().
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#create a CarGrid object
foo = gridsim(100, 100, 0.2, 200)

#summary
summary(foo)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
