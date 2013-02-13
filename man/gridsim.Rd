\name{gridsim}
\alias{gridsim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function for BML Grid Simulation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This is a function for creating a BML grid with red and blue cars for given density then simulating the movements of cars on the grid during user defined time period. The function will return an object in "CarGrid" S3 class.
}
\usage{
gridsim(nrow, ncol, density, time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nrow}{
%%     ~~Describe \code{nrow} here~~
Numeric. Define how many rows the grid has.
}
  \item{ncol}{
%%     ~~Describe \code{ncol} here~~
Numeric. Define how many columns the grid has
}
  \item{density}{
%%     ~~Describe \code{density} here~~
Numeric. If it has only one number (<0.5), it will be the density for both red and blue cars equally. If it has two numbers whose sum is less than 1, the first number will be used as density for red cars, second will be density for blue cars. If it has three numbers whose sum is 1, they will be used as density for red, blue cars and empty cells.
}
  \item{time}{
%%     ~~Describe \code{time} here~~
Numeric. Define the total number of movements cars on the grid are executed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
If the given density is not among the three conditions, the function will not be executed instead an error message should prompt.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
An object with S3 class "CarGrid" will be returned as result of the function. The object is a list containing:
\item{griddf}{Data frame reflecting the final state of the grid}
\item{origin}{Data frame for the original state of the grid}
\item{dimension}{Dimensions of the grid}
\item{Density}{Density of elements on the grid}
\item{time}{Total number of movements}
}
\author{
%%  ~~who you are~~
Rick Wang
}
\examples{
#define a 100 by 100 grid with red and blue cars density at 0.2 each
#let the cars move 1000 times
foo = gridsim(nrow = 100, ncol = 100, density = 0.2, time = 1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
