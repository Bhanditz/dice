\name{dice-package}
\alias{dice-package}
\alias{dice}
\docType{package}
\title{
Calculate probabilities of various dice-rolling events
}
\description{
This package provides utilities to calculate the probabilities of various dice-rolling events, such as the probability of rolling a four-sided die six times and getting a 4, a 3, and either a 1 or 2 among the six rolls (in any order); the probability of rolling two six-sided dice three times and getting a 10 on the first roll, followed by a 4 on the second roll, followed by anything but a 7 on the third roll; or the probabilities of each possible total of rolling five six-sided dice, dropping the lowest two rolls, and summing the remaining dice.
}
\details{
\tabular{ll}{
Package: \tab dice\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2008-09-04\cr
License: \tab GPL (>= 2)\cr
}
Although initially conceived as a utility for role-playing game calculations, functions in the \code{dice} package can be used to answer questions in any dice-rolling context (e.g., calculating probabilities for the game of craps, solving problems for an introductory probability course, etc.)

The \code{dice} package requires the \code{gtools} package.

For a complete list of functions, use \code{library(help="dice")}.
}
\author{
Dylan Arena <dylanarena1@gmail.com>
}
\references{
The implementation for the \link{getTotalProbs} function originated with the ideas
presented in the following forum thread:

\url{http://www.enworld.org/showthread.php?t=56352&page=1&pp=40}
}
\keyword{ package }
\examples{
getEventProb(nrolls = 6,
             ndicePerRoll = 1,
             nsidesPerDie = 4,
             eventList = list(4, 3, c(1,2)),
             orderMatters = FALSE)

getEventProb(nrolls = 3,
             ndicePerRoll = 2,
             nsidesPerDie = 6,
             eventList = list(10, 4, c(2:6, 8:12)),
             orderMatters = TRUE)

getTotalProbs(ndicePerRoll = 5,
              nsidesPerDie = 6,
              nkept = 3)
}
