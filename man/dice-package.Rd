\name{dice-package}
\alias{dice-package}
\alias{dice}
\docType{package}
\title{
Calculate Probabilities of Various Dice-Rolling Events
}
\description{
This package provides utilities to calculate the probabilities of various dice-rolling events, such as the probability of rolling six four-sided dice and getting a 4, a 3, and either a 1 or 2 among the six rolls (in any order) or the probabilities of each possible total of rolling five six-sided dice, dropping the lowest two rolls, and summing the remaining dice.
}
\details{
\tabular{ll}{
Package: \tab dice\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-03-24\cr
License: \tab GPL version 2 or newer\cr
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
getOutcomeProb(ndice = 6,
               nsides = 4,
               outcomeList = list(4, 3, c(1,2)))


getTotalProbs(ndice = 5,
              nsides = 6,
              nkept = 3)
}
