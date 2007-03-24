\name{getOutcomeProb}
\alias{getOutcomeProb}
\title{ Calculate the Probability of a Specified Set of Outcomes of a Dice-Roll }
\description{
For a specified dice-rolling process, \code{getOutcomeProb} calculates the probability of an outcome (actually, in technical terms, an event consisting of a non-empty set of outcomes) that is specified by passing a \code{list} object in to \code{outcomeList}.
}
\usage{
getOutcomeProb(ndice, nsides, outcomeList, orderMatters = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ndice}{ A single positive integer representing the number of dice to roll }
  \item{nsides}{ A single positive integer representing the number of sides on each die (\code{getOutcomeProb}'s dice-rolling process involves only one type of die per call) }
  \item{outcomeList}{ A \code{list} object, each element of which is a vector that constrains a single die in the dice-roll (see Details below)}
  \item{orderMatters}{ A logical flag indicating whether the order of the elements of \code{outcomeList} should constrain the outcome space; if TRUE, \code{outcomeList} must specify constraints for every die roll--i.e., it must contain exactly \code{ndice} elements (some of which may be "empty" constraints of the form \code{1:n}, where \code{n} is the same value passed in to \code{nsides}) }
}
\details{
The crux of this function is \code{outcomeList}, which sets the conditions that acceptable dice-rolls must meet.  E.g., to get the probability of rolling at least one 6 when rolling four six-sided dice, \code{outcomeList} would be \code{list(6)} and \code{orderMatters} would be FALSE; to get the probability of rolling a 6, followed by a 5, followed by either a 1, 2, or 3 when rolling three six-sided dice, \code{outcomeList} would be \code{list(6,5,1:3)} and \code{orderMatters} would be TRUE.
}
\value{
A single number representing the probability of getting an outcome that meets the constraints of the specified dice-rolling process
}
\author{ Dylan Arena }

\examples{

\dontshow{
stopifnot(all.equal(getOutcomeProb(4, 6, list(6, c(5, 6), c(4, 5, 6))), 0.1952160, tolerance=.0001))
}

## Probability of rolling at least one 6 when rolling four six-sided dice

getOutcomeProb(ndice = 4,
               nsides = 6,
               outcomeList = list(6))

## Probability of rolling a 6, followed by a 5, followed by either a 1, 2,
## or 3 when rolling three six-sided dice

getOutcomeProb(ndice = 3,
               nsides = 6,
               outcomeList = list(6, 5, 1:3),
               orderMatters = TRUE)

## Probability of rolling no 10's when rolling two ten-sided dice

getOutcomeProb(ndice = 2,
               nsides = 10,
               outcomeList = list(1:9,1:9))


}
\keyword{ distribution }
