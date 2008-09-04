\name{getEventProb}
\alias{getEventProb}
\title{ Calculate the probability of a specified set of dice-rolling events }
\description{
For a specified dice-rolling process, \code{getEventProb} calculates the probability of an event (i.e., a non-empty set of outcomes) that is specified by passing a \code{list} object in to \code{eventList}.
}
\usage{
getEventProb(nrolls, ndicePerRoll, nsidesPerDie, eventList, orderMatters = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nrolls}{ A single positive integer representing the number of dice rolls to make }
  \item{ndicePerRoll}{ A single positive integer representing the number of dice to use in each dice roll }
  \item{nsidesPerDie}{ A single positive integer representing the number of sides on each die (\code{getEventProb}'s dice-rolling process involves only one type of die per call) }
  \item{eventList}{ A \code{list} object, each element of which is a vector that constrains a single dice roll in the dice-rolling process (see Details below)}
  \item{orderMatters}{ A logical flag indicating whether the order of the elements of \code{eventList} should constrain the event space; if TRUE, \code{eventList} must specify constraints for every dice roll--i.e., it must contain exactly \code{nrolls} elements (some of which may be "empty" constraints listing all possible outcomes of a dice roll, i.e., a vector from \code{ndicePerRoll} to \code{(ndicePerRoll * nsidesPerDie)}) }
}
\details{
The crux of this function is \code{eventList}, which sets the conditions that acceptable dice-rolls must meet.  E.g., to get the probability of rolling at least one 6 when rolling four six-sided dice, \code{eventList} would be \code{list(6)} and \code{orderMatters} would be FALSE; to get the probability of rolling a 6, followed by a 5, followed by either a 1, 2, or 3 when rolling three six-sided dice, \code{eventList} would be \code{list(6,5,1:3)} and \code{orderMatters} would be TRUE.
}
\value{
A single number representing the probability of an event that meets the constraints of the specified dice-rolling process
}
\author{ Dylan Arena }

\examples{

\dontshow{
stopifnot(all.equal(getEventProb(nrolls=1, ndicePerRoll=1, nsidesPerDie=6, eventList=list(c(4, 5, 6))), .5, tolerance=.0001))
stopifnot(all.equal(getEventProb(nrolls=4, ndicePerRoll=1, nsidesPerDie=6, eventList=list(6, c(5, 6), c(4, 5, 6))), 0.1952160, tolerance=.0001))
}

## Probability of rolling at least one 6 when rolling four six-sided dice

getEventProb(nrolls = 4,
             ndicePerRoll = 1,
             nsidesPerDie = 6,
             eventList = list(6))

## Probability of rolling a 6, followed by a 5, followed by either a 1, 2,
## or 3 when rolling three six-sided dice

getEventProb(nrolls = 3,
             ndicePerRoll = 1,
             nsidesPerDie = 6,
             eventList = list(6, 5, 1:3),
             orderMatters = TRUE)

## Probability of rolling no 10's when rolling two ten-sided dice

getEventProb(nrolls = 2,
             ndicePerRoll = 1,
             nsidesPerDie = 10,
             eventList = list(1:9,1:9))


}
\keyword{ distribution }
