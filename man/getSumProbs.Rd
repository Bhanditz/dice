\name{getSumProbs}
\alias{getSumProbs}
\title{ Calculate the probabilities of all possible outcome sums of a dice roll }
\description{
For a specified number of dice with a specified number of sides per die (and dropping a specified number of dice--those with either the lowest or highest values), \code{getSumProbs} calculates the probabilities of all possible outcome sums (i.e., all possible sums of those dice whose results are not dropped); the function also accommodates modifiers (either to each die roll or to the sum), such as rolling five four-sided dice and adding 1 to the outcome of each roll, or rolling one twenty-sided die and adding 12 to the outcome.  (Such modified rolls frequently occur in the context of role-playing games, e.g., Dungeons & Dragons, Mutants & Masterminds, or BESM.)
}
\usage{
getSumProbs(ndicePerRoll, 
nsidesPerDie, 
nkept = ndicePerRoll, 
dropLowest = TRUE,
sumModifier = 0,
perDieModifier = 0,
perDieMinOfOne = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ndicePerRoll}{ A single positive integer representing the number of dice to roll }
  \item{nsidesPerDie}{ A single positive integer representing the number of sides on each die (\code{getSumProbs}'s dice-rolling process involves only one type of die per call) }
  \item{nkept}{ A single positive integer representing the number of dice whose values to include when calculating the sum (the dice to be kept will always be those with the \bold{highest} values) }
  \item{dropLowest}{ A single logical indicating whether to drop the lowest outcome values (FALSE drops the highest values instead) }
  \item{sumModifier}{ A single integer representing an amount to add to or subtract from the outcome sum }
  \item{perDieModifier}{ A single integer representing an amount to add to or subtract from each die roll }
  \item{perDieMinOfOne}{ A logical flag indicating whether each die roll should be considered to have a minimum value of 1 (as is often true in role-playing-game contexts) }
}
\value{
  \item{probabilities }{A matrix with a row for each possible outcome sum and three columns: one that lists each sum, one for the probability of that sum, and one for the number of ways to roll that sum}
  \item{average }{A single number representing the expected value of the specified dice-rolling process}
}
\references{ 
This function's implementation originated with the ideas presented in the following forum thread: 

\url{http://www.enworld.org/showthread.php?t=56352&page=1&pp=40}
}
\author{ Dylan Arena }
\examples{

\dontshow{
stopifnot(all.equal(target=getSumProbs(ndicePerRoll=5, nsidesPerDie=6, nkept=3)$probabilities[11,2], current=0.1356739, check.attributes=FALSE, tolerance=.000001))
stopifnot(all.equal(target=getSumProbs(ndicePerRoll=2, nsidesPerDie=20, nkept=1, dropLowest=FALSE)$probabilities[1,2], current=0.0975, check.attributes=FALSE, tollerance=.000001))
}

## Rolling four six-sided dice and keeping the three highest die rolls

getSumProbs(ndicePerRoll = 4,
            nsidesPerDie = 6,
            nkept = 3)

## Rolling five four-sided dice and adding 1 to each die roll

getSumProbs(ndicePerRoll = 5,
            nsidesPerDie = 4,
            perDieModifier = 1)

## Rolling one twenty-sided die and adding 12 to the result

getSumProbs(ndicePerRoll = 1,
            nsidesPerDie = 20,
            sumModifier = 12)

}
\keyword{ distribution }
