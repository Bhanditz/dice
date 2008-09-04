# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# These helper functions check parameter integrity

.checkIntParam = function(param, paramName, positive)
{
  if ((!missing(positive) && param < (if (positive) 1 else 0)) ||
      (param != floor(param)) ||
      (length(param) > 1))
  {
    if (missing(positive))  
    {
      paste("\n*", paramName, "must contain a single integer instead of", param)
    }
    else if (positive)
    {
      paste("\n*", paramName, "must contain a single positive integer instead of", param)
    }
    else 
    {
      paste("\n*", paramName, "must contain a single non-negative integer instead of", param)
    }
  }
}


.checkLogicalParam = function(param, paramName)
{
  if (length(param) > 1 ||
      !is.logical(param))
  {
    paste("\n* ", paramName, " must contain a single logical value (i.e., TRUE or FALSE)", sep="")
  }
}


# This helper function returns the probabilities of each element of eventList

.getEventListProbs = function(ndicePerRoll, nsidesPerDie, eventList)
{
  probs = getTotalProbs(ndicePerRoll, nsidesPerDie)$probabilities

  # On the assumption that eventList has length nrolls (which is safe since this is a 
  # private helper function), we calculate the probability of getting an acceptable 
  # outcome (a "success") on each of the rolls by iterating through the vector of 
  # successes for that roll and adding the corresponding probability to our tally

  eventListProbs = c()
  for (i in 1:length(eventList)) 
  {
    successesForThisRoll = sort(eventList[[i]])
    successProbForThisRoll = 0
    for (j in 1:length(successesForThisRoll))
    {
      successProbForThisRoll = successProbForThisRoll + probs[(successesForThisRoll[j] - (ndicePerRoll - 1)),2]
    }
    eventListProbs[i] = successProbForThisRoll
  }
  eventListProbs 
}


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# NOTE: the parameters nrolls, ndicePerRoll, nsidesPerDie, and nkept in the function
# signatures below depart slightly from our usual coding conventions (i.e., are not 
# numRolls, numDicePerRoll, numSidesPerDie, and numDiceKept) so that they are easily 
# abbreviated as "nr", "nd", "ns", and "nk", respectively, in function calls

getEventProb = function(nrolls,
                        ndicePerRoll,
                        nsidesPerDie,
                        eventList,
                        orderMatters=FALSE)
{
  errorVector = character()
  errorVector = append(errorVector, .checkIntParam(nrolls, "nrolls", positive=TRUE))
  errorVector = append(errorVector, .checkIntParam(ndicePerRoll, "ndicePerRoll", positive=TRUE))
  errorVector = append(errorVector, .checkIntParam(nsidesPerDie, "nsidesPerDie", positive=TRUE))

  if (length(eventList) > nrolls)
  {
    errorVector = append(errorVector, "\n* The length of eventList must not be greater than nrolls")
  }
  if (orderMatters & length(eventList) != nrolls)
  {
    errorVector = append(errorVector, "\n* If orderMatters is passed as TRUE, the length of eventList must equal\n  nrolls (i.e., there must be an element of eventList for each roll)")
  }
  if (!all(sapply(eventList, is.numeric)))
  {
    errorVector = append(errorVector, "\n* All elements of eventList must be numeric vectors")
  }
  if (!all(as.logical(sapply(sapply(eventList, function(x) x == floor(x)), min))))
  {
    errorVector = append(errorVector, "\n* All numbers in each element of eventList must be positive integers")
  }
  if (min(sapply(eventList, min)) < ndicePerRoll ||
      max(sapply(eventList, max)) > (ndicePerRoll * nsidesPerDie))
  {
    errorVector = append(errorVector, "\n* All numbers in each element of eventList must be between ndicePerRoll\n  and (ndicePerRoll * nsidesPerDie)")
  }
  errorVector = append(errorVector, .checkLogicalParam(orderMatters, "orderMatters"))

  if (length(errorVector) > 0)
  {
    stop(errorVector)
  }

  eventList = lapply(eventList, unique)

  # If eventList doesn't have an element for each roll, we add elements until it does;
  # after this point, each element of eventList will constrain one roll (but some of 
  # those constraints may be simply {min:max} for that roll--i.e., trivial constraints)

  if (length(eventList) < nrolls)
  {
    eventList = lapply(c(eventList, rep(0, nrolls - length(eventList))), function(x){x = (if (max(x) == 0) ndicePerRoll:(ndicePerRoll*nsidesPerDie) else x)})    
  }

  if (orderMatters)
  {
    outcomeProb = prod(.getEventListProbs(ndicePerRoll, nsidesPerDie, eventList))
  }
  else # i.e., if (!orderMatters)
  {
    # We only calculate probabilities if each element of eventList is a length-1 vector
    # (i.e., a single number), e.g., {2, 3, 2}; if any element is longer than that, e.g.,
    # {2, {3, 4}, 2}, we call ourselves recursively on each list we can construct of only
    # length-1 vectors (e.g., in the example above we'd call ourselves on {2, 3, 2} and 
    # {2, 4, 2}); then we sum the resulting probabilities (which, since orderMatters is 
    # FALSE, account for all permutations of each of {2, 3, 2} and {2, 4, 2}) to arrive 
    # at our probability for the original list of {2, {3, 4}, 2}

    listElemLengths = sapply(eventList, length)
    maxListElemLength = max(listElemLengths)
    if (maxListElemLength > 1)
    {
      # Here we populate combMatrix with the elements of eventList to produce a 
      # matrix each row of which is a selection of one element from each element of
      # eventList; e.g., given the eventList {{1, 2}, {1, 2, 4}, 2}, we'd produce
      # a 6 x 3 matrix with rows {1, 1, 2}, {1, 2, 2}, {1, 4, 2}, {2, 1, 2}, {2, 2, 2},
      # and {2, 4, 2}

      combMatrix = matrix(nrow = prod(listElemLengths), ncol = nrolls)
      if (nrolls > 1)
      {
        for (i in 1:(nrolls-1))
        {
          combMatrix[,i] = rep(eventList[[i]], each = prod(listElemLengths[(i+1):nrolls]))
        }
      }
      combMatrix[,nrolls] = rep(eventList[[nrolls]])

      # Next we eliminate all rows that are permutations of other rows (otherwise we
      # would over-count in the calculations that follow)

      if (nrolls > 1)
      {
        combMatrix = unique(t(apply(combMatrix,1,sort)))
      }
      else
      {
        combMatrix = unique(combMatrix)
      }

      # Now we make a recursive call for each row of combMatrix and sum the resulting
      # probabilities to arrive at our probability for the original eventList

      sumOfProbs = sum(apply(combMatrix,
                             1,
                             function(x) getEventProb(nrolls,
                                                      ndicePerRoll,
                                                      nsidesPerDie,
                                                      as.list(x),
                                                      orderMatters)))
      outcomeProb = sumOfProbs
    }
    else
    {

      # If each element of eventList is a length-1 vector, we can convert eventList
      # itself to a vector; then we calculate the probability of getting the specific
      # set of outcomes specified by eventList in any order (reflecting the fact that 
      # orderMatters was passed in as FALSE)
     
      eventListAsVector = sapply(eventList, max)
      eventListProb = prod(.getEventListProbs(ndicePerRoll, nsidesPerDie, eventListAsVector))
      outcomeProb = eventListProb * factorial(nrolls) / prod(factorial(table(eventListAsVector)))
    }
  }

  outcomeProb
}


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


getTotalProbs = function(ndicePerRoll,
                         nsidesPerDie,
                         nkept = ndicePerRoll,
                         totalModifier = 0,
                         perDieModifier = 0,
                         perDieMinOfOne = TRUE)
{

  # We begin with preliminary error-checking

  errorVector = vector(mode = "character", length = 0)
  errorVector = append(errorVector, .checkIntParam(ndicePerRoll, "ndicePerRoll", positive=TRUE))
  errorVector = append(errorVector, .checkIntParam(nsidesPerDie, "nsidesPerDie", positive=TRUE))
  errorVector = append(errorVector, .checkIntParam(nkept, "nkept", positive=TRUE))
  if (nkept > ndicePerRoll)
  {
    errorVector = append(errorVector, "\n* nkept must not be greater than ndicePerRoll")
  }
  errorVector = append(errorVector, .checkIntParam(totalModifier, "totalModifier"))
  errorVector = append(errorVector, .checkIntParam(perDieModifier, "perDieModifier"))
  errorVector = append(errorVector, .checkLogicalParam(perDieMinOfOne, "perDieMinOfOne"))

  if (length(errorVector) > 0)
  {
    stop(errorVector)
  }

  numOutcomes = nsidesPerDie^ndicePerRoll
  numDiceToDrop = ndicePerRoll - nkept
  currNumArrangements = 0
  
  totalModifier = totalModifier + (perDieModifier * nkept)
  
  currentTotal = 0

  vectorOfTotals = as.integer((nkept + totalModifier) :
                                        ((nsidesPerDie * nkept) + totalModifier))

  numPossibleTotals = length(vectorOfTotals)


  # totalTallyMatrix is used to track the number of times we see every possible outcome total,
  # which we will use to produce the probabilities of every total (e.g., for the 3d6 case we 
  # see 10 as a total 27 times, so the probability of a total of 10 is 27/216 = .125, while for
  # the 5d6 drop 2 case we see 13 as a total 1055 times, so the probability of a total of 13
  # is 1055/7776 = .1356739).

  totalTallyMatrix = matrix(data = c(vectorOfTotals,
                                     as.integer(rep.int(0, numPossibleTotals)),
                                     as.integer(rep.int(0, numPossibleTotals))),
                            nrow = numPossibleTotals,
                            ncol = 3,
                            dimnames = list(NULL,
                                            c("Total",
                                              "Probability",
                                              "Ways to Roll")))

  # c is the lowest die roll value that will be kept (i.e., it is the die roll "cut-off" value:
  # e.g., in the 5d6 drop 2 case, if our sorted die rolls are {2 4 4 5 6}, c is 4).  We'll call
  # all dice with this value the "c" dice.

  for (c in 1 : nsidesPerDie)
  {

    # d is the number of dice whose values are less than c (e.g., in the 5d6 drop 2 case, if we
    # roll {1 3 4 5 6}, c is 4, so d is 2).  We'll call these dice the "l" dice (since they're 
    # "lower" than c).  NOTE: the letter d represents the number of dice whose values are less
    # than c, and the letter l is our _name_ for this group of dice--l is *not* a number of
    # dice.  NOTE: we have the embedded if clause in our for loop declaration because if c is
    # 1, there *cannot* be any dice whose values are lower than c, and hence d can only be 0.
    # The following loop syntax might look suspicious, but we *do* want to iterate once in the 
    # c == 1 case (in which case there are no dice with lower values, so d must be 0).

    for (d in 0 : (if (c == 1) 0 else numDiceToDrop))
    {

      # k is the number of c's that will be kept (e.g., in the 5d6 drop 2 case, if we roll
      # {2 4 4 5 6}, k is 1, because one of the two 4's will be kept).
      # Now, since we're discarding numDiceToDrop dice (including the d l's), we'll discard 
      # (numDiceToDrop - d) of the c's and keep k of them, and thus the total number of c's is 
      # (k + numDiceToDrop - d).  
      # NOTE: hence, the number of dice whose values exceed c is (nkept - k).  We will
      # call these higher dice the "h" dice (since they're "higher" than c).  As noted above,
      # the letter h is our _name_ for this group of dice--h is *not* number of dice.

      for (k in 1 : nkept)
      {

        # By this part of the function, we've specified a class of outcomes identified by their 
        # (c, d, k) values--i.e., every outcome in this class has the following properties...
        # 1). the die roll cut-off value c; 
        # 2). d "l" dice, whose values are lower than c (all of which will be dropped); and
        # 3). k "c" dice that will be kept.  Furthermore, each such outcome has
        # 4). (k + numDiceToDrop - d) "c" dice in total, and 
        # 5). (nkept - k) "h" dice, whose values are higher than c.
        # Now, we're interested in totals for the various outcomes in this class, and these 
        # totals don't depend upon the order in which the various values appear in our sequence
        # of rolls; i.e., multiple outcomes in this class will have the same total but have the
        # l's, the c's, and the h's appear at different places in the sequence of rolls.  To 
        # account for this, we need to multiply each distinct outcome by the number of times
        # outcomes identical to it but for the order of appearance of the l's, c's, and h's
        # (NOTE: we do not account here for the orders *within* these groups--we account for the
        # order within the l's immediately below, and we account for the order within the h's
        # in the section of the code in which we enumerate the h's).  For now, we will define a
        # term by which to multiply each total we find; the term is a result of the multinomial
        # theorem's combinatoric interpretation as the number of ways to put n distinct objects
        # (in this case, our die rolls) into 3 bins of size d, (k + numDiceToDrop - d), and 
        # (nkept - k), corresponding to the number of l's, c's, and h's in this class:

        numCs = (k + numDiceToDrop - d)
        numHs = (nkept - k)
        
        # NOTE: this formula could overflow if ndicePerRoll gets large, but I think the function
        # would keel over before reaching that point anyway; if not, consider lfactorial() 
        numArrangementsOfDice = (factorial(ndicePerRoll) / 
                                 (factorial(d) * factorial(numCs) * factorial(numHs)))

        # Next: in this loop the value of c is fixed, but there are many "l" dice values that
        # outcomes in this class might have; since we don't care about these values, we need to 
        # increase our multiplicity term to account for the outcomes that will be identical 
        # to this iteration's distinct outcome but for the values (and order) of the l's:

        numArrangementsOfDice = numArrangementsOfDice * (c - 1)^d

        # Now that we've accounted for sorting the values into three bins and for all the 
        # possible l values that are immaterial to our calculations, we can treat our outcome 
        # class as sorted into groups and can focus our attention on the k c's we keep and the 
        # h's.  The k c's will contribute a known amount to our total for all outcomes in this 
        # class (viz., k * c); but the h's will contribute various amounts, depending on their
        # values.  We next turn to determining the possible distinct outcomes for this class by
        # enumerating the possible values for the h's.  We will proceed as follows:
        # rangeOfHs is the distance between the smallest and largest possible h values for this
        # class of outcomes, and we use it to determine the number of distinct outcomes for this
        # class, which is given by rangeOfHs^numHs.  We create an outcomeMatrix with as many
        # rows as there are distinct outcomes for this class and nkept columns: each element
        # in a row corresponds to a die roll value, and the sum of the row elements is the
        # total for that distinct outcome.  We populate outcomeMatrix in such a way that all
        # possible values for the h's in this class (and hence all distinct outcomes) are
        # accounted for.  We then calculate the number of permutations of each distinct outcome
        # (e.g., in the 3d6 case, the outcome {1, 1, 2} has three permutations) and use this
        # information to calculate the probability of every possible outcome.

        rangeOfHs = nsidesPerDie - c

        if (k == nkept)
        {
          currentTotal = (k * c) + totalModifier
          totalTallyMatrix[currentTotal - totalModifier - (nkept - 1), 2] = totalTallyMatrix[currentTotal - totalModifier - (nkept - 1), 2] + numArrangementsOfDice
        }
        else
        {
          outcomeMatrix = matrix(nrow = choose((rangeOfHs + numHs - 1), numHs), ncol = nkept)

          if (dim(outcomeMatrix)[1] > 0)
          {
            outcomeMatrix[,1:k] = c

            hCombs = combinations(n = rangeOfHs,
                                  r = numHs,
                                  v = ((c+1) : nsidesPerDie),
                                  repeats.allowed = TRUE)
            hPermCounts = apply(hCombs, 1, function(x) factorial(numHs)/prod(factorial(table(x))))

            outcomeMatrix[,((k+1) : nkept)] = hCombs

            for (j in 1 : dim(outcomeMatrix)[1])
            {
              currentTotal = sum(outcomeMatrix[j,]) + totalModifier
              currNumArrangements = numArrangementsOfDice * hPermCounts[j]
              totalTallyMatrix[currentTotal - totalModifier - (nkept - 1), 2] = totalTallyMatrix[currentTotal - totalModifier - (nkept - 1), 2] + currNumArrangements
            }
          }
        }
      }
    }
  }

  if (perDieMinOfOne)
  {
    if (totalTallyMatrix[numPossibleTotals,1] <= nkept)
    {
      totalTallyMatrix = matrix(data = c(nkept, numOutcomes, numOutcomes),
                                  nrow = 1,
                                  ncol = 3,
                                  dimnames = list(NULL,
                                                  c("  Total  ","  Probability  ", "  Ways to Roll  ")))
    }
    else
    {
      extraWaysToRollMin = sum(totalTallyMatrix[totalTallyMatrix[,1] < nkept,2])
      totalTallyMatrix = totalTallyMatrix[totalTallyMatrix[,1] >= nkept,]
      totalTallyMatrix[1,2] = totalTallyMatrix[1,2] + extraWaysToRollMin
    }
  }

  totalTallyMatrix[,3] = totalTallyMatrix[,2]
  totalTallyMatrix[,2] = totalTallyMatrix[,2] / numOutcomes

  overallAverageTotal = sum(totalTallyMatrix[,1] * totalTallyMatrix[,3] / numOutcomes)

  
  list(probabilities = totalTallyMatrix, average = overallAverageTotal)
}
