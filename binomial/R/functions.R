#Description: This is the private function check_prob() which
#tests if an input value is a probability
#Input
#prob: This is a numeric value
#Output
#Return TRUE if it is a valid probability, else return an error if invalid
check_prob <- function(prob){
  if(!is.numeric(prob))
    {
    stop("invalid probability value")
  }
  if(prob < 0 | prob > 1)
    {
    stop("p has to be a number between 0 and 1")
  }
  else
    {
    return(TRUE)
  }
}


#Description: This is the private auxiliary
#function check_trials() which tests if the number of trials is a valid value
#Input
#trials: number of trials must be a non-negative numeric value
#Output
#  Return TRUE if valid, return error if invalid
check_trials <- function(trials)
  {
  if(!is.numeric(trials))
    {
    stop("invalid trials value")
  }

  if(trials < 0 | floor(trials) != trials)
    {
    stop("The number of trials has to be a positive integer")
  }
  else{
    return(TRUE)
  }
}


#Description: a private auxiliary function
#check_success() which tests if an input is a valid number of successes
#Input
#success: number of successes, a vector of nonnegative integers
#Output
#Return TRUE if valid, return error if invalid
check_success <- function(success,trials)
  {
  if(!is.numeric(success))
    {
    stop("invalid success value")
  }
  if(any(success < 0) | any(floor(success)!= success) )
    {
    stop("success must be a vector of non-negative values")
  }
  if(any(success > trials))
    {
    stop("success cannot be greater than trials")
  }
  return(TRUE)
}


#1.2) Private Auxiliary Functions

#Description: a private auxiliary function aux_mean() which returns the
#mean of the binomial experiment
#Input
#trials:The number of trials
#prob: a numeric probability value
#Output
# This computes the mean value of the binomial experiment

aux_mean <- function(trials, prob)
  {
  meanval <- trials * prob
  return(meanval)
}


#Description: a private auxiliary function
#aux_variance() which returns the variance of the experiment
#Input
# trials: The number of trials
# probs: probability,numeric value
#Output
# Returns the computed variance value of the binomial experiment
aux_variance <- function(trials,probs)
  {
  var <- (trials*probs*(1-probs))^.5
  return(var)
}


#Description: This is a private auxiliary function aux_mode()
#which returns the mode of the experiment
#Input
#trials: This is the number of trials
#prob: This is a numeric value of a probability
#Output
#Returns the mode
aux_mode = function(trials, prob)
  {
  v1 = (trials * prob + prob)
  if (as.integer(v1) == v1)

    {
    return (c(v1, v1-1))
  }
  else
    {

    return(floor(v1))
  }
}


#Description: a private auxiliary function
#aux_skewness() which returns the skewness of a probability distribution
#Input
#trials:This is teh number of trials
#probs: This is a probability value
#Output
#Returns the skewness of the binomial experiment
aux_skewness <- function(trials, probs)
  {
  skewn <- (1-2*probs)/(trials*probs*(1-probs))^.5
  return(skewn)
}


#Description: a private auxiliary function aux_kurtosis() which returns a measure of the 'tailedness'
#of the probability distribution of a random variable
#Input
#trials: This is the number of trials
#probs: This is a probability value
#Output
# Returns the computed kurtosis value of the binomial experiment
aux_kurtosis <- function(trials, probs)
  {
  kurto <- (1-6*probs*(1-probs))/(trials*probs*(1-probs))
  return(kurto)
}

#1.3) Function bin_choose()

#' @title binomial choose factor
#' @description This computes the number of ways in which k successes could occur in n trials
#' @param n :number of trials
#' @param k :number of successes
#' @return the choose factor
#' @export
#' @examples
#' #5 choose 2
#' bin_choose(n = 5, k = 2)
#'
#' #5 choose 0
#' bin_choose(5, 0)
#'
#' #5 choose 1:3
#' bin_choose(5, 1:3)
#'

bin_choose <- function(n,k)
  {
  if (any(k > n))
    {
    stop("k cannot be greater than n")
  }
  else
    {
    val = (factorial(n))/(factorial(k)*factorial(n-k))
    return(val)
  }
}

#1.4) Function bin_probability()

#' @title binomial probability
#' @description This computes the probability of getting the desired successes from a given number of trials
#' @param success : Number of successes
#' @param trials : This is the number of trials
#' @param probs : This is the probability of success in a single tried
#' @return The calculated binomial probability
#' @export
#' @examples
#' #probability of getting 2 successes in 5 trials (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, probs = 0.5)
#'
#' #probability of getting 0 to 2 successes in 5 trials (assuming prob of success = 0.5)
#' bin_probability(0:2, trials = 5, probs = 0.5)
#'
#' #probability of getting 55 successes in 100 trials (assuming prob of success = 0.45)
#' bin_probability(55, trials = 100, probs = 0.45)
#'
bin_probability <- function(success, trials, probs)
  {
  if(check_trials(trials) != TRUE)
    {
    stop("invalid trials value")
  }
  if(check_prob(probs) != TRUE)
    {
    stop("invalid probability value")
  }
  if(any(check_success(success,trials)!= TRUE))
    {
    stop("invalid success value")
  }
  else
    {
    probab <- bin_choose(n=trials,k=success)*(probs^success)*(1-probs)^(trials-success)
    return(probab)
  }
}



#1.5) Function bin_distribution()
#' @title binomial distribution
#' @description This calculates the probability of successes
#' @param trials : The number of trials for the experiment
#' @param prob : This is a probability value
#' @return a data frame of possible successes and associated probability values
#' @export
#' @examples
#' #Builds a binomial distribution
#' dis1 <- bin_distribution(trials = 5, probs = 0.5)
#' #plot it
#' plot(dis1)
bin_distribution <- function(trials, probs)
  {
  successes <- 0:trials
  probab <- bin_probability(success, trials, probs)
  pro <- data.frame(success = success, probability = probab)
  class(pro) <- c("bindis", "data.frame")
  return(pro)
}

#' @export
plot.bindis <- function(y)
  {
  barplot(y$probability,width = 0.5,
  xlab = "successes", ylab = "probability", names.arg = c(0:max(x$success)))
}



#1.6) Function bin_cumulative()

#' @title binomial cumulative distribution
#' @description This calculates the cumulative probability of getting the successes
#' @param trials : This is the number of trials
#' @param probs :This computes the probability of success
#' @return data frame representation of potential successes and related (cumulative) probability
#' @export
#' @examples
#' #build a binomial distribution
#' dis2 <- bin_cumulative(trials = 5, probs = 0.5)
#'#You can plot it through the code below
#'#plot(dis2)
bin_cumulative <- function(trials,probs)
  {
  successes <- 0:trials
  probab <- bin_probability(successes,trials,probs)
  cumu <- cumsum(probab)
  a <- data.frame(success=successes,probability=probab,cumulative=cumu)
  class(a) <- c("bincum","data.frame")
  return(a)
}

#' @export
plot.bincum <- function(a)
  {
  plot(a$success,a$cumulative,type = "b",xlab = "success", ylab = "cumulative probability")
}

#1.7) Function bin_variable()
#' @title binomial random variable
#' @description This verifies if the trials and the probability are valid
#' and returns an object of class "binvar"
#' @param trials : This is the number of trials
#' @param probs : This is the probability of success
#' @return an object of class "binvar"
#' @export
#' @examples
#' #build a binomial distribution
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' #summarizes it
#' binsum1 <- summary(bin1)
#' #print the summary
#' binsum1
bin_variable <- function(trials,probs)
  {
  if(check_trials(trials) != TRUE)
    {
    stop("invalid trials value")
  }
  if(check_prob(probs) != TRUE)
    {
    stop("invalid probability value")
  }
  x <- list("number of trials" = trials, "probability of success" = probs)
  class(x) <- "binvar"
  return(x)
}

#' @export

print.binvar <- function(binvar)
  {
  cat('"Binomial Variable"\n\n')
  cat('Parameters\n')
  cat('-number of trials:', binvar$trials, '\n')
  cat('-prob of success:', binvar$probability, '\n')
}

#' @export
summary.binvar <- function(binvar)
  {
  trials <- binvar$trials
  probs <- binvar$probability
  mean <- aux_mean(trials, probs)
  vari <- aux_variance(trials, probs)
  mode <- aux_mode(trials, probs)
  skew <- aux_skewness(trials, probs)
  kurt <- aux_kurtosis(trials, probs)
  stru <- structure(list('trials' = trials, 'prob' = probs,
                           'mean' = mean, 'variance' = vari, 'mode' = mode,
                           'skewness' = skew, 'kurtosis' = kurt), class = 'summary.binvar')
  return(stru)
}





#' @export
print.summary.binvar <- function(summary.binvar)
  {
  cat('"Summary Binomial"\n\n')
  cat('Parameters\n')
  cat('-number of trials:', summary.binvar$trials, '\n')
  cat('-prob of success:', summary.binvar$probs, '\n\n')
  cat('Measures\n')
  cat('-mean    :', summary.binvar$mean, '\n')
  cat('-variance:', summary.binvar$variance, '\n')
  cat('-mode    :', summary.binvar$mode, '\n')
  cat('-skewness:', summary.binvar$skewness, '\n')
  cat('-kurtosis:', summary.binvar$kurtosis, '\n')
}




#1.8) Functions of measures

#' @title binomial mean
#' @description This calculates the mean of the binomial distribution
#' @param trials This is the number of trials
#' @param probs This is the probability of success
#' @return The mean is returned
#' @export
#' @examples
#' bin_mean(trials=5,probs=0.5)
bin_mean <- function(trials, probs)
  {
  check_trials(trials)
  check_prob(probs)
  return(aux_mean(trials, probs))
}



#' @title binomial variance
#' @description This calculates the variance of the binomial experiment
#' @param trials This is the number of trials
#' @param probs This is the probability of success
#' @return The variance is returned
#' @export
#' @examples
#' bin_variance(trials=5,prob=0.5)
bin_variance <- function(trials,probs)
  {
  if(check_trials(trials) != TRUE)
    {
    stop("Not q valid number of trials")
  }
  if(check_prob(probs)!= TRUE)
    {
    stop("Not a valid probability")
  }
  varian <- aux_variance(trials,probs)
  return(varian)
}



#' @title binomial mode
#' @description This calculates the mode of the binomial distribution
#' @param trials This is the number of trials
#' @param probs This is the probability of success
#' @return Mode of the distribution
#' @export
#' @examples
#' bin_mode(trials=5,probs=0.5)
bin_mode <- function(trials, probs)
  {
  check_trials(trials)
  check_prob(probs)
  return(aux_mode(trials, probs))
}


#' @title binomial skewness
#' @description This calculates the skewness of the given binomial experiment
#' @param trials This is the number of trials
#' @param probs This is the probability of success
#' @return skewness of the binomial distribution
#' @export
#' @examples
#' bin_skewness(trials=5,probs=0.5)
bin_skewness <- function(trials,probs)
  {
  if(check_trials(trials)!= TRUE)
    {
    stop("Not a valid trial value")
  }
  if(check_prob(probs) != TRUE)
{
    stop("Not a valid probability value")
  }
  skew <- aux_skewness(trials,probs)
  return(skew)
}

#' @title binomial kurtosis
#' @description This calculates the kurtosis of the binomial experiment
#' @param trials The number of trials
#' @param probs The probability of success
#' @return kurtosis value
#' @export
#' @examples
#' bin_kurtosis(trials=5,probs=0.5)
#'
bin_kurtosis <- function(trials,probs)
  {
  if(check_trials(trials)!= TRUE)
    {
    stop("invalid trials val")
  }
  if(check_prob(probs)!= TRUE)
    {
    stop("invalid probability val")
  }
  ku <- aux_kurtosis(trials,probs)
  return(ku)
}
