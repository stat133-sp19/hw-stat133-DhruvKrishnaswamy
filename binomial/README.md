
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#Overview

------------------------------------

Binomial is a package that allows the enduser to manipulate and moodify binomial distributions with a prefered probability of success and input of the number of trials. This package also enables endusers to compute various statistics of distributions the such as mode, variance, skewness etc. 

##Installation

------------------------------------

Install the package from Github using "devtools":

```{r, eval=FALSE}
#  install.packages("devtools") 
# install "binomial" (with vignettes)
#devtools::install_github("https://github.com/stat133-sp19/hw-stat133-DhruvKrishnaswamy/tree/master/binomial", build_vignettes = TRUE)
```

##Using this package

```{r, eval=FALSE}
# create a binomial variable
varian <- bin_variable(5, 0.5)
varian
#> "Binomial Variable"
#>
#>
#> Parameters
#> -number of trials: 5 
#> -prob of success: 0.5 
# calculate bin probability
prob <- bin_probability(3, 5, 0.5)
prob
#> 0.3125
# calculate n choose k
cho <- bin_choose(5,3)
cho
#> 10
# return the distribution
dis <- bin_distribution(5, 0.5)
dis
# return the mean of the binomial distribution
bin_mean(5, 0.5)
#> 2.5
# return the variance of the binomial distribution
bin_variance(5, 0.5)
#> 1.25
# return the mode of the binomial distribution
bin_mode(5, 0.5)
#> 3
# return the skewness of the binomial distribution
bin_skewness(5, 0.5)
#> 0
# return the kurtosis of the binomial distribution
bin_kurtosis(5, 0.5)
#> -0.4
```

