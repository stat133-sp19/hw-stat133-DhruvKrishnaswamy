Data-Dictionary
================

The following .csv files were read in:

``` r
data1 <- 'https://raw.githubusercontent.com/ucb-stat133/stat133-hws/master/data/andre-iguodala.csv'
download.file(data1, destfile ='andre-iguodala.csv' )
andre <- read.csv('andre-iguodala.csv', stringsAsFactors = FALSE)


data2<- 'https://raw.githubusercontent.com/ucb-stat133/stat133-hws/master/data/draymond-green.csv'
download.file(data2, destfile ='draymond-green.csv' )
dray <- read.csv('draymond-green.csv', stringsAsFactors = FALSE)

data3 <- 'https://raw.githubusercontent.com/ucb-stat133/stat133-hws/master/data/kevin-durant.csv'
download.file(data3, destfile ='kevin-durant.csv')
kevin <- read.csv('kevin-durant.csv', stringsAsFactors = FALSE)

data4 <- 'https://raw.githubusercontent.com/ucb-stat133/stat133-hws/master/data/klay-thompson.csv'
download.file(data4, destfile = 'klay-thompson.csv')
klay <- read.csv('klay-thompson.csv', stringsAsFactors = FALSE)

data5 <- 'https://raw.githubusercontent.com/ucb-stat133/stat133-hws/master/data/stephen-curry.csv'
download.file(data5, destfile ='stephen-curry.csv')
steph <- read.csv('stephen-curry.csv', stringsAsFactors = FALSE)
```

### Variable Names and their meanings

##### Here's the description of data variables:

-   **team\_name** refers to the basket ball for which the players plays

-   **game\_date** is the date when the game took place

-   **season** refers to the year when the game was played

-   **opponent** refers to the team that the player was playing against

-   **Period**: an NBA game is divided in 4 periods of 12 mins each. For example, a value for period = 1 refers to the first period (the first 12 mins of the game).

-   **Minutes\_remaining** and **seconds\_remaining** have to do with the amount of time in minutes and seconds, respectively, that remained to be played in a given period.

-   **Shot\_made\_flag** indicates whether a shot was made (y) or missed (n).

-   **action\_type** has to do with the basketball moves used by players, either to pass by defenders to gain access to the basket, or to get a clean pass to a teammate to score a two pointer or three pointer.

-   **shot\_type** indicates whether a shot is a 2-point field goal, or a 3-point field goal.

-   **shot\_distance**: distance to the basket (measured in feet).

-   **x** and **y** refer to the court coordinates (measured in inches) where a shot occurred

-   The **minute** column is added later which is calculated by

> (**Period** \* 12 ) - **minutes\_remaining**
