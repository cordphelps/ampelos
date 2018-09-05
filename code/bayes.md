ampelos
================

|  obsOne|  obsTwo|  obsThree| conjecture |  conjectureMean|
|-------:|-------:|---------:|:-----------|---------------:|
|       0|       0|         0| 0-0-0      |       0.0000000|
|       0|       0|         1| 0-0-1      |       0.3333333|
|       0|       0|         2| 0-0-2      |       0.6666667|
|       0|       0|         3| 0-0-3      |       1.0000000|
|       0|       0|         4| 0-0-4      |       1.3333333|
|       0|       0|         5| 0-0-5      |       1.6666667|
|       0|       1|         1| 0-1-1      |       0.6666667|
|       0|       1|         2| 0-1-2      |       1.0000000|
|       0|       1|         3| 0-1-3      |       1.3333333|
|       0|       1|         4| 0-1-4      |       1.6666667|
|       0|       1|         5| 0-1-5      |       2.0000000|
|       0|       2|         2| 0-2-2      |       1.3333333|
|       0|       2|         3| 0-2-3      |       1.6666667|
|       0|       2|         4| 0-2-4      |       2.0000000|
|       0|       2|         5| 0-2-5      |       2.3333333|
|       0|       3|         3| 0-3-3      |       2.0000000|
|       0|       3|         4| 0-3-4      |       2.3333333|
|       0|       3|         5| 0-3-5      |       2.6666667|
|       0|       4|         4| 0-4-4      |       2.6666667|
|       0|       4|         5| 0-4-5      |       3.0000000|
|       0|       5|         5| 0-5-5      |       3.3333333|
|       1|       1|         1| 1-1-1      |       1.0000000|
|       1|       1|         2| 1-1-2      |       1.3333333|
|       1|       1|         3| 1-1-3      |       1.6666667|
|       1|       1|         4| 1-1-4      |       2.0000000|
|       1|       1|         5| 1-1-5      |       2.3333333|
|       1|       2|         2| 1-2-2      |       1.6666667|
|       1|       2|         3| 1-2-3      |       2.0000000|
|       1|       2|         4| 1-2-4      |       2.3333333|
|       1|       2|         5| 1-2-5      |       2.6666667|
|       1|       3|         3| 1-3-3      |       2.3333333|
|       1|       3|         4| 1-3-4      |       2.6666667|
|       1|       3|         5| 1-3-5      |       3.0000000|
|       1|       4|         4| 1-4-4      |       3.0000000|
|       1|       4|         5| 1-4-5      |       3.3333333|
|       1|       5|         5| 1-5-5      |       3.6666667|
|       2|       2|         2| 2-2-2      |       2.0000000|
|       2|       2|         3| 2-2-3      |       2.3333333|
|       2|       2|         4| 2-2-4      |       2.6666667|
|       2|       2|         5| 2-2-5      |       3.0000000|
|       2|       3|         3| 2-3-3      |       2.6666667|
|       2|       3|         4| 2-3-4      |       3.0000000|
|       2|       3|         5| 2-3-5      |       3.3333333|
|       2|       4|         4| 2-4-4      |       3.3333333|
|       2|       4|         5| 2-4-5      |       3.6666667|
|       2|       5|         5| 2-5-5      |       4.0000000|
|       3|       3|         3| 3-3-3      |       3.0000000|
|       3|       3|         4| 3-3-4      |       3.3333333|
|       3|       3|         5| 3-3-5      |       3.6666667|
|       3|       4|         4| 3-4-4      |       3.6666667|
|       3|       4|         5| 3-4-5      |       4.0000000|
|       3|       5|         5| 3-5-5      |       4.3333333|
|       4|       4|         4| 4-4-4      |       4.0000000|
|       4|       4|         5| 4-4-5      |       4.3333333|
|       4|       5|         5| 4-5-5      |       4.6666667|
|       5|       5|         5| 5-5-5      |       5.0000000|

``` r
# compute the frequency of each mean and add columns for count and frequency
# https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
meanFreq.df <- uC.df %>%
  dplyr::group_by(conjectureMean) %>%
  dplyr::summarise (countWays = n()) %>%
  dplyr::mutate(freq = countWays / sum(countWays)) 
```

    ## Warning: Grouping rowwise data frame strips rowwise nature

``` r
nextGen <- function(vector1, vector2) {
  # compute the next generation
  # https://stackoverflow.com/questions/11388359/unique-combination-of-all-elements-from-two-or-more-vectors
  
  # testing....
  #vector1 <- c('a', 'b', 'c')
  #vector2 <- c('x', 'y', 'z')
  #test.df <- dplyr::tribble(~obs, ~x, ~y,  
  #                           "obs1", 'a', 'a',
  #                            "obs2", 'b', 'b',
  #                            "obs3", 'c', 'c',
  #                            "obs4", 'd', 'd',
  #                            "obs5", 'e', 'e',
  #                            "obs6", 'f', 'f') 
  
  
  
  library(tidyr)
  
  # find all unique combinations
  next.df <- as.data.frame(tidyr::crossing(vector1, vector2))
  # remove the mirror images (because the lists are exactly the same)
  withDuplicates.df <- as.data.frame(tidyr::crossing(test.df$x, test.df$y))
  
  noDuplicates.df <- removeDup(as.data.frame(t(withDuplicates.df)))
  
  noDuplicates.df <- as.data.frame(t(noDuplicates.df))

}
```

``` r
removeDup <- function(df) {
  
  # remove columns containing duplicate row data interpreted as a list
  #       col1 col2 col3
  # row1   1    0    1
  # row2   0    0    0
  # row3   1    1    1
  #                  ^--- duplicate
  
  # testing.....
  # t.df <- as.data.frame(t(withDuplicates.df))
  #df <- t.df
  
  browser()
  
  columns <- ncol(df)
  for (i in 1:columns) {
    k <- i+1
    if (k > columns) {
      break
    } else {
      for (j in columns:k) {         # walk backwards along the columns
        c.list <- compare(as.list(df[,i]), as.list(df[,j]), ignoreOrder=TRUE)
        if ( c.list[[1]] == TRUE) {
          df[,j] <- NULL                 # delete the duplicate column
        }
      }
    }
    columns <- ncol(df)
  }
  
  return(df)
  
}
```
