

diversityZeros <- function(data.tbl, whichColumns) {
  
  library(tidyverse)
  library(rlang)
  
  # for each row in a table, count zeros and non-zeros
  # across specified columns; add two new columns
  
  # arguments
  # data.tble : tibble
  # countZero : boolean
  # whichColumns : an expression of 2 integers separated by colon and packaged by rlang::quos() 
  #                example :  quos(2:4)
  
  # return : new table with 2 new columns of counts
  
  # ref : https://stackoverflow.com/questions/37731987/count-number-of-values-in-row-using-dplyr
  #       https://stackoverflow.com/questions/44593596/how-to-pass-strings-denoting-expressions-to-dplyr-0-7-verbs
  #       https://community.rstudio.com/t/should-tidyeval-be-abandoned/2238/55
  
  # testing data :
  if (FALSE) {
    tribble(
      ~x, ~y, ~z, ~w, ~v, ~u,
      "a", 175, 0, 1, 0, 0,
      "b", 176, 1, 1, 1, 1,
      "c", 175, 1, 0, 1, 0
    ) -> test.tbl
  }
    
  test.tbl <- data.tbl %>% select(UQS(whichColumns))
  
    # test.tbl
    # A tibble: 3 x 4
    #     z     w     v     u
    #  <dbl> <dbl> <dbl> <dbl>
    #     0     1     0     0
    #     1     1     1     1
    #     1     0     1     0
    
  
  temp1.tbl <- as.tibble(cbind(diversityZeros=t(t(rowSums(test.tbl==0))), 
                               diversityNonZeros=t(t(rowSums(data.tbl!=0)))  
                               )
                        )
  
  temp1.tbl 
  
  return(dplyr::bind_cols(data.tbl, temp1.tbl) )
  
}
