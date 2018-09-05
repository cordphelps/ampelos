
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

