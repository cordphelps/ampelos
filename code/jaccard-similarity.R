
# https://stats.stackexchange.com/questions/176613/jaccard-similarity-in-r

library(dplyr)

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/apply.html
# MARGIN : a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 
# 2 indicates columns, c(1, 2) indicates rows and columns. Where X has named dimnames, it can be a character vector 
# selecting dimension names.
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

jaccard_per_row <- function(df, margin=1){
   key_pairs <- expand.grid(row.names(df), row.names(df))  # two columns with all possible pairs of c1 - c6
   results <- t(apply(key_pairs, 1, function(row) jaccard(df[c(row[1], row[2]),], margin=margin)))
   key_pair <- key_pairs %>% mutate(pair = paste(Var1,"_",Var2,sep=""))   # pair-ID in the form "c1-c2"
   results <- data.frame(results)
   row.names(results) <- key_pair$pair
   results
}

# Function returns the Jaccard index and Jaccard distance
# Parameters:
# 1. df, dataframe of interest
# 2. margin, axis in which the apply function is meant to move along
jaccard <- function(df, margin=1) {
  if (margin == 1 | margin == 2) {
    M_00 <- apply(df, margin, sum) == 0
    M_11 <- apply(df, margin, sum) == 2
    if (margin == 1) {
      df <- df[!M_00, ]
      JSim <- sum(M_11) / nrow(df)
    } else {
      df <- df[, !M_00]
      JSim <- sum(M_11) / length(df)
    }
    JDist <- 1 - JSim
    return(c(JSim = JSim, JDist = JDist))
  } else break
}


makeDemoDataset <- function() {

	df <- data.frame(t(data.frame(c1=rnorm(100),  # random generation with normal distribution
                              c2=rnorm(100),      # If mean or sd are not specified they assume 
                              c3=rnorm(100),      # the default values of 0 and 1, respectively.
                              c4=rnorm(100),
                              c5=rnorm(100),      # row names are c1..c6
                              c6=rnorm(100))))    # column names are X1..X100

	df[df > 0] <- 1                               # values > 0 re-written as 1
	df[df <= 0] <- 0                              # values <= 0 re-written as 0

	return(df)

}


bugsJaccardSimilarity <- function(df) {

	# input is bugs.df 

	test.df <- bugs.df %>% 
        dplyr::select(-positionX, -position, -date, -time) %>%
        dplyr::rename(cucumber.beetle = Diabrotica.undecimpunctata..Cucumber.Beetle.) %>%
        dplyr::rename(Lygus.hesperus = Lygus.hesperus..western.tarnished.plant.bug.) %>%
        dplyr::filter(transect=="oakMargin", week==26) 

    test.df <- test.df %>%
    	dplyr::group_by(row) %>%
    	dplyr::select(-transect, -julian) %>%
    	dplyr::summarise_all(funs(sum)) 
    	# dplyr::mutate(uniqueRow = paste(row, "_", sep="")) %>%

    name.list <- data.frame(test.df$row)

    test.df[test.df > 0] <- 1

    row.names(test.df) <- name.list$test.df.row   # NOTE: apparently deprecated
    test.df <- test.df %>% 
        dplyr::select(-row, -week)   # now jaccard operates on species only....

    key_pairs <- expand.grid(row.names(test.df), row.names(test.df))
    key_pair <- key_pairs %>% mutate(pair = paste(Var1,"_",Var2,sep=""))   # pair-ID in the form "row1-row2"
    results <- t(apply(key_pairs, 1, function(row) jaccard(test.df[c(row[1], row[2]),], margin=2)))
   	results <- data.frame(results)
   	row.names(results) <- key_pair$pair

   	# results is a df of 2 columns, JSim and JDist.
   	# row names represent the similarity of all possible row combinations

   	# remove duplicates and mirrored data
   	cleanResults.df <- simplifyJaccardSimilarityResults(results)

   	# cleanResults.df is a simplified similarity table
   	#
   	# > results.df
  	#   left right      JSim      JDist
	# 1   83    85 0.9230769 0.07692308
	# 2   83    87 0.7857143 0.21428571
	# 3   85    87 0.8461538 0.15384615

   	# 
    
   	return(cleanResults.df)


}

simplifyJaccardSimilarityResults <- function(results) {



	results.df <- results

	# walk through rows deleting all rows that are compared to themselves

	results.df <-  tibble::rownames_to_column(results.df, var = "rowname")  # added a column "rowname",
	                                                                        # contents are : "rowA_rowB"

	results.df <- results.df %>% tidyr::separate("rowname", c("left", "right"), "_") # replace column 'rowname' with two new rows
	                                                                                 # contains 'rowA' and 'rowB'

	results.df <- results.df %>% dplyr::filter(results.df$left != results.df$right)  # remove self comparisons
	results.df <- results.df %>% dplyr::filter(results.df$left < results.df$right)   # remove duplicates (mirror images)

	return(results.df)


}


