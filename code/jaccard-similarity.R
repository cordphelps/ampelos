
# https://stats.stackexchange.com/questions/176613/jaccard-similarity-in-r


library(dplyr)

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/apply.html
# MARGIN : a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 
# 2 indicates columns, c(1, 2) indicates rows and columns. Where X has named dimnames, it can be a character vector 
# selecting dimension names.
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

#
# by: stats.stackexchange user 'bmc' https://stats.stackexchange.com/users/145523/bmc
# https://stats.stackexchange.com/questions/176613/jaccard-similarity-in-r
#
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
#
# by: stats.stackexchange user 'bmc' https://stats.stackexchange.com/users/145523/bmc
# https://stats.stackexchange.com/questions/176613/jaccard-similarity-in-r
#
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


bugRowsJaccardSimilarity <- function(df, t, w) {

	# input is bugs.df 

	#t <- "oakMargin"
	#w <- 26

	test.df <- bugs.df %>% 
        dplyr::select(-positionX, -position, -date, -time) %>%
        dplyr::rename(cucumber.beetle = Diabrotica.undecimpunctata..Cucumber.Beetle.) %>%
        dplyr::rename(Lygus.hesperus = Lygus.hesperus..western.tarnished.plant.bug.) %>%
        dplyr::filter(transect == UQ(t), week == UQ(w)) # last comment
        # https://www.reddit.com/r/rstats/comments/6zu5od/when_writing_functions_involving_dplyr_how_do_you/

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

		#    > results
		#           JSim      JDist
		# [1,] 1.0000000 0.00000000
		# [2,] 0.9230769 0.07692308
		# [3,] 0.7857143 0.21428571
		# [4,] 0.9230769 0.07692308
		# [5,] 1.0000000 0.00000000
		# [6,] 0.8461538 0.15384615
		# [7,] 0.7857143 0.21428571
		# [8,] 0.8461538 0.15384615
		# [9,] 1.0000000 0.00000000

   	results <- data.frame(results)
   	row.names(results) <- key_pair$pair

		# > results
		#           JSim      JDist
		# 83_83 1.0000000 0.00000000
		# 85_83 0.9230769 0.07692308
		# 87_83 0.7857143 0.21428571
		# 83_85 0.9230769 0.07692308
		# 85_85 1.0000000 0.00000000
		# 87_85 0.8461538 0.15384615
		# 83_87 0.7857143 0.21428571
		# 85_87 0.8461538 0.15384615
		# 87_87 1.0000000 0.00000000

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

	cleanResults.df <- cleanResults.df %>% 
        dplyr::select(-left, -right, -JDist) %>%
        dplyr::summarise_all(funs(mean, sd))

   	# > cleanResults.df
	#        mean         sd
	# 1 0.8516484 0.06884596
    
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


compareJaccardMultiWeek <- function() {

	# > unique(bugs.df$week)
	# [1] 23 24 25 26 27 28
	#

	if (!exists("output.df")) {

		output.df <- bugRowsJaccardSimilarity(df=bugs.df, t="oakMargin", w=23)

		# output.df is a simplified similarity table
   		#
   		# > output.df
		#        mean         sd
		# 1 0.8516484 0.06884596

    	accumulated.df <- tibble(week = 23, JSim = output.df$mean, JSimSD = output.df$sd)  # initialize the df

    } 

    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("oakMargin"), w=quo(24))
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 24, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("oakMargin"), w=quo(25))
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 25, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("oakMargin"), w=quo(26))
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 26, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("oakMargin"), w=quo(27))
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 27, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("oakMargin"), w=quo(28))
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 28, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("oakMargin"), w=quo(29))
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 29, JSim = output.df$mean, JSimSD = output.df$sd)


	ggOak <- ggplot(accumulated.df, aes(x=week, y=JSim)) + 
  		geom_point(aes(col=JSimSD, size=JSimSD*2)) + 
  		geom_smooth(method="loess", se=F) + 
  		geom_hline(yintercept=.8) +
  		ylim(c(0, 1)) + 
  		scale_y_continuous(breaks = seq(min(0), max(1), by = 0.1)) +
  		expand_limits(y=c(0,1)) + 
  		labs(title="row triad similarity of oakMargin populations", 
  			subtitle=paste("dot size/color represents 2X standard deviation", sep=""), 
       		y="jaccard index", 
       		x="week", 
       		caption = "https://en.wikipedia.org/wiki/Jaccard_index") +
  		theme(legend.position="none") +
  		coord_fixed() # control the aspect ratio of the output
  		# https://stackoverflow.com/questions/20581400/how-to-control-ggplots-plotting-area-proportions-instead-of-fitting-them-to-dev


	rm("output.df")

############################################################################################################

	if (!exists("output.df")) {

		output.df <- bugRowsJaccardSimilarity(df=bugs.df, t="control", w=23)

		# output.df is a simplified similarity table
   		#
   		# > output.df
		#        mean         sd
		# 1 0.8516484 0.06884596

    	accumulated.df <- tibble(week = 23, JSim = output.df$mean, JSimSD = output.df$sd)  # initialize the df

    } 

    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("control"), w=24)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 24, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("control"), w=25)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 25, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("control"), w=26)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 26, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("control"), w=27)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 27, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(bugs.df, t=quo("control"), w=28)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 28, JSim = output.df$mean, JSimSD = output.df$sd)


	ggControl <- ggplot(accumulated.df, aes(x=week, y=JSim)) + 
  		geom_point(aes(col=JSimSD, size=JSimSD*2)) + 
  		geom_smooth(method="loess", se=F) + 
  		geom_hline(yintercept=.8) +
  		ylim(c(0, 1)) + 
  		scale_y_continuous(breaks = seq(min(0), max(1), by = 0.1)) +
  		expand_limits(y=c(0,1)) + 
  		labs(title="row triad similarity of control populations", 
  			subtitle=paste("dot size/color represents 2X standard deviation", sep=""), 
       		y="jaccard index", 
       		x="week", 
       		caption = "https://en.wikipedia.org/wiki/Jaccard_index") +
  		theme(legend.position="none") +
  		coord_fixed() # control the aspect ratio of the output
  		# https://stackoverflow.com/questions/20581400/how-to-control-ggplots-plotting-area-proportions-instead-of-fitting-them-to-dev

	rm("output.df")






	return( arrangeGrob(ggOak, ggControl, ncol=1, nrow=2) )


}

compareJaccardMultiWeekV2 <- function(data, transect, transectText) {

	if (!exists("output.df")) {

		output.df <- bugRowsJaccardSimilarity(df=data, t=transect, w=23)

		# output.df is a simplified similarity table
   		#
   		# > output.df
		#        mean         sd
		# 1 0.8516484 0.06884596

    	accumulated.df <- tibble(week = 23, JSim = output.df$mean, JSimSD = output.df$sd)  # initialize the df

    } 

    output.df <- bugRowsJaccardSimilarity(data, t=transect, w=24)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 24, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(data, t=transect, w=25)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 25, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(data, t=transect, w=26)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 26, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(data, t=transect, w=27)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 27, JSim = output.df$mean, JSimSD = output.df$sd)
    output.df <- bugRowsJaccardSimilarity(data, t=transect, w=28)
	accumulated.df <- accumulated.df %>% tibble::add_row(week = 28, JSim = output.df$mean, JSimSD = output.df$sd)


	gg <- ggplot(accumulated.df, aes(x=week, y=JSim)) + 
  		geom_point(aes(col=JSimSD, size=JSimSD*2)) + 
  		geom_smooth(method="loess", se=F) + 
  		geom_hline(yintercept=.8) +
  		ylim(c(0, 1)) + 
  		# scale_y_continuous(breaks = seq(min(0), max(1), by = 0.1)) +
  		expand_limits(y=c(0,1)) + 
  		labs(title=paste(transectText, " transect: row triad population similarity", sep=""),
  			subtitle=paste("dot size/color represents 2X standard deviation", sep=""), 
       		y="jaccard index", 
       		x="week", 
       		caption = "https://en.wikipedia.org/wiki/Jaccard_index") +
  		theme(legend.position="none") +
  		coord_fixed(ratio=3) # control the aspect ratio of the output
  		# https://stackoverflow.com/questions/7056836/how-to-fix-the-aspect-ratio-in-ggplot

	rm("output.df")

	return(grid.arrange(gg, ncol=1, nrow=1))

}

getWeeks <- function(data) {

	# return a list of the weeks occurring in the dataset
	# https://stackoverflow.com/questions/29832411/use-dplyr-to-get-values-of-a-column

	library(dplyr)
	weeks <- data %>%
   				select(week) %>%
   				unique() %>% 
   				.$week

   	return(weeks)

}

buildIndexByWeek <- function(data, transect) {

	output.df <- NULL
	accumulated.df <- NULL

	weeks.vector <- getWeeks(data)

	output.df <- bugRowsJaccardSimilarity(df=data, t=transect, w=weeks.vector[1])
	accumulated.df <- tibble(week = weeks.vector[1], JSim = output.df$mean, JSimSD = output.df$sd)  # initialize the df
	weeks.vector <- weeks.vector[-(1)]  # delete the first element as it was just used.....

		# output.df is a simplified similarity table
   		#
   		# > output.df
		#        mean         sd
		# 1 0.8516484 0.06884596

	for(i in weeks.vector){
		# print(paste("week: ", i, sep=""))
		output.df <- bugRowsJaccardSimilarity(df=bugs.df, t=transect, w=i)
		accumulated.df <- accumulated.df %>% tibble::add_row(week = i, JSim = output.df$mean, JSimSD = output.df$sd)
	}

	return(accumulated.df)

}

compareJaccardMultiWeekV3 <- function(data, transect, transectText) {

	output.df <- NULL
	accumulated.df <- NULL

	weeks.vector <- getWeeks(data)

	output.df <- bugRowsJaccardSimilarity(df=data, t=transect, w=weeks.vector[1])
	accumulated.df <- tibble(week = weeks.vector[1], JSim = output.df$mean, JSimSD = output.df$sd)  # initialize the df
	weeks.vector <- weeks.vector[-(1)]  # delete the first element as it was just used.....

		# output.df is a simplified similarity table
   		#
   		# > output.df
		#        mean         sd
		# 1 0.8516484 0.06884596

	for(i in weeks.vector){
		# print(paste("week: ", i, sep=""))
		output.df <- bugRowsJaccardSimilarity(df=bugs.df, t=transect, w=i)
		accumulated.df <- accumulated.df %>% tibble::add_row(week = i, JSim = output.df$mean, JSimSD = output.df$sd)
	}


	gg <- ggplot(accumulated.df, aes(x=week, y=JSim)) + 
  		geom_point(aes(col=JSimSD, size=JSimSD*2)) + 
  		geom_smooth(method="loess", se=F) + 
  		geom_hline(yintercept=.8) +
  		ylim(c(0, 1)) + 
  		# scale_y_continuous(breaks = seq(min(0), max(1), by = 0.1)) +
  		expand_limits(y=c(0,1)) + 
  		labs(title=paste(transectText, " transect: row triad population similarity", sep=""),
  			subtitle=paste("dot size/color represents 2X standard deviation", sep=""), 
       		y="jaccard index", 
       		x="week", 
       		caption = "https://en.wikipedia.org/wiki/Jaccard_index") +
  		theme(legend.position="none") +
  		coord_fixed(ratio=3) # control the aspect ratio of the output
  		# https://stackoverflow.com/questions/7056836/how-to-fix-the-aspect-ratio-in-ggplot

	rm("output.df")

	return(grid.arrange(gg, ncol=1, nrow=1))

	}



  ade4Similarity <- function(data) {

    # checking the logic by hand
    # for a matrix
    #
    #      1 obs1   1   0   1
    #      2 obs2   0   0   0
    #      3 obs3   0   0   1
    #      4 obs4   1   0   1
    #      5 obs5   1   1   0
    #      6 obs6   1   1   1
    #      7 obs7   1   0   0
    #      8 obs8   1   0   0
    #      9 obs9   1   0   1
    #
    # the jaccard distance between observation 1 and observation 3 is:
    #
    # - shared = count the number of matches (where a 1 in the first member matches a 1 in the second) :  1
    # - total = count the number of 1 vs 0 and 0 vs 1 : 1
    # - similarity coefficient : divide shared by (total plus shared) : .5
    # - distance : square root of 1 minus the similarity coefficient : .707
    #
    #
    # the jaccard distance between observation 5 and observation 6 is:
    #
    # - shared = count the number of matches (where a 1 in the first member matches a 1 in the second) :  2
    # - total = count the number of 1 vs 0 and 0 vs 1 : 1
    # - similarity coefficient : divide shared by (total plus shared) : .666
    # - distance : square root of 1 minus the similarity coefficient :  .577

    # R> # remove non-numeric values (the row names)
    # R> test.df <- test.df %>% dplyr::select(-X)
    # R> ade4::dist.binary(test.df, method=1, diag=F, upper=F)  
    # 
#          1         2         3         4         5         6         7         8         9
#1 0.0000000                                                                                
#2 1.0000000 0.0000000                                                                      
#3 0.7071068 1.0000000 0.0000000                                                            
#4 0.0000000 1.0000000 0.7071068 0.0000000                                                  
#5 0.8164966 1.0000000 1.0000000 0.8164966 0.0000000                                        
#6 0.5773503 1.0000000 0.8164966 0.5773503 0.5773503 0.0000000                              
#7 0.7071068 1.0000000 1.0000000 0.7071068 0.7071068 0.8164966 0.0000000                    
#8 0.7071068 1.0000000 1.0000000 0.7071068 0.7071068 0.8164966 0.0000000 0.0000000          
#9 0.0000000 1.0000000 0.7071068 0.0000000 0.8164966 0.5773503 0.7071068 0.7071068 0.0000000

    # https://pbil.univ-lyon1.fr/ade4/ade4-html/dist.binary.html
    # 
#> data
#  sp1 sp2 sp3
#1   1   0   1
#2   0   0   0
#3   0   0   1
#4   1   0   1
#5   1   1   0
#6   1   1   1
#7   1   0   0
#8   1   0   0
#9   1   0   1

    # add row numbers to the df
    # https://stackoverflow.com/questions/23518605/add-an-index-numeric-id-column-to-large-data-frame
    data <- tibble::rowid_to_column(data, "rowID")

#> data
#  rowID sp1 sp2 sp3
#1     1   1   0   1
#2     2   0   0   0
#3     3   0   0   1
#4     4   1   0   1
#5     5   1   1   0
#6     6   1   1   1
#7     7   1   0   0
#8     8   1   0   0
#9     9   1   0   1

    # save the list
    name.list <- data.frame(data$rowID)

#> name.list
#  data.rowID
#1          1
#2          2
#3          3
#4          4
#5          5
#6          6
#7          7
#8          8
#9          9

    # values > 1 re-written at 1
    data[data > 0] <- 1

    row.names(data) <- name.list$data.row   # NOTE: apparently deprecated

    key_pairs <- expand.grid(name.list$data.row, name.list$data.row)  # column names are Var1 and Var2

    key_pair <- key_pairs %>% mutate(pair = paste(Var1,"_",Var2,sep=""))  # column names are Var1 and Var2
                                                                          # plus pair-ID in the form "row1-row2"


        # for each combination, make a 2 row matrix 
        for (i in 1:nrow(key_pairs)) {   # 9 rows of 2 variables expands to 81 pairs
          print(i)
          print(key_pairs$Var1[i])
          print(paste( slice ( data, key_pairs$Var1[i]   )) )
        }

#> key_pairs
#........
#79    7    9
#80    8    9
#81    9    9
#> key_pairs$Var1[81]
#[1] 9
#> key_pairs$Var1[80]
#[1] 8
#> key_pairs$Var2[80]
#[1] 9

    # remove duplicates and mirrored data
    # walk through rows deleting all rows that are compared to themselves

    results <-  tibble::rownames_to_column(key_pair, var = "rowname")  # added a column "rowname",
                                                                            # contents are : "rowA_rowB"

    results2 <- key_pair %>% tidyr::separate("pair", c("left", "right"), "_") # replace column 'rowname' with two new rows
                                                                                    # contains 'rowA' and 'rowB'
    results3 <- results2 %>% dplyr::filter(results2$left != results2$right)  # remove self comparisons
    results4 <- results3 %>% dplyr::filter(results3$left < results3$right)   # remove duplicates (mirror images)

#> nrow(results4)
#[1] 36
#> results4
#   Var1 Var2 left right
#1     1    2    1     2
#2     1    3    1     3
#3     2    3    2     3
#4     1    4    1     4
#5     2    4    2     4
#6     3    4    3     4
#7     1    5    1     5


    data.matrix <- as.matrix(dplyr::select(data, -rowID))

#> data.matrix
#  sp1 sp2 sp3
#1   1   0   1
#2   0   0   0
#3   0   0   1
#4   1   0   1
#5   1   1   0
#6   1   1   1
#7   1   0   0
#8   1   0   0
#9   1   0   1

    for (i in 1:nrow(results4)) {   # 9 rows of 2 variables expands to 81 pairs; reduced to 36 unique pairs

          #print(key_pairs$Var1[i])

          print(paste(  " sliceA ", data.matrix[ as.integer(results4$left[i]), ]  , 
                        " sliceB ", data.matrix[ as.integer(results4$right[i]), ] , sep="") )

          leftData.matrix <- data.matrix[ as.integer(results4$left[2]), ]
          rightData.matrix <- data.matrix[ as.integer(results4$right[2]), ]

          # ! matrix filled column by column, not row by row !
          #join.matrix <- matrix(c(leftData.matrix, rightData.matrix), 2, 3)
          leftData.df <- slice(data.frame(t(data.frame(leftData.matrix, row.names=NULL))),1)   #
          rightData.df <- slice(data.frame(t(data.frame(rightData.matrix, row.names=NULL))),1)  # 

          test.df <- bind_rows(leftData.df, rightData.df)
#> test.df
# A tibble: 2 x 3
#     X1    X2    X3
#  <int> <int> <int>
#1     1     0     1
#2     0     0     1

          dist.binary(test.df, method=1, diag=F, upper=F) 
          dist.binary(test.df, method=2, diag=F, upper=F) 


          join.matrix <- matrix(c(leftData.matrix[, rightData.matrix), 2, 3)

    }



    results <- t(apply(key_pairs, 1, function(row) measuresOfSimilarity( data[c(row[1], row[2]),] ) ) )



    #    > results
    #           JSim      JDist
    # [1,] 1.0000000 0.00000000
    # [2,] 0.9230769 0.07692308
    # [3,] 0.7857143 0.21428571
    # [4,] 0.9230769 0.07692308
    # [5,] 1.0000000 0.00000000
    # [6,] 0.8461538 0.15384615
    # [7,] 0.7857143 0.21428571
    # [8,] 0.8461538 0.15384615
    # [9,] 1.0000000 0.00000000

    results <- data.frame(results)
    row.names(results) <- key_pair$pair

    # > results
    #           JSim      JDist
    # 83_83 1.0000000 0.00000000
    # 85_83 0.9230769 0.07692308
    # 87_83 0.7857143 0.21428571
    # 83_85 0.9230769 0.07692308
    # 85_85 1.0000000 0.00000000
    # 87_85 0.8461538 0.15384615
    # 83_87 0.7857143 0.21428571
    # 85_87 0.8461538 0.15384615
    # 87_87 1.0000000 0.00000000

    # results is a df of 2 columns, JSim and JDist.
    # row names represents the similarity of all possible row combinations



      # cleanResults.df is a simplified similarity table
      #
      # > results.df
      #   left right      JSim      JDist
    # 1   83    85 0.9230769 0.07692308
    # 2   83    87 0.7857143 0.21428571
    # 3   85    87 0.8461538 0.15384615

  results <- results %>% 
        dplyr::select(-left, -right, -JDist) %>%
        dplyr::summarise_all(funs(mean, sd))

    # > cleanResults.df
  #        mean         sd
  # 1 0.8516484 0.06884596
    
    return(results)



  }

  measuresOfSimilarity <- function(inbound.df) {


    # note regarding class(dist) : https://stackoverflow.com/questions/9879608/how-do-i-manipulate-access-elements-of-an-instance-of-dist-class-using-core-r
    #temp1.df <- as.data.frame ( as.matrix ( dist.binary(inbound.df, method=1, diag=F, upper=F) ) ) # Jaccard

    print(inbound.df %>% row_number() )

    #print(dplyr::row_number(inbound.df[1,]))
    #print(dplyr::row_number(inbound.df[2,]))

  

    rowPair <- which(inbound.df[,1]>=0)

    #print(paste("row pair: ", rowPair, "\n",  sep=""))

    #return ( dist())

    #dist.binary(inbound.df, method=2, diag=F, upper=F) 

    #print(temp2.df)

    # return( dplyr::bind_cols(temp1.df, temp2.df) )

  }

  distXY <- function(X,Y,n){ # @NandaDorea 
  # https://stackoverflow.com/questions/9879608/how-do-i-manipulate-access-elements-of-an-instance-of-dist-class-using-core-r
  # provide X and Y, the original rows of the elements in the matrix from which you calculated dist, 
  # and n is the total number of elements in that matrix. The result is the position in the dist vector 
  # where the distance will be.
  A=min(X,Y)
  B=max(X,Y)

  d=eval(parse(text=
               paste0("(A-1)*n  -",paste0((1:(A-1)),collapse="-"),"+ B-A")))

  return(d)

}
