
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

bugRowsJaccardSimilarityV2 <- function(df, t, w) {

  # input is bugs.df 

  #t <- quo("oakMargin")
  #w <- quo(26)
  #df <- data

  test.df <- df %>% 
        dplyr::select(-positionX, -position, -date, -time) %>%
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

    return(test.df)

}

compareJaccardMultiWeekV4 <- function(data, ignoreBees, transect, transectText) {

  # develop the data for similarity graphs that compare the populations of adjacent rows
  #
  # the resulting dataframe has similarity variables (jaccard and SME) for each
  # week by transect 
  #
  # this function is intended to be called from ampelos.Rmd
  #

  data <- bugs.df
  ignoreBees <- TRUE
  t <- "control"

  weeks.vector <- getWeeks(data)                    # determine weeks in the dataset
  weeks.df <- dplyr::bind_cols(week = weeks.vector) # place weeks in a column

  data <- data %>%  dplyr::rename(
                        Lygus.hesperus = Lygus.hesperus..western.tarnished.plant.bug., 
                        cucumber.beetle = Diabrotica.undecimpunctata..Cucumber.Beetle.) 

  if (ignoreBees == TRUE) { 

    data <- data %>% dplyr::select( 
      -Agapostemon.sp....green..native.bee.,
      -Bombus.californicus..bumble.,
      -checkerspot.butterfly,
      -cucumber.beetle,
      -Halictus.sp....3.part..native.bee.,
      -Honey.Bee,
      -Osmia.sp...native.bee.)
  }

  data <- data %>% 
    dplyr::filter(transect == t) %>% 
    dplyr::select(-position, -positionX, -transect, -julian, -time, -date) 


  
  ## testing : > bugRowsJaccardSimilarityV2(df=bugs.df, t="control", w=getWeeks(data)[1])

## A tibble: 3 x 20
#    row Diptera..Agromyzid… Braconid.wasp Halictus.sp....3.p… pencilBug Agapostemon.sp...…
#  <dbl>               <dbl>         <int>               <dbl>     <dbl>              <dbl>
#1     1                   1             0                   1         1                  0
#2     1                   1             0                   1         0                  1
#3     1                   1             0                   1         0                  0
# ... with 14 more variables: Osmia.sp...native.bee. <dbl>, Honey.Bee <dbl>,
#   Bombus.californicus..bumble. <dbl>, Thomisidae..crab.spider. <dbl>,
#   spider.other <dbl>, ladyBug <dbl>, Lygus.hesperus <dbl>,
#   pentamonidae...stinkBug. <int>, other <dbl>, checkerspot.butterfly <dbl>,
#   Pyralidae..Snout.Moth. <dbl>, cucumber.beetle <dbl>, Orius..pirate.bug. <int>,
#   week <dbl>


  obs <- list()
  j <- 0
  temp.df <- data %>% dplyr::group_by(row) 

  for(i in weeks.vector) {

    j <- j + 1

    temp2.df <- temp.df %>%
      dplyr::filter(week == i) %>%
      dplyr::summarise_all(funs(sum)) 

    temp3.df <- ade4Similarity(temp2.df) # 

#> ade4Similarity(data)
#  Var1 Var2 left right   jaccard       SME
#1    1    2    1     2 0.5477226 0.4803845
#2    1    3    1     3 0.4472136 0.3922323
#3    2    3    2     3 0.6741999 0.6201737

#> mean(data$jaccard)   [1] 0.5563787 > mean(data$SME)  [1] 0.4975968

    obs[[j]] <- data.frame(transect=t, week=i, jaccard=mean(temp3.df$jaccard), SME=mean(temp3.df$SME))

  }

  # make one df for plotting
  plot.df <- dplyr::bind_rows(obs)

  plotSimilarity(plot.df, transectText)

}

plotSimilarity <- function(df, transectText) {


  gg <- ggplot(plot.df) + 
      geom_point(aes(x=week, y=SME), shape = 21, size=5, colour = "mediumvioletred", fill = "plum1") + 
      geom_point(aes(x=week, y=jaccard), shape = 21, size=5, colour = "mediumvioletred", fill = "purple1") + 

      ylim(c(0, 1)) + 
      # scale_y_continuous(breaks = seq(min(0), max(1), by = 0.1)) +
      expand_limits(y=c(0,1)) + 
      labs(title=paste(transectText, " transect: row triad population similarity", sep=""),
        subtitle=paste("(none)", sep=""), 
          y="index", 
          x="week", 
          caption = "https://en.wikipedia.org/wiki/Jaccard_index") +
      #theme(legend.position="none") +
      theme(legend.position = "bottom", legend.direction = "horizontal") +
      coord_fixed(ratio=5) # control the aspect ratio of the output
      # https://stackoverflow.com/questions/7056836/how-to-fix-the-aspect-ratio-in-ggplot

  return(grid.arrange(gg, ncol=1, nrow=1))

  }

  ade4Similarity <- function(data) {

    # checking the logic by hand
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

    # values > 1 re-written as 1
    data[data > 0] <- 1

    row.names(data) <- name.list$data.row   # NOTE: apparently deprecated

    key_pairs <- expand.grid(name.list$data.row, name.list$data.row)  # column names are Var1 and Var2

    key_pair <- key_pairs %>% mutate(pair = paste(Var1,"_",Var2,sep=""))  # column names are Var1 and Var2
                                                                          # plus pair-ID in the form "row1-row2"


        # for each combination, make a 2 row matrix 
        #for (i in 1:nrow(key_pairs)) {   # 9 rows of 2 variables expands to 81 pairs
          #print(i)
          #print(key_pairs$Var1[i])
          #print(paste( slice ( data, key_pairs$Var1[i]   )) )
        #}

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

    
    new_column_val1.vector <- vector()   # init an empty vector to contain a distance measurement (from dist.ade4() )
    new_column_val2.vector <- vector()

    for (i in 1:nrow(results4)) {   # 9 rows of 2 variables expands to 81 pairs; reduces to 36 unique pairs

          #print(paste(  " sliceA ", data.matrix[ as.integer(results4$left[i]), ]  , 
          #              " sliceB ", data.matrix[ as.integer(results4$right[i]), ] , sep="") )

          leftData.matrix <- as.matrix(data.matrix[ as.integer(results4$left[i]), ], nrow=1)   # get data by a row number
          rightData.matrix <- as.matrix(data.matrix[ as.integer(results4$right[i]), ], nrow=1)  # get data by a row number

          # ! matrix filled column by column, not row by row !
          # join.matrix <- matrix(c(leftData.matrix, rightData.matrix), 2, 3)

          # (so work with data frames....)

          temp.df <- t(bind_cols(data.frame(leftData.matrix, row.names=NULL), data.frame(rightData.matrix, row.names=NULL)))

#>temp.df
#   V1 V2 V3
# 1  1  0  1
# 2  0  0  0
#   V1 V2 V3
# 1  1  0  1
# 2  0  0  1
#   V1 V2 V3
# 1  0  0  0
# 2  0  0  1

          #dist.binary(temp.df, method=1, diag=F, upper=F)  # output: 0.7071068   class='dist'
          #dist.binary(temp.df, method=2, diag=F, upper=F)  # output: 0.5773503

          # Jaccard distance
          # object dist is coerced to a matrix with as.matrix()
          row1.row2.m1.matrix <- as.matrix(dist.binary(temp.df, method=1, diag=F, upper=F))  
          # Simple Matching Coefficient distance
          row1.row2.m2.matrix <- as.matrix(dist.binary(temp.df, method=2, diag=F, upper=F)) 

#> row1.row2.m1.matrix
#          1         2
#1 0.0000000 0.7071068
#2 0.7071068 0.0000000

          # get the distance values
          jaccard.Distance <- row1.row2.m1.matrix[1,2]
          SME.Distance <- row1.row2.m2.matrix[1,2]


          # add them to the vector
          if (length(new_column_val1.vector)>0) {
            new_column_val1.vector <- c(new_column_val1.vector, jaccard.Distance)
            new_column_val2.vector <- c(new_column_val2.vector, SME.Distance)
          } else {
            new_column_val1.vector <- jaccard.Distance
            new_column_val2.vector <- SME.Distance
          }

    }

    # new_column_val1.vector now contains distance data for each 'unique' row conbination
    # a new column (for each distance value) that can be bound to result4 with dplyr::bind_cols()

    # add the vector(s) to the result4.df as a new column(s)
    results5 <- dplyr::bind_cols(results4, data.frame(jaccard=new_column_val1.vector))
    results5 <- dplyr::bind_cols(results5, data.frame(SME=new_column_val2.vector))

    
    return(results5)



  }

  ade4SimilarityTest <- function() {

    # create a 9 observation sequence of 3 variables to test ade4Similarity() 

    #test.df <- dplyr::tribble(~obs, ~x, ~y, ~z, 
    #                         "obs1", 1, 0, 1,
    #                          "obs2", 0, 0, 0,
    #                          "obs3", 0, 0, 1,
    #                          "obs4", 1, 0, 1,
    #                          "obs5", 1, 1, 0,
    #                          "obs6", 1, 1, 1,
    #                          "obs7", 1, 0, 0,
    #                          "obs8", 1, 0, 0,
    #                          "obs0", 1, 0, 1 ) 

    test.df <- as.data.frame(do.call(rbind, list(
                list(1,0,1),
                list(0,0,0),
                list(0,0,1),
                list(1,0,1),
                list(1,1,0),
                list(1,1,1),
                list(1,0,0),
                list(1,0,0),
                list(1,0,1)
                 ), quote=FALSE) )

#> class(test.df)
#[1] "data.frame"
#> test.df
#    V1 V2 V3 V4
#1 obs1  1  0  1
#2 obs2  0  0  0
#3 obs3  0  0  1


#> data.saved <- data

    ade4Similarity(test.df)

}