
# ref: https://www.datacamp.com/community/tutorials/k-means-clustering-r

# objective: classify (determine clusters of) the trap locations containing crab spiders 
# for a week/transect

kmReduce <- function(df, ft, fw, fs, intWeek) {

# filter data based on transect, week, and species presence

#t <- "oakMargin"
#w <- 26
#formula.t <- (~ transect == "oakMargin" )
#formula.w <- (~ week == 24 )
#formula.s <- (~ Thomisidae..crab.spider. > 0 )
#s<- "Thomisidae..crab.spider."
#df <- bugs.df
#intWeek <- 26


#t <- enquo(t)
#w <- enquo(w)
#s<- enquo(s)



	data <- df %>%
      #dplyr::filter(transect == !! t, week == !! w) %>%    # get all traps with spiders for transect/week
	  dplyr::filter_(ft) %>%
	  dplyr::filter_(fw) %>%
      dplyr::filter_(fs) %>%  # https://stackoverflow.com/questions/36647468/creating-a-function-with-an-argument-passed-to-dplyrfilter-what-is-the-best-wa
      dplyr::select(row, position, week, transect) 

      		#  !! transect
			# The bang bang says we want the expression that item is referring to, not item itself.
			# http://r.danal.com/tutorials/quosures_inside_out.html
      		# 

    
    data$row <- as.character(data$row)  			# replace the row number with a week number  **** CHECK LOGIC ****
    data$row <- as.factor(as.character(intWeek))  	# replace the row number with a week number 
    #data$row <- as.factor(data$row)
                                     				# this effectively combines the row data and
                                     				# enables the collection of weekly cluster info
                                     				# on one plot 
    #https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement


	return(data)

}

kmAssignClusters <- function(list, cn) {

	kdata <- list

	set.seed(20)
	clusters <- kmeans(kdata[,1:2], cn, iter.max=100)
	kdata$cluster <- as.factor(clusters$cluster)

	# problem: the cluster numbers are not necessary in order of the trap positions

	# for each cluster, create a list of the positions 
	v.clusterVector <- getClusters(kdata)

	v.position.list <- list()
	v.mean.list <- list()
	v.cluster.df <- tibble(cluster=integer(), mean=integer())

	for (i in 1:length(v.clusterVector)) {

			positionVector <- kdata %>%
							dplyr::filter(cluster == i) %>%	
   							dplyr::select(position) %>%
   							unique() %>% 
   							.$position

   			v.position.list[[i]] <- positionVector

   			v.mean.list[[i]] <- mean(v.position.list[[i]])
	}

	# create a df that associates the mean of the positions with the cluster number
	# (this will allow the row groups to be sorted)
	for (i in 1:length(v.clusterVector)) {
		v.cluster.df <- v.cluster.df %>% tibble::add_row(cluster = i, mean = v.mean.list[[i]])
	}
	# now use the mean sort the rows  
	v.cluster.df <- dplyr::arrange(v.cluster.df, mean)
	# build a vector of row numbers
	# this is the new order for the cluster assignments
	v.rowVector <- list()
	for (i in 1:nrow(v.cluster.df)) {
		v.rowVector[[i]] <- i
	}
	# create a new column representing a revised cluster number
	v.cluster.df <- v.cluster.df %>% dplyr::mutate(newCluster = as.list(v.rowVector))

	# add an empty column to the target data
	kdata$newCluster <- NULL
	#
	# walk through data; compare data$cluster to v.cluster.df$cluster
	# 
	for (i in 1:nrow(kdata)) {

		for (j in 1:nrow(v.cluster.df)) {
			if ( kdata$cluster[[i]] == v.cluster.df$cluster[[j]] ) {
				kdata$newCluster[[i]] <- v.cluster.df$newCluster[[j]]
			}
		}
	}

	kdata$newCluster <- as.factor(kdata$newCluster)


	return(kdata)

}


getClusters <- function(data) {

	# return a list of the clusters occurring in the dataset
	# https://stackoverflow.com/questions/29832411/use-dplyr-to-get-values-of-a-column

	library(dplyr)
	clusterVector <- data %>%
   					select(cluster) %>%
   					unique() %>% 
   					.$cluster

   	return(clusterVector)


}

kmPlot <- function(list, transectText) {

	#list <- datalist

	ggplot() + 

  		geom_point(aes(x = position[], y = row[], fill = newCluster),
  			data = list[[1]], shape = 21, size=5) +

  		geom_point(aes(x = position[], y = row[], fill = newCluster),
  			data = list[[3]], shape = 21, size=5) +

  		geom_point(aes(x = position[], y = row[], fill = newCluster),
  			data = list[[5]], shape = 21, size=5) +

  		geom_point(aes(x = position[], y = row[], fill = newCluster),
  			data = list[[7]], shape = 21, size=5) +

  		geom_point(aes(x = position[], y = row[], fill = newCluster),
  			data = list[[10]], shape = 21, size=5) +

  		xlim(c(1, 10)) + 
      	expand_limits(x=c(1,10)) +
      	scale_x_continuous(breaks = seq(min(1), max(10), by = 1)) +

      	#scale_y_continuous(breaks=seq(22,40,2)) +

      	scale_y_discrete() +

      	labs(title=paste("crab spider clusters using KMean\n", sep=""),
        subtitle=paste(transectText, " transect", sep=""), 
          y="week number", 
          x="trap position", 
          caption = paste("https://en.wikipedia.org/wiki/K-means_clustering", sep="") ) +

  		theme_bw()


}

buildClustersByWeek <- function(df, t, species, cn) {

	#df <- bugs.df
	#i <- 1
	#species <- "Thomisidae..crab.spider."
	#cn <- 3
	#formula.t <- (~ transect == "oakMargin" )
	#formula.w <- (~ week == 25 )
	#formula.s <- (~ Thomisidae..crab.spider. > 0 )
	#t <- "control"

	transectString <- paste("~transect=='", t, "'", sep="")
	formula.t <- as.formula(transectString)
	#transectExp <- parse(text = transectString)

	speciesString <- paste("~", species, ">0", sep="")
	formula.s <- as.formula(speciesString)
	#speciesExp <- parse(text = speciesString)

	weeks.vector <- getWeeks(df)

	dataList <- NULL
	dataList <- list()

	for (i in 1:length(weeks.vector)) {

			w <- weeks.vector[[i]]

			weekString <- paste("~week==", weeks.vector[[i]], sep="") # dynamically create an expression to
			formula.w <- as.formula(weekString)
			#weekExp <- parse(text = weekString)                    # filter by week
																	 # http://adv-r.had.co.nz/Expressions.html#parsing-and-deparsing

			dataList[[i]] <- kmReduce(df, ft=formula.t, fw=formula.w, fs=formula.s, intWeek=w)

			clusterList <- dataList[[i]]

			dataList[[i]] <- kmAssignClusters(list=clusterList, cn=cn)

	}

	return(dataList)

}
