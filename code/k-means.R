
# ref: https://www.datacamp.com/community/tutorials/k-means-clustering-r

# objective: classify (determine clusters of) the trap locations containing crab spiders 
# for a week/transect

kmReduce <- function(df, t, w, s, intWeek) {

# filter data based on transect, week, and species presence

#t <- quo("oakMargin")
#w <- quo(26)
#df <- bugs.df
#s<- quo(Thomisidae..crab.spider.)
#intWeek <- 26

	data <- df %>%
      dplyr::filter(transect == UQ(t), week == UQ(w), UQ(s) > 0) %>%    # get all traps with spiders for transect/week
      dplyr::select(row, position, week, transect) 

    
    data$row <- as.character(data$row)  			# replace the row number with a week number  **** CHECK LOGIC ****
    data$row <- as.factor(as.character(intWeek))  	# replace the row number with a week number 
    #data$row <- as.factor(data$row)
                                     				# this effectively combines the row data and
                                     				# enables the collection of weekly cluster info
                                     				# on one plot 
    #https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement


	return(data)

}

kmAssignClusters <- function(kdata, cn) {

	#kdata <- dataList[[1]]
	#cn <- 3

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
	kdata$newCluster <- "NA"
	#
	# walk through data; compare data$cluster to v.cluster.df$cluster
	# 
	for (i in 1:nrow(kdata)) {

		for (j in 1:nrow(v.cluster.df)) {
			if ( kdata$cluster[[i]] == v.cluster.df$cluster[[j]] ) {
				kdata$newCluster[[i]] <- as.factor(v.cluster.df$newCluster[[j]])
			}
		}

	}


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

buildClustersByWeek <- function(df, transect, cn) {

	#df <- bugs.df
	#transect <- "control"
	#cn <- 3

	weeks.vector <- getWeeks(df)

	dataList <- list()

	for (i in 1:length(weeks.vector)) {

			dataList[[i]] <- kmReduce(df, 
                 quo(transect), quo(weeks.vector[[i]]), quo(Thomisidae..crab.spider.),
                 weeks.vector[[i]])

			dataList[[i]] <- kmAssignClusters(dataList[[i]], cn)

	}

	return(dataList)

}
