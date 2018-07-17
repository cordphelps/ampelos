
simMatrix <- function(data) {

  	# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

  	# return grob : side-by-side similarity matricies for the two transects
  
  	library("dplyr")
    library(reshape2)
    library(ggplot2)
    library(gridExtra)
  
  	## get the insect data by transect and by week
  
    # https://stackoverflow.com/questions/21644848/summarizing-multiple-columns-with-dplyr
    oak.df <- data %>% 
        dplyr::select(-row, -positionX, -position, -date, -time, -julian) %>%
        dplyr::filter(transect=="oakMargin") %>%
        dplyr::group_by(week) %>%
        dplyr::select(-transect) %>%
        dplyr::summarise_all(funs(mean)) %>%
        dplyr::select(-week) 
    # now all columns are species, rows are 'week averages'

     ## Compute the correlation matrix
    sim.matrix <- as.matrix(oak.df)
    cormat <- round(cor(sim.matrix),2)

    # Create the correlation heatmap with ggplot2
	 melted_cormat <- melt(cormat)
  #	library(ggplot2)
  #ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  #geom_tile()
  upper_tri <- get_upper_tri(cormat)

  # Finished correlation matrix heatmap
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Heatmap
  simOak <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Pearson\nCorrelation") +
    labs(title= paste("Species Correlation Matrix\n", 
                      "transect: Oak Margin", sep="")) +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 24)) +
    theme(axis.text.y = element_text(size = 24)) +
    theme(plot.title = element_text(size=36)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    coord_fixed()


#########################################################################
#########################################################################

    control.df <- data %>% 
        dplyr::select(-row, -positionX, -position, -date, -time, -julian) %>%
        dplyr::filter(transect=="control") %>%
        dplyr::group_by(week) %>%
        dplyr::select(-transect) %>%
        dplyr::summarise_all(funs(mean)) %>%
        dplyr::select(-week) 
    # now all columns are species, rows are 'week averages'

     ## Compute the correlation matrix
    sim.matrix <- as.matrix(control.df)
    cormat <- round(cor(sim.matrix),2)

    # Create the correlation heatmap with ggplot2
   melted_cormat <- melt(cormat)
  # library(ggplot2)
  #ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  #geom_tile()
  upper_tri <- get_upper_tri(cormat)

  # Finished correlation matrix heatmap
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Heatmap
  simControl <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Pearson\nCorrelation") +
    labs(title= paste("Species Correlation Matrix\n", 
                      "transect: Control", sep="")) +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 24)) +
    theme(axis.text.y = element_text(size = 24)) +
    theme(plot.title = element_text(size=36)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    coord_fixed()


#########################################################################
#########################################################################
  

  return(grid.arrange(simOak, simControl, ncol=1, nrow=2))

}

# Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
