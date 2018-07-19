
simMatrix <- function(data) {

  	# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

  	# return grob : side-by-side similarity matricies for the two transects

# produce correlation coefficients between the possible pairs of variables 
# cor(matrix, method = c("pearson", "kendall", "spearman"))
# ?cor
# ... For cor(), if method is "kendall" or "spearman", Kendall's tau or Spearman's rho 
# statistic is used to estimate a rank-based measure of association. These are 
# more robust and have been recommended if the data do not necessarily come from 
# a bivariate normal distribution.

# Correlation matrix list with significance levels (coefficients matrix plus p-value matrix)
# > ?rcorr
# .... Spearman correlations are the Pearson linear correlations computed on the ranks of 
# non-missing elements, using midranks for ties.
  
  	library("dplyr")
    library("reshape2")
    library("ggplot2")
    library("gridExtra")
    library("grid")
  
  	## get the insect data by transect and by week
  
    # https://stackoverflow.com/questions/21644848/summarizing-multiple-columns-with-dplyr
    oak.df <- data %>% 
        dplyr::select(-row, -positionX, -position, -date, -time, -julian) %>%
        dplyr::rename(cucumber.beetle = Diabrotica.undecimpunctata..Cucumber.Beetle.) %>%
        dplyr::rename(Lygus.hesperus = Lygus.hesperus..western.tarnished.plant.bug.) %>%
        dplyr::filter(transect=="oakMargin") %>%
        dplyr::group_by(week) %>%
        dplyr::select(-transect) %>%
        dplyr::summarise_all(funs(mean)) %>%
        dplyr::select(-week) 
    # now all columns are species, rows are 'week averages'

     ## Compute the correlation matrix
    sim.matrix <- as.matrix(oak.df)
    cormat <- round(cor(sim.matrix, method="spearman"),2)

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
    name="Spearman\nCorrelation") +
    labs(title= paste("Species Correlation Matrix (Spearman)\n", 
                      "transect: Oak Margin", sep="")) +
    # theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 36)) +
    theme(axis.text.y = element_text(size = 36)) +
    theme(plot.title = element_text(size=36)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    theme(  # https://www.r-graph-gallery.com/239-custom-layout-legend-ggplot2/
    legend.position = c(.2, .8),
    legend.justification = c(0, 1),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = unit(2, "cm")      # changes the size of the spectrum
    ) +
    theme(
      legend.title = element_text(
                            size = 30,
                            face = "italic",
                            colour = "black",
                            angle = 0
                          ),
      legend.text = element_text(
                            size = 30
                          )

      ) +
    coord_fixed()

    # Reorder the correlation matrix
    #cormat <- reorder_cormat(cormat)
    #upper_tri <- get_upper_tri(cormat)
    # Melt the correlation matrix
    #melted_cormat <- melt(upper_tri, na.rm = TRUE)




#########################################################################
#########################################################################

    control.df <- data %>% 
        dplyr::select(-row, -positionX, -position, -date, -time, -julian) %>%
        dplyr::rename(cucumber.beetle = Diabrotica.undecimpunctata..Cucumber.Beetle.) %>%
        dplyr::rename(Lygus.hesperus = Lygus.hesperus..western.tarnished.plant.bug.) %>%
        dplyr::filter(transect=="control") %>%
        dplyr::group_by(week) %>%
        dplyr::select(-transect) %>%
        dplyr::summarise_all(funs(mean)) %>%
        dplyr::select(-week) 
    # now all columns are species, rows are 'week averages'

     ## Compute the correlation matrix
    sim.matrix <- as.matrix(control.df)
    cormat <- round(cor(sim.matrix, method="spearman"),2)

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
    name="Spearman\nCorrelation") +
    labs(title= paste("Species Correlation Matrix (Spearman)\n", 
                      "transect: Control", sep="")) +
    # theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 36)) +
    theme(axis.text.y = element_text(size = 36)) +
    theme(plot.title = element_text(size=36)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    theme(  # https://www.r-graph-gallery.com/239-custom-layout-legend-ggplot2/
    legend.position = c(.2, .8),
    legend.justification = c(0, 1),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = unit(2, "cm")      # changes the size of the spectrum
    ) +
    theme(
      legend.title = element_text(
                            size = 30,
                            face = "italic",
                            colour = "black",
                            angle = 0
                          ),
      legend.text = element_text(
                            size = 30
                          )

      ) +
    #guides(color = guide_legend(override.aes = list(size = 5))) +
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


  reorder_cormat <- function(cormat){
# Use correlation between variables as distance
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]
}


simMatrixV2 <- function(data, transect, transectText) {

    # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

    # return grob : side-by-side similarity matricies for the two transects

# produce correlation coefficients between the possible pairs of variables 
# cor(matrix, method = c("pearson", "kendall", "spearman"))
# ?cor
# ... For cor(), if method is "kendall" or "spearman", Kendall's tau or Spearman's rho 
# statistic is used to estimate a rank-based measure of association. These are 
# more robust and have been recommended if the data do not necessarily come from 
# a bivariate normal distribution.

# Correlation matrix list with significance levels (coefficients matrix plus p-value matrix)
# > ?rcorr
# .... Spearman correlations are the Pearson linear correlations computed on the ranks of 
# non-missing elements, using midranks for ties.
  
    library("dplyr")
    library("reshape2")
    library("ggplot2")
    library("gridExtra")
    library("grid")
  
    ## get the insect data by transect and by week
  
    # https://stackoverflow.com/questions/21644848/summarizing-multiple-columns-with-dplyr
    oak.df <- data %>% 
        dplyr::select(-row, -positionX, -position, -date, -time, -julian) %>%
        dplyr::rename(cucumber.beetle = Diabrotica.undecimpunctata..Cucumber.Beetle.) %>%
        dplyr::rename(Lygus.hesperus = Lygus.hesperus..western.tarnished.plant.bug.) %>%
        dplyr::filter(transect==UQ(transect)) %>%
        dplyr::group_by(week) %>%
        dplyr::select(-transect) %>%
        dplyr::summarise_all(funs(mean)) %>%
        dplyr::select(-week) 
    # now all columns are species, rows are 'week averages'

     ## Compute the correlation matrix
    sim.matrix <- as.matrix(oak.df)
    cormat <- round(cor(sim.matrix, method="spearman"),2)

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
  sim <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="correlation") +
    labs(title= paste("Species Correlation Matrix (Spearman)\n", 
                      "transect: ", transectText, sep="")) +
    # theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    #theme(  # https://www.r-graph-gallery.com/239-custom-layout-legend-ggplot2/
    #legend.position = c(.2, .8),
    #legend.justification = c(0, 1),
    #legend.box.just = "left",
    #legend.margin = margin(6, 6, 6, 6),
    #legend.key.size = unit(2, "cm")      # changes the size of the spectrum
    #) +
    theme(
      legend.title = element_text(colour = "black", angle = 0),
      legend.text = element_text() ) +
    coord_fixed(ratio=1)

    # Reorder the correlation matrix
    #cormat <- reorder_cormat(cormat)
    #upper_tri <- get_upper_tri(cormat)
    # Melt the correlation matrix
    #melted_cormat <- melt(upper_tri, na.rm = TRUE)



  return(grid.arrange(sim, ncol=1, nrow=1))

}
