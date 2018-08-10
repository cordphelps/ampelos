
countRowSpecies <- function(data, row) {
  j <- 0
  for (i in 1:ncol(data)) {
          #if (unlist(as.data.frame(t(data))[,row][i]) > 0 ) {
      if (as.integer(unlist(data[row,][i])) > 0 ) {
              j <- j +1
           }
  }
  return(j)
}

bugsOnlyByWeek <- function(data, iB, tR, w) {

  
  #
  # this function is intended to be called from ampelos.Rmd
  #

  #data <- bugs.df
  #ignoreBees <- FALSE
  #t <- "control"
  #w <- 26


  weeks.vector <- getWeeks(data)                    # determine weeks in the dataset
  weeks.df <- dplyr::bind_cols(week = weeks.vector) # place weeks in a column

  data <- data %>%  dplyr::rename(
                        Lygus.hesperus = Lygus.hesperus..western.tarnished.plant.bug., 
                        cucumber.beetle = Diabrotica.undecimpunctata..Cucumber.Beetle.) 

  if (iB == TRUE) { 
    data <- data %>% dplyr::select( 
      -Agapostemon.sp....green..native.bee.,
      -Bombus.californicus..bumble.,
      -Halictus.sp....3.part..native.bee.,
      -Honey.Bee,
      -Osmia.sp...native.bee.)
  }

  data <- data %>% 
    
    dplyr::filter(transect == tR) %>% 
    dplyr::select(-row, -position, -positionX, -transect, -julian, -time, -date)  %>%
    dplyr::group_by(week)  %>%
    dplyr::summarise_all(sum)
     
  
  return(as.data.frame(data))
}



countRowIndividuals <- function(data,row) {
  
  return(sum(unlist(as.data.frame(t(data))[,row])[1:ncol(data)]))
}

plotDivIndividuals <- function(df, titleText, subText, captionText) {


  gg <- ggplot(df) + 

      # geom_jitter(aes(x=rowCounts, y=rowSum, col=week), width = 0.1, height = 0.1, show.legend = TRUE, shape = 21, size=5, colour = "mediumvioletred", fill = "plum1") + 
      
      geom_jitter(aes(x=week, y=rowSum), width = 0.1, height = 0.1, show.legend = TRUE, shape = 21, size=5, colour = "mediumvioletred", fill = "plum1") + 
      

      ylim(c(0, 500)) +
      expand_limits(y=c(0,500)) +
      scale_y_continuous(breaks = seq(min(0), max(500), by = 50)) +
    
      xlim(c(22, 40)) + 
      expand_limits(x=c(22,40)) +
      scale_x_continuous(breaks = seq(min(22), max(40), by = 5)) +
    
      coord_fixed(ratio=.05) +  # control the aspect ratio of the output 
    
    #geom_smooth(aes(x=rowCounts, y=rowSum), method = "loess", size = 1.5) +
    #geom_smooth(aes(x=rowCounts, y=rowSum), method="lm", level=0.95) +
    #geom_smooth(aes(x=rowCounts, y=rowSum, colour = 'Exponential'), method = 'nls', formula = y ~ a*exp(b * x), se = FALSE, start = list(a=1,b=1), fullrange = T) +  
       
      labs(title=paste(titleText, sep=""),
        subtitle=paste(subText, sep=""), 
          y="total number of individuals in sample", 
          #x="total number of 'vane trap apparent' species in sample", 
          x="week",
          caption = paste(captionText, sep="") ) +
      #theme(legend.position="none") +
      theme_bw() +
      theme(legend.position = "bottom", legend.direction = "horizontal") 
      
      # https://stackoverflow.com/questions/7056836/how-to-fix-the-aspect-ratio-in-ggplot

  return(grid.arrange(gg, ncol=1, nrow=1))

  }

plotDivSpecies <- function(df, titleText, subText, captionText) {


  gg <- ggplot(df) + 

      # geom_jitter(aes(x=rowCounts, y=rowSum, col=week), width = 0.1, height = 0.1, show.legend = TRUE, shape = 21, size=5, colour = "mediumvioletred", fill = "plum1") + 
      
      geom_jitter(aes(x=week, y=rowCounts), width = 0.1, height = 0.1, show.legend = TRUE, shape = 21, size=5, colour = "mediumvioletred", fill = "plum1") + 
      

      ylim(c(0, 30)) +
      expand_limits(y=c(0,30)) +
      scale_y_continuous(breaks = seq(min(0), max(30), by = 5)) +
    
      xlim(c(22, 40)) + 
      expand_limits(x=c(22,40)) +
      scale_x_continuous(breaks = seq(min(22), max(40), by = 5)) +
    
      coord_fixed(ratio=.5) +  # control the aspect ratio of the output 
    
    #geom_smooth(aes(x=rowCounts, y=rowSum), method = "loess", size = 1.5) +
    #geom_smooth(aes(x=rowCounts, y=rowSum), method="lm", level=0.95) +
    #geom_smooth(aes(x=rowCounts, y=rowSum, colour = 'Exponential'), method = 'nls', formula = y ~ a*exp(b * x), se = FALSE, start = list(a=1,b=1), fullrange = T) +  
       
      labs(title=paste(titleText, sep=""),
        subtitle=paste(subText, sep=""), 
          y="total number of species in sample", 
          #x="total number of 'vane trap apparent' species in sample", 
          x="week",
          caption = paste(captionText, sep="") ) +
      #theme(legend.position="none") +
      theme_bw() +
      theme(legend.position = "bottom", legend.direction = "horizontal") 
      
      # https://stackoverflow.com/questions/7056836/how-to-fix-the-aspect-ratio-in-ggplot

  return(grid.arrange(gg, ncol=1, nrow=1))

  }

div <- function(data, species, ignoreBees, t) {
  
  #data <- test.df
  #data <- bugs.df
  
  data <- bugsOnlyByWeek(data, iB=ignoreBees, tR=t)
  
  weeks.df <- data %>%     # just weeks
    dplyr::select(week) 
  
  data <- data %>%        # bugs only
    dplyr::select(-week)
  
  rowSum <- NULL
  rowCounts <- NULL
  for (row in 1:nrow(data)) {
    rowSum[row] <- countRowIndividuals(data, row)
    rowCounts[row] <- countRowSpecies(data, row) 
  }
  
  input.df <- cbind(data, 
                    data.frame(rowSum), 
                    data.frame(rowCounts),
                    weeks.df)
  
  if (species == FALSE) {

    plotDivIndividuals(input.df, 
                paste("weekly individual count diversity: ", "\ntransect ", t, sep=""), 
                paste("ignoreBees : ", ignoreBees, sep=""),
                "caption")
  } else {

    plotDivSpecies(input.df, 
                paste("weekly species diversity ", "\ntransect ", t, sep=""), 
                paste("ignoreBees : ", ignoreBees, sep=""),
                "caption")

  }

}
