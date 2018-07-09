
library(ggplot2)
library(ggridges)
library(gridExtra)

library(dplyr)
library(rlang) # see sym()
#Attaching package: ‘dplyr’
#The following objects are masked from ‘package:stats’:
#    filter, lag
#The following objects are masked from ‘package:base’:
#    intersect, setdiff, setequal, union
# protect dplyr function select()
# https://stackoverflow.com/questions/35839408/r-dplyr-drop-multiple-columns
#detach( "package:MASS", unload = TRUE )
#detach( "package:skimr", unload = TRUE )
# get loaded packages with sessionInfo()
# get versions: packageVersion("dplyr")
library(tidyverse)
library(knitr)
library(kableExtra)

plotRidges <- function(data, combined, bugs, speciesText, where, when, wk, caption) {


  # https://stackoverflow.com/questions/7310186/function-in-r-passing-a-dataframe-and-a-column-name

  if (FALSE) {   # available for debug
    # https://stackoverflow.com/questions/9726705/assign-multiple-objects-to-globalenv-from-within-a-function
    assign("data", data, envir=.GlobalEnv)
    assign("bugs", bugs, envir=.GlobalEnv)
    assign("where", where, envir=.GlobalEnv)
    assign("when", when, envir=.GlobalEnv)
    assign("wk", wk, envir=.GlobalEnv)
    assign("caption", caption, envir=.GlobalEnv)
  }
  
  # https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
  
  if (wk < 23 | wk > 52) {  # we definitely don't have a valid week
    # this case indicates 'use data from all weeks'
    cumulative <- "cumulative"
    
    if (when != "am" & when != "pm") {    # use all the data (am and pm) for each day
      filteredBugs.df <- filter(data, transect == where)
    } else {                              # use partial data (am or pm) for each day
      filteredBugs.df <- filter(data, transect == where & time == when)
    }
    
  } else {  #  we might have a 'valid' week (data for the specified week could be
    #  missing....)
    cumulative <- as.character(wk)
    
    if (when != "am" & when != "pm") {   # use all the data (am and pm) for each day
      filteredBugs.df <- filter(data, transect == where & week == wk)
    } else {                             # use partial data (am or pm) for each day
      filteredBugs.df <- filter(data, transect == where & time == when & week == wk)
    }
    
    
  }
  
  
  # simplify to include the trap position and the bug in the list
  newBugs.df <- subset(filteredBugs.df, select= c("positionX", bugs))
  
  ################################################################################################
  # get some stats to add to the plot

  # note: nuance of using the > operator and sum()
  # As a result you can SUM over this to find the number of values which are TRUE (>2000), 
  # ie Count. While you may have been expecting this input to SUM the actual values themselves
  # https://stackoverflow.com/questions/22690255/count-observations-greater-than-a-particular-value

  spider_rows <- nrow(newBugs.df)
  trapsWithSpiders <- nrow(newBugs.df[newBugs.df[,bugs] > 0, ])
  # trapsWithSpiders <- sum(newBugs.df[,bugs])
  percentOcurrence <- (trapsWithSpiders / spider_rows) * 100
  # https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
  percentOcurrence <- format(round(percentOcurrence, 2), nsmall = 2)

  ################################################################################################
  
  # get factors for geom_density_ridges
  # (grouping by "count")
  factor.list <- newBugs.df[,bugs]   #     
  newBugs.df$geomFactors <- NULL                        
  newBugs.df$geomFactors <- as.factor(factor.list)
  
  assign("newBugs.df", newBugs.df, envir=.GlobalEnv)
  
  #Density plots can be thought of as plots of smoothed histograms.
  #The smoothness is controlled by a bandwidth parameter that is analogous 
  #to the histogram binwidth.
  #Most density plots use a kernel density estimate, but there are other 
  #possible strategies; qualitatively the particular strategy rarely matters.
  # https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/histdens.html
  
  if (combined == FALSE) {
    # the x axis data is multi-level
  gg2 <- ggplot(newBugs.df, aes_string(x="positionX", y="geomFactors", fill="geomFactors")) +
    geom_density_ridges(
      #aes(point_color = spider, point_fill=spider, point_shape=spider),
      # https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot
      aes_string(point_color = "geomFactors", point_fill="geomFactors", point_shape="geomFactors"),
      alpha = .2, jittered_points = TRUE, show.legend=F) +
    scale_point_color_hue(l = 40)  +
    scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23, 24, 25)) +
    #stat_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = .2, jittered_points = TRUE) +
    
    xlim(1,10) +
    # http://ggplot2.tidyverse.org/reference/sec_axis.html
    scale_x_continuous(breaks=seq(4,200,16), 
                       sec.axis = sec_axis(~.*.3048,
                                           breaks= seq(0, 80, 10),
                                           name= "trap distance from row start (m)"))  +
    labs(title= paste(speciesText, " Probability Density\n", 
                      "transect: ", where, sep=""), 
         subtitle = paste("week: ", cumulative, ", collection time: ", when, 
                          "\ntotal observations: ", spider_rows,
                          "\ntraps with ", speciesText, "s: ", percentOcurrence, " %", 
                          
                          sep=""),
         x="trap distance from row start (ft)",
         y= paste(speciesText, "\ncounts per trap", sep=""),
         #caption="10 June 2018")
         caption=paste(caption, 
                       "\nhttps://en.wikipedia.org/wiki/Kernel_density_estimation", 
                       sep="")) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))

  } else {   # the x axis data combined for values > 0 ; adjust labels with scale_y_discrete()

    gg2 <- ggplot(newBugs.df, aes_string(x="positionX", y="geomFactors", fill="geomFactors")) +
    geom_density_ridges(
      aes_string(point_color = "geomFactors", point_fill="geomFactors", point_shape="geomFactors"),
      alpha = .2, jittered_points = TRUE, show.legend=F) +
    scale_point_color_hue(l = 40)  +
    scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23, 24, 25)) +
    xlim(1,10) +
    # http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels#change-tick-mark-labels
    scale_y_discrete(labels=c("0" = "none", "1" = "multiple")) +
    scale_x_continuous(breaks=seq(4,200,16), 
                       sec.axis = sec_axis(~.*.3048,
                                           breaks= seq(0, 80, 10),
                                           name= "trap distance from row start (m)"))  +
    labs(title= paste(speciesText, " Probability Density\n", 
                      "transect: ", where, sep=""), 
         subtitle = paste("week: ", cumulative, ", collection time: ", when, 
                          "\ntotal observations: ", spider_rows,
                          "\ntraps with ", speciesText, "s: ", percentOcurrence, " %", 
                          
                          sep=""),
         x="trap distance from row start (ft)",
         y= paste(speciesText, "\ncounts per trap", sep=""),
         #caption="10 June 2018")
         caption=paste(caption, 
                       "\nhttps://en.wikipedia.org/wiki/Kernel_density_estimation", 
                       sep="")) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))

  }
  
  # grid.arrange(gg1, gg2, ncol=1, nrow=2)
  # ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")
  
  return(gg2)
}


plotSpeciesTrend <- function(data, bugs, speciesText, where, when, caption) {
  # bugs has to come in as a pointer
  # speciesText is presentable text for the plot labels, 'quo_name(bugs)' gets the mashed df column label
  
  # https://stackoverflow.com/questions/9726705/assign-multiple-objects-to-globalenv-from-within-a-function
  # assign("data", data, envir=.GlobalEnv)
  # assign("bugs", bugs, envir=.GlobalEnv)
  # assign("where", where, envir=.GlobalEnv)
  # assign("when", when, envir=.GlobalEnv)
  # assign("wk", wk, envir=.GlobalEnv)
  # assign("caption", caption, envir=.GlobalEnv)
  

  
  # https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
  # dyplr group: https://stackoverflow.com/questions/48714625/error-in-data-frame-unused-argument
  # dyplr multiple groups: https://stackoverflow.com/questions/21653295/dplyr-issues-when-using-group-bymultiple-variables
  

  ########### stand-alone dplyr test code ##########
  if (FALSE) {   # this is essentially a multi-line comment
    temp.df <- bugs.df %>%
      filter( time == "pm",  transect == "oakMargin") %>%
      group_by( week ) %>%
      summarise( sp_by_week = sum( Thomisidae..crab.spider. , na.rm = TRUE ) , n=n()) %>%
      mutate(ave_per_week = sp_by_week / (n / 30))

      sp_percent <- sum(temp.df$sp_by_week) / sum(temp.df$n)

    new.df <- bugs.df %>% mutate(newColumn = ifelse(Thomisidae..crab.spider. > 0, 1, 0))



  }
  ########### end stand-alone dplyr test code ##########


  temp.df <- bugs.df %>%
    #filter( time == "pm",  transect == "oakMargin") %>%
    filter(transect == "oakMargin") %>%
    group_by( week ) %>%
    summarise( sp_by_week = sum( !!bugs , na.rm = TRUE ) , n=n()) %>%
    mutate(ave_per_week = sp_by_week / (n / 30))
  
  # get statistics
  sp_percentOak <- sum(temp.df$sp_by_week) / sum(temp.df$n) * 100
  sp_percentOak <- format(round(sp_percentOak, 2), nsmall = 2)

  temp.df <- bugs.df %>%
    filter( time == "pm",  transect == "control") %>%
    group_by( week ) %>%
    summarise( sp_by_week = sum( !!bugs , na.rm = TRUE ) , n=n()) %>%
    mutate(ave_per_week = sp_by_week / (n / 30))
  
  # get statistics
  sp_percentControl <- sum(temp.df$sp_by_week) / sum(temp.df$n) * 100
  sp_percentControl <- format(round(sp_percentControl, 2), nsmall = 2)
  
  
  oakPM.df <- data %>%
    filter( time == "pm",  transect == "oakMargin") %>%
    group_by( week ) %>%
    summarise( oakPMtotal = sum( !!bugs , na.rm = TRUE ) ) 
  
  controlPM.df <- data %>%
    filter( time == "pm",  transect == "control") %>%
    group_by( week ) %>%
    summarise( controlPMtotal = sum( !!bugs , na.rm = TRUE ) ) 
  
  # join the data frames
  pm.df <- merge(oakPM.df, controlPM.df)
  
  oakAM.df <- data %>%
    filter( time == "am",  transect == "oakMargin") %>%
    group_by( week ) %>%
    summarise( oakAMtotal = sum( !!bugs , na.rm = TRUE ) ) 
  
  controlAM.df <- data %>%
    filter( time == "am",  transect == "control") %>%
    group_by( week ) %>%
    summarise( controlAMtotal = sum( !!bugs , na.rm = TRUE ) )  
  
  # join the data frames
  am.df <- merge(oakAM.df, controlAM.df)
  
  sliced.df <- merge(am.df, pm.df)
    
  assign("sliced.df", sliced.df, envir=.GlobalEnv)
  
  ggOAK <- ggplot(sliced.df, aes(x=week, y=oakPMtotal)) +
    geom_point(shape=21) +
    geom_line(aes(x=week, y=oakPMtotal, colour="afternoon")) +
    geom_point(aes(x=week, y=oakAMtotal), shape=22) +
    geom_line(aes(x=week, y=oakAMtotal, colour="morning")) +   
    ylim(0,100) +
    labs(title= paste(speciesText, " Population Counts", 
                      "\ntransect: oakMargin", sep=""), 
         subtitle = paste("\ntraps with ", speciesText, "s: ", sp_percentOak, " %", 
                          sep=""),
         x="week number",
         y= "weekly total",
         caption=paste(caption, "\n(NO CAPTION)", sep="")) +
    # https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r
    scale_colour_manual(values=c(afternoon="red", morning="blue")) + 
    # https://stackoverflow.com/questions/47584766/draw-a-box-around-a-legend-ggplot2
    theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
    # Put bottom-right corner of legend box in bottom-right corner of graph
    theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))

    ggCONTROL <- ggplot(sliced.df, aes(x=week, y=controlPMtotal)) +
      geom_point(shape=21) +
      geom_line(aes(x=week, y=controlPMtotal, colour="afternoon")) +
      geom_point(aes(x=week, y=controlAMtotal), shape=22) +
      geom_line(aes(x=week, y=controlAMtotal, colour="morning")) +   
      ylim(0,100) +
      labs(title= paste(speciesText, " Population Counts",
                        "\ntransect: control", sep=""), 
           subtitle = paste("\ntraps with ", speciesText, "s: ", sp_percentControl, " %", 
                           sep=""),
         x="week number",
         y= "weekly total",
         caption=paste(caption, "\n(NO CAPTION)", sep="")) +
    # https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r
    scale_colour_manual(values=c(afternoon="red", morning="blue")) + 
    # https://stackoverflow.com/questions/47584766/draw-a-box-around-a-legend-ggplot2
    theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
    # Put bottom-right corner of legend box in bottom-right corner of graph
    theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))
    
  grid.arrange(ggOAK, ggCONTROL, ncol=2, nrow=1)



  ######################################################################################
  ######################################################################################
  ######################################################################################

  ######## develop the same graphs, but by day (not week)

  oakPM.df <- data %>%
    filter( time == "pm",  transect == "oakMargin") %>%
    group_by( julian ) %>%
    summarise( oakPMtotal = sum( !!bugs , na.rm = TRUE ) ) 
  
  controlPM.df <- data %>%
    filter( time == "pm",  transect == "control") %>%
    group_by( julian ) %>%
    summarise( controlPMtotal = sum( !!bugs , na.rm = TRUE ) ) 
  
  # join the data frames
  pm.df <- merge(oakPM.df, controlPM.df)
  
  oakAM.df <- data %>%
    filter( time == "am",  transect == "oakMargin") %>%
    group_by( julian ) %>%
    summarise( oakAMtotal = sum( !!bugs , na.rm = TRUE ) ) 
  
  controlAM.df <- data %>%
    filter( time == "am",  transect == "control") %>%
    group_by( julian ) %>%
    summarise( controlAMtotal = sum( !!bugs , na.rm = TRUE ) )  
  
  # join the data frames
  am.df <- merge(oakAM.df, controlAM.df)
  
  sliced.df <- merge(am.df, pm.df)
    
  assign("sliced.df", sliced.df, envir=.GlobalEnv)
  
  ggOAK <- ggplot(sliced.df, aes(x=julian, y=oakPMtotal)) +
    geom_point(shape=21) +
    geom_line(aes(x=julian, y=oakPMtotal, colour="afternoon")) +
    geom_point(aes(x=julian, y=oakAMtotal), shape=22) +
    geom_line(aes(x=julian, y=oakAMtotal, colour="morning")) +   
    ylim(0,100) +
    labs(title= paste(speciesText, " Population Counts", 
                      "\ntransect: oakMargin", sep=""), 
         subtitle = paste("\ntraps with ", speciesText, "s: ", sp_percentOak, " %", 
                          sep=""),
         x="julian day",
         y= "daily total",
         caption=paste(caption, "\n(NO CAPTION)", sep="")) +
    # https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r
    scale_colour_manual(values=c(afternoon="red", morning="blue")) + 
    # https://stackoverflow.com/questions/47584766/draw-a-box-around-a-legend-ggplot2
    theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
    # Put bottom-right corner of legend box in bottom-right corner of graph
    theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))

    ggCONTROL <- ggplot(sliced.df, aes(x=julian, y=controlPMtotal)) +
      geom_point(shape=21) +
      geom_line(aes(x=julian, y=controlPMtotal, colour="afternoon")) +
      geom_point(aes(x=julian, y=controlAMtotal), shape=22) +
      geom_line(aes(x=julian, y=controlAMtotal, colour="morning")) +   
      ylim(0,100) +
      labs(title= paste(speciesText, " Population Counts",
                        "\ntransect: control", sep=""), 
           subtitle = paste("\ntraps with ", speciesText, "s: ", sp_percentControl, " %", 
                           sep=""),
         x="julian day",
         y= "daily total",
         caption=paste(caption, "\n(NO CAPTION)", sep="")) +
    # https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r
    scale_colour_manual(values=c(afternoon="red", morning="blue")) + 
    # https://stackoverflow.com/questions/47584766/draw-a-box-around-a-legend-ggplot2
    theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
    # Put bottom-right corner of legend box in bottom-right corner of graph
    theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))
    
  grid.arrange(ggOAK, ggCONTROL, ncol=2, nrow=1)




  # ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")
  
  return()
}

compareTransect <- function () {


#####################################################
  # https://stackoverflow.com/questions/29554796/meaning-of-band-width-in-ggplot-geom-smooth-lm


    oak.df <- bugs.df %>% 
    dplyr::filter(position<4, time=="pm", transect=="oakMargin") %>% 
    dplyr::group_by(julian) %>% 
    dplyr::summarise(oakEdgeMean=mean(Thomisidae..crab.spider.))

  center.df <- bugs.df %>% 
    dplyr::filter(position<4, time=="pm", transect=="control") %>% 
    dplyr::group_by(julian) %>% 
    dplyr::summarise(controlEdgeMean=mean(Thomisidae..crab.spider.))

    combo.df <- merge(oak.df, center.df)

  combo.df <- combo.df %>% 
    dplyr::mutate(deltaMean=(oakEdgeMean-controlEdgeMean)/controlEdgeMean)

  assign("combo.df", combo.df, envir=.GlobalEnv)

    ggCompare1 <- ggplot(combo.df, aes(x=julian, y=deltaMean)) +
      geom_point(shape=21) + 
      geom_smooth(method="lm", level=0.95) +  
      geom_hline(yintercept=0) +
      ylim(-1.1,1.1) +
      labs(title= paste("average spiders per trap ",
                        "\ntransect positions 1 - 3",
                        "\noakMargin as a fraction of control", sep=""), 
           subtitle = paste("(95% confidence interval)", sep=""),
         x="julian day",
         y= "oakMargin fraction of control",
         caption=paste("120 observations per day", "\npositions 1 - 10 inclusive", sep="")) +
    # https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r
    scale_colour_manual(values=c(afternoon="red", morning="blue")) + 
    # https://stackoverflow.com/questions/47584766/draw-a-box-around-a-legend-ggplot2
    theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
    # Put bottom-right corner of legend box in bottom-right corner of graph
    theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))
    
  grid.arrange(ggCompare1, ncol=1, nrow=1)


    oak.df <- bugs.df %>% 
    dplyr::filter(between(position, 3, 7), time=="pm", transect=="oakMargin") %>% 
    dplyr::group_by(julian) %>% 
    dplyr::summarise(oakEdgeMean=mean(Thomisidae..crab.spider.))

  center.df <- bugs.df %>% 
    dplyr::filter(between(position, 3, 7), time=="pm", transect=="control") %>% 
    dplyr::group_by(julian) %>% 
    dplyr::summarise(controlEdgeMean=mean(Thomisidae..crab.spider.))

    combo.df <- merge(oak.df, center.df)

  combo.df <- combo.df %>% 
    dplyr::mutate(deltaMean=(oakEdgeMean-controlEdgeMean)/controlEdgeMean)

  assign("combo.df", combo.df, envir=.GlobalEnv)

    ggCompare2 <- ggplot(combo.df, aes(x=julian, y=deltaMean)) +
      geom_point(shape=21) + 
      geom_smooth(method="lm", level=0.95) +  
      geom_hline(yintercept=0) +
      ylim(-1.1,1.1) +
      labs(title= paste("average spiders per trap ",
                        "\ntransect positions 4 - 6",
                        "\noakMargin as a fraction of control", sep=""), 
           subtitle = paste("(95% confidence interval)", sep=""),
         x="julian day",
         y= "oakMargin fraction of control",
         caption=paste("120 observations per day", "\npositions 1 - 10 inclusive", sep="")) +
    # https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r
    scale_colour_manual(values=c(afternoon="red", morning="blue")) + 
    # https://stackoverflow.com/questions/47584766/draw-a-box-around-a-legend-ggplot2
    theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
    # Put bottom-right corner of legend box in bottom-right corner of graph
    theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))
    
  grid.arrange(ggCompare2, ncol=1, nrow=1)


    oak.df <- bugs.df %>% 
    dplyr::filter(position>6, time=="pm", transect=="oakMargin") %>% 
    dplyr::group_by(julian) %>% 
    dplyr::summarise(oakEdgeMean=mean(Thomisidae..crab.spider.))

  center.df <- bugs.df %>% 
    dplyr::filter(position>6, time=="pm", transect=="control") %>% 
    dplyr::group_by(julian) %>% 
    dplyr::summarise(controlEdgeMean=mean(Thomisidae..crab.spider.))

    combo.df <- merge(oak.df, center.df)

  combo.df <- combo.df %>% 
    dplyr::mutate(deltaMean=(oakEdgeMean-controlEdgeMean)/controlEdgeMean)



    ggCompare3 <- ggplot(combo.df, aes(x=julian, y=deltaMean)) +
      geom_point(shape=21) + 
      geom_smooth(method="lm", level=0.95) +  
      geom_hline(yintercept=0) +
      ylim(-1.1,1.1) +
      labs(title= paste("average spiders per trap ",
                        "\ntransect positions 7 - 10",
                        "\noakMargin as a fraction of control", sep=""), 
           subtitle = paste("(95% confidence interval)", sep=""),
         x="julian day",
         y= "oakMargin fraction of control",
         caption=paste("120 observations per day", "\npositions 1 - 10 inclusive", sep="")) +
    # https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r
    scale_colour_manual(values=c(afternoon="red", morning="blue")) + 
    # https://stackoverflow.com/questions/47584766/draw-a-box-around-a-legend-ggplot2
    theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
    # Put bottom-right corner of legend box in bottom-right corner of graph
    theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))
    
  grid.arrange(ggCompare3, ncol=1, nrow=1)

  g <- arrangeGrob(ggCompare1, ggCompare2, ggCompare3, nrow=3) #generates g
  
  return(g)

}

bugCount <- function() {
  
  total <- sum(bugs.df$DBfly, na.rm=TRUE) +
    sum(bugs.df$LBfly, na.rm=TRUE) +
    sum(bugs.df$X3partFly, na.rm=TRUE) +
    sum(bugs.df$houseFly, na.rm=TRUE) +
    sum(bugs.df$greenFly, na.rm=TRUE) +
    sum(bugs.df$wasp, na.rm=TRUE) +
    sum(bugs.df$wildBee, na.rm=TRUE) +
    sum(bugs.df$bumble, na.rm=TRUE) +
    sum(bugs.df$spider, na.rm=TRUE) +
    sum(bugs.df$spiderJumping, na.rm=TRUE) +
    sum(bugs.df$ladyBug, na.rm=TRUE) +
    sum(bugs.df$hopper, na.rm=TRUE) +
    sum(bugs.df$ant, na.rm=TRUE) +
    sum(bugs.df$other, na.rm=TRUE) +
    sum(bugs.df$butterFly, na.rm=TRUE) +
    sum(bugs.df$microMoth, na.rm=TRUE) +
    sum(bugs.df$cucumberBeetle, na.rm=TRUE)
  
  return(total)
  
}

bugNames <- function(df) {
  #column names to be ignored
  ignore <- c("transect", "row", "position",
              "date", "time", "julian", "week",
              "positionX")
  df[ignore] <- NULL
  return(as.list(colnames(df)))
}

bigTable <- function(df) {
  # https://rdrr.io/cran/dplyr/man/summarise_all.html 
  # https://stackoverflow.com/questions/9723208/aggregate-summarize-multiple-variables-per-group-e-g-sum-mean
  
  list <- as.character(bugNames(df))
  charVector <- as.character(c("sum", "max"))
  df2 <- df  %>% summarise_at(list, funs(sum, max))
  
  # https://stackoverflow.com/questions/35839408/r-dplyr-drop-multiple-columns
  #iris %>% select(-one_of(drop.cols))
  
  
  return(df2)
}



compareTransectUsingQuosure <- function (data, species, operator, initialPosition, secondaryPosition, positionText) {

  # Programming with dplyr, Different input variable
  # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

  # This function provides the flexibility to configure the dplyr::filter() position argument on-the-fly,
  # we need to support the following cases
  #
  # filter(position < initialPosition)
  # filter(position > initialPosition)
  # filter(between(position, initialPosition, secondaryPosition))

  #####################################################
  # https://stackoverflow.com/questions/29554796/meaning-of-band-width-in-ggplot-geom-smooth-lm

  #species <- enquo(species)
  #initialPosition <- enquo(initialPosition)
  #secondaryPosition <- enquo(secondaryPosition)

  if (FALSE) {   # available for debug
    # https://stackoverflow.com/questions/9726705/assign-multiple-objects-to-globalenv-from-within-a-function
    assign("data", data, envir=.GlobalEnv)
    assign("species", species, envir=.GlobalEnv)
    assign("operator", operator, envir=.GlobalEnv)
    assign("initialPosition", initialPosition, envir=.GlobalEnv)
    assign("secondaryPosition", secondaryPosition, envir=.GlobalEnv)
  }

  if (operator == "BETWEEN") {

      oak.df <- data %>% 
      dplyr::filter(between(position, !!initialPosition , !!secondaryPosition), time=="pm", transect=="oakMargin") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(oakEdgeMean=mean(!!species))

      center.df <- data %>% 
      dplyr::filter(between(position, !!initialPosition , !!secondaryPosition), time=="pm", transect=="control") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(controlEdgeMean=mean(!!species))
      

  } else {

    if (operator == "LT") {

      oak.df <- data %>% 
      dplyr::filter(position < !! initialPosition, time=="pm", transect=="oakMargin") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(oakEdgeMean=mean(!!species))

      center.df <- data %>% 
      dplyr::filter(position < !! initialPosition, time=="pm", transect=="control") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(controlEdgeMean=mean(!!species))

      ip <- initialPosition 
      positionString <- paste("\ntransect positions < ", ip , sep="")

    } else if (operator == "GT") {

      oak.df <- data %>% 
      dplyr::filter(position > !! initialPosition, time=="pm", transect=="oakMargin") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(oakEdgeMean=mean(!! species))

      center.df <- data %>% 
      dplyr::filter(position > !! initialPosition, time=="pm", transect=="control") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(controlEdgeMean=mean(!! species))

      ip <- initialPosition
      positionString <- paste("\ntransect positions > ", ip , sep="")

    } else {

      message( paste("compareTransectUsingQuosure(): ", operator, " not supported", sep=""))
      stop()

    }

  }

  combo.df <- merge(oak.df, center.df)

  combo.df <- combo.df %>% 
  dplyr::mutate(deltaMean=(oakEdgeMean-controlEdgeMean)/controlEdgeMean)

  assign("combo.df", combo.df, envir=.GlobalEnv)


  ggCompare1 <- ggplot(combo.df, aes(x=julian, y=deltaMean)) +
      geom_point(shape=21) + 
      geom_smooth(method="lm", level=0.95) +  
      geom_hline(yintercept=0) +
      ylim(-1.1,1.1) +
      labs(title= paste("average spiders per trap ",
                        positionText,
                        "\noakMargin as a fraction of control", sep=""), 
           subtitle = paste("(95% confidence interval)", sep=""),
         x="julian day",
         y= "oakMargin fraction of control",
         caption=paste("120 observations per day", "\npositions 1 - 10 inclusive", sep="")) +
    # https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r
    scale_colour_manual(values=c(afternoon="red", morning="blue")) + 
    # https://stackoverflow.com/questions/47584766/draw-a-box-around-a-legend-ggplot2
    theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
    # Put bottom-right corner of legend box in bottom-right corner of graph
    theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5))
    
  return(grid.arrange(ggCompare1, ncol=1, nrow=1))

}

