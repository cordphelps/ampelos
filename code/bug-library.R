
suppressPackageStartupMessages(library(dplyr))

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
      alpha = .2, jittered_points = TRUE, show.legend=F, scale = 0.9) +

    scale_point_color_hue(l = 40)  +
    scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23, 24, 25) ) +
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
                          "\ntotal ", speciesText,  " observations: ", spider_rows,
                          "\ntraps with ", speciesText, "s: ", percentOcurrence, " %", 
                          sep=""),
         x="trap distance from row start (ft)",
         y= paste(speciesText, "\ncounts per trap", sep=""),
         #caption="10 June 2018")
         caption=paste(caption, 
                       "\nhttps://en.wikipedia.org/wiki/Kernel_density_estimation", 
                       sep="")) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5)) +
    theme_bw()

  } else {   # the x axis data combined for values > 0 ; adjust labels with scale_y_discrete()

    gg2 <- ggplot(newBugs.df, aes_string(x="positionX", y="geomFactors", fill="geomFactors")) +
    geom_density_ridges(
      aes_string(point_color = "geomFactors", point_fill="geomFactors", point_shape="geomFactors"),
      alpha = .2, jittered_points = TRUE, show.legend=F, scale = 0.9) +
    scale_point_color_hue(l = 40)  +
    scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23, 24, 25)) +
    xlim(1,10) +
    # http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels#change-tick-mark-labels
    scale_y_discrete(labels=c("0" = "none", "1" = "one or more")) +
    scale_x_continuous(breaks=seq(4,200,16), 
                       sec.axis = sec_axis(~.*.3048,
                                           breaks= seq(0, 80, 10),
                                           name= "trap distance from row start (m)"))  +
    labs(title= paste(speciesText, " Probability Density\n", 
                      "transect: ", where, sep=""), 
         subtitle = paste("week: ", cumulative, ", collection time: ", when, 
                          "\ntotal ", speciesText,  " observations: ", spider_rows,
                          "\ntraps with ", speciesText, "s: ", percentOcurrence, " %", 
                          sep=""),
         x="trap distance from row start (ft)",
         y= paste(speciesText, "\ncounts per trap", sep=""),
         #caption="10 June 2018")
         caption=paste(caption, 
                       "\nhttps://en.wikipedia.org/wiki/Kernel_density_estimation", 
                       sep="")) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5)) +
    theme_bw()

  }
  
  # grid.arrange(gg1, gg2, ncol=1, nrow=2)
  # ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")
  
  return(gg2)
}



plotSpeciesTrendV2 <- function(data, bugs, speciesText, where, when, caption) {
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
    

  
  ggOAK <- ggSpeciesWeekTrend(sliced.df, j="week", PM="oakPMtotal", AM="oakAMtotal", st="Crab Spider", t="oakMargin", spct=sp_percentOak, caption="oCaption")

  ggCONTROL <- ggSpeciesWeekTrend(sliced.df, j="week", PM="controlPMtotal", AM="controlAMtotal", st="Crab Spider", t="control", spct=sp_percentControl, caption="cCaption")
  
    
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


  
  ggOAK <- ggSpeciesJulianTrend(sliced.df, j="julian", PM="oakPMtotal", AM="oakAMtotal", st="Crab Spider", t="oakMargin", spct=sp_percentOak, caption="oCaption")

  ggCONTROL <- ggSpeciesJulianTrend(sliced.df, j="julian", PM="controlPMtotal", AM="controlAMtotal", st="Crab Spider", t="control", spct=sp_percentControl, caption="cCaption")

    
  grid.arrange(ggOAK, ggCONTROL, ncol=2, nrow=1)




  # ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")
  
  return()
}

ggSpeciesJulianTrend <- function(df, j, PM, AM, st, t, spct, caption) {

  # https://stackoverflow.com/questions/19826352/pass-character-strings-to-ggplot2-within-a-function

  # note: these two functions can't be combined with logic to manipluate the labels
  # because labels can't be changed  ... https://ggplot2.tidyverse.org/reference/gg-add.html

  gg <- ggplot(df, aes_string(x=j, y=PM)) +
      geom_point(shape=21) +
      geom_line(aes_string(x=j, y=PM), colour="red") +
      geom_point(aes_string(x=j, y=AM), shape=22) +
      geom_line(aes_string(x=j, y=AM), colour="blue") +   
      ylim(0,100) +
      labs(title= paste(st, " Population Counts",
                        "\ntransect: ", t, sep=""), 
           subtitle = paste("\ntraps with ", st, "s: ", spct, " %", 
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
    theme(axis.title.y = element_text(angle = 90, vjust=.5)) +
    theme_bw() 

    return(gg)

}

ggSpeciesWeekTrend <- function(df, j, PM, AM, st, t, spct, caption) {

  # https://stackoverflow.com/questions/19826352/pass-character-strings-to-ggplot2-within-a-function

  # note: these two functions can't be combined with logic to manipluate the labels
  # because labels can't be changed  ... https://ggplot2.tidyverse.org/reference/gg-add.html
  gg <- ggplot(df, aes_string(x=j, y=PM)) +
      geom_point(shape=21) +
      geom_line(aes_string(x=j, y=PM), colour="red") +
      geom_point(aes_string(x=j, y=AM), shape=22) +
      geom_line(aes_string(x=j, y=AM), colour="blue") + 

      ylim(0,100) +

      #xlim(c(22, 40)) + 
      #expand_limits(x=c(22,40)) +
      #scale_x_continuous(breaks = seq(min(22), max(40), by = 5)) +
      scale_x_continuous(breaks = seq(min(20), max(40), by = 2), labels = fmt_dcimals(0)) +

      labs(title= paste(st, " Population Counts",
                        "\ntransect: ", t, sep=""), 
           subtitle = paste("\ntraps with ", st, "s: ", spct, " %", 
                           sep=""),
         x="week",
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
    theme(axis.title.y = element_text(angle = 90, vjust=.5)) +
    theme_bw() 

    return(gg)

}

fmt_dcimals <- function(decimals=0){
  # https://stackoverflow.com/questions/10035786/ggplot2-y-axis-label-decimal-precision
    function(x) format(x,nsmall = decimals,scientific = FALSE)
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




compareTransectG2V1 <- function (data, species, operator, initialPosition, secondaryPosition, positionText) {

  # Programming with dplyr, Different input variable (how and when to quote/unquote)
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
  
  #######################################
  # rMarkdown
  #
  #positionText <- paste("\ntransect positions ", "1-4", sep="")
  #g3 <- compareTransectG2V1(data=bugs.df, 
                            #species=quo(Thomisidae..crab.spider.), 
                            #operator="LT",
                            #initialPosition=quo(5), 
                            #secondaryPosition=quo(0),
                            #positionText)
  #positionText <- paste("\ntransect positions ", "5-7", sep="")
  #g46 <- compareTransectG2V1(data=bugs.df, 
                             #species=quo(Thomisidae..crab.spider.), 
                             #operator="BETWEEN",
                             #initialPosition=quo(4), 
                             #secondaryPosition=quo(8),
                             #positionText)
  #positionText <- paste("\ntransect positions ", "8-10", sep="")
  #g7 <- compareTransectG2V1(data=bugs.df, 
                            #species=quo(Thomisidae..crab.spider.), 
                            #operator="GT",
                            #initialPosition=quo(7), 
                            #secondaryPosition=quo(0),
                            #positionText)
  #
  #######################################

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
      # dplyr::filter(position < !! initialPosition, time=="pm", transect=="oakMargin") %>% 
      dplyr::filter(position < !! initialPosition, transect=="oakMargin") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(oakEdgeMean=mean(!!species))

      center.df <- data %>% 
      # dplyr::filter(position < !! initialPosition, time=="pm", transect=="control") %>% 
      dplyr::filter(position < !! initialPosition, transect=="control") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(controlEdgeMean=mean(!!species))

      ip <- initialPosition 
      positionString <- paste("\ntransect positions < ", ip , sep="")

    } else if (operator == "GT") {

      oak.df <- data %>% 
      dplyr::filter(position > !! initialPosition, transect=="oakMargin") %>% 
      dplyr::group_by(julian) %>% 
      dplyr::summarise(oakEdgeMean=mean(!! species))

      center.df <- data %>% 
      dplyr::filter(position > !! initialPosition, transect=="control") %>% 
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
    dplyr::mutate(deltaMean=as.numeric( oakEdgeMean/controlEdgeMean ) )
  
  combo.df <- combo.df %>% filter(!is.na(deltaMean), !is.infinite(deltaMean))

  
  # assign("combo.df", combo.df, envir=.GlobalEnv)  # for debugging
  # the dataframe contains columns 'julian', 'oakEdgeMean', 'controlEdgeMean', 'deltaMean'

  # arbitraty week groupings (from bayes.R): "weeks 23-25",  "weeks 26-30", "weeks 31-34"
  #
  # "weeks 23-25" : julian range 155 - 175
  # "weeks 26-30" : julian range 176 - 210
  # "weeks 31-34" : julian range 211 - 238

  # add a 'julianGroup' factor for geom_smooth ()
  combo.df <- combo.df %>% dplyr::mutate(julianGroup = 
    case_when( julian >= 155 & julian <= 175  ~ "red", 
               julian >= 176 & julian <= 210  ~ "green",
               julian >= 211 & julian <= 238  ~ "blue" ))

assign("combo.df", combo.df, envir=.GlobalEnv)

# https://stackoverflow.com/questions/29880210/as-numeric-removes-decimal-places-in-r-how-to-change

  ggCompare1 <- ggplot(data=combo.df, shape=21) +
      # ggplot really only likes to draw legends for things that have aesthetic mappings.
      #
      geom_point(data=subset(combo.df, julianGroup=="red"), aes(x=julian, y=deltaMean, color=julianGroup, fill=julianGroup) ) +
      geom_point(data=subset(combo.df, julianGroup=="green"), aes(x=julian, y=deltaMean, color=julianGroup, fill=julianGroup) ) +
      geom_point(data=subset(combo.df, julianGroup=="blue"), aes(x=julian, y=deltaMean, color=julianGroup, fill=julianGroup) ) +
      geom_smooth(method="lm", level=0.89, aes(x=julian, y=deltaMean, color=julianGroup)) +   # aes() sections the data even though 'line' is undefined

      # aes(color=julianGroup, fill=julianGroup),
      #geom_hline(yintercept=0) +
      xlim(154,239) +
      ylim(-.1,1.2) +

      geom_hline(yintercept=1) + 

      labs(title= paste("average spiders per trap ",
                        positionText,
                        "\n(oak average / control average)", sep=""), 
           subtitle = paste("89% confidence interval", sep=""),
         x="julian day",
         y= "oakMargin fraction of control",
         caption=paste("120 observations per day", "\npositions 1 - 10 inclusive", sep="")) +

      theme(legend.title=element_blank(), 
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA)) + 
      # Put bottom-right corner of legend box in bottom-right corner of graph
      theme(legend.justification=c(1,0), legend.position=c(.9,.7)) +
      theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
      theme(axis.title.y = element_text(angle = 90, vjust=.5)) +
      theme_bw() 
    

  return(grid.arrange(ggCompare1, ncol=1, nrow=1))

}


selectDataAcrossTransects <- function(data, week, species) {

        #dplyr::filter(transect == UQ(t), week == UQ(w)) # last comment
        # https://www.reddit.com/r/rstats/comments/6zu5od/when_writing_functions_involving_dplyr_how_do_you/

    test.df <- data %>% 
        dplyr::filter(week == UQ(week))  %>%
        dplyr::select(positionX, row, UQ(species)) %>%
        dplyr::group_by(positionX, row) %>%
        summarize(totalSpiders = sum(UQ(species), na.rm = T))

  return(test.df)

}


plotBugDistribution <- function (data, title, caption) {


  # http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-6-weighted-scatterplots.html
  gg <- ggplot(data, aes(positionX, row, size=totalSpiders)) +
    geom_point(shape=21, colour = "purple", fill = "plum", alpha=0.6) +           # 
    #scale_size_area(max_size = 20) +
    # geom_count() probably more appropriate http://ggplot2.tidyverse.org/reference/scale_size.html
    scale_size(range = c(1, 10)) +
    scale_fill_continuous(low = "plum1", high = "purple4") +
    scale_y_reverse(breaks = seq(40, 100, 5),
                    sec.axis = sec_axis(~. * 2.4384 - 103.622,         #### 8*.3048=2.4384  40*8*.3084=103.622
                                           breaks= seq(0, 350, 50),
                                           name= "distance (m)")    ) +
    expand_limits(y=c(30,100)) + 
    scale_x_continuous(breaks=seq(-12,200,16), 
                       sec.axis = sec_axis(~.*.3048,
                                           breaks= seq(0, 80, 10),
                                           name= "trap distance from field margin (m)"))  +
    ggtitle(title) +
    labs( x = "trap distance from field margin (ft)", 
          y = "row",
          caption = paste(caption, sep="")) +

    annotate("rect", xmin=4, xmax=210, ymin=42.5,ymax=54.5, alpha=0.2, fill="blue") +
    annotate("rect", xmin=-12, xmax=5, ymin=42.5,ymax=54.5, alpha=0.2, fill="red") +

    annotate("rect", xmin=4, xmax=210, ymin=78,ymax=90, alpha=0.2, fill="blue") +
    annotate("rect", xmin=-12, xmax=5, ymin=78,ymax=90, alpha=0.2, fill="red") +

    #geom_point(aes(x=-20, y=83), size=10, shape=21, alpha=0.1, colour="blue", fill="yellowgreen") +  # (tree)
    #geom_point(aes(x=10, y=75), size=10, shape=21, alpha=0.1, colour="blue", fill="yellowgreen") +  # (tree)
    #geom_point(aes(x=5, y=95), size=10, shape=21, alpha=0.1, colour="blue", fill="yellowgreen") +  # (tree)
    #annotate("segment", x = 12, xend = 36, y = 98, yend = 100, colour = "black") +    
    #annotate("text", x = 47, y = 100, label = "oak tree", colour="black") +
    annotate("text", x = 95, y = 76, label = "oak transect", colour="black") +
    annotate("text", x = 95, y = 40, label = "control transect", colour="black") + # fill="white", 
    theme_bw() +
    theme() + 
    # theme(legend.position = "bottom", legend.direction = "horizontal") +
    theme(legend.position = "none", legend.direction = "horizontal") +
    theme(legend.box = "horizontal", legend.key.size = unit(1, "cm")) +
    # coord_fixed(ratio=4)
    coord_fixed(ratio=2)
  
  return(grid.arrange(gg, ncol=1, nrow=1))
}


plotRidgesV2 <- function(data, combined, bugs, speciesText, when, wk, caption) {


  # https://stackoverflow.com/questions/7310186/function-in-r-passing-a-dataframe-and-a-column-name

  if (FALSE) {   # available for debug
    # https://stackoverflow.com/questions/9726705/assign-multiple-objects-to-globalenv-from-within-a-function
    assign("data", data, envir=.GlobalEnv)
    assign("bugs", bugs, envir=.GlobalEnv)

    assign("when", when, envir=.GlobalEnv)
    assign("wk", wk, envir=.GlobalEnv)
    assign("caption", caption, envir=.GlobalEnv)
  }
  
  # https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
  
  if (wk < 23 | wk > 52) {  # we definitely don't have a valid week
    # this case indicates 'use data from all weeks'
    cumulative <- "(cumulative)"
    
    if (when != "am" & when != "pm") {    # use all the data (am and pm) for each day
      #filteredBugs.df <- filter(data, transect == where)
    } else {                              # use partial data (am or pm) for each day
      filteredBugs.df <- filter(data, time == when)
    }
    
  } else {  #  we might have a 'valid' week (data for the specified week could be
    #  missing....)
    cumulative <- as.character(wk)
    
    if (when != "am" & when != "pm") {   # use all the data (am and pm) for each day
      filteredBugs.df <- filter(data, week == wk)
    } else {                             # use partial data (am or pm) for each day
      filteredBugs.df <- filter(data, time == when & week == wk)
    }
    
    
  }
  
  
  # simplify to include the trap position and the bug in the list
  newBugs.df <- subset(filteredBugs.df, select= c("positionX", "transect", bugs))

  # exclude counts == 0
  newBugs.df <- newBugs.df %>% dplyr::filter(newColumn > 0)
  
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
      alpha = .2, jittered_points = TRUE, show.legend=F, scale = 0.9) +

    scale_point_color_hue(l = 40)  +
    scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23, 24, 25) ) +
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
                          "\ntotal ", speciesText,  " observations: ", spider_rows,
                          "\ntraps with ", speciesText, "s: ", percentOcurrence, " %", 
                          sep=""),
         x="trap distance from row start (ft)",
         y= paste(speciesText, "\ncounts per trap", sep=""),
         #caption="10 June 2018")
         caption=paste(caption, 
                       "\nhttps://en.wikipedia.org/wiki/Kernel_density_estimation", 
                       sep="")) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5)) +
    theme_bw()

  } else {   # the x axis data combined for values > 0 ; adjust labels with scale_y_discrete()

    gg2 <- ggplot(newBugs.df, aes(x=positionX, y=transect, fill=transect)) +

      geom_density_ridges(aes(point_color = transect, point_fill = transect, point_shape = transect),
                          alpha = .2, jittered_points = TRUE) +

    scale_point_color_hue(l = 40)  +
    scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23)) +

    xlim(1,10) +

    # http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels#change-tick-mark-labels
    #scale_y_discrete(labels=c("0" = "none", "1" = "one or more")) +
    scale_x_continuous(breaks=seq(4,200,16), 
                       sec.axis = sec_axis(~.*.3048,
                                           breaks= seq(0, 80, 10),
                                           name= "trap distance from row start (m)"))  +
    labs(title= paste(speciesText, " Probability Density",  sep=""), 
         subtitle = paste("week: ", cumulative, ", collection time: ", when, sep=""),
         x="trap distance from row start (ft)",
         y= paste("counts per trap > 0", sep=""),
         #caption="10 June 2018")
         caption=paste(caption, 
                       "\nhttps://en.wikipedia.org/wiki/Kernel_density_estimation", 
                       sep="")) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 90, vjust=.5)) +
    theme_bw()

  }
  
  # grid.arrange(gg1, gg2, ncol=1, nrow=2)
  # ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")
  
  return(gg2)
}
