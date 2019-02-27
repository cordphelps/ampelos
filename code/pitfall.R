
compressBugsData <- function(df) {

  # compress bugs data and return
  #
  # > head(total.df, 10)                          <-- 372 rows
  #   week  transect time cluster totalSpiders
  #1    23 oakMargin   am     one            2
  #2    23 oakMargin   am     one            6
  #3    23 oakMargin   am     two            2
  #4    23 oakMargin   am     two            4
  #5    23 oakMargin   am   three            2
  #6    23 oakMargin   am   three            3
  #7    23 oakMargin   pm     one            1
  #8    23 oakMargin   pm     one            7
  #9    23 oakMargin   pm     one           14
  #10   23 oakMargin   pm     two            5


  species <- "Thomisidae..crab.spider."
  speciesString <- paste("~", species, ">0", sep="")
  formula.s <- as.formula(speciesString)
  
  formula.cluster1 <- paste("~ position==1 | position==2 | position==3 | position==4", sep="")
  formula.cluster2 <- paste("~ position==5 | position==6 | position==7", sep="")
  formula.cluster3 <- paste("~ position==8 | position==9 | position==10", sep="")
  
  weeks.vector <- getWeeks(bugs.df)          #  ./code/jaccard-similarity.R
  transectList <- c('oakMargin', 'control')

  timeList <- c('am', 'pm')

  clusterFormulaList <- c(formula.cluster1, formula.cluster2, formula.cluster3)
  
  total.df <- NULL
  
  for (i in 1:length(weeks.vector)) {
    
    for (n in 1:length(transectList)) {
      
      for (j in 1:length(timeList)) {
        
        for (k in 1:length(clusterFormulaList)) {
          if (k==1) {
            clusterListChars <- "one" 
          } else if (k==2) {
            clusterListChars <- "two"
          } else {
            clusterListChars <- "three"
          }
          
          twtString <- paste("~ week==", weeks.vector[[i]], " & time=='", timeList[[j]], "'", sep="")
          
          clusterString <- paste(clusterFormulaList[[k]], sep="")
          
          dailySum.list <- dailySumByClusterTimeWeek(df, ft=as.formula(twtString), fc=as.formula(clusterString))
          
          # create a df record for each of the averages in the list
          for (m in 1:length(dailySum.list[[1]])) {
            temp.df <- data.frame(weeks.vector[[i]],
                                  transectList[[n]],
                                  timeList[[j]], 
                                  clusterListChars,
                                  dailySum.list[[1]][[m]]
            )
            # column names need to match for rbind() to work
            names(temp.df) <- c("week", "transect", "time", "cluster", "totalSpiders")
            
            # tack it on
            # https://stackoverflow.com/questions/35366187/how-to-write-if-else-statements-if-dataframe-is-empty
            if (!exists('total.df')) {
              total.df <- temp.df
              # (so, on the first pass the column names are cool)
            } else {
              total.df <- rbind(total.df, temp.df)
            }
            
          } # end create a df record
          
        } # clusterFormulaList
      } # timeList
    } # transectList
  } # weeks.vector
  
# > head(total.df, 10)                          <-- 372 rows
#   week  transect time cluster totalSpiders
#1    23 oakMargin   am     one            2
#2    23 oakMargin   am     one            6
#3    23 oakMargin   am     two            2
#4    23 oakMargin   am     two            4
#5    23 oakMargin   am   three            2
#6    23 oakMargin   am   three            3
#7    23 oakMargin   pm     one            1
#8    23 oakMargin   pm     one            7
#9    23 oakMargin   pm     one           14
#10   23 oakMargin   pm     two            5

  return(total.df)

}


getObservedJulianSpidersTemps <- function(spiders, temps) {

  # return a list of the julian days with the week occurring in the dataset
  # 
  # each non-NULL list element (the list 'index') represents a julian day from the original dataset
  # the element value is a list of week and delta C for teat julian day 
  #


  library(dplyr)
  
  julian.vector <- spiders %>%
          select(julian) %>%
          unique() %>% 
          .$julian

  outputList<-list()

  for (i in 1:length(julian.vector)) {

    index <- julian.vector[[i]]

    temp.df <- data.frame(index, trunc((index/7) + 1), getJulianDeltaC(t=temps, day=index) )


    # tack it on
    # https://stackoverflow.com/questions/35366187/how-to-write-if-else-statements-if-dataframe-is-empty
    if (!exists('deltaTemp.df')) {
          deltaTemp.df <- temp.df
          # (so, on the first pass the column names are cool)
    } else {
          deltaTemp.df <- rbind(deltaTemp.df, temp.df)
    }

  }

  colNames <- c("julian","week","deltaC")
  colnames(deltaTemp.df) <- colNames

  if (TRUE) {    # just to help with QA
    deltaTemp.df <- deltaTemp.df %>% 
      select(week, deltaC) %>%
      dplyr::group_by(week) %>%
      dplyr::summarise_all(funs(mean))
    }

  return(deltaTemp.df)

  # examine the outputList like this :
  #
  #     for (i in outputList) {
  #
  #       if ( is.integer() )  { # avoid NULL entries

  #         c.ave <- getDeltaC(df=temps.df, day=i)  
  #
  #         new.df <- new.df %>% tibble::add_row(julian = i, jDelta = c.ave)

  #       }
  #
  #     }
  

}

getJulianDeltaC <- function(t, day) {

  # input df :
  #
  # > head(temps.df)
  #         Julian DayAirTmpMax     DayAirTmpMin    diff
  # 1       1             18.0              2.1     15.9
  # 2       2             19.0              5.2     13.8

  # day is an index of julian day
  #
  # return the value of diff

  return(t[which(t$Julian==day),]$diff)


}





