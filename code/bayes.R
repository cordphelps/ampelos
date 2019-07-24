
renameFunkyColumns <- function(df, oldName, newName) {

  # because ggplot() chokes on brm() 'interaction' column names.....

  # http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/

  # Rename column by name: change "beta" to "two"
  names(df)[names(df)==oldName] <- newName

  return(df)

}

calculateSummaryStats <- function(df) {


    library(reshape2)
    melt.total.df <- melt(df, id.vars=c("week", "transect", "time", "cluster"))

    library(dplyr)
    grouped <- group_by(melt.total.df, week, transect, time, cluster)
    grouped.total <- summarise(grouped, mean=mean(value), sd=sd(value))

    cl1.by.week <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'one')
    cl2.by.week <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'two')
    cl3.by.week <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'three')

    melt.cl <- melt(cl1.by.week, id.vars=c("week", "transect", "time", "cluster"))
    grouped <- group_by(melt.cl, week, transect, time, cluster)
    summarise(grouped, mean=mean(value))

    # use these mean counts to estime the cluster population in the different time frames
    cl.by.week <- df %>% dplyr::filter(week < 26)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    cl.by.week <- df %>% dplyr::filter(week > 25 & week < 32)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    cl.by.week <- df %>% dplyr::filter(week > 31)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    #         cluster    traps per      vines per
    #                    cluster        cluster
    #
    #          1           12            16*5= 80
    #          2            9            12*5= 60
    #          3            9            40*5=200
    #
    #     
    #   seasonal     trapped spiders   cluster    cluster mean          normalize by       ASSUMED weekly     
    #   time frame   by cluster          mean     normalized by         # of weeks         population per 
    #                (sum)                        # of traps                               trapped vine
    #                                                                                      (each observation
    #                                                                                      traps 1% of pop )
    #       1        93,87,95           92       92 / 12  =  7.66       7.66 / 3 = 2.55        255              
    #       2        39,43,48           43       43 / 9   =  4.77       4.77 / 6 = 0.80         80
    #       3        6,5,8               6        6 / 9   =  0.16       0.16 / 2 = 0.08          8
    #
    # so, total.df, with 'total spider' observations in the form :   week  transect time cluster totalSpiders
    # 'total spider' evidence to be folded into priors that are based on relative populations for each 
    # seasonal period
    #
    #                pop    SD (imaginary)    log(pop)
    #
    #      1         255         64  85           2.4
    #      2          80         20  27           1.9
    #      3           8          2   3           0.9 
    #
    # CAUTION: using SD roughly 25% of the mean to prevent the sampler from choking on 
    #          negative values 




  }

evaluateDailySpiderCountsTables <- function(df) {

  
  library(dplyr)
  library(ggplot2)
  
  
  # build a list of spider counts by transect, by cluster, by week, by time-of-day (ref: buildClustersByWeek() )
  
  if (FALSE) {
    source.url <- c("https://raw.githubusercontent.com/cordphelps/ampelos/master/data/bugs.csv")
    bugs.df <- read.csv(source.url, header=TRUE, row.names=NULL)
    df <- bugs.df
    i <- 1
    # formula.w <- (~ week == 25 )
    formula.s <- (~ Thomisidae..crab.spider. > 0 )
  }
  
  species <- "Thomisidae..crab.spider."
  speciesString <- paste("~", species, ">0", sep="")
  formula.s <- as.formula(speciesString)
  
  formula.cluster1 <- paste("~ position==1 | position==2 | position==3 | position==4", sep="")
  formula.cluster2 <- paste("~ position==5 | position==6 | position==7", sep="")
  formula.cluster3 <- paste("~ position==8 | position==9 | position==10", sep="")
  
  weeks.vector <- getWeeks(bugs.df)
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
          
          twtString <- paste("~ transect=='", transectList[[n]], "' & week=='", weeks.vector[[i]], "' & time=='", timeList[[j]], "'", sep="")
          
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

evaluateDailySpiderCounts <- function(df) {
  
  if (FALSE) {
    print(returnList[[1]])  # ggplot() spiders per day per position by week
    print(returnList[[2]])  # 
    print(returnList[[3]])  # plot() of the model (line graphs)
    print(returnList[[4]])  # 
    print(returnList[[5]])  # likelihood.df
    print(returnList[[6]])  # likelihood by clustered position by seasonal week group
  }
  
  
  library(dplyr)
  library(ggplot2)
  
  if (FALSE) {
    source.url <- c("https://raw.githubusercontent.com/cordphelps/ampelos/master/data/bugs.csv")
    bugs.df <- read.csv(source.url, header=TRUE, row.names=NULL)
  }
  
  # interesting / obscure stuff about creating a dataframe column 
  # that contains lists
  # http://stat545.com/block023_dplyr-do.html
  # http://ijlyttle.github.io/isugg_purrr/presentation.html#(18)
  # https://stackoverflow.com/questions/42179298/double-nesting-with-tidyverse-and-purrr
  # (defer for now)
  
  # build a list of spider counts by transect, by cluster, by week, by time-of-day (ref: buildClustersByWeek() )
  
  if (FALSE) {
    df <- bugs.df
    i <- 1
    # formula.w <- (~ week == 25 )
    formula.s <- (~ Thomisidae..crab.spider. > 0 )
  }
  
  species <- "Thomisidae..crab.spider."
  speciesString <- paste("~", species, ">0", sep="")
  formula.s <- as.formula(speciesString)
  
  formula.cluster1 <- paste("~ position==1 | position==2 | position==3 | position==4", sep="")
  formula.cluster2 <- paste("~ position==5 | position==6 | position==7", sep="")
  formula.cluster3 <- paste("~ position==8 | position==9 | position==10", sep="")
  
  weeks.vector <- getWeeks(bugs.df)
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
          
          twtString <- paste("~ transect=='", transectList[[n]], "' & week=='", weeks.vector[[i]], "' & time=='", timeList[[j]], "'", sep="")
          
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

  if (FALSE) {        # calculate summary stats *by hand* to develop brm() prior mean and SD for each seasonal time frame
                      # https://onunicornsandgenes.blog/2014/03/26/using-r-quickly-calculating-summary-statistics-with-dplyr/

    library(reshape2)
    melt.total.df <- melt(total.df, id.vars=c("week", "transect", "time", "cluster"))

    library(dplyr)
    grouped <- group_by(melt.total.df, week, transect, time, cluster)
    grouped.total <- summarise(grouped, mean=mean(value), sd=sd(value))

    cl1.by.week <- total.df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'one')
    cl2.by.week <- total.df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'two')
    cl3.by.week <- total.df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'three')

    melt.cl <- melt(cl1.by.week, id.vars=c("week", "transect", "time", "cluster"))
    grouped <- group_by(melt.cl, week, transect, time, cluster)
    summarise(grouped, mean=mean(value))

    # use these mean counts to estimate the vine population in the different time frames
    cl.by.week <- total.df %>% dplyr::filter(week < 26)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    cl.by.week <- total.df %>% dplyr::filter(week > 25 & week < 32)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    cl.by.week <- total.df %>% dplyr::filter(week > 31)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    #         cluster    traps per      vines per
    #                    cluster        cluster
    #
    #          1           12            16*5= 80
    #          2            9            12*5= 60
    #          3            9            40*5=200
    #
    #     
    #   seasonal     trapped spiders   cluster    cluster mean          normalize by       ASSUMED weekly     
    #   time frame   by cluster          mean     normalized by         # of weeks         mean population  
    #                (sum)                        # of traps                               per trapped vine
    #                                                                                      (each observation
    #                                                                                      traps 1% of pop )
    #       1        93,87,95           92       92 / 12  =  7.66       7.66 / 3 = 2.55        255              
    #       2        39,43,48           43       43 / 9   =  4.77       4.77 / 6 = 0.80         80
    #       3        6,5,8               6        6 / 9   =  0.16       0.16 / 2 = 0.08          8
    #
    # so, total.df, with 'total spider' observations in the form :   week  transect time cluster totalSpiders
    # 'total spider' evidence to be folded into priors that are based on relative populations for each 
    # seasonal period
    #
    #                pop    SD (imaginary)    log10(pop)  ln(pop)
    #
    #      1         255         64  85           2.4       5.5
    #      2          80         20  27           1.9       4.4
    #      3           8          2   3           0.9       2.1
    #
    # CAUTION: using SD roughly 25% of the mean to prevent the sampler from choking on 
    #          negative values 


  }
  
  returnList <- list()
  
  returnList[[1]] <- plotWeekly(total.df)

  # total spiders by week/cluster differentiated by time of day
  returnList[[2]] <- plotRawWeekly(total.df)

  returnList[[3]] <- plotTransectWeekly(total.df)
  
  ## total.df
  ##
  ## multiple records per week (372 rows) with columns
  ## week, transect, time, cluster, totalSpiders
  ##

  returnList[[4]] <- total.df

  # lists used as parameters in generateLikelihoodV2() ; developed 'by hand' (above)
  #

  returnList[[11]] <- list(255, 80, 8, 255, 80, 8, 255, 80, 8)                # mean population for 9 models
  

  
  return(returnList)
  
}

likelihoodPlusModelDiags <- function(rl=returnList) {

  graphList <- list()


  if (TRUE) {
    #source('./code/bayes.R')

    # organize data into
    #       "week", "transect", "time", "cluster", "totalSpiders"
    # ( already done by evaluateDailySpiderCounts() )
    #
    # plot the weekly raw data : plotWeekly()
    # create txt files saving the status output of 9 brm() cycles

    rl <- generateLikelihoodV2(df=rl[[4]], list=rl, daytime='24h')
  
    # reserved positions:
    # returnList[[1]]) # scatter plot by cluster with seasonal timeframes
    # returnList[[2]]) # scatter plot by am/pm
    # returnList[[3]]) # scatter plot by transect
    #
    # returnList[[4]] is the dataframe used for the graphics (total.df)
    # from bayes.R evaluateDailySpiderCounts()
    # it is raw, not averaged, bugs.df data transformed to : 
    #          ("week", "transect", "time", "cluster", "totalSpiders")
    #
    # returnList[[5]] <- likelihood.df
    # returnList[[6]]                   # likelihood by cluster with seasonal timeframes
    # returnList[[7]] <- modelInput     # that is a list of the 9 data sources
    # returnList[[8]] <- modelOutput    # that is a list of the 9 models
    # returnList[[9]] <- label.list     # 
  
    rl[[6]] <- plotLikelihood(df=rl[[5]],  cap=paste("population seasonal trend, daytime: ", daytime, sep=""))
    
    graphList[[1]] <- rl[[6]] # likelihood by cluster with seasonal timeframes
  
    # 
  
    graphList[[2]] <- weightedModelGraph(df=rl[[7]][[1]], model=rl[[8]][[1]], label=rl[[9]][[1]])

    # graphList[[3]] <- weightedModelGraph(df=rl[[7]][[4]], model=rl[[8]][[4]], label=rl[[9]][[4]])
    # graphList[[4]] <- weightedModelGraph(df=rl[[7]][[7]], model=rl[[8]][[7]], label=rl[[9]][[7]])
    # glitch at the smaller end of the range: Error in sample.int(x, size, replace, prob) : 
    #                                         cannot take a sample larger than the population when 'replace = FALSE' 
    #
    # make a local copy for modification : > sample.int <- base:::sample.int

  
    if (FALSE) {
  
    temp.df <- rl[[4]]
    rl[[4]] <- rl[[4]] %>% dplyr::filter(time == 'pm')
    rl <- generateLikelihoodV2(df=rl[[4]], list=rl, daytime='pm')
    rl[[6]] <- plotLikelihood(df=rl[[5]],  cap=paste("population seasonal trend, daytime: ", daytime, sep=""))
    print(rl[[6]]) # likelihood by cluster with seasonal timeframes
  
  
    rl[[4]] <- temp.df %>% dplyr::filter(time == 'am')
    rl <- generateLikelihoodV2(df=rl[[4]], list=rl, daytime='am')
    rl[[6]] <- plotLikelihood(df=rl[[5]],  cap=paste("population seasonal trend, daytime: ", daytime, sep=""))
    print(rl[[6]]) # likelihood by cluster with seasonal timeframes
  
    }
    # kruskal.test(likelihood ~ seasonalTimeframe, data = lh.df)

    # pairwise.wilcox.test(lh.df$likelihood, lh.df$seasonalTimeframe,
     #                     p.adjust.method = "BH")

  
  }  # end if TRUE

  detach("package:brms", unload=TRUE) 
  detach("package:rstan", unload=TRUE) 


  return(graphList)


}


modelDiags <- function(daytime, hp) {

  # https://cran.rstudio.com/web/packages/tidybayes/vignettes/tidy-brms.html

  library(broom)
  library(tidybayes)

  # get a list of 9 models

  modelTime <- list()
  modelTime[[1]] <- "seasonal timeframe: weeks 23-25"      # model #1 
  modelTime[[2]] <- "seasonal timeframe: weeks 26-31"      # model #2 
  modelTime[[3]] <- "seasonal timeframe: weeks 32-34"    # model #3 


  # daytime <- 'pm'

  list <- readRDS(paste("./code/output/list-", daytime, ".rds", sep=""))

    
    # models were created assuming spider populations 
    #        rnorm() mean      cluster spider population         log population
    #   
    #             75                    794                            2.9       
    #             25                    264                            2.4
    #             15                    159                            2.2
    #log.pop.list <- c(2.9, 2.4, 2.2, 2.9, 2.4, 2.2, 2.9, 2.4, 2.2) 

    # models were created assuming spider populations 
    #             mean          cluster spider population         log population
    #   
    #             8.14                    81.4                            1.9       
    #             2.41                    24.1                            1.4
    #             0.72                     7.2                            0.9
    # log.pop.list <- c(2.4, 1.9, 0.9, 2.4, 1.9, 0.9, 2.4, 1.9, 0.9) 

    post.df <- list()
    combo.df <- list()
    gg.list <- list()
    gg.tmp.list <- list()

    # 1. build a df of posterior samples

    for (j in 1:9) {

      post.df[[j]] <- brms::posterior_samples(list[[8]][[j]])
    # 2. add a column of predicted trappedSpiders for two clusters of population log.pop.list[[j]], 
    # one with high oak influence, the other with low oak influence 

      post.df[[j]] <- post.df[[j]] %>%   
          mutate(trappedSpiders_high = exp(b_Intercept + b_contact_high + (b_log_pop + `b_log_pop:contact_high`) * log(hp) ),
                 trappedSpiders_low  = exp(b_Intercept + b_log_pop * log(hp) )) %>% 
          mutate(diff        = trappedSpiders_high - trappedSpiders_low) %>% 
          mutate(trappedSpiders_high_pct = trappedSpiders_high / (trappedSpiders_high + trappedSpiders_low)) %>% 
          mutate(trappedSpiders_low_pct = trappedSpiders_low / (trappedSpiders_high + trappedSpiders_low)) 

    # add a cluster indicator for the graphics
      if ((j == 1) || (j == 4) || (j == 7)) {
        post.df[[j]] <- post.df[[j]] %>% mutate(cluster = '1')
      } else if ((j == 2) || (j == 5) || (j == 8)) {
        post.df[[j]] <- post.df[[j]] %>% mutate(cluster = '2')
      } else {
        post.df[[j]] <- post.df[[j]] %>% mutate(cluster = '3')
      }

    # 3. separate into two df; likelihood diff on either side of .7, add a factor, merge to new df
    postGT.7.df <- post.df[[j]] %>% filter(diff >= .7) %>% mutate(oakInfluence = 'active')
    postLT.7.df <- post.df[[j]] %>% filter(diff <= .699999) %>% mutate(oakInfluence = 'inactive')
    combo.df[[j]] <- dplyr::bind_rows(postGT.7.df, postLT.7.df)

  }


  gg.list[[1]] <- plotPosteriorDensity(df1= combo.df[[1]], df2= combo.df[[2]], df3=combo.df[[3]], mt = modelTime[[1]])
  gg.list[[2]] <- plotPosteriorDensity(df1= combo.df[[4]], df2= combo.df[[5]], df3=combo.df[[6]], mt = modelTime[[2]])
  gg.list[[3]] <- plotPosteriorDensity(df1= combo.df[[7]], df2= combo.df[[8]], df3=combo.df[[9]], mt = modelTime[[3]])

  for (j in 1:9) {
    combo.df[[j]] <- renameFunkyColumns(df=combo.df[[j]], oldName='b_contact_high', newName='bc')
    combo.df[[j]] <- renameFunkyColumns(df=combo.df[[j]], oldName='b_log_pop:contact_high', newName='bpc')
  }

  gg.tmp.list <- plotPosteriorJitter(df1= combo.df[[1]], df2= combo.df[[2]], df3=combo.df[[3]], mt = modelTime[[1]])

  gg.list[[4]] <- gg.tmp.list[[1]]
  gg.list[[5]] <- gg.tmp.list[[2]]
  gg.list[[6]] <- gg.tmp.list[[3]]

  gg.tmp.list <- plotPosteriorJitter(df1= combo.df[[4]], df2= combo.df[[5]], df3=combo.df[[6]], mt = modelTime[[2]])

  gg.list[[7]] <- gg.tmp.list[[1]]
  gg.list[[8]] <- gg.tmp.list[[2]]
  gg.list[[9]] <- gg.tmp.list[[3]]

  gg.tmp.list <- plotPosteriorJitter(df1= combo.df[[7]], df2= combo.df[[8]], df3=combo.df[[9]], mt = modelTime[[3]])

  gg.list[[10]] <- gg.tmp.list[[1]]
  gg.list[[11]] <- gg.tmp.list[[2]]
  



  return(gg.list)
  
  }


  plotPosteriorJitter <- function(df1, df2, df3, mt) {


    colours = c("1" = "red", "2" = "green", "3" = "blue")

    gg.list <- list()

    gg.list[[1]] <- ggplot() + 

    geom_jitter(data=df1, aes(x=bc, y=bpc, fill = cluster), shape=21, size=1, alpha=.3, show.legend=TRUE) +
    
    scale_y_continuous(breaks = seq(min(0), max(2), by = 1)) +
    scale_x_continuous(breaks=seq(-15,5,5)) + 
    
    #labs(title=paste("the joint posterior distribution of bc and bpc", sep=""),
         #subtitle=paste(mt, sep=""), 
    #labs(title=paste("the joint posterior distribution of bc and bpc", sep=""),
         #subtitle=paste(mt, sep=""), 
    labs(y="bpc", 
         x="bc", 
         caption = paste("the joint posterior distribution of bc and bpc\n", mt, sep="") ) +
    
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("1", "2", "3"),
                      labels = c("cluster 1", "cluster 2", "cluster 3")) +
    scale_shape_manual(
                      values = 21) +
    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 

    gg.list[[2]] <- ggplot() + 

    geom_jitter(data=df2, aes(x=bc, y=bpc, fill = cluster), shape=21, size=1, alpha=.3, show.legend=TRUE) +
    
    scale_y_continuous(breaks = seq(min(0), max(2), by = 1)) +
    scale_x_continuous(breaks=seq(-15,5,5)) + 
    
    #labs(title=paste("the joint posterior distribution of bc and bpc", sep=""),
         #subtitle=paste(mt, sep=""),
    labs(y="bpc", 
         x="bc", 
         caption = paste("the joint posterior distribution of bc and bpc\n", mt, sep="") ) +
    
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("1", "2", "3"),
                      labels = c("cluster 1", "cluster 2", "cluster 3")) +
    scale_shape_manual(
                      values = 21) +
    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 

  gg.list[[3]] <- ggplot() + 

    geom_jitter(data=df3, aes(x=bc, y=bpc, fill = cluster), shape=21, size=1, alpha=.3, show.legend=TRUE) +
    
    scale_y_continuous(breaks = seq(min(0), max(2), by = 1)) +
    scale_x_continuous(breaks=seq(-15,5,5)) + 
    
    #labs(title=paste("the joint posterior distribution of bc and bpc", sep=""),
         #subtitle=paste(mt, sep=""), 
    labs(y="bpc", 
         x="bc", 
         caption = paste("the joint posterior distribution of bc and bpc\n", mt, sep="") ) +
    
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("1", "2", "3"),
                      labels = c("cluster 1", "cluster 2", "cluster 3")) +
    scale_shape_manual(
                      values = 21) +
    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 



    return(gg.list)


  }

  plotPosteriorDensity <- function(df1, df2, df3, mt) {


    colours = c("1" = "red", "2" = "green", "3" = "blue")

    gg <- ggplot() + 
    
    geom_density(data=df1, aes(x=diff, fill = cluster), alpha=.7, show.legend=TRUE, show_legend=FALSE) +
    geom_density(data=df2, aes(x=diff, fill = cluster), alpha=.7, show.legend=TRUE, show_legend=FALSE) +
    geom_density(data=df3, aes(x=diff, fill = cluster), alpha=.7, show.legend=TRUE, show_legend=FALSE) +

    # stat_density(aes(x=diff), geom="point") +   # there is some magic here to adjust legend shapes
    # https://stackoverflow.com/questions/46597079/change-the-shape-of-the-legend-in-density-plots-with-ggplot2
    
    geom_vline(xintercept=0) + #
    
    #xlim(c(-15, 5)) + 
    #expand_limits(x=c(-15, 5)) + 
    #coord_fixed(ratio=20/1) +     # control the aspect ratio of the output; "ratio" refers to the 
                                  # ratio of the axis limits themselves
    
    coord_cartesian(ylim=c(0, .6), xlim=c(-25, 25))  + # clip

    scale_y_continuous(breaks = seq(min(0), max(.6), by = .2)) +
    #scale_x_continuous(breaks=seq(-15,5,5)) + 
    
    
    #labs(title=paste("the distribution of the plausible difference\nin average trapped spiders", sep=""),
         #subtitle=paste(mt, sep=""), 
    labs(y="density", 
         x="trapped spider rate\ncontrol minus oak margin", 
         caption = paste("the distribution of the plausible difference in average trapped spiders\n", mt, sep="") ) +
    
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("1", "2", "3"),
                      labels = c("cluster 1", "cluster 2", "cluster 3")) +
    guides(shape = guide_legend(override.aes = list(shape = 21))) +  # define the shape presented in the legend

    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 

    return(gg)

}


plotLikelihood <- function(df, hypoPop, cap) {
  
  # input df :
  # cluster{one, two, three}, seasonalTimeframe{one, two, three}, plausibility(decimal)
  #
  
  #assign("dfLikely", df, envir=.GlobalEnv)
  
  if (FALSE) {
    df <- likelihood.df
  }
  
  library(gridExtra)
  
  colours = c("three" = "blue", "two" = "green", "one" = "red")
  
  gg <- ggplot(df, aes(x=seasonalTimeframe, y=plausibility)) +
    
    geom_jitter(aes(fill = cluster), shape = 21, size=5, show.legend=TRUE, width=.1) +
    
    ylim(c(0, 1)) + 
    expand_limits(y=c(0,1)) + 
    #scale_x_continuous(breaks=seq(22,40,2)) +

    #labs(title=paste("'plausibility' of an oakMargin effect\non the crab spider population", sep=""),
     #    subtitle=paste(sub, sep=""), 
    labs(y="plausibility", 
         x="seasonal timeframe", 
         caption = paste("the 'plausibility' of an SNH effect on the crab spider population\n", 
                          "for a hypothetical vine population of ", hypoPop, " spiders\n", 
                          cap, sep="") ) +
    
    scale_x_discrete(labels=c("one" = "weeks\n23-25", "two" = "weeks\n26-31",
                              "three" = "weeks\n32-34")) +
    
    #guides(fill=FALSE) +
    #guides(alpha=FALSE) +
    #guides(factor=FALSE) +
    
    coord_fixed(1.5) + # control the aspect ratio of the output
    
    #theme(legend.position="none") +
    #theme(legend.position = "bottom", legend.direction = "horizontal") +
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("one", "two", "three"),
                      labels = c("cluster 1", "cluster 2", "cluster 3")) +
    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          #legend.position=c(.9,.7),
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 
  
  
  
  if (FALSE) {
    # is there a significant difference in these likelihood measures?
    # http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
    # grid.arrange(tableGrob(data.frame(unlist(kruskal.test(count ~ spray, data = InsectSprays))), theme = ttheme_minimal()))
  
    kw.text.grob <- tableGrob(data.frame(unlist(
    
      #kruskal.test(count ~ spray, data = InsectSprays)
      kruskal.test(likelihood ~ seasonalTimeframe, data = df)
    
    )), theme = ttheme_minimal())
  
    pw.text.grob <- tableGrob(data.frame(unlist(
    
      pairwise.wilcox.test(df$likelihood, df$seasonalTimeframe,
                          p.adjust.method = "BH")
    
    )), theme = ttheme_minimal())
  
    return(list[[2]] <- grid.arrange(gg, 
                        kw.text.grob,
                        pw.text.grob,
                        ncol=1, nrow=3))
  }
  
  return(gg)
  
  
}

generateLikelihoodV2models <- function(df, inboundList, daytime) {

# read the structure from disk

    inboundList <- readRDS(paste("./code/output/list-", daytime, ".rds", sep=""))

    return(inboundList[[8]])   # that is a list of the 9 models

}

generateLikelihoodV2tables <- function(df, inboundList, daytime) {

# read the structure from disk

    inboundList <- readRDS(paste("./code/output/list-", daytime, ".rds", sep=""))

    return(inboundList[[5]])   # likelihood.df

}

generateLikelihoodV2 <- function(df, inboundList, daytime, fromDisc, path, hp, populationAdjustmentFactor) {


    ##
    ##
    ## build a df of likelihood (the probability that the
    ## spider population is influenced by the oakMargin) by cluster by week
    ## UPDATE ELEMENT 5 of the inbound list (a list of 9 likelihoods) and 
    ## returning the new list.
    ##
    ## save a text file summary() of each model to disc
    ##
    ## return a plot the likelihoods
    ##
    ## return plots of the mcmc_intervals for 9 models
    ##

    ## parameter df is the 4th member of the output of evaluateDailySpiderCounts(bugs.df)

    ## parameter inboundList is the output of evaluateDailySpiderCounts(bugs.df)

    ## parameter hp is the hypotheticalPopulation used to generate likelihood

  
    if("rethinking" %in% (.packages())){
     detach("package:rethinking", unload=TRUE) 
    }
  
    # https://bookdown.org/connect/#/apps/1850/access
    library(brms)
    library(rstan)
    #For execution on a local, multicore CPU with excess RAM we recommend calling
    options(mc.cores = parallel::detectCores())
    #To avoid recompilation of unchanged Stan programs, we recommend calling
    rstan_options(auto_write = TRUE)




  if (fromDisc==TRUE) {   # read the structure from disk

    inboundList <- readRDS(paste(path, "list-", daytime, ".rds", sep=""))

    likelihood.df <- inboundList[[5]] 
    modelInput <- inboundList[[7]]        # a list of the 9 data sources
    modelOutput <- inboundList[[8]]       # a list of the 9 models
    label.list <- inboundList[[9]]        # 
    post.df.list <- inboundList[[10]]     # a list of the 9 posterior distributions


  } else {   # go through the tedious process of creating the brm models

  
    ## total.df ( = df) , generated by evaluateDailySpiderCounts()
    ##                    'am' plus 'pm' data per 'timeList <- c('am', 'pm')'
    ##
    ## multiple records per week with columns
    ## week, transect, time, cluster, totalSpiders
    ##
  
    # estimate different mean populations across 3 groups of weeks reflecting the 
    # seasonal population decline
    # 
    # week 23, 24, 25           : mean 75, sd 15
    # week 26, 27, 28, 29, 30   : mean 50, sd 8
    # week 31, 32, 33, 34       : mean 15, sd 2
    
    
    
    likelihood.df <- NULL
    modelInput <-list()
    post.df.list <- list()
    modelOutput <- list()
    postOutput <- list()

  
    cl.st.list <- list()  # for each cluster  
    # build list of dataframes represting the seasonal population 
    # plus a variable log-population
    #
    label.list <- list()  # remember which cluster and seasonal timeframe
    label.list[[1]] <- "cluster: one, seasonal timeframe: one"
    label.list[[2]] <- "cluster: one, seasonal timeframe: two"
    label.list[[3]] <- "cluster: one, seasonal timeframe: three"
    label.list[[4]] <- "cluster: two, seasonal timeframe: one"
    label.list[[5]] <- "cluster: two, seasonal timeframe: two"
    label.list[[6]] <- "cluster: two, seasonal timeframe: three"
    label.list[[7]] <- "cluster: three, seasonal timeframe: one"
    label.list[[8]] <- "cluster: three, seasonal timeframe: two"
    label.list[[9]] <- "cluster: three, seasonal timeframe: three"

    
    #
    # lists containing parameters developed in evaluateDailySpiderCounts()
    # 
    # 34 observations of 8 variables
    # week, transect, time, cluster, totalSpiders, population, log_pop, contact_high
    #
    #
    # mean population for 9 models     :  inboundList[[11]]            
    #  
  
    cl.st.list[[1]] <- df %>% dplyr::filter(week < 26 & cluster == 'one')
    cl.st.list[[1]]$population <- inboundList[[11]][[1]] * populationAdjustmentFactor
    cl.st.list[[1]]$log_pop <- log(cl.st.list[[1]]$population)  # R code 10.40
    cl.st.list[[1]]$contact_high <- ifelse( cl.st.list[[1]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[2]] <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'one')
    cl.st.list[[2]]$population <- inboundList[[11]][[2]] * populationAdjustmentFactor
    cl.st.list[[2]]$log_pop <- log(cl.st.list[[2]]$population)  # R code 10.40
    cl.st.list[[2]]$contact_high <- ifelse( cl.st.list[[2]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[3]] <- df %>% dplyr::filter(week > 31 & cluster == 'one')
    cl.st.list[[3]]$population <- inboundList[[11]][[3]] * populationAdjustmentFactor
    cl.st.list[[3]]$log_pop <- log(cl.st.list[[3]]$population)  # R code 10.40
    cl.st.list[[3]]$contact_high <- ifelse( cl.st.list[[3]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[4]] <- df %>% dplyr::filter(week < 26 & cluster == 'two')
    cl.st.list[[4]]$population <- inboundList[[11]][[4]] * populationAdjustmentFactor
    cl.st.list[[4]]$log_pop <- log(cl.st.list[[4]]$population)  # R code 10.40
    cl.st.list[[4]]$contact_high <- ifelse( cl.st.list[[4]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[5]] <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'two')
    cl.st.list[[5]]$population <- inboundList[[11]][[5]] * populationAdjustmentFactor
    cl.st.list[[5]]$log_pop <- log(cl.st.list[[5]]$population)  # R code 10.40
    cl.st.list[[5]]$contact_high <- ifelse( cl.st.list[[5]]$transect=="oakMargin" , 1 , 0 )
  
    cl.st.list[[6]] <- df %>% dplyr::filter(week > 31 & cluster == 'two')
    cl.st.list[[6]]$population <- inboundList[[11]][[6]] * populationAdjustmentFactor
    cl.st.list[[6]]$log_pop <- log(cl.st.list[[6]]$population)  # R code 10.40
    cl.st.list[[6]]$contact_high <- ifelse( cl.st.list[[6]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[7]] <- df %>% dplyr::filter(week < 26 & cluster == 'three')
    cl.st.list[[7]]$population <- inboundList[[11]][[7]] * populationAdjustmentFactor
    cl.st.list[[7]]$log_pop <- log(cl.st.list[[7]]$population)  # R code 10.40
    cl.st.list[[7]]$contact_high <- ifelse( cl.st.list[[7]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[8]] <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'three')
    cl.st.list[[8]]$population <- inboundList[[11]][[8]] * populationAdjustmentFactor
    cl.st.list[[8]]$log_pop <- log(cl.st.list[[8]]$population)  # R code 10.40
    cl.st.list[[8]]$contact_high <- ifelse( cl.st.list[[8]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[9]] <- df %>% dplyr::filter(week > 31 & cluster == 'three')
    cl.st.list[[9]]$population <- inboundList[[11]][[9]] * populationAdjustmentFactor
    cl.st.list[[9]]$log_pop <- log(cl.st.list[[9]]$population)  # R code 10.40
    cl.st.list[[9]]$contact_high <- ifelse( cl.st.list[[9]]$transect=="oakMargin" , 1 , 0 )
  
    
    #### model building ####
    #### model building ####
    #### model building ####
    for (i in 1:length(cl.st.list)) {  # build model for each seasonal timeframe

      # ref: Oceanic Tool Complexity Model
      # https://bookdown.org/connect/#/apps/1850/access 
    
        
        b10.10 <-
          brm(data = cl.st.list[[i]], family = poisson,
              totalSpiders ~ 1 + log_pop + contact_high + contact_high:log_pop,  # yes, + contact_high:log_pop
              prior = c(prior(normal(0, 100), class = Intercept),
                        prior(normal(0, 1), class = b)),
             iter = 3000, warmup = 1000, chains = 4, cores = 4, seed=10.2)

        #print(b10.10)

        modelInput[[i]] <- cl.st.list[[i]]
        modelOutput[[i]] <- b10.10
      

        post.df.list[[i]] <- brms::posterior_samples(modelOutput[[i]])  # 


 
        temp.df <- post.df.list[[i]] %>%   
          mutate(lambda_high = exp(b_Intercept + b_contact_high + (b_log_pop + `b_log_pop:contact_high`) * log(hp) ),
                 lambda_low  = exp(b_Intercept + b_log_pop * log(hp) ) ) %>% 
          mutate(diff        = lambda_high - lambda_low) 
        
        like.df <- temp.df %>%
          summarise(sum = sum(diff > 0)/length(diff))  # 
        

    
      # figure out which cluster we are referring to
      if (i < 4) {
        cluster <- "one" 
      } else if (i > 6) {
        cluster <- "three"
      } else {
        cluster <- "two"
      }
    
      # figure out which seasonal timeframe we are referring to
      if (i == 1 | i == 4 | i == 7) {
        seasonalTimeframe <- "one" 
      } else if (i == 2 | i == 5 | i == 8) {
        seasonalTimeframe <- "two"
      } else {
        seasonalTimeframe <- "three"
      }      
    
      temp2.df <- data.frame(cluster, seasonalTimeframe, like.df$sum) 
      # column names need to match for rbind() to work
      names(temp2.df) <- c("cluster", "seasonalTimeframe", "plausibility")
    
      # tack it on
      if (!exists('likelihood.df')) {
        likelihood.df <- temp2.df
        # (so, on the first pass the column names are cool)
      } else {
        likelihood.df <- rbind(likelihood.df, temp2.df)
      }
    
    }  
    #### end model building ####
    #### end model building ####
    #### end model building ####

    
    fileName <- paste(path, "list-", daytime, ".rds", sep="")
    if (file.exists(fileName)) { file.remove(fileName) }

    inboundList[[5]] <- likelihood.df
    inboundList[[7]] <- modelInput  # that is a list of the 9 data sources
    inboundList[[8]] <- modelOutput  # that is a list of the 9 models
    inboundList[[9]] <- label.list  # 
    inboundList[[10]] <- post.df.list

    saveRDS(inboundList, fileName)

  } 

  # print(inboundList[[5]])

  ggList <- list()

  # plot the likelihood for the 9 models
  ggList[[1]] <- plotLikelihood(df=inboundList[[5]],  hypoPop=hp,
                                cap=paste("daytime: ", daytime,
                                " , population adjustment factor: ", populationAdjustmentFactor,           
                                sep=""))

  #library(ggthemes)
  library(bayesplot)

  for (i in 1:9) {

    fileName <- paste(path, "clBRMsummary-", daytime, "-", i, ".txt", sep="")
    if (file.exists(fileName)) file.remove(fileName)

    sink(file=fileName, append = TRUE, type = "output")
    print(writeLines(paste("\ngenerateLikelihoodV2()               ", "\n\n", sep="")))  # Sys.time(), 
    print(writeLines(paste("i = ", i, "\n\n", sep="")))
        
    print(summary(modelOutput[[i]], prob=.89))
    sink(NULL)

  }

  # print the posterior graphs separately so that the color can be adjusted to math the cluster info

  color_scheme_set("red")      # cluster 'one'

  index <- c(1,2,3)

  for (i in index) {

    # plot the posterior distributions
    ggList[[i+1]] <- post.df.list[[i]] %>%

      select(-lp__) %>% 
      rename(b_interaction = `b_log_pop:contact_high`) %>%

      bayesplot::mcmc_intervals(prob = .5, prob_outer = .89) +
      theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0)) +
      theme_bw() +
      ggplot2::labs(
        caption = paste("posterior distribution coefficient plot with medians and 89% intervals",
                        "\ncluster: ", likelihood.df[[1]][[i]], 
                        " ; seasonal timeframe: ", likelihood.df[[2]][[i]], 
                        "\npopulation adjustment factor: ", populationAdjustmentFactor,
                        "\ninteraction plausibility: ", round(likelihood.df[[3]][[i]],2), sep="")
                    )
    }


  color_scheme_set("green")      # cluster 'two'
  index <- c(4,5,6)

  for (i in index) {

    # plot the posterior distributions
    ggList[[i+1]] <- post.df.list[[i]] %>%

      select(-lp__) %>% 
      rename(b_interaction = `b_log_pop:contact_high`) %>%

      bayesplot::mcmc_intervals(prob = .5, prob_outer = .89) +
      theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0)) +
      theme_bw() +
      ggplot2::labs(
        caption = paste("posterior distribution coefficient plot with medians and 89% intervals",
                        "\ncluster: ", likelihood.df[[1]][[i]], 
                        " ; seasonal timeframe: ", likelihood.df[[2]][[i]], 
                        "\npopulation adjustment factor: ", populationAdjustmentFactor,
                        "\ninteraction plausibility: ", round(likelihood.df[[3]][[i]],2), sep="")
                    )
    }


  color_scheme_set("blue")      # cluster 'three'
  index <- c(7,8,9)

  for (i in index) {

    # plot the posterior distributions
    ggList[[i+1]] <- post.df.list[[i]] %>%

      select(-lp__) %>% 
      rename(b_interaction = `b_log_pop:contact_high`) %>%

      bayesplot::mcmc_intervals(prob = .5, prob_outer = .89) +
      theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0)) +
      theme_bw() +
      ggplot2::labs(
        caption = paste("posterior distribution coefficient plot with medians and 89% intervals",
                        "\ncluster: ", likelihood.df[[1]][[i]], 
                        " ; seasonal timeframe: ", likelihood.df[[2]][[i]], 
                        "\npopulation adjustment factor: ", populationAdjustmentFactor,
                        "\ninteraction plausibility: ", round(likelihood.df[[3]][[i]],2), sep="")
                    )


  }

  
      # reserved positions:
    # returnList[[1]]) # scatter plot by cluster with seasonal timeframes
    # returnList[[2]]) # scatter plot by am/pm
    # returnList[[3]]) # scatter plot by transect
    #
    # returnList[[4]] is the dataframe used for the graphics (total.df)
    # from bayes.R evaluateDailySpiderCounts()
    # it is raw, not averaged, bugs.df data transformed to : 
    #          ("week", "transect", "time", "cluster", "totalSpiders")
    #
    # returnList[[5]] <- likelihood.df
    # returnList[[6]]                   # likelihood by cluster with seasonal timeframes
    # returnList[[7]] <- modelInput     # that is a list of the 9 data sources
    # returnList[[8]] <- modelOutput    # that is a list of the 9 models
    # returnList[[9]] <- label.list     # 
  


  # cleanup
  rm(inboundList)
  detach("package:brms", unload=TRUE) 
  detach("package:rstan", unload=TRUE) 

  detach("package:bayesplot", unload=TRUE) 
  #
  
  return(ggList)
  
}



weightedModelGraph <- function(df, model, label) {

  # https://bookdown.org/connect/#/apps/1850/access  end of 10.2.1

  # no interaction
  b10.11 <- stats::update(model, formula = totalSpiders ~ 1 + log_pop + contact_high)

  # no contact rate
  b10.12 <- stats::update(model, formula = totalSpiders ~ 1 + log_pop)

  # no log-population
  b10.13 <- stats::update(model, formula = totalSpiders ~ 1 + contact_high)

  # intercept only
  b10.14 <- stats::update(model, formula = totalSpiders ~ 1)


  nd <- tibble(log_pop  = seq(from = 2.0, to = 3.0, length.out = 50) %>% 
           rep(., times = 2), contact_high = rep(0:1, each = 50))


  ppa_10.9 <- brms::pp_average(model, b10.11, b10.12, weights = "loo", method  = "fitted", newdata = nd) %>%
    as_tibble() %>%
    bind_cols(nd)

  ggWeighted <- ggplot(ppa_10.9, aes(x = log_pop, group = contact_high)) +

    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = contact_high), alpha = 1/4) +

    geom_line(aes(y = Estimate, color = contact_high)) +

    geom_text(data = df,  # that's the original 'island' dataset (top of section)
             aes(y = totalSpiders, label = totalSpiders, color = contact_high), size = 3.5) +

    coord_cartesian(xlim = c(1.75, 3.25), ylim = c(0, 40)) +

    labs(x = "log population",
       y = "total crab spiders",
       title = label,
       subtitle = label,
       caption = "Blue is the high contact rate; black is the low.") +

    theme(legend.position = "none", panel.border = element_blank()) +

    theme_bw()

  return(ggWeighted)

}

bayesAnalysis <- function(list) {

library(GGally)

if (FALSE) {     

  for (i in 1:length(list)) {
      # same problem as below
      print(plot((list[[i]])))
      
    }
  }

if (FALSE) {     

  for (i in 1:length(list)) {

      post <- posterior_samples(list[[i]])
      
      ggally <- post %>% ggpairs() +
      
        labs(title=paste("brm model check", sep=""),
         subtitle=paste("variable selection (i = ", i, ")", sep="")) +

        theme_bw()
      
        # this list is unreasonably long, perhaps due to the multi-chain operation
        # of brm(); set to TRUE and uncomment the print() statement to get the pairs() 
        # pairwise scatterplots
        #
      print(ggally)
    }
  }
}





plotWeekly <- function(df) {
  
  # input df :
  #  week, transect, time{am, pm}, cluster{one, two, three}, totalSpiders
  #

  # > head(total.df, 10)
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
  
  #assign("plotWeekly.df", df, envir=.GlobalEnv)
  #assign("dfX", df, envir=.GlobalEnv)
  
  colours = c("three" = "blue", "two" = "green", "one" = "red")
  
  gg <- ggplot(df, aes(x=week, y=totalSpiders)) + 
    #geom_jitter(aes(x=week, y=aveSpiders), width = 0.1, height = 0.1, show.legend = TRUE, shape = 21, size=5, colour = "mediumvioletred", fill = cluster) + 
    
    geom_jitter(aes(fill = cluster), shape=21, size=5, alpha=.5, show.legend=TRUE) +
    
    geom_vline(xintercept=25.5) + # seasonal timeframe seperators
    geom_vline(xintercept=31.5) + #
    
    ylim(c(0, 31)) + 
    expand_limits(y=c(0,31)) + 
    coord_fixed(ratio=1/4) +     # control the aspect ratio of the output; "ratio" refers to the 
  # ratio of the axis limits themselves
    
    scale_y_continuous(breaks = seq(min(0), max(31), by = 5)) +
    scale_x_continuous(breaks=seq(22,40,2)) + 
    
    
    #labs(title=paste("total spiders trapped per week", sep=""),
         #subtitle=paste("oakMargin and control transects, counts by cluster", sep=""), 
    labs(y="total spiders", 
         x="week", 
         caption = paste("oakMargin and control transects, total spiders by cluster" ) ) +
    
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("one", "two", "three"),
                      labels = c("cluster 1", "cluster 2", "cluster 3")) +
    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          #legend.position=c(.9,.7),
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 
    
  
  
  return(gg)
  
}


plotRawWeekly <- function(df) {

  # derived from plotWeekly()

  # plot cluster spider counts, by week, differentiate by time
  
  # input df :
  #  week, transect, time{am, pm}, cluster{one, two, three}, totalSpiders
  #

  # > head(total.df, 10)
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
  
  #assign("plotWeekly.df", df, envir=.GlobalEnv)
  #assign("dfX", df, envir=.GlobalEnv)
  
  colours = c("pm" = "violet", "am" = "purple")
  
  gg <- ggplot(df, aes(x=week, y=totalSpiders)) + 
    
    geom_jitter(aes(fill = time), shape=21, size=5, alpha=.7, show.legend=TRUE) +
    
    # geom_vline(xintercept=25.5) + # seasonal timeframe seperators
    # geom_vline(xintercept=31.5) + #
    
    ylim(c(0, 31)) + 
    expand_limits(y=c(0,31)) + 
    coord_fixed(ratio=1/4) +     # control the aspect ratio of the output; "ratio" refers to the 
  # ratio of the axis limits themselves
    
    scale_y_continuous(breaks = seq(min(0), max(31), by = 5)) +
    scale_x_continuous(breaks=seq(22,40,2)) + 
    
    # labs(title=paste("total spiders trapped by week", sep=""),
    labs(
         y="total spiders", 
         x="week", 
         caption = paste("oakMargin and control transects, total spiders by cluster", sep="") ) +
    
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("am", "pm"),
                      labels = c("overnight", "daytime")) +
    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          #legend.position=c(.9,.7),
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 
    
  
  
  return(gg)
  
}


plotTransectWeekly <- function(df) {

  # derived from plotWeekly()

  # plot cluster spider counts, by week, differentiate by time
  
  # input df :
  #  week, transect, time{am, pm}, cluster{one, two, three}, totalSpiders
  #

  # > head(total.df, 10)
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
  
  #assign("plotWeekly.df", df, envir=.GlobalEnv)
  #assign("dfX", df, envir=.GlobalEnv)
  
  colours = c("oakMargin" = "#405E00", "control" = "#9BCC94")
  
  gg <- ggplot(df, aes(x=week, y=totalSpiders)) + 
    
    geom_jitter(aes(fill = transect), shape=21, size=5, alpha=.7, show.legend=TRUE, width = .1) +
    
    geom_vline(xintercept=25.5) + # seasonal timeframe seperators
    geom_vline(xintercept=31.5) + #
    
    ylim(c(0, 31)) + 
    expand_limits(y=c(0,31)) + 
    coord_fixed(ratio=1/4) +     # control the aspect ratio of the output; "ratio" refers to the 
  # ratio of the axis limits themselves
    
    scale_y_continuous(breaks = seq(min(0), max(31), by = 5)) +
    scale_x_continuous(breaks=seq(22,40,2)) + 
    
    #labs(title=paste("total spiders trapped by week", sep=""),
    #     subtitle=paste("oakMargin and control transects, counts by cluster", sep=""),

    labs(y="total spiders", 
         x="week", 
         caption = paste("oakMargin and control transects, total spiders by transect", sep="") ) +
    
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("oakMargin", "control"),
                      labels = c("oak margin", "control")) +
    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          #legend.position=c(.9,.7),
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 
    
  
  
  return(gg)
  
}

dailySumByClusterTimeWeek <- function(df, ft, fc) {
  
  # build and return a list of daily row average spiders for each week / cluster / time

  if (FALSE) {

    paste("~ position==1 | position==2 | position==3 | position==4", sep="")

    twtString <- paste("~ week==23 & time=='pm'", sep="")
          
    clusterString <- paste("one", sep="")
          
    dailySum.list <- dailySumByClusterTimeWeek(df, ft=as.formula(twtString), fc=as.formula(clusterString))
          
  }
  
  dailySum.list <- df %>%
    #dplyr::filter(transect == "oakMargin" & week == 23 & time == 'pm') %>%
    #dplyr::filter(position==1 | position==2 | position==3 | position==4) %>%
    dplyr::filter_(ft)  %>%
    dplyr::filter_(fc)  %>%
    dplyr::group_by(julian) %>%
    dplyr::summarise(aveSpiders = sum(Thomisidae..crab.spider.) ) %>%
    dplyr::select(-julian) %>%  # remove julian column
    as.list()                   # list now contains the average for
  # each of the julian days in the week
  
  return(dailySum.list)
}

sinkAll <- function() {
  # https://stackoverflow.com/questions/18730491/sink-does-not-release-file
  i <- sink.number()
  while (i > 0) {
    sink()
    i <- i - 1
  }
}

visualizeBRMSdraws <- function(df) {

  # https://mjskay.github.io/tidybayes/articles/tidy-brms.html

  library(ggstance)

  if (FALSE) {
    df< total.df

    # use these mean counts to estime the cluster population in the different time frames
    cl.by.week <- df %>% dplyr::filter(week < 26)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    cl.by.week <- df %>% dplyr::filter(week > 25 & week < 32)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    cl.by.week <- df %>% dplyr::filter(week > 31)
    cl.pm <- cl.by.week %>% filter(time=='pm' & transect=='oakMargin')
    cl.pm %>% group_by(cluster) %>% summarize(count.pm.Spiders=sum(totalSpiders))

    #> head(cl.pm)
    #  week  transect time cluster totalSpiders
    #1   32 oakMargin   pm     one            1
    #2   32 oakMargin   pm     one            3
    #3   32 oakMargin   pm     one            0
    #4   32 oakMargin   pm     two            1
    #5   32 oakMargin   pm     two            3


    # get models from disk
    daytime <- "pm"
    inboundList <- readRDS(paste("./code/output/list-", daytime, ".rds", sep=""))
    model.list <- inboundList[[8]]
    post.df <- brms::posterior_samples(modelOutput[[i]])


  }

  cl.pm %>%
    ggplot(aes(y = week, x = totalSpiders, size=totalSpiders)) +
    geom_point()


  # Extracting draws from a fit in tidy-format using spread_draws

  get_variables(model.list[[1]])

  # spread_draws() splits the variable indices by commas and spaces. 
  # It lets you assign columns to the resulting indices in order. 

  model.list[[1]] %>% spread_draws(b_Intercept, b_log_pop, b_contact_high) %>% median_qi() %>% head(10)

  model.list[[1]] %>% spread_draws(`b_Intercept`, `b_contact_high`, `b_log_pop`) %>%

                      mutate(condition_mean = `b_Intercept` + `b_log_pop`) %>%

                      median_qi(condition_mean) %>% 

                      ggplot(aes(y = b_log_pop, x = condition_mean, xmin = .lower, xmax = .upper)) +

                            ggstance::geom_pointrangeh()


}

