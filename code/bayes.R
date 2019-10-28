

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
  
  returnList[[1]] <- plotWeekly(total.df)  # by clluster

  returnList[[2]] <- plotRawWeekly(total.df) # scatter plot by am/pm

  returnList[[3]] <- plotTransectWeekly(total.df)
  
  ## total.df
  ##
  ## multiple records per week (372 rows) with columns
  ## week, transect, time, cluster, totalSpiders
  ##

  returnList[[4]] <- total.df

  # lists used as parameters in generateLikelihoodV2() ; developed 'by hand' (above)
  #

  ###########   returnList[[11]] <- list(255, 80, 8)                # mean population for 9 models
  

  
  return(returnList)
  
}


modelDiagsV2 <- function(daytime, hp, path) {

  prev.list <- list()
  post.df.list <- list()

  prev.list <- readRDS(paste(path, "list-", daytime, ".rds", sep=""))

  post.df.list <- prev.list[[10]] 

  modelTime <- list()
  modelTime[[1]] <- "seasonal timeframe: weeks 23-25"      # model #1 
  modelTime[[2]] <- "seasonal timeframe: weeks 26-31"      # model #2 
  modelTime[[3]] <- "seasonal timeframe: weeks 32-34"    # model #3 

  #combo.df <- list()

  for (j in 1:9) {   

    # adjust hypothetical population by seasonal timeframe 
    if ((j == 1) || (j == 2) || (j == 3)) {
      hPop <- hp[[1]]
    } else if ((j == 4) || (j == 5) || (j == 6)) {
      hPop <- hp[[2]]
    } else {
      hPop <- hp[[3]]
    }  

    post.df.list[[j]] <- post.df.list[[j]] %>%   
          mutate(trappedSpiders_high = exp(b_Intercept + b_contact_high + (b_log_pop + `b_log_pop:contact_high`) * log(hPop) ),
                 trappedSpiders_low  = exp(b_Intercept + b_log_pop * log(hPop) )) %>% 
          #mutate(diff        = (trappedSpiders_high - trappedSpiders_low) / trappedSpiders_low )
          mutate(diff        = trappedSpiders_high - trappedSpiders_low) 

     # add a cluster indicator for the graphics
    if ((j == 1) || (j == 4) || (j == 7)) {
      post.df.list[[j]] <- post.df.list[[j]] %>% mutate(cluster = '1')
    } else if ((j == 2) || (j == 5) || (j == 8)) {
      post.df.list[[j]] <- post.df.list[[j]] %>% mutate(cluster = '2')
    } else {
      post.df.list[[j]] <- post.df.list[[j]] %>% mutate(cluster = '3')
    }

      names(post.df.list[[j]])[names(post.df.list[[j]])=='b_contact_high'] <- 'bc'
      names(post.df.list[[j]])[names(post.df.list[[j]])=='b_log_pop:contact_high'] <- 'bpc'

  }

  gg.list <- list()

  gg.list[[1]] <- plotPosteriorDensity(df1= post.df.list[[1]], df2= post.df.list[[2]], 
    df3=post.df.list[[3]], mt = modelTime[[1]], pop = hp[[1]])
  gg.list[[2]] <- plotPosteriorDensity(df1= post.df.list[[4]], df2= post.df.list[[5]], 
    df3=post.df.list[[6]], mt = modelTime[[2]], pop = hp[[2]])
  gg.list[[3]] <- plotPosteriorDensity(df1= post.df.list[[7]], df2= post.df.list[[8]], 
    df3=post.df.list[[9]], mt = modelTime[[3]], pop = hp[[3]])


  gg.tmp.list <- plotPosteriorJitterV2(df1= post.df.list[[1]], df2= post.df.list[[2]], df3=post.df.list[[3]], mt = modelTime[[1]], pop = hp[[1]])

  gg.list[[4]] <- gg.tmp.list[[1]]
  gg.list[[5]] <- gg.tmp.list[[2]]
  gg.list[[6]] <- gg.tmp.list[[3]]

  gg.tmp.list <- plotPosteriorJitterV2(df1= post.df.list[[4]], df2= post.df.list[[5]], df3=post.df.list[[6]], mt = modelTime[[2]], pop = hp[[2]])

  gg.list[[7]] <- gg.tmp.list[[1]]
  gg.list[[8]] <- gg.tmp.list[[2]]
  gg.list[[9]] <- gg.tmp.list[[3]]

  gg.tmp.list <- plotPosteriorJitterV2(df1= post.df.list[[7]], df2= post.df.list[[8]], df3=post.df.list[[9]], mt = modelTime[[3]], pop = hp[[3]])

  gg.list[[10]] <- gg.tmp.list[[1]]
  gg.list[[11]] <- gg.tmp.list[[2]]
  gg.list[[12]] <- gg.tmp.list[[3]]


  gg.list[[13]] <- plotPosteriorTrappedHigh(df1= post.df.list[[1]], df2= post.df.list[[2]], 
    df3=post.df.list[[3]], mt = modelTime[[1]], pop = hp[[1]])
  gg.list[[14]] <- plotPosteriorTrappedHigh(df1= post.df.list[[4]], df2= post.df.list[[5]], 
    df3=post.df.list[[6]], mt = modelTime[[2]], pop = hp[[2]])
  gg.list[[15]] <- plotPosteriorTrappedHigh(df1= post.df.list[[7]], df2= post.df.list[[8]], 
    df3=post.df.list[[9]], mt = modelTime[[3]], pop = hp[[3]])

  gg.list[[16]] <- plotPosteriorTrappedLow(df1= post.df.list[[1]], df2= post.df.list[[2]], 
    df3=post.df.list[[3]], mt = modelTime[[1]], pop = hp[[1]])
  gg.list[[17]] <- plotPosteriorTrappedLow(df1= post.df.list[[4]], df2= post.df.list[[5]], 
    df3=post.df.list[[6]], mt = modelTime[[2]], pop = hp[[2]])
  gg.list[[18]] <- plotPosteriorTrappedLow(df1= post.df.list[[7]], df2= post.df.list[[8]], 
    df3=post.df.list[[9]], mt = modelTime[[3]], pop = hp[[3]])

  return(gg.list)

}

plotPosteriorJitterV2 <- function(df1, df2, df3, mt, pop) {

  colours = c("1" = "red", "2" = "green", "3" = "blue")

  gg.list <- list()

  gg.list[[1]] <- justJitterPlot(df1, mt, pop)
  gg.list[[2]] <- justJitterPlot(df2, mt, pop)
  gg.list[[3]] <- justJitterPlot(df3, mt, pop)

  return(gg.list)

}

justJitterPlot <- function(df, mt, pop) {

    gg <- ggplot() + 

    geom_jitter(data=df, aes(x=bc, y=bpc, fill = cluster), shape=21, size=2, alpha=.3, 
      show.legend=TRUE) +

      # ggplot2 issue # 3460 
      # https://github.com/tidyverse/ggplot2/issues/3460
      # show.legend=TRUE, key_glyph = draw_key_dotplot(data=data.frame(), size=5)) +

    guides(fill = guide_legend(override.aes = list(size = 5, alpha = 1))) + 
    
    scale_y_continuous(breaks = seq(min(0), max(2), by = 1)) +
    scale_x_continuous(breaks=seq(-15,5,5)) + 
    
    labs(y="bpc", 
         x="bc", 
         caption = paste("the joint posterior distribution of bc and bpc\n", "spider population per vine : ", pop, "\n", mt, sep="") ) +
    
    theme_bw() +

    scale_fill_manual(values = c("1" = "red", "2" = "green", "3" = "blue"), 
                      breaks = c("1", "2", "3"),
                      labels = c("cluster 1", "cluster 2", "cluster 3")) +

    scale_shape_manual(values = 21) +
    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 

    return(gg)

}


plotPosteriorDensity <- function(df1, df2, df3, mt, pop) {

    colours = c("1" = "red", "2" = "green", "3" = "blue")

    gg <- ggplot() + 
    
    geom_density(data=df1, aes(x=diff, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +
    geom_density(data=df2, aes(x=diff, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +
    geom_density(data=df3, aes(x=diff, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +

    # stat_density(aes(x=diff), geom="point") +   # there is some magic here to adjust legend shapes
    # https://stackoverflow.com/questions/46597079/change-the-shape-of-the-legend-in-density-plots-with-ggplot2
    
    geom_vline(xintercept=0) + #
    
    #xlim(c(-15, 5)) + 
    #expand_limits(x=c(-15, 5)) + 
    #coord_fixed(ratio=20/1) +     # control the aspect ratio of the output; "ratio" refers to the 
                                  # ratio of the axis limits themselves
    
    coord_cartesian(ylim=c(0, 1.1), xlim=c(-1, 2))  + # clip
    #coord_cartesian(ylim=c(0, 1.5), xlim=c(-3, 5))  + # clip

    scale_y_continuous(breaks = seq(min(0), max(1.5), by = .2)) +
    #scale_x_continuous(breaks=seq(-15,5,5)) + 
    
    
    #labs(title=paste("the distribution of the plausible difference\nin average trapped spiders", sep=""),
         #subtitle=paste(mt, sep=""), 
    labs(y="density", 
         x="trapped spider rate\nSNH treatment compared to control", 
         caption = paste("the distribution of the plausible\ndifference in average trapped spiders\n", "spider population per vine : ", pop, "\n", mt, sep="") ) +
    
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

plotPosteriorTrappedHigh <- function(df1, df2, df3, mt, pop) {

    colours = c("1" = "red", "2" = "green", "3" = "blue")

    gg <- ggplot() + 
    
    geom_density(data=df1, aes(x=trappedSpiders_high, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +
    geom_density(data=df2, aes(x=trappedSpiders_high, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +
    geom_density(data=df3, aes(x=trappedSpiders_high, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +

    # stat_density(aes(x=diff), geom="point") +   # there is some magic here to adjust legend shapes
    # https://stackoverflow.com/questions/46597079/change-the-shape-of-the-legend-in-density-plots-with-ggplot2
    
    geom_vline(xintercept=0) + #
    
    coord_cartesian(ylim=c(0, 1.1), xlim=c(0, 6))  + # clip

    scale_y_continuous(breaks = seq(min(0), max(1.5), by = .2)) +
    #scale_x_continuous(breaks=seq(-15,5,5)) + 
    
    
    #labs(title=paste("the distribution of the plausible difference\nin average trapped spiders", sep=""),
         #subtitle=paste(mt, sep=""), 
    labs(y="density", 
         x="SNH effects modelled: trapped spider rate\n(spiders per daylight sampling period)", 
         caption = paste("spider population per vine : ", pop, "\n", mt, sep="") ) +
    
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


plotPosteriorTrappedLow <- function(df1, df2, df3, mt, pop) {

    colours = c("1" = "red", "2" = "green", "3" = "blue")

    gg <- ggplot() + 
    
    geom_density(data=df1, aes(x=trappedSpiders_low, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +
    geom_density(data=df2, aes(x=trappedSpiders_low, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +
    geom_density(data=df3, aes(x=trappedSpiders_low, y=..scaled.., fill = cluster), alpha=.7, show.legend=TRUE, key_glyph = "dotplot") +

    # stat_density(aes(x=diff), geom="point") +   # there is some magic here to adjust legend shapes
    # https://stackoverflow.com/questions/46597079/change-the-shape-of-the-legend-in-density-plots-with-ggplot2
    
    geom_vline(xintercept=0) + #
    
    coord_cartesian(ylim=c(0, 1.1), xlim=c(0, 6))  + # clip

    scale_y_continuous(breaks = seq(min(0), max(1.5), by = .2)) +
    #scale_x_continuous(breaks=seq(-15,5,5)) + 
    
    
    #labs(title=paste("the distribution of the plausible difference\nin average trapped spiders", sep=""),
         #subtitle=paste(mt, sep=""), 
    labs(y="density", 
         x="SNH effect not modelled: trapped spider rate\n(spiders per daylight sampling period)", 
         caption = paste("spider population per vine : ", pop, "\n", mt, sep="") ) +
    
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
  
  
  if (FALSE) {
    df <- likelihood.df
  }
  
  library(gridExtra)
  
  colours = c("three" = "blue", "two" = "green", "one" = "red")
  
  gg <- ggplot(df, aes(x=seasonalTimeframe, y=plausibility)) +
    
        geom_jitter(aes(fill = cluster), shape = 21, size=5, show.legend=TRUE, width=0.05) +
    
    ylim(c(0, 1)) + 
    expand_limits(y=c(0,1)) + 
    #scale_x_continuous(breaks=seq(22,40,2)) +

    #labs(title=paste("'plausibility' of an oakMargin effect\non the crab spider population", sep=""),
     #    subtitle=paste(sub, sep=""), 
    labs(y="plausibility", 
         x="seasonal timeframe", 
         caption = paste("the 'plausibility' of an SNH effect on the crab spider population\n", 
                          "for a hypothetical vine population of ", 
                          hypoPop[[1]], " / ", hypoPop[[2]], " / ", hypoPop[[3]] , " spiders\n",
                          "across three seasonal timeframes\n",
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



generateLikelihoodV2 <- function(df, inboundList, daytime, fromDisc, path, randomSeed,  hp) {


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

  fileName <- paste(path, "list-", daytime, ".rds", sep="")



  if (fromDisc==TRUE) {   # read the structure from disk

    inboundList <- readRDS(fileName)

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

  
    cl.st.list <- list()  # for each cluster  
    # build list of dataframes represting the seasonal population 
    # plus a variable log-population
    #
    label.list <- list()  # remember which cluster and seasonal timeframe
    label.list[[1]] <- "cluster: one, seasonal timeframe: one"
    label.list[[2]] <- "cluster: two, seasonal timeframe: one"
    label.list[[3]] <- "cluster: three, seasonal timeframe: one"
    label.list[[4]] <- "cluster: one, seasonal timeframe: two"
    label.list[[5]] <- "cluster: two, seasonal timeframe: two"
    label.list[[6]] <- "cluster: three, seasonal timeframe: two"
    label.list[[7]] <- "cluster: one, seasonal timeframe: three"
    label.list[[8]] <- "cluster: two, seasonal timeframe: three"
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
    cl.st.list[[1]]$population <- hp[[1]]  
    cl.st.list[[1]]$log_pop <- log(cl.st.list[[1]]$population)  # R code 10.40
    cl.st.list[[1]]$contact_high <- ifelse( cl.st.list[[1]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[2]] <- df %>% dplyr::filter(week < 26 & cluster == 'two')
    cl.st.list[[2]]$population <- hp[[1]]  
    cl.st.list[[2]]$log_pop <- log(cl.st.list[[2]]$population)  # R code 10.40
    cl.st.list[[2]]$contact_high <- ifelse( cl.st.list[[2]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[3]] <- df %>% dplyr::filter(week < 26 & cluster == 'three')
    cl.st.list[[3]]$population <- hp[[1]]  
    cl.st.list[[3]]$log_pop <- log(cl.st.list[[3]]$population)  # R code 10.40
    cl.st.list[[3]]$contact_high <- ifelse( cl.st.list[[3]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[4]] <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'one')
    cl.st.list[[4]]$population <- hp[[2]]  
    cl.st.list[[4]]$log_pop <- log(cl.st.list[[4]]$population)  # R code 10.40
    cl.st.list[[4]]$contact_high <- ifelse( cl.st.list[[4]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[5]] <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'two')
    cl.st.list[[5]]$population <- hp[[2]]  
    cl.st.list[[5]]$log_pop <- log(cl.st.list[[5]]$population)  # R code 10.40
    cl.st.list[[5]]$contact_high <- ifelse( cl.st.list[[5]]$transect=="oakMargin" , 1 , 0 )
  
    cl.st.list[[6]] <- df %>% dplyr::filter(week > 25 & week < 32 & cluster == 'three')
    cl.st.list[[6]]$population <- hp[[2]]  
    cl.st.list[[6]]$log_pop <- log(cl.st.list[[6]]$population)  # R code 10.40
    cl.st.list[[6]]$contact_high <- ifelse( cl.st.list[[6]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[7]] <- df %>% dplyr::filter(week > 31 & cluster == 'one')
    cl.st.list[[7]]$population <- hp[[3]]  
    cl.st.list[[7]]$log_pop <- log(cl.st.list[[7]]$population)  # R code 10.40
    cl.st.list[[7]]$contact_high <- ifelse( cl.st.list[[7]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[8]] <- df %>% dplyr::filter(week > 31 & cluster == 'two')
    cl.st.list[[8]]$population <- hp[[3]]  
    cl.st.list[[8]]$log_pop <- log(cl.st.list[[8]]$population)  # R code 10.40
    cl.st.list[[8]]$contact_high <- ifelse( cl.st.list[[8]]$transect=="oakMargin" , 1 , 0 )
    
    cl.st.list[[9]] <- df %>% dplyr::filter(week > 31 & cluster == 'three')
    cl.st.list[[9]]$population <- hp[[3]]  
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
             iter = 3000, warmup = 1000, chains = 4, cores = 4, seed = randomSeed)

        #print(b10.10)

        modelInput[[i]] <- cl.st.list[[i]]
        modelOutput[[i]] <- b10.10
      

        post.df.list[[i]] <- brms::posterior_samples(modelOutput[[i]])  # 


        # adjust hypothetical population by seasonal timeframe 
        if ((i == 1) || (i == 2) || (i == 3)) {
          hPop <- hp[[1]]
        } else if ((i == 4) || (i == 5) || (i == 6)) {
          hPop <- hp[[2]]
        } else {
          hPop <- hp[[3]]
        }  


 
        temp.df <- post.df.list[[i]] %>%   
          mutate(lambda_high = exp(b_Intercept + b_contact_high + (b_log_pop + `b_log_pop:contact_high`) * log(hPop) ),
                 lambda_low  = exp(b_Intercept + b_log_pop * log(hPop) ) ) %>% 
          mutate(diff        = lambda_high - lambda_low) 
        
        like.df <- temp.df %>%
          summarise(sum = sum(diff > 0)/length(diff))  # 
        

    
      # figure out which cluster we are referring to
      if (i == 1 | i == 4 | i == 7) {
        cluster <- "one"  
      } else if (i == 2 | i == 5 | i == 8) {
        cluster <- "two"
      } else {
        cluster <- "three"
      }      

       # figure out which seasonal timeframe we are referring to
      if (i < 4) {
        seasonalTimeframe <- "one" 
      } else if (i > 6) {
        seasonalTimeframe <- "three"
      } else {
        seasonalTimeframe <- "two"
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
                                  sep=""))

    library(bayesplot)

    for (i in 1:9) {

      fileName <- paste(path, "clBRMsummary-", daytime, "-", i, ".txt", sep="")
      if (file.exists(fileName)) file.remove(fileName)

      sink(file=fileName, append = TRUE, type = "output")
      print(writeLines(paste("generateLikelihoodV2()               ", "  i= ", i, sep="")))  # Sys.time(), 
        
      print(summary(modelOutput[[i]], prob=.89))
      sink(NULL)

    }

    # print the posterior graphs separately so that the color can be adjusted to math the cluster info

    color_scheme_set("red")     # setting used by bayesplot mcmc_intervals()  

    index <- c(1,4,7)           # cluster 'one'

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
        caption = paste("posterior distribution coefficient plot\nmedians with 50% and 89% credible intervals",
                        "\ncluster: ", likelihood.df[[1]][[i]], 
                        " ; seasonal timeframe: ", likelihood.df[[2]][[i]], 
                        "\ninteraction plausibility: ", round(likelihood.df[[3]][[i]],2), sep="")
                    )
    }


    color_scheme_set("green")       # setting used by bayesplot mcmc_intervals()  
    index <- c(2,5,8)               # cluster 'two'

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
                        "\ninteraction plausibility: ", round(likelihood.df[[3]][[i]],2), sep="")
                    )
    }


    color_scheme_set("blue")      # setting used by bayesplot mcmc_intervals() 
    index <- c(3,6,9)             # cluster 'three'

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
    rm(likelihood.df)
    rm(modelInput)
    rm(modelOutput)
    rm(label.list)
    rm(post.df.list)

    detach("package:brms", unload=TRUE) 
    detach("package:rstan", unload=TRUE) 

    detach("package:bayesplot", unload=TRUE) 
    #
  
    return(ggList)
  
}

modelMCMCcheck <- function(path, daytime, debug) {

  # ref: https://www.rensvandeschoot.com/brms-wambs/
  # BRMS Tutorial: Applying the WAMBS – checklist
  #

  library(brms)
  library(ggmcmc)
  library(mcmcplots) 

  # For execution on a local, multicore CPU with excess RAM we recommend calling
  options(mc.cores = parallel::detectCores())

  gg.list <- list()


  # l2. Does the trace-plot exhibit convergence?


  fileName <- paste(path, "list-", daytime, ".rds", sep="")

  inboundList <- readRDS(fileName)

  modelOutput <- inboundList[[8]]       # a list of the 9 models

  for (i in 1:length(modelOutput)) { 

    modeltransformed<-ggs(modelOutput[[i]]) # the ggs function transforms the BRMS output into a longformat tibble, 
                                            # that we can use to make different types of plots.

    gg.list[[i]] <- ggplot(filter(modeltransformed, Parameter==c("b_Intercept", "b_log_pop", "b_contact_high", "b_log_pop:b_contact_high"), 
                                  Iteration>1000), aes(x=Iteration, y=value, col=as.factor(Chain))) +
                            geom_line() +
                            theme_bw() +
                            facet_grid(Parameter ~ .,scale='free_y',switch = 'y') +
                            labs(caption=paste("mcmc chains caterpillar plot\n", "i= ", i, sep=""), col= "chains")



    if (debug) {
      break
    }

  }

  # cleanup
    
  rm(inboundList)
  rm(modelOutput)

  detach("package:brms", unload=TRUE) 
  detach("package:ggmcmc", unload=TRUE) 
  detach("package:mcmcplots", unload=TRUE) 


  return(gg.list)


}

modelComparison <- function(path, daytime, randomSeed, debug) {

  # Statistical Rethinking, code 10.45 and Statistical Rethinking Recoded

  library(dplyr)
  library(knitr)
  library("tibble")    # for rownames_to_column

  library(brms)
  library(rstan)
  library(Rcpp)   # brms bug: avoids  
                  #         Error in cpp_object_initializer(.self, .refClassDef, ...) 
                  #         could not find function "cpp_object_initializer"
                  #         failed to create the sampler; sampling not done

  #For execution on a local, multicore CPU with excess RAM we recommend calling
  options(mc.cores = parallel::detectCores())
  #To avoid recompilation of unchanged Stan programs, we recommend calling
  rstan_options(auto_write = TRUE)

  if (FALSE) {
    daytime<-"pm"
    path<- "./code/output/"
  }

  modelOutput <- list()

  noInteraction <- list()
  noContact <- list()
  noLogPop <- list()
  onlyIntercept <- list()
  gg.list <- list()

  cl <- c("one", "two", "three")
  st <- c("weeks 23-25", "weeks 26-31", "weeks 32-34")
  cluster <- NULL
  timeframe <- NULL


  fileName <- paste(path, "list-", daytime, ".rds", sep="")

  inboundList <- readRDS(fileName)

  modelOutput <- inboundList[[8]]       # a list of the 9 models

  for (i in 1:length(modelOutput)) { 

    # logic from https://bookdown.org/connect/#/apps/1850/access

    # no interaction
    noInteraction[[i]] <- update(modelOutput[[i]], formula = totalSpiders ~ 1 + log_pop + contact_high)

    # no contact rate
    noContact[[i]] <- update(modelOutput[[i]], formula = totalSpiders ~ 1 + log_pop)

    # no log-population
    noLogPop[[i]] <- update(modelOutput[[i]], formula = totalSpiders ~ 1 + contact_high)

    # intercept only
    onlyIntercept[[i]] <- update(modelOutput[[i]], formula = totalSpiders ~ 1, seed=randomSeed)

    modelOutput[[i]]   <- brms::add_criterion(modelOutput[[i]], criterion = "waic")
    noInteraction[[i]] <- brms::add_criterion(noInteraction[[i]], criterion = "waic")
    noContact[[i]]     <- brms::add_criterion(noContact[[i]], criterion = "waic")
    noLogPop[[i]]      <- brms::add_criterion(noLogPop[[i]], criterion = "waic")
    onlyIntercept[[i]] <- brms::add_criterion(onlyIntercept[[i]], criterion = "waic")


    w <- brms::loo_compare(modelOutput[[i]], noInteraction[[i]], noContact[[i]], noLogPop[[i]], onlyIntercept[[i]], criterion = "waic")

    cbind(waic_diff = w[, 1] * -2, se= w[, 2] *  2) %>% 
    round(digits = 2)

      # detect seasonal cluster 
    if ((i == 1) || (i == 4) || (i == 7)) {
      cluster <- cl[[1]]
      color <- "red"
    } else if ((i == 2) || (i == 5) || (i == 8)) {
      cluster <- cl[[2]]
      color <- "green"
    } else {
      cluster <- cl[[3]]
      color <- "blue"
    }  

    # detect seasonal timeframe 
    if ((i == 1) || (i == 2) || (i == 3)) {
      timeframe <- st[[1]]
    } else if ((i == 4) || (i == 5) || (i == 6)) {
      timeframe <- st[[2]]
    } else {
      timeframe <- st[[3]]
    }  


    gg.list[[i]] <- w %>% data.frame() %>% 
          tibble::rownames_to_column(var = "model") %>%
 
          ggplot(aes(x = reorder(model, -waic), 
                 y    = waic,
                 ymin = waic - se_waic,   # 4x "standard error" ~= 95% confidence interval
                 ymax = waic + se_waic
                 #color = model)) +
                 )) +
          theme_bw() +
          geom_pointrange(shape = 21, size=1, fill=color, show.legend = F) + 
          coord_flip() +
          labs(x = NULL, y = NULL,
                caption = paste("Watanabe-Akaike Information Criterion\n+/- the standard error\nseasonal timeframe: ", 
                  timeframe, "\ncluster: ", cluster, " (i = ", i, ")", sep="") ) +
          theme(axis.ticks.y    = element_blank())


    if (debug) {
      break
    }


  }


  # cleanup
    
  rm(inboundList)
  rm(modelOutput)

  rm(noInteraction)
  rm(noContact)
  rm(noLogPop)
  rm(onlyIntercept)

  detach("package:brms", unload=TRUE) 
  detach("package:rstan", unload=TRUE) 


  return(gg.list)

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
    
    # geom_jitter(aes(fill = time), shape=21, size=5, alpha=.7, show.legend=TRUE) +
    geom_jitter(aes(fill = time), shape=21, size=5, alpha=.7, show.legend=TRUE, width = .1) +
    
    geom_vline(xintercept=25.5) + # seasonal timeframe seperators
    geom_vline(xintercept=31.5) + #
    
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
         caption = paste("SNH and control transects, total spiders by time of day", sep="") ) +
    
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
         caption = paste("total spiders by transect", sep="") ) +
    
    theme_bw() +

    scale_fill_manual(values = colours, 
                      breaks = c("oakMargin", "control"),
                      labels = c("SNH treatment", "control")) +
    
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



