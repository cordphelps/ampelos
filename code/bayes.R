
evaluateDailySpiderCounts <- function(df) {
  
  if (FALSE) {
    print(returnList[[1]])  # ggplot() spiders per day per position by week
    print(returnList[[2]])  # rethinking model precis() output
    print(returnList[[3]])  # plot() of the model (line graphs)
    print(returnList[[4]])  # plot() of the model posterior (faceted)
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
          
          twtString <- paste("~ week==", weeks.vector[[i]], 
                             " & time=='", timeList[[j]], "'", sep="")
          
          clusterString <- paste(clusterFormulaList[[k]], sep="")
          
          dailySum.list <- dailySumByClusterTimeWeek(bugs.df, 
                                                     ft=as.formula(twtString),
                                                     fc=as.formula(clusterString))
          
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
  
  # remove the 'am' data points
  #ave.df <- ave.df %>%
  #dplyr::filter(time=='am')
  
  returnList <- list()
  
  returnList[[1]] <- plotWeekly(total.df)
  
  ## total.df
  ##
  ## multiple records per week with columns
  ## week, transect, time, cluster, totalSpiders
  ##
  
  returnList <- generateLikelihood(df=total.df, 
                                   list=returnList,
                                   showPlot=FALSE)

  
  return(returnList)
  
}


plotLikelihood <- function(df) {
  
  if (FALSE) {
    df <- likelihood.df
  }
  
  library(gridExtra)
  
  colors <- c("one" = "red", "two" = "green", "three" = "blue")
  
  gg <- ggplot(df, aes(x=seasonalTimeframe, y=plausibility)) +
    
    geom_point(aes(colour = factor(cluster),
                   fill = factor(cluster), 
                   alpha = .9 ), 
               shape = 21, size=5) +
    
    ylim(c(0, 1)) + 
    expand_limits(y=c(0,1)) + 
    #scale_x_continuous(breaks=seq(22,40,2)) +
    labs(title=paste("'plausibility' of an oakMargin effect\non the spider population", sep=""),
         subtitle=paste("subtitle ", sep=""), 
         y="plausibility", 
         x="seasonal timeframe", 
         caption = paste("", sep="") ) +
    
    scale_color_manual(name="cluster postions", 
                       labels = c("1-4", "5-7", "8-10"), 
                       values = colors) +
    
    scale_x_discrete(labels=c("one" = "weeks 23-25", "two" = "weeks 26-30",
                              "three" = "weeks 31-34")) +
    
    guides(fill=FALSE) +
    guides(alpha=FALSE) +
    guides(factor=FALSE) +
    
    #theme(legend.position="none") +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    theme_bw() +
    coord_fixed() # control the aspect ratio of the output
  
  
  
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


generateLikelihood <- function(df, list, showPlot) {
  
  if("rethinking" %in% (.packages())){
    detach("package:rethinking", unload=TRUE) 
  }

  # https://bookdown.org/connect/#/apps/1850/access
  library(brms)
  
  # build and return a df of likelihood (the probability that the
  # spider population is influenced by the oakMargin) by cluster by week
  
  ## total.df ( = df)
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
  
  # generate nrow(ave.df) random population numbers with a normal distribution
  # t.df$population <- rnorm(nrow(t.df), mean=50, sd=5)
  
  set.seed(10.2) # make the results reproducible
  
  df$population <- ifelse( (df$week>22 & df$week<26), rnorm(nrow(df), mean=75, sd=15),
                           ifelse( (df$week>25 & df$week<31), rnorm(nrow(df), mean=25, sd=8),
                                   rnorm(nrow(df), mean=15, sd=2)) )
  
  df$population <- as.integer(df$population)
  # R code 10.40
  df$log_pop <- log(df$population)
  df$contact_high <- ifelse( df$transect=="oakMargin" , 1 , 0 )
  
  cl.st.list <- list()  # for each cluster  
  # build list of dataframes represting the seasonal population 
  #
  # this is a list of 9
  # 34 observations of 8 variables
  # week, transect, time, cluster, totalSpiders, population, log_pop, contact_high
  #
  cl.st.list[[1]] <- df %>% dplyr::filter(week < 26 & cluster == 'one')
  cl.st.list[[2]] <- df %>% dplyr::filter(week > 25 & week < 31 & cluster == 'one')
  cl.st.list[[3]] <- df %>% dplyr::filter(week > 30 & cluster == 'one')
  
  cl.st.list[[4]] <- df %>% dplyr::filter(week < 26 & cluster == 'two')
  cl.st.list[[5]] <- df %>% dplyr::filter(week > 25 & week < 31 & cluster == 'two')
  cl.st.list[[6]] <- df %>% dplyr::filter(week > 30 & cluster == 'two')
  
  cl.st.list[[7]] <- df %>% dplyr::filter(week < 26 & cluster == 'three')
  cl.st.list[[8]] <- df %>% dplyr::filter(week > 25 & week < 31 & cluster == 'three')
  cl.st.list[[9]] <- df %>% dplyr::filter(week > 30 & cluster == 'three')
  
  # plotWeekly(total.df)
  
  #########
  ## the model
  #########
  
  # the number of trapped spiders increases with log(population)
  
  # the number of trapped spiders increases with natural habital support
  
  # the impact of population on trapped spiders increases with natural habital support
  
  # the number of trapped spiders increases with daylight
  
  likelihood.df <- NULL
  
  for (i in 1:length(cl.st.list)) {  # build model for each seasonal timeframe
    
    
    if (TRUE) {
      
      b10.10 <-
        brm(data = cl.st.list[[i]], family = poisson,
            totalSpiders ~ 1 + log_pop + contact_high + contact_high:log_pop,
            prior = c(prior(normal(0, 100), class = Intercept),
                      prior(normal(0, 1), class = b)),
            iter = 3000, warmup = 1000, chains = 4, cores = 4)
      
      print(b10.10)
      
      post <-
        posterior_samples(b10.10)
      
      post <-
        post %>%
        mutate(lambda_high = exp(b_Intercept + b_contact_high + (b_log_pop + `b_log_pop:contact_high`)*8),
               lambda_low  = exp(b_Intercept + b_log_pop*8)) %>% 
        mutate(diff        = lambda_high - lambda_low) 
      
      like.df <- post %>%
        summarise(sum = sum(diff > 0)/length(diff))
      
    }
    

    
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
  
  list[[5]] <- likelihood.df
  
  # list2env(list, envir = .GlobalEnv)
  
  return(list)
  
}


plotWeekly <- function(df) {
  
  
  gg <- ggplot(df, aes(x=week, y=totalSpiders, colour=cluster)) + 
    #geom_jitter(aes(x=week, y=aveSpiders), width = 0.1, height = 0.1, show.legend = TRUE, shape = 21, size=5, colour = "mediumvioletred", fill = cluster) + 
    
    geom_jitter(aes(colour=cluster, fill = cluster), shape=21, size=5, alpha=.5) +
    
    geom_vline(xintercept=25.5) + # seasonal timeframe seperators
    geom_vline(xintercept=30.5) + #
    
    ylim(c(0, 31)) + 
    expand_limits(y=c(0,31)) + 
    scale_y_continuous(breaks = seq(min(0), max(31), by = 5)) +
    scale_x_continuous(breaks=seq(22,40,2)) +
    labs(title=paste("total spiders trapped per day", sep=""),
         subtitle=paste("oakMargin and control transects", sep=""), 
         y="spiders per day per trap postion", 
         x="week", 
         caption = paste("arbitrary seasonal timeframes", sep="") ) +
    
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    theme_bw() +
    coord_fixed(ratio=1/4)  # control the aspect ratio of the output; "ratio" refers to the 
  # ratio of the axis limits themselves
  
  
  return(gg)
  
}


dailySumByClusterTimeWeek <- function(df, ft, fc) {
  
  # build and return a list of daily row average spiders for each week / cluster / time
  
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
