
clusterSetup <- function() {
  
  # strip out the other arthropods and misc stuff
  
  # Visualize cluster data using box plots
  # http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
  # http://t-redactyl.io/blog/2016/04/creating-plots-in-r-using-ggplot2-part-10-boxplots.html
  

  if (FALSE) {
  
  }

  source.url <- c("https://raw.githubusercontent.com/cordphelps/ampelos/master/data/bugs.csv")
  bugs.df <- read.csv(source.url, header=TRUE, row.names=NULL)


  library(dplyr)

  spider.df <- bugs.df %>%
    dplyr::select(-positionX, -julian) %>%
    dplyr::select(-Agapostemon.sp....green..native.bee., -Bombus.californicus..bumble., -Braconid.wasp, -checkerspot.butterfly, -Diabrotica.undecimpunctata..Cucumber.Beetle., -Diptera..Agromyzidae..leafminer.., -Halictus.sp....3.part..native.bee., -Honey.Bee, -ladyBug, -Lygus.hesperus..western.tarnished.plant.bug., -Orius..pirate.bug., -Osmia.sp...native.bee., -other, -pencilBug, -pentamonidae...stinkBug., -Pyralidae..Snout.Moth., -spider.other) 

  return(spider.df)
}

getBoxplotStats <- function(column) {
  # get mean and quantile width
  stats <-list()
  stats[[1]] <- median(column)
  stats[[2]] <- quantile(column, .75, names=FALSE) - quantile(column, .25, names=FALSE)
  return(stats)
}

clusterStats <- function(df, column) {
  # just get the data used by the boxPlots
  
  # inbound data is: week, cluster#, spiders
  
  temp.df <- df %>% filter(cluster="cl1")
  
  temp.df <- temp.df %>% 
    dplyr::group_by(week) %>%
    dplyr::summarise( 
      spiders = median(Thomisidae..crab.spider., na.rm = TRUE),
      week = week
    )
    
    
  
}

clusterBoxplot <- function(df, t, time) {
  
  library(ggplot2)
  library(scales) # to access break formatting functions
  library(RColorBrewer) # customise the colours of the boxes by adding the scale_fill_brewer to the plot
  
  # **************
  #  caution: boxplots are designed to illustrate CONTINUOUS variables 
  # **************

  p10 <- ggplot(df, aes(x = as.factor(week), y = spiders, fill = cluster )) +
    
    geom_boxplot(alpha = 0.7) +
    
    # ylim(0, 1.75, .25) +
    expand_limits(y=c(0, 1.75)) +
    ggtitle(paste("cluster comparisons by week\n", "transect: ", t, ", period: ", time, sep="")) +
    theme_bw() +
    
    #scale_fill_manual(values=c("red", "green", "blue"),
                      #breaks = 1:3, 
                     # labels = c('cluster 1','cluster 2','cluster 3')) +
    scale_x_discrete(name = "week") +
    scale_y_continuous(name = "crab spiders observed") +

    
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          #legend.position=c(.9,.7),
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 
  
  p10
  
  
  
}

clusterStats <- function(df, t, daytime) {
  
  
  if (FALSE) {
    t <- "control"
    daytime <- "pm"
    df <- input.df
  }
  
  position1.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 1) %>%
    dplyr::mutate(cluster = "cl1") %>%
    dplyr::mutate(transect = t)
  
# > position1.df
#  transect row position Thomisidae..crab.spider.        date time
#  1   control  48        1                        0  6-Jun-2018   pm
#  2   control  50        1                        0  6-Jun-2018   pm
#  3   control  52        1                        0  6-Jun-2018   pm

  
  position2.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 2) %>%
    dplyr::mutate(cluster = "cl1") %>%
    dplyr::mutate(transect = t)
  
  position3.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 3) %>%
    dplyr::mutate(cluster = "cl1") %>%
    dplyr::mutate(transect = t)
  
  if (t == "control") {
    position4.df <- df %>%
      dplyr::filter( time == daytime,  transect == t) %>%
      dplyr::filter(position == 4) %>%
      dplyr::mutate(cluster = "cl2") %>%
      dplyr::mutate(transect = t)
  } else {
    position4.df <- df %>%
      dplyr::filter( time == daytime,  transect == t) %>%
      dplyr::filter(position == 4) %>%
      dplyr::mutate(cluster = "cl1") %>%
      dplyr::mutate(transect = t)
  }
  
  
  position5.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 5) %>%
    dplyr::mutate(cluster = "cl2") %>%
    dplyr::mutate(transect = t)
  
  position6.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 6) %>%
    dplyr::mutate(cluster = "cl2") %>%
    dplyr::mutate(transect = t)
  
  position7.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 7) %>%
    dplyr::mutate(cluster = "cl3") %>%
    dplyr::mutate(transect = t)
  
  position8.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 8)  %>%
    dplyr::mutate(cluster = "cl3") %>%
    dplyr::mutate(transect = t)
  
  position9.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 9) %>%
    dplyr::mutate(cluster = "cl3") %>%
    dplyr::mutate(transect = t)
  
  position10.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 10) %>%
    dplyr::mutate(cluster = "cl3") %>%
    dplyr::mutate(transect = t)
  
  if (t == "control") {
    cluster1.df <- bind_rows(position1.df, position2.df, position3.df)
    cluster2.df <- bind_rows(position4.df, position5.df, position6.df, position7.df)
    cluster3.df <- bind_rows(position8.df, position9.df, position10.df)
  } else {
    cluster1.df <- bind_rows(position1.df, position2.df, position3.df, position4.df)
    cluster2.df <- bind_rows(position5.df, position6.df, position7.df)
    cluster3.df <- bind_rows(position8.df, position9.df, position10.df)
  }
  
# > tempCL.df
#      transect row position Thomisidae..crab.spider.        date time week cluster
#  1    control  48        1                        0  6-Jun-2018   pm   23     cl1
#  2    control  50        1                        0  6-Jun-2018   pm   23     cl1
#  3    control  52        1                        0  6-Jun-2018   pm   23     cl1                         
 
  weeks.vector <- getWeeks(df)
  clusterStats1.df <- data.frame()
  clusterStats2.df <- data.frame()
  clusterStats3.df <- data.frame()
  
  for (i in 1:length(weeks.vector)) {
    
    temp1.df <- cluster1.df %>%
    dplyr::filter( week ==  weeks.vector[[i]]) %>%
    dplyr::summarise( 
        transect = t,
        time = daytime,
        cluster = "cl1",
        week = weeks.vector[[i]],
        mean = mean(Thomisidae..crab.spider.),
        sd = sd(Thomisidae..crab.spider.)
    )
    
    temp2.df <- cluster2.df %>%
      dplyr::filter( week ==  weeks.vector[[i]]) %>%
      dplyr::summarise( 
        transect = t,
        time = daytime,
        cluster = "cl2",
        week = weeks.vector[[i]],
        mean = mean(Thomisidae..crab.spider.),
        sd = sd(Thomisidae..crab.spider.)
      )
    
    temp3.df <- cluster3.df %>%
      dplyr::filter( week ==  weeks.vector[[i]]) %>%
      dplyr::summarise( 
        transect = t,
        time = daytime,
        cluster = "cl3",
        week = weeks.vector[[i]],
        mean = mean(Thomisidae..crab.spider.),
        sd = sd(Thomisidae..crab.spider.)
      )
    clusterStats1.df <- bind_rows(temp1.df, clusterStats1.df)
    clusterStats2.df <- bind_rows(temp2.df, clusterStats2.df)
    clusterStats3.df <- bind_rows(temp3.df, clusterStats3.df)
  }
  
  clusterStats.df <- rbind(clusterStats1.df, clusterStats2.df, clusterStats3.df)
  
  #> clusterStats.df
  #    transect time cluster week       mean        sd
  # 1   control   pm     cl1   34 0.03703704 0.1924501
  # 2   control   pm     cl1   32 0.03703704 0.1924501
  # 3   control   pm     cl1   31 0.00000000 0.0000000
  # 4   control   pm     cl1   30 0.03703704 0.1924501
  
  # normalize
  maxMean <- max(clusterStats.df$mean)
  maxSD <- max(clusterStats.df$sd)
  clusterStats.df <- clusterStats.df %>% 
    dplyr::mutate(normalMean = (mean/maxMean)) %>%
    dplyr::mutate(normalSD = (sd/maxSD))
  # calculate distance
  clusterStats.df <- clusterStats.df %>% 
    dplyr::mutate(distanceTenX = sqrt( (normalMean*10)**2 + (normalSD*10)**2))
  
# > clusterStats.df
#     transect time cluster week       mean        sd normalMean  normalSD distanceTenX
#  1   control   pm     cl1   34 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
#  2   control   pm     cl1   32 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
#  3   control   pm     cl1   31 0.00000000 0.0000000 0.00000000 0.0000000     0.000000
#  4   control   pm     cl1   30 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
      
  return(clusterStats.df)
  
}


clusterAccumulate <- function(df, t, daytime) {
  
  if (FALSE) {
    t <- "control"
    daytime <- "pm"
    df <- input.df
    }
  
  cluster1.df <- df %>%
    # dplyr::filter( time == (!! as.name(daytime)),  transect ==  (!! as.name(t))) %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 1) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  #  > head(cluster1.df)
  #  # A tibble: 6 x 3
  #        week spiders cluster
  #       <int>   <dbl> <chr>  
  #   1    23        1   cl1    
  #   2    24        10  cl1    
  #   3    25        0   cl1    
  #   4    26        1   cl1 
  
  cluster2.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 2) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  cluster3.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 3) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  if (t == "control") {
    cluster4.df <- df %>%
      dplyr::filter( time == daytime,  transect == t) %>%
      dplyr::filter(position == 4) %>%
      dplyr::group_by( week ) %>%
      dplyr::summarise( 
        #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
        spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
        cluster = "cl2"
      )
  } else {
    cluster4.df <- df %>%
      dplyr::filter( time == daytime,  transect == t) %>%
      dplyr::filter(position == 4) %>%
      dplyr::group_by( week ) %>%
      dplyr::summarise( 
        #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
        spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
        cluster = "cl1"
      )
  }
  
  
  cluster5.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 5) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  cluster6.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 6) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  cluster7.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 7) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  
  cluster8.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 8) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl3"
    )
  
  cluster9.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 9) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl3"
    )
  
  cluster10.df <- df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 10) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      #spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      spiders = sum(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl3"
    )
  
  clusters.df <- bind_rows(cluster1.df, cluster2.df, cluster3.df, cluster4.df, cluster5.df, cluster6.df, cluster7.df, cluster8.df, cluster9.df, cluster10.df)
  
  
  return(clusters.df)
  
}


clusterAccumulateTotal <- function(spider.df, t) {
  
  cluster1.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 1) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  cluster2.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 2) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  cluster3.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 3) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  if (t == "control") {
    cluster4.df <- spider.df %>%
      dplyr::filter( transect == t) %>%
      dplyr::filter(position == 4) %>%
      dplyr::group_by( week ) %>%
      dplyr::summarise( 
        spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
        cluster = "cl1"
      )
  } else {
    cluster4.df <- spider.df %>%
      dplyr::filter( transect == t) %>%
      dplyr::filter(position == 4) %>%
      dplyr::group_by( week ) %>%
      dplyr::summarise( 
        spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
        cluster = "cl2"
      )
  }
  
  
  cluster5.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 5) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  cluster6.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 6) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  cluster7.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 7) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  
  cluster8.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 8) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl3"
    )
  
  cluster9.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 9) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl3"
    )
  
  cluster10.df <- spider.df %>%
    dplyr::filter( transect == t) %>%
    dplyr::filter(position == 10) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl3"
    )
  
  clusters.df <- bind_rows(cluster1.df, cluster2.df, cluster3.df, cluster4.df, cluster5.df, cluster6.df, cluster7.df, cluster8.df, cluster9.df, cluster10.df)
  
  
  return(clusters.df)
  
}
