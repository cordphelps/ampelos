
clusterSetup <- function() {
  # Visualize cluster data using box plots
  # http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

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

clusterBoxplot <- function(df, t, time) {
  
  library(ggplot2)
  library(scales) # to access break formatting functions
  library(RColorBrewer) # customise the colours of the boxes by adding the scale_fill_brewer to the plot
  
  # **************
  #  caution: boxplots are designed to illustrate CONTINUOUS variables 
  # **************
  p10 <- ggplot(df, aes(x = as.factor(week), 
                                 y = spiders,
                                 fill = cluster )) +
    # geom_boxplot(fill = "purple", colour = "plum", alpha = 0.7) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values=c("red", "green", "blue")) +
    #scale_fill_brewer(palette = "Accent") +
    scale_x_discrete(name = "week") +
    scale_y_continuous(name = "crab spiders observed per trap") +
    # ylim(0, 1.75, .25) +
    expand_limits(y=c(0, 1.75)) +
    ggtitle(paste("cluster comparisons by week\n", "transect: ", t, ", daytime: ", time, sep="")) +
    theme_bw()
  
  p10
  
  
  
}

clusterAccumulate <- function(spider.df, t, daytime) {
  
  
  # t <- "control"
  # time <- "pm"
  
  cluster1.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 1) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  cluster2.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 2) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  cluster3.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 3) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl1"
    )
  
  if (t == "control") {
    cluster4.df <- spider.df %>%
      dplyr::filter( time == daytime,  transect == t) %>%
      dplyr::filter(position == 4) %>%
      dplyr::group_by( week ) %>%
      dplyr::summarise( 
        spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
        cluster = "cl1"
      )
  } else {
    cluster4.df <- spider.df %>%
      dplyr::filter( time == daytime,  transect == t) %>%
      dplyr::filter(position == 4) %>%
      dplyr::group_by( week ) %>%
      dplyr::summarise( 
        spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
        cluster = "cl2"
      )
  }
  
  
  cluster5.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 5) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  cluster6.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 6) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  cluster7.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 7) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl2"
    )
  
  
  cluster8.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 8) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl3"
    )
  
  cluster9.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 9) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
      cluster = "cl3"
    )
  
  cluster10.df <- spider.df %>%
    dplyr::filter( time == daytime,  transect == t) %>%
    dplyr::filter(position == 10) %>%
    dplyr::group_by( week ) %>%
    dplyr::summarise( 
      spiders = mean(Thomisidae..crab.spider., na.rm = TRUE),
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
