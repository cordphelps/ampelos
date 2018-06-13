ampelos
================

``` r
plotRidges <- function(data, bugs, where, when, wk, caption) {
  
  if (wk < 23 | wk > 52) {  # we definitely don't have a valid week
                            # this case indicates 'use data from all weeks'
      cumulative <- "cumulative"
    
      if (when != "am" & when != "pm") {    # use all the data (am and pm) for each day
        filteredBugs.df <- filter(data, transect== where)
      } else {                              # use partial data (am or pm) for each day
        filteredBugs.df <- filter(data, transect== where & time== when)
      }
    
  } else {  #  we might have a 'valid' week (data for the specified week could be
            #  missing....)
      cumulative <- as.character(wk)
    
      if (when != "am" & when != "pm") {   # use all the data (am and pm) for each day
        filteredBugs.df <- filter(data, transect== where & week== wk)
      } else {                             # use partial data (am or pm) for each day
        filteredBugs.df <- filter(data, transect== where & time== when & week== wk)
      }
    
    
  }
  


  # simplify to include the trap position and the bug in the list
  newBugs.df <- subset(filteredBugs.df, select= c("positionX", bugs))

  spider_rows <- count(newBugs.df)
  trapsWithSpiders <- count(add_count(newBugs.df) %>% filter(spider>0))
  percentOcurrance <- (trapsWithSpiders / spider_rows) * 100
  # https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
  percentOcurrance <- format(round(percentOcurrance, 2), nsmall = 2)
  
  
  spider.list <- newBugs.df$spider   #     
  newBugs.df$spider <- NULL                        
    #newBugs.df$position <- as.character(position.list)
  newBugs.df$spider <- as.factor(spider.list)
  
#Density plots can be thought of as plots of smoothed histograms.
#The smoothness is controlled by a bandwidth parameter that is analogous 
#to the histogram binwidth.
#Most density plots use a kernel density estimate, but there are other 
#possible strategies; qualitatively the particular strategy rarely matters.
# https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/histdens.html
  
  #gg2 <- ggplot(newBugs.df,aes(x=positionX, y=spider, fill=spider))+
  gg2 <- ggplot(newBugs.df, aes_string(x="positionX", y=bugs[1], fill=bugs[1])) +
  geom_density_ridges(
    #aes(point_color = spider, point_fill=spider, point_shape=spider),
    # https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot
    aes_string(point_color = bugs[1], point_fill=bugs[1], point_shape=bugs[1]),
    alpha = .2, jittered_points = TRUE, show.legend=F) +
    scale_point_color_hue(l = 40)  +
    scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23, 24)) +
    #stat_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = .2, jittered_points = TRUE) +
    
    xlim(1,10) +
    # http://ggplot2.tidyverse.org/reference/sec_axis.html
    scale_x_continuous(breaks=seq(4,200,16), 
                       sec.axis = sec_axis(~.*.3048,
                                           breaks= seq(0, 80, 10),
                                           name= "trap distance from row end (m)"))  +
    labs(title= paste("Apparent Probability Density, ", 
                      "transect: ", where, sep=""), 
         subtitle = paste("week: ", cumulative, ", collection time: ", when, 
                          "\ntraps with ", bugs[1], "s: ", percentOcurrance, " %", 
                          
                          sep=""),
       x="trap distance from row end (ft)",
       y= paste(bugs[1], " counts\nper trap", sep=""),
       #caption="10 June 2018")
       caption=paste(caption, 
                     "\nhttps://en.wikipedia.org/wiki/Kernel_density_estimation", 
                     sep="")) +
    theme(panel.grid.minor=element_blank()) +  # hide the minor gridlines
    theme(axis.title.y = element_text(angle = 0, vjust=.5))

  return(gg2)
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

# ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")
```

``` r
source.url <- c("https://raw.githubusercontent.com/cordphelps/ampelos/master/bugs.csv")
bugs.df <- read.csv(source.url, header=TRUE, row.names=NULL)

speciesList <- c("spider")

total <- bugCount() 

print(total)
```

    ## [1] 928

``` r
# https://github.com/zonination/perceptions
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.7.2     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.2.0

    ## ── Conflicts ────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggridges)
#library(gridExtra)
#library(scales)
```

``` r
speciesList <- c("spider")

print(plotRidges(data=bugs.df, bugs=speciesList, 
                 where="oakMargin", when="pm", wk=1, caption=Sys.Date()))
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Picking joint bandwidth of 25.1

![](ampelos_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
speciesList <- c("spider")

print(plotRidges(data=bugs.df, bugs=speciesList, 
                 where="control", when="pm", wk=1, caption=Sys.Date()))
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Picking joint bandwidth of 33.5

![](ampelos_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# grid.arrange(gg1, gg2, ncol=1, nrow=2)

# ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")
```
