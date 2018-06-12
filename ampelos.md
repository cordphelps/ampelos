ampelos
================

``` r
plotRidges <- function(data, bugs, where, when, wk, caption) {
  
  if (wk < 23 | wk > 52) {  # we don't have a valid week
    
      if (when != "am" & when != "pm") {    # use all the data for each day
        filteredBugs.df <- filter(data, transect== where)
      } else {                              # use partial daily data
        filteredBugs.df <- filter(data, transect== where & time== when)
      }
    
  } else {  # maybe we have a valid week....
    
      if (when != "am" & when != "pm") {   # use all the data for each day
        filteredBugs.df <- filter(data, transect== where & week== wk)
      } else {                             # use partial daily data
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
  
  #gg2 <- ggplot(newBugs.df,aes(x=positionX, y=spider, fill=spider))+
  gg2 <- ggplot(newBugs.df,aes_string(x="positionX", y=bugs[1], fill=bugs[1])) +
  geom_density_ridges(
    #aes(point_color = spider, point_fill=spider, point_shape=spider),
    # https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot
    aes_string(point_color = bugs[1], point_fill=bugs[1], point_shape=bugs[1]),
    alpha = .2, jittered_points = FALSE, show.legend=F) +
    scale_point_color_hue(l = 40)  +
    scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23)) +
    xlim(1,10) +
    scale_x_continuous(breaks=seq(-12,200,16))  +
    labs(title= paste("Apparent Probability Density, ", 
                      "transect: ", where, sep=""), 
         subtitle = paste("traps with ", bugs[1], "s: ", percentOcurrance, 
                        " %", sep=""),
       x="trap distance from row edge (ft)",
       y= paste(bugs[1], " counts\nper trap", sep=""),
       #caption="10 June 2018")
       caption=caption) +
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

    ## [1] 399

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
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
filteredBugs.df <- filter(bugs.df, transect=="oakMargin" & time== "pm")
#speciesFactors <- factor(bugs.df, levels=species)

newBugs.df <- subset(filteredBugs.df, select= c("position", speciesList))
# http://tidyr.tidyverse.org/articles/tidy-data.html
#newBugs.df <- newBugs.df %>% gather(species, count, spider)

position.list <- newBugs.df$position   #     
newBugs.df$position <- NULL                        
#newBugs.df$position <- as.character(position.list)
newBugs.df$position <- as.factor(position.list)

#row.list <- newBugs.df$row   # convert numeric to char    
#newBugs.df$row <- NULL                        
#newBugs.df$row <- as.character(row.list)

gg1 <- ggplot(newBugs.df,aes(y=position, x=spider, fill=position))+
  geom_density_ridges(
    aes(point_color = position),
                      alpha = .2, jittered_points = TRUE) +
  scale_point_color_hue(l = 40)  +
  xlim(0,4) +
  labs(title="Perceptions of Probability: oakMargin transect",
       y="trap position",
       x="bug counts",
       caption="10 June 2018")

print(gg1)
```

    ## Picking joint bandwidth of 0.386

![](ampelos_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
speciesList <- c("spider")

print(plotRidges(data=bugs.df, bugs=speciesList, 
                 where="oakMargin", when="pm", wk=23, caption=Sys.Date()))
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Picking joint bandwidth of 24.3

![](ampelos_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# grid.arrange(gg1, gg2, ncol=1, nrow=2)

# ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")
```
