ampelos
================

![landscape](./photos/landscapeOak.JPG)

how does a 'natural habitat' field margin influence the population of beneficial insects in an organic vineyard?
----------------------------------------------------------------------------------------------------------------

``` r
source("./code/bug-library.R")
source("./code/similarity.R")
source("./code/jaccard-similarity.R")
source("./code/diversity.R")
source("./code/k-means.R")
source("./code/clusterSimilarity.R")
source('./code/bayes.R')
source('./code/ranking.R')

source.url <- c("https://raw.githubusercontent.com/cordphelps/ampelos/master/data/bugs.csv")
bugs.df <- read.csv(source.url, header=TRUE, row.names=NULL)
```

### weekly composition of species and individuals?

``` r
returnList <- scanBugPercentages(bugs.df)

returnList <- createFamilyPercentages(returnList)

gg <- plotBugPercentages(returnList, spidersOnly=FALSE)

print(gg)
```

![](ampelos_files/figure-markdown_github/insectPop-1.png)

``` r
gg <- plotBugPercentages(returnList, spidersOnly=TRUE)

print(gg)
```

![](ampelos_files/figure-markdown_github/insectPop-2.png)

#### TO-DO: annotate charts with key dates ( spray events, cover crop collapse, veraision, )

``` r
# (fig.keep='none' suppresses the plots temporarily)

gg.Ind.joint <- divV2(bugs.df, species=FALSE, ignoreBees=FALSE)

print(gg.Ind.joint)
```

![](ampelos_files/figure-markdown_github/diversity-1.png)

``` r
gg.Species.joint <- divV2(bugs.df, species=TRUE, ignoreBees=FALSE)

print(gg.Species.joint)
```

![](ampelos_files/figure-markdown_github/diversity-2.png)

![transect layout](./images/transectLayout.jpg)

### each of the two transects consists of 3 rows of 10 traps in each row. Is the total insect population relatively uniform among the 3 rows of a transect? Does this uniformity change over time? Compute the Jaccard Index for each week: the index *'is a statistic used for comparing the similarity and diversity of sample sets.'*

##### Note that *'... the SMC counts both mutual presences (when an attribute is present in both sets) and mutual absence (when an attribute is absent in both sets) as matches and compares it to the total number of attributes in the universe, whereas the Jaccard index only counts mutual presence as matches and compares it to the number of attributes that have been chosen by at least one of the two sets.'* (<https://en.wikipedia.org/wiki/Jaccard_index>)

``` r
library(dplyr)

gOak <- compareJaccardMultiWeekV4(data=bugs.df, ignoreBees=TRUE,
                                  t="oakMargin",
                                  transectText="oakMargin")
```

![](ampelos_files/figure-markdown_github/similarity-1.png)

``` r
gControl <- compareJaccardMultiWeekV4(data=bugs.df, ignoreBees=TRUE,
                                  t="control",
                                  transectText="control")
```

![](ampelos_files/figure-markdown_github/similarity-2.png)

### the crab spider is a dominant species in the vineyard. How are they distributed along the length of the row?

#### TO-DO: develop and apply normalization method

``` r
new.df <- bugs.df %>% mutate(newColumn = ifelse(Thomisidae..crab.spider. > 0, 1, 0))

v2.1 <- plotRidgesV2(data=new.df, combined=TRUE, bugs="newColumn", speciesText="crab spider", when="pm", wk=1, caption=Sys.Date())

print(v2.1)
```

![](ampelos_files/figure-markdown_github/ridges-1.png)

``` r
v2.2 <- plotRidgesV2(data=new.df, combined=TRUE, bugs="newColumn", speciesText="crab spider", when="am", wk=1, caption=Sys.Date())

print(v2.2)
```

![](ampelos_files/figure-markdown_github/ridges-2.png)

### is there a difference in the spider populations for the two transects?

``` r
reducedData.df <- selectDataAcrossTransects(data=bugs.df, week=quo(24), species=quo(Thomisidae..crab.spider.))

g24 <- plotBugDistribution(data=reducedData.df, 
                          title=paste("crab spider occurrences", "\nweek 24", sep=""), 
                          caption="stuff")
```

![](ampelos_files/figure-markdown_github/overheadCompare-1.png)

``` r
reducedData.df <- selectDataAcrossTransects(data=bugs.df, week=quo(30), species=quo(Thomisidae..crab.spider.))

g30 <- plotBugDistribution(data=reducedData.df, 
                          title=paste("crab spider occurrences", "\nweek 30", sep=""), 
                          caption="stuff")
```

![](ampelos_files/figure-markdown_github/overheadCompare-2.png)

``` r
# g <- arrangeGrob(g1, g2, nrow=1)
```

### are clusters appearing and do they persist across multiple weeks?

``` r
clusterNumber <- 3
df <- bugs.df
species <- "Thomisidae..crab.spider."

dataList <- buildClustersByWeek(df, t="control", species="Thomisidae..crab.spider.", cn=clusterNumber)

cl1.gg <- kmPlot(list=dataList, transectText="control")

dataList <- buildClustersByWeek(df, t="oakMargin", species="Thomisidae..crab.spider.", cn=clusterNumber)

cl2.gg <- kmPlot(list=dataList, transectText="oakMargin")

print(cl1.gg)
```

![](ampelos_files/figure-markdown_github/overheadClusters-1.png)

``` r
print(cl2.gg)
```

![](ampelos_files/figure-markdown_github/overheadClusters-2.png)

#### (control cluster \#2 is slightly wider than oakMargin cluster \#2)

### 'big picture' data by cluster, time, and transect

``` r
if (TRUE) {
  #source('./code/bayes.R')

  # organize data into
  # ("week", "transect", "time", "cluster", "totalSpiders")
  #
  # plot the weekly raw data : plotWeekly()
  # create txt files saving the status output of 9 brm() cycles
  # build the likelihood data : generateLikelihoodV2() 
  returnList <- evaluateDailySpiderCounts(bugs.df)
  
  print(returnList[[1]]) # scatter plot by cluster with seasonal timeframes
  print(returnList[[2]]) # scatter plot by am/pm
  print(returnList[[3]]) # scatter plot by transect
  # returnList[[4]] is the data 'dataframe' used for the graphics (total.df)
  # from bayes.R evaluateDailySpiderCounts()
 
  
}
```

![](ampelos_files/figure-markdown_github/bigPicture-1.png)![](ampelos_files/figure-markdown_github/bigPicture-2.png)![](ampelos_files/figure-markdown_github/bigPicture-3.png)

### How plausible is it that an oakMargin transect row will have more spiders than a control transect row?

``` r
if (TRUE) {
  #source('./code/bayes.R')

  # organize data into
  # ("week", "transect", "time", "cluster", "totalSpiders")
  #
  # plot the weekly raw data : plotWeekly()
  # create txt files saving the status output of 9 brm() cycles

  returnList <- generateLikelihoodV2(df=returnList[[4]], list=returnList, showPlot=FALSE)
  returnList[[6]] <- plotLikelihood(df=returnList[[5]], sub=paste("daytime: ", 'am and pm', sep=""), cap="model 794:264:159 spider population seasonal trend")
  print(returnList[[6]]) # likelihood by cluster with seasonal timeframes
  
  gg <- weightedModelGraph(df=returnList[[7]][[1]], model=returnList[[8]][[1]], label=returnList[[9]][[1]])
  print(gg)
  gg <- weightedModelGraph(df=returnList[[7]][[2]], model=returnList[[8]][[2]], label=returnList[[9]][[2]])
  print(gg)
  gg <- weightedModelGraph(df=returnList[[7]][[3]], model=returnList[[8]][[3]], label=returnList[[9]][[3]])
  print(gg)
  
  detach("package:brms", unload=TRUE) 
  detach("package:rstan", unload=TRUE) 
  
  if (FALSE) {
  
  temp.df <- returnList[[4]]
  returnList[[4]] <- returnList[[4]] %>% dplyr::filter(time == 'pm')
  returnList <- generateLikelihoodV2(df=returnList[[4]], list=returnList, showPlot=FALSE)
  returnList[[6]] <- plotLikelihood(df=returnList[[5]], sub=paste("daytime: ", 'pm', sep=""), cap="model 794:264:159 spider population seasonal trend")
  print(returnList[[6]]) # likelihood by cluster with seasonal timeframes
  
  
  returnList[[4]] <- temp.df %>% dplyr::filter(time == 'am')
  returnList <- generateLikelihoodV2(df=returnList[[4]], list=returnList, showPlot=FALSE)
  returnList[[6]] <- plotLikelihood(df=returnList[[5]], sub=paste("daytime: ", 'am', sep=""), cap="model 794:264:159 spider population seasonal trend")
  print(returnList[[6]]) # likelihood by cluster with seasonal timeframes
  
}
  # kruskal.test(likelihood ~ seasonalTimeframe, data = lh.df)

  # pairwise.wilcox.test(lh.df$likelihood, lh.df$seasonalTimeframe,
     #                     p.adjust.method = "BH")

  
}
```

![](ampelos_files/figure-markdown_github/clusterBayes-1.png)![](ampelos_files/figure-markdown_github/clusterBayes-2.png)![](ampelos_files/figure-markdown_github/clusterBayes-3.png)![](ampelos_files/figure-markdown_github/clusterBayes-4.png)

### how do the clusters compare to each other across multiple weeks?

``` r
# strip out the other arthropods and misc stuff
input.df <- clusterSetup()

# for each 'position', get spiders and assign to a cluster number

#   !!!!!!!! function can't handle "both" !!!!!!!!!!!!!!
#cluster.df <- clusterAccumulateTotal(df, "control", "both")
#clusterBoxplot(cluster.df, "control", "(24 hours)")


#cluster.df <- clusterAccumulateTotal(df, "oakMargin", "both")
#clusterBoxplot(cluster.df, "oakMargin", "(24 hours)")


cluster.df <- clusterAccumulate(df=input.df, t="control", daytime="pm")
  #> cluster.df
  # A tibble: 110 x 3
  #    week spiders cluster
  #   <int>   <int> <chr>  
 #1    23       1 cl1    
 #2    24       8 cl1    
 #3    25       1 cl1    
 #4    26       1 cl1    
 #5    27       1 cl1
clusterBoxplot(cluster.df, "control", "pm")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-1.png)

``` r
temp.df <- clusterStats(df=input.df, t="control", daytime="pm")
#> temp.df
#   transect time cluster week       mean        sd normalMean  normalSD distanceTenX
#1   control   pm     cl1   34 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
#2   control   pm     cl1   32 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
#3   control   pm     cl1   31 0.00000000 0.0000000 0.00000000 0.0000000     0.000000
#4   control   pm     cl1   30 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
#5   co

write.table(temp.df, file="./code/output/clBoxPlotControlPM.txt", append = FALSE, sep = '\t', quote = FALSE, col.names = TRUE, dec = ".")

rankControlPM.df <- rankByWeek(df=temp.df)
# > rankControlPM.df
#   week first second third
#1    23   cl2    cl3   cl1
#2    24   cl3    cl1   cl2
#3    25   cl2    cl3   cl1
#4    26   cl2    cl3   cl1
#5    27   cl2    cl1   cl3
bubbleClusterRanks(rankControlPM.df, "control", "pm")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-2.png)

``` r
cluster.df <- clusterAccumulate(df=input.df, t="oakMargin", daytime="pm")
clusterBoxplot(cluster.df, "oakMargin", "pm")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-3.png)

``` r
temp.df <- clusterStats(df=input.df, t="oakMargin", daytime="pm")

write.table(temp.df, file="./code/output/clBoxPlotOakPM.txt", append = FALSE, sep = '\t', quote = FALSE, col.names = TRUE, dec = ".")

rankOakPM.df <- rankByWeek(df=temp.df)
bubbleClusterRanks(rankOakPM.df, "oakMargin", "pm")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-4.png)

``` r
cluster.df <- clusterAccumulate(df=input.df, "control", "am")
clusterBoxplot(cluster.df, "control", "am")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-5.png)

``` r
temp.df <- clusterStats(df=input.df, t="control", daytime="am")

write.table(temp.df, file="./code/output/clBoxPlotControlAM.txt", append = FALSE, sep = '\t', quote = FALSE, col.names = TRUE, dec = ".")

rankControlAM.df <- rankByWeek(df=temp.df)
bubbleClusterRanks(rankControlAM.df, "control", "am")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-6.png)

``` r
cluster.df <- clusterAccumulate(df=input.df, "oakMargin", "am")
clusterBoxplot(cluster.df, "oakMargin", "am")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-7.png)

``` r
temp.df <- clusterStats(df=input.df, t="oakMargin", daytime="am")

write.table(temp.df, file="./code/output/clBoxPlotOakAM.txt", append = FALSE, sep = '\t', quote = FALSE, col.names = TRUE, dec = ".")

rankOakAM.df <- rankByWeek(df=temp.df)
bubbleClusterRanks(rankOakPM.df, "oakMargin", "am")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-8.png)

### does the crab spider population appear to change over time? Is there a difference between the two transects?

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="am", trend=TRUE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/pop-trends-am-1.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="am", trend=FALSE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/pop-trends-am-2.png)![](ampelos_files/figure-markdown_github/pop-trends-am-3.png)![](ampelos_files/figure-markdown_github/pop-trends-am-4.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="pm", trend=TRUE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/pop-trends-pm-1.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="pm", trend=FALSE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/pop-trends-pm-2.png)![](ampelos_files/figure-markdown_github/pop-trends-pm-3.png)![](ampelos_files/figure-markdown_github/pop-trends-pm-4.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="both", trend=TRUE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/population-trends-both-1.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="both", trend=FALSE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/population-trends-both-2.png)![](ampelos_files/figure-markdown_github/population-trends-both-3.png)![](ampelos_files/figure-markdown_github/population-trends-both-4.png)

    ## NULL

### and the species counts?

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
count
</th>
<th style="text-align:right;">
percentage
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Diptera..Agromyzidae..leafminer..
</td>
<td style="text-align:right;">
893
</td>
<td style="text-align:right;">
19.09
</td>
</tr>
<tr>
<td style="text-align:left;">
Braconid.wasp
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
1.56
</td>
</tr>
<tr>
<td style="text-align:left;">
Halictus.sp....3.part..native.bee.
</td>
<td style="text-align:right;">
522
</td>
<td style="text-align:right;">
11.16
</td>
</tr>
<tr>
<td style="text-align:left;">
pencilBug
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
1.28
</td>
</tr>
<tr>
<td style="text-align:left;">
Agapostemon.sp....green..native.bee.
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
1.73
</td>
</tr>
<tr>
<td style="text-align:left;">
Osmia.sp...native.bee.
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
1.33
</td>
</tr>
<tr>
<td style="text-align:left;">
Honey.Bee
</td>
<td style="text-align:right;">
476
</td>
<td style="text-align:right;">
10.17
</td>
</tr>
<tr>
<td style="text-align:left;">
Bombus.californicus..bumble.
</td>
<td style="text-align:right;">
279
</td>
<td style="text-align:right;">
5.96
</td>
</tr>
<tr>
<td style="text-align:left;">
Thomisidae..crab.spider.
</td>
<td style="text-align:right;">
680
</td>
<td style="text-align:right;">
14.53
</td>
</tr>
<tr>
<td style="text-align:left;">
spider.other
</td>
<td style="text-align:right;">
171
</td>
<td style="text-align:right;">
3.65
</td>
</tr>
<tr>
<td style="text-align:left;">
ladyBug
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
0.98
</td>
</tr>
<tr>
<td style="text-align:left;">
Lygus.hesperus..western.tarnished.plant.bug.
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
0.79
</td>
</tr>
<tr>
<td style="text-align:left;">
pentamonidae...stinkBug.
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.32
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
1213
</td>
<td style="text-align:right;">
25.92
</td>
</tr>
<tr>
<td style="text-align:left;">
checkerspot.butterfly
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.58
</td>
</tr>
<tr>
<td style="text-align:left;">
Pyralidae..Snout.Moth.
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
0.36
</td>
</tr>
<tr>
<td style="text-align:left;">
Diabrotica.undecimpunctata..Cucumber.Beetle.
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.38
</td>
</tr>
<tr>
<td style="text-align:left;">
Orius..pirate.bug.
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.19
</td>
</tr>
</tbody>
</table>
### how about the insect populations themselves? Is the presence of any particular species correlated with the presence of a different species?

``` r
m1 <- simMatrixV3(data=bugs.df, transect=quo("oakMargin"),
                                transectText="oakMargin")
```

<img src="ampelos_files/figure-markdown_github/speciesMatrixOak-1.png" width="100%" />

``` r
#g <- arrangeGrob(m1, m2, nrow=2)
```

``` r
m2 <- simMatrixV3(data=bugs.df, transect=quo("control"),
                                transectText="control")
```

<img src="ampelos_files/figure-markdown_github/speciesMatrixControl-1.png" width="100%" />

``` r
#g <- arrangeGrob(m1, m2, nrow=2)
```

### bottom of the Oak Transect; bird repellant streamers indicating the prevailing wind direction

![landscape](./photos/windDirection.JPG)

### top of the Control Transect

![landscape](./photos/topOfControl.JPG)

### bottom of the Control Transect with bird repellant streamers

![landscape](./photos/bottomOfControl.JPG)

### typical trap positioning; bowl in the fruit zone, vanes intersecting the canopy

![landscape](./photos/typicalTrap.JPG)

### example trap sequence

![landscape](./photos/trapSequence.JPG)
