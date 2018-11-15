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

source.url <- c("https://raw.githubusercontent.com/cordphelps/ampelos/master/data/bugs.csv")
bugs.df <- read.csv(source.url, header=TRUE, row.names=NULL)
```

### weekly composition of species and individuals?

#### TO-DO: annotate charts with key dates ( spray events, cover crop collapse, veraision, )

``` r
#grid.arrange(ggO, ggC, ncol=1, nrow=2)
#ggsave("./code/output/diversity2.png", plot=grid.arrange(ggO, ggC, ncol=1, nrow=2), width = 6, height = 4, units = "in")

grid.arrange(gg.Species.joint, gg.Ind.joint, ncol=1, nrow=2)
```

![](ampelos_files/figure-markdown_github/diversityArrange-1.png)

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

v2.1 <- plotRidgesV2(data=new.df, combined=TRUE, bugs="newColumn", speciesText="Crab Spider", when="pm", wk=1, caption=Sys.Date())

print(v2.1)
```

![](ampelos_files/figure-markdown_github/ridges-1.png)

``` r
v2.2 <- plotRidgesV2(data=new.df, combined=TRUE, bugs="newColumn", speciesText="Crab Spider", when="am", wk=1, caption=Sys.Date())

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
```

``` r
grid.arrange(cl1.gg, cl2.gg, ncol=2, nrow=1)
```

<img src="ampelos_files/figure-markdown_github/clustersArrange-1.png" width="100%" />

#### (control cluster \#2 is slightly wider than oakMargin cluster \#2)

### How plausible is it that an oakMargin transect row will have more spiders than a control transect row?

``` r
if (TRUE) {
  #source('./code/bayes.R')

  returnList <- evaluateDailySpiderCounts(bugs.df)

  lh.df <- returnList[[5]]

  returnList[[6]] <- plotLikelihood(df=lh.df)

  print(returnList[[1]])
  print(returnList[[6]])

  # kruskal.test(likelihood ~ seasonalTimeframe, data = lh.df)

  # pairwise.wilcox.test(lh.df$likelihood, lh.df$seasonalTimeframe,
     #                     p.adjust.method = "BH")

  
}
```

![](ampelos_files/figure-markdown_github/clusterBayes-1.png)![](ampelos_files/figure-markdown_github/clusterBayes-2.png)

<table>
<thead>
<tr>
<th style="text-align:left;">
cluster
</th>
<th style="text-align:left;">
seasonalTimeframe
</th>
<th style="text-align:right;">
plausibility
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
one
</td>
<td style="text-align:left;">
one
</td>
<td style="text-align:right;">
0.668375
</td>
</tr>
<tr>
<td style="text-align:left;">
one
</td>
<td style="text-align:left;">
two
</td>
<td style="text-align:right;">
0.306375
</td>
</tr>
<tr>
<td style="text-align:left;">
one
</td>
<td style="text-align:left;">
three
</td>
<td style="text-align:right;">
0.449500
</td>
</tr>
<tr>
<td style="text-align:left;">
two
</td>
<td style="text-align:left;">
one
</td>
<td style="text-align:right;">
0.528500
</td>
</tr>
<tr>
<td style="text-align:left;">
two
</td>
<td style="text-align:left;">
two
</td>
<td style="text-align:right;">
0.346000
</td>
</tr>
<tr>
<td style="text-align:left;">
two
</td>
<td style="text-align:left;">
three
</td>
<td style="text-align:right;">
0.528000
</td>
</tr>
<tr>
<td style="text-align:left;">
three
</td>
<td style="text-align:left;">
one
</td>
<td style="text-align:right;">
0.285250
</td>
</tr>
<tr>
<td style="text-align:left;">
three
</td>
<td style="text-align:left;">
two
</td>
<td style="text-align:right;">
0.397375
</td>
</tr>
<tr>
<td style="text-align:left;">
three
</td>
<td style="text-align:left;">
three
</td>
<td style="text-align:right;">
0.701625
</td>
</tr>
</tbody>
</table>
### how do the clusters compare to each other across multiple weeks?

``` r
# strip out the other arthropods and misc stuff
df <- clusterSetup()

# for each 'position', get spiders and assign to a cluster number
cluster.df <- clusterAccumulateTotal(df, "control")
clusterBoxplot(cluster.df, "control", "(24 hours)")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-1.png)

``` r
cluster.df <- clusterAccumulateTotal(df, "oakMargin")
clusterBoxplot(cluster.df, "oakMargin", "(24 hours)")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-2.png)

``` r
cluster.df <- clusterAccumulate(df, "control", "pm")
clusterBoxplot(cluster.df, "control", "pm")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-3.png)

``` r
cluster.df <- clusterAccumulate(df, "oakMargin", "pm")
clusterBoxplot(cluster.df, "oakMargin", "pm")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-4.png)

``` r
cluster.df <- clusterAccumulate(df, "control", "am")
clusterBoxplot(cluster.df, "control", "am")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-5.png)

``` r
cluster.df <- clusterAccumulate(df, "oakMargin", "am")
clusterBoxplot(cluster.df, "oakMargin", "am")
```

![](ampelos_files/figure-markdown_github/clusterBoxPlots-6.png)

### does the crab spider population appear to change over time? Is there a difference between the two transects?

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="am", trend=TRUE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/population-trends-1.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="am", trend=FALSE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/population-trends-2.png)![](ampelos_files/figure-markdown_github/population-trends-3.png)![](ampelos_files/figure-markdown_github/population-trends-4.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="pm", trend=TRUE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/population-trends-5.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="pm", trend=FALSE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/population-trends-6.png)![](ampelos_files/figure-markdown_github/population-trends-7.png)![](ampelos_files/figure-markdown_github/population-trends-8.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="both", trend=TRUE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/population-trends-9.png)

    ## NULL

``` r
plotSpeciesTrendV3(data=bugs.df, species=quo(Thomisidae..crab.spider.), period="both", trend=FALSE, speciesText="Crab Spider", lowerWeekLimit=23, upperWeekLimit=34, caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/population-trends-10.png)![](ampelos_files/figure-markdown_github/population-trends-11.png)![](ampelos_files/figure-markdown_github/population-trends-12.png)

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
