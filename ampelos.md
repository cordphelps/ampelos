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
source('./code/bayes.R')

source.url <- c("https://raw.githubusercontent.com/cordphelps/ampelos/master/data/bugs.csv")
bugs.df <- read.csv(source.url, header=TRUE, row.names=NULL)
```

weekly composition of species and individuals?
----------------------------------------------

#### TO-DO: annotate charts with key dates ( spray events, cover crop collapse, veraision, )

``` r
# (fig.keep='none' suppresses the plots temporarily)

ggC <- div(bugs.df, species=FALSE, ignoreBees=FALSE, t="control")
ggO <- div(bugs.df, species=FALSE, ignoreBees=FALSE, t="oakMargin")

ggC2 <- div(bugs.df, species=TRUE, ignoreBees=FALSE, t="control")
ggO2 <- div(bugs.df, species=TRUE, ignoreBees=FALSE, t="oakMargin")
```

``` r
grid.arrange(ggO2, ggC2, ncol=2, nrow=1)
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
grid.arrange(ggO, ggC, ncol=2, nrow=1)
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-3-2.png)

![transect layout](./images/transectLayout.jpg)

each of the two transects consists of 3 rows of 10 traps in each row. Is the total insect population relatively uniform among the 3 rows of a transect? Does this uniformity change over time? Compute the Jaccard Index for each week: the index *'is a statistic used for comparing the similarity and diversity of sample sets.'*
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#### Note that *'... the SMC counts both mutual presences (when an attribute is present in both sets) and mutual absence (when an attribute is absent in both sets) as matches and compares it to the total number of attributes in the universe, whereas the Jaccard index only counts mutual presence as matches and compares it to the number of attributes that have been chosen by at least one of the two sets.'* (<https://en.wikipedia.org/wiki/Jaccard_index>)

``` r
library(dplyr)

gOak <- compareJaccardMultiWeekV4(data=bugs.df, ignoreBees=TRUE,
                                  t="oakMargin",
                                  transectText="oakMargin")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
gControl <- compareJaccardMultiWeekV4(data=bugs.df, ignoreBees=TRUE,
                                  t="control",
                                  transectText="control")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
g <- arrangeGrob(gOak, gControl, nrow=2)
```

is there a difference in the spider populations for the two transects?
----------------------------------------------------------------------

``` r
reducedData.df <- selectDataAcrossTransects(data=bugs.df, week=quo(24), species=quo(Thomisidae..crab.spider.))

g24 <- plotBugDistribution(data=reducedData.df, 
                          title=paste("crab spider occurrences", "\nweek 24", sep=""), 
                          caption="stuff")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
reducedData.df <- selectDataAcrossTransects(data=bugs.df, week=quo(30), species=quo(Thomisidae..crab.spider.))

g30 <- plotBugDistribution(data=reducedData.df, 
                          title=paste("crab spider occurrences", "\nweek 30", sep=""), 
                          caption="stuff")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
# g <- arrangeGrob(g1, g2, nrow=1)
```

are clusters appearing and do they persist across multiple weeks?
-----------------------------------------------------------------

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

<img src="ampelos_files/figure-markdown_github/unnamed-chunk-7-1.png" width="100%" />

``` r
source('./code/bayes.R')

returnList <- evaluateDailySpiderCounts(bugs.df)

lh.df <- returnList[[5]]

returnList[[6]] <- plotLikelihood(df=lh.df)

print(returnList[[1]])
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
print(returnList[[6]])
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
kruskal.test(likelihood ~ seasonalTimeframe, data = lh.df)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  likelihood by seasonalTimeframe
    ## Kruskal-Wallis chi-squared = 0.088889, df = 2, p-value = 0.9565

``` r
pairwise.wilcox.test(lh.df$likelihood, lh.df$seasonalTimeframe,
                          p.adjust.method = "BH")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test 
    ## 
    ## data:  lh.df$likelihood and lh.df$seasonalTimeframe 
    ## 
    ##       one two
    ## two   1   -  
    ## three 1   1  
    ## 
    ## P value adjustment method: BH

using the control transect as a baseline, how do the populations in the primary transect segments compare over time? (cluster analysis suggests trap segments 1-4, 5-7, and 8-10)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#### TO-DO: refine normalization method

``` r
if (FALSE) {
  positionText <- paste("\ntransect positions ", "1-4", sep="")
  g3 <- compareTransectUsingQuosure(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="LT",
                                 initialPosition=quo(5), 
                                 secondaryPosition=quo(0),
                                 positionText)
  positionText <- paste("\ntransect positions ", "5-7", sep="")
  g46 <- compareTransectUsingQuosure(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="BETWEEN",
                                 initialPosition=quo(4), 
                                 secondaryPosition=quo(8),
                                 positionText)
  positionText <- paste("\ntransect positions ", "8-10", sep="")
  g7 <- compareTransectUsingQuosure(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="GT",
                                 initialPosition=quo(7), 
                                 secondaryPosition=quo(0),
                                 positionText)
  g <- arrangeGrob(g3, g46, g7, nrow=3)
  newFile <- paste("ampelos-", format(Sys.time(), "%d-%m-%Y-%H%M"), ".pdf", sep = "")
  ggsave(file=newFile, g, width=20, height=30, device = "pdf", units = "cm") #saves g
  
} else {
  
  positionText <- paste("\ntransect positions ", "1-4", sep="")
  g3 <- compareTransectG2V1(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="LT",
                                 initialPosition=quo(5), 
                                 secondaryPosition=quo(0),
                                 positionText)
  positionText <- paste("\ntransect positions ", "5-7", sep="")
  g46 <- compareTransectG2V1(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="BETWEEN",
                                 initialPosition=quo(4), 
                                 secondaryPosition=quo(8),
                                 positionText)
  positionText <- paste("\ntransect positions ", "8-10", sep="")
  g7 <- compareTransectG2V1(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="GT",
                                 initialPosition=quo(7), 
                                 secondaryPosition=quo(0),
                                 positionText)
  
}
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-9-1.png)![](ampelos_files/figure-markdown_github/unnamed-chunk-9-2.png)![](ampelos_files/figure-markdown_github/unnamed-chunk-9-3.png)

how about the insect populations themselves? Is the presence of any particular species correlated with the presence of a different species?
-------------------------------------------------------------------------------------------------------------------------------------------

``` r
m1 <- simMatrixV3(data=bugs.df, transect=quo("oakMargin"),
                                transectText="oakMargin")
```

<img src="ampelos_files/figure-markdown_github/unnamed-chunk-10-1.png" width="100%" />

``` r
#g <- arrangeGrob(m1, m2, nrow=2)
```

``` r
m2 <- simMatrixV3(data=bugs.df, transect=quo("control"),
                                transectText="control")
```

<img src="ampelos_files/figure-markdown_github/unnamed-chunk-11-1.png" width="100%" />

``` r
#g <- arrangeGrob(m1, m2, nrow=2)
```

does the crab spider population appear to change over time? Is there a difference between the two transects?
------------------------------------------------------------------------------------------------------------

``` r
# pass variables to dyplr pipes
# https://stackoverflow.com/questions/27975124/pass-arguments-to-dplyr-functions
#plotSpeciesTrend(data=bugs.df, bugs=quo(Thomisidae..crab.spider.), #speciesText="Crab Spider", where="control", when="pm", #caption=Sys.Date())

plotSpeciesTrendV2(data=bugs.df, bugs=quo(Thomisidae..crab.spider.), speciesText="Crab Spider", where="control", when="pm", caption=Sys.Date())
```

<img src="ampelos_files/figure-markdown_github/unnamed-chunk-12-1.png" width="100%" /><img src="ampelos_files/figure-markdown_github/unnamed-chunk-12-2.png" width="100%" />

    ## NULL

the crab spider is a dominant species in the vineyard. How are they distributed along the length of the row?
------------------------------------------------------------------------------------------------------------

TO-DO: develop and apply normalization method
---------------------------------------------

``` r
plotRidges(data=bugs.df, combined=FALSE, bugs="Thomisidae..crab.spider.", speciesText="Crab Spider", where="control", when="pm", wk=1, caption=Sys.Date())
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Picking joint bandwidth of 27.8

![](ampelos_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
new.df <- bugs.df %>% mutate(newColumn = ifelse(Thomisidae..crab.spider. > 0, 1, 0))
plotRidges(data=new.df, combined=TRUE, bugs="newColumn", speciesText="Crab Spider", where="control", when="pm", wk=1, caption=Sys.Date())
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Picking joint bandwidth of 16.9

![](ampelos_files/figure-markdown_github/unnamed-chunk-13-2.png)

``` r
plotRidges(data=new.df, combined=TRUE, bugs="newColumn", speciesText="Crab Spider", where="oakMargin", when="pm", wk=1, caption=Sys.Date())
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Picking joint bandwidth of 16.5

![](ampelos_files/figure-markdown_github/unnamed-chunk-13-3.png)

and the species counts?
-----------------------

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
bottom of the Oak Transect; bird repellant streamers indicating the prevailing wind direction
---------------------------------------------------------------------------------------------

![landscape](./photos/windDirection.JPG)

top of the Control Transect
---------------------------

![landscape](./photos/topOfControl.JPG)

bottom of the Control Transect with bird repellant streamers
------------------------------------------------------------

![landscape](./photos/bottomOfControl.JPG)

typical trap positioning; bowl in the fruit zone, vanes intersecting the canopy
-------------------------------------------------------------------------------

![landscape](./photos/typicalTrap.JPG)

example trap sequence
---------------------

![landscape](./photos/trapSequence.JPG)
