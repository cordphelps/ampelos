ampelos
================

![landscape](./photos/landscapeOak.JPG)

how does a 'natural' field margin influence the population of beneficial insects?
---------------------------------------------------------------------------------

``` r
source("./code/bug-library.R")
source("./code/similarity.R")
source("./code/jaccard-similarity.R")

source.url <- c("https://raw.githubusercontent.com/cordphelps/ampelos/master/data/bugs.csv")
bugs.df <- read.csv(source.url, header=TRUE, row.names=NULL)
```

is there a difference in the spider populations for the two transects?
----------------------------------------------------------------------

``` r
reducedData.df <- selectDataAcrossTransects(data=bugs.df, week=quo(26), species=quo(Thomisidae..crab.spider.))

g1 <- plotBugDistribution(data=reducedData.df, 
                          title=paste("crab spider occurrences", "\nweek 26", sep=""), 
                          caption="stuff")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
reducedData.df <- selectDataAcrossTransects(data=bugs.df, week=quo(30), species=quo(Thomisidae..crab.spider.))

g2 <- plotBugDistribution(data=reducedData.df, 
                          title=paste("crab spider occurrences", "\nweek 30", sep=""), 
                          caption="stuff")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
g <- arrangeGrob(g1, g2, nrow=1)
```

using the control transect as a baseline, how do the populations in the primary transect segments compare over time? (segments are traps 1-3, 4-6, and 7-10)
------------------------------------------------------------------------------------------------------------------------------------------------------------

TO-DO: refine normalization method
----------------------------------

``` r
positionText <- paste("\ntransect positions ", "1-3", sep="")
g3 <- compareTransectUsingQuosure(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="LT",
                                 initialPosition=quo(4), 
                                 secondaryPosition=quo(0),
                                 positionText)
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
positionText <- paste("\ntransect positions ", "4-6", sep="")
g46 <- compareTransectUsingQuosure(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="BETWEEN",
                                 initialPosition=quo(3), 
                                 secondaryPosition=quo(7),
                                 positionText)
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
positionText <- paste("\ntransect positions ", "7-10", sep="")
g7 <- compareTransectUsingQuosure(data=bugs.df, 
                                 species=quo(Thomisidae..crab.spider.), 
                                 operator="GT",
                                 initialPosition=quo(6), 
                                 secondaryPosition=quo(0),
                                 positionText)
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
g <- arrangeGrob(g3, g46, g7, nrow=3)
newFile <- paste("ampelos-", format(Sys.time(), "%d-%m-%Y-%H%M"), ".pdf", sep = "")
ggsave(file=newFile, g, width=20, height=30, device = "pdf", units = "cm") #saves g
```

transect design
---------------

![transect layout](./images/transectLayout.jpg)

each of the two transects consists of 3 rows of 10 traps in each row. Is the total insect population relatively uniform among the 3 rows of a transect? Does this uniformity change over time? Compute the Jaccard Index for each week: the index *'is a statistic used for comparing the similarity and diversity of sample sets.'*
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### Note that *'the Jaccard index only counts mutual presence as matches and compares it to the number of attributes that have been chosen by at least one of the two sets.'* (<https://en.wikipedia.org/wiki/Jaccard_index>)

### TO-DO: compare Jaccard to the Simple Matching Coefficient (which also counts mutual absence): <https://en.wikipedia.org/wiki/Simple_matching_coefficient>

``` r
gOak <- compareJaccardMultiWeekV3(data=bugs.df, 
                                  transect=quo("oakMargin"),
                                  transectText="oakMargin")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
gControl <- compareJaccardMultiWeekV3(data=bugs.df, 
                                      transect=quo("control"),
                                      transectText="control")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
g <- arrangeGrob(gOak, gControl, nrow=2)
```

how about the insect populations themselves? Is the presence of any particular species correlated with the presence of a different species?
-------------------------------------------------------------------------------------------------------------------------------------------

``` r
m1 <- simMatrixV2(data=bugs.df, transect=quo("oakMargin"),
                                transectText="oakMargin")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
m2 <- simMatrixV2(data=bugs.df, transect=quo("control"),
                                transectText="control")
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
g <- arrangeGrob(m1, m2, nrow=2)
```

does the crab spider population appear to change over time? Is there a difference between the two transects?
------------------------------------------------------------------------------------------------------------

``` r
# pass variables to dyplr pipes
# https://stackoverflow.com/questions/27975124/pass-arguments-to-dplyr-functions
plotSpeciesTrend(data=bugs.df, bugs=quo(Thomisidae..crab.spider.), speciesText="Crab Spider", where="control", when="pm", caption=Sys.Date())
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-6-1.png)![](ampelos_files/figure-markdown_github/unnamed-chunk-6-2.png)

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

    ## Picking joint bandwidth of 28.1

![](ampelos_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
new.df <- bugs.df %>% mutate(newColumn = ifelse(Thomisidae..crab.spider. > 0, 1, 0))
plotRidges(data=new.df, combined=TRUE, bugs="newColumn", speciesText="Crab Spider", where="control", when="pm", wk=1, caption=Sys.Date())
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Picking joint bandwidth of 17.7

![](ampelos_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
plotRidges(data=new.df, combined=TRUE, bugs="newColumn", speciesText="Crab Spider", where="oakMargin", when="pm", wk=1, caption=Sys.Date())
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Picking joint bandwidth of 17.5

![](ampelos_files/figure-markdown_github/unnamed-chunk-7-3.png)

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
761
</td>
<td style="text-align:right;">
19.73
</td>
</tr>
<tr>
<td style="text-align:left;">
Braconid.wasp
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
1.35
</td>
</tr>
<tr>
<td style="text-align:left;">
Halictus.sp....3.part..native.bee.
</td>
<td style="text-align:right;">
327
</td>
<td style="text-align:right;">
8.48
</td>
</tr>
<tr>
<td style="text-align:left;">
pencilBug
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
1.40
</td>
</tr>
<tr>
<td style="text-align:left;">
Agapostemon.sp....green..native.bee.
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
1.40
</td>
</tr>
<tr>
<td style="text-align:left;">
Osmia.sp...native.bee.
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
1.53
</td>
</tr>
<tr>
<td style="text-align:left;">
Honey.Bee
</td>
<td style="text-align:right;">
411
</td>
<td style="text-align:right;">
10.65
</td>
</tr>
<tr>
<td style="text-align:left;">
Bombus.californicus..bumble.
</td>
<td style="text-align:right;">
235
</td>
<td style="text-align:right;">
6.09
</td>
</tr>
<tr>
<td style="text-align:left;">
Thomisidae..crab.spider.
</td>
<td style="text-align:right;">
627
</td>
<td style="text-align:right;">
16.25
</td>
</tr>
<tr>
<td style="text-align:left;">
spider.other
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
3.06
</td>
</tr>
<tr>
<td style="text-align:left;">
ladyBug
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
0.88
</td>
</tr>
<tr>
<td style="text-align:left;">
Lygus.hesperus..western.tarnished.plant.bug.
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.86
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
0.39
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
1015
</td>
<td style="text-align:right;">
26.31
</td>
</tr>
<tr>
<td style="text-align:left;">
checkerspot.butterfly
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
0.65
</td>
</tr>
<tr>
<td style="text-align:left;">
Pyralidae..Snout.Moth.
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.39
</td>
</tr>
<tr>
<td style="text-align:left;">
Diabrotica.undecimpunctata..Cucumber.Beetle.
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.36
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
0.23
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
