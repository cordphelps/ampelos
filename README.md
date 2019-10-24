ampelos
================

![landscape](./photos/landscapeOak.JPG)

## how does a ‘semi-natural habitat’ (SNH) field margin influence the population of beneficial insects in an organic vineyard?

### what was the weekly composition of species and individuals?

#### observation: crab spiders were the most prevalent canopy dwelling beneficial arthropods

    ## Warning: Transformation introduced infinite values in continuous y-axis
    
    ## Warning: Transformation introduced infinite values in continuous y-axis
    
    ## Warning: Transformation introduced infinite values in continuous y-axis
    
    ## Warning: Transformation introduced infinite values in continuous y-axis
    
    ## Warning: Transformation introduced infinite values in continuous y-axis
    
    ## Warning: Transformation introduced infinite values in continuous y-axis

<img src="ampelos_files/figure-gfm/insectPop-1.png" width="50%" /><img src="ampelos_files/figure-gfm/insectPop-2.png" width="50%" />

### paint the ‘big picture’ of the crab spider data by time and transect.

#### observation: a visual inspection of the data suggests a seasonal break in crab spider populations initially after week 25 and another break after week 31.

#### observation: trapped crab spiders are more numerous at the end of the day than they are at the end of the night.

    ## Warning: filter_() is deprecated. 
    ## Please use filter() instead
    ## 
    ## The 'programming' vignette or the tidyeval book can help you
    ## to program with filter() : https://tidyeval.tidyverse.org
    ## This warning is displayed once per session.

<img src="ampelos_files/figure-gfm/bigPicture-1.png" width="50%" /><img src="ampelos_files/figure-gfm/bigPicture-2.png" width="50%" />

### as spiders are collected along the length of the transects, are physical clusters apparent and do they persist across multiple weeks?

#### observation: using the kmeans() algorithm, for both the SNH and control treatments, clusters designated as follows: cluster 1 is rows 1-4, cluster 2 is rows 5-7, and cluster 3 is rows 8-10.

<img src="ampelos_files/figure-gfm/overheadClusters-1.png" width="50%" /><img src="ampelos_files/figure-gfm/overheadClusters-2.png" width="50%" />

### create 9 models, one for each cluster and seasonal timeframe, based on the Oceanic Tool Complexity model of Kline. The model predicts the rate of trapped spiders, model parameters are log(population), contact rate, and the interaction of both. For each model, calculate the ‘likelihood’ that the interaction of population and SNH contact influences the model prediction by normalizing the difference between the prediction of a model including the interaction parameter with the prediction of a model that does not include that parameter. How plausible is it that a “high contact” (“SNH”) transect row will have more trapped spiders than a “low contact” (control) transect row?

#### observation: generally, it seems plausible that the interaction of log(population) and contact rate positively influences the number of trapped spiders for cluster one and two during seasonal timeframe one and two. This influence appears implausible in cluster three. (Further analysis suggests that the model breaks down in seasonal timeframe three.)

    ## Warning: 'bayesplot' namespace cannot be unloaded:
    ##   namespace 'bayesplot' is imported by 'shinystan' so cannot be unloaded

![](ampelos_files/figure-gfm/clusterBayes-1.png)<!-- -->

## model evaluation

### Plot central (quantile-based) posterior interval estimates from MCMC draws. Evaluate each for ‘significance’

#### except for cluster two, seasonal timeframe 3, all confidence intervals include 0, so they are not classically ‘significant’)

<img src="ampelos_files/figure-gfm/clusterCoefficients-1.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterCoefficients-2.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterCoefficients-3.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterCoefficients-4.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterCoefficients-5.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterCoefficients-6.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterCoefficients-7.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterCoefficients-8.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterCoefficients-9.png" width="33%" />

## model evaluation (continued)

### do the mcmc chains for the models seem reasonable?

#### yes, trace plots suggest convergence of the chains that form the model parameter posterior distributions (<https://www.rensvandeschoot.com/brms-wambs/> paragraph 2)

<img src="ampelos_files/figure-gfm/clusterMCMC-1.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterMCMC-2.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterMCMC-3.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterMCMC-4.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterMCMC-5.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterMCMC-6.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterMCMC-7.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterMCMC-8.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterMCMC-9.png" width="33%" />

## model evaluation (continued)

### does Rhat from the summary() statistics confirm convergence?

#### observation: Rhat is equal to 1 for each of the 9 models indicates convergence. (Displaying results for i=9)

``` 
   readLines(paste(ggsave.path, "clBRMsummary-pm-", i, ".txt", sep = ""))
```

1 generateLikelihoodV2() i= 9 2 NULL 3 Family: poisson 4 Links: mu = log
5 Formula: totalSpiders ~ 1 + log\_pop + contact\_high +
contact\_high:log\_pop 6 Data: cl.st.list\[\[i\]\] (Number of
observations: 12) 7 Samples: 4 chains, each with iter = 3000; warmup =
1000; thin = 1; 8 total post-warmup samples = 8000 9  
10 Population-Level Effects: 11 Estimate Est.Error l-89% CI u-89% CI
Eff.Sample Rhat 12 Intercept 0.03 2.19 -3.45 3.57 5721 1.00 13 log\_pop
-0.00 0.98 -1.58 1.57 5750 1.00 14 contact\_high -0.34 0.92 -1.79 1.14
5241 1.00 15 log\_<pop:contact_high> -0.71 0.56 -1.64 0.16 4611 1.00
16  
17 Samples were drawn using sampling(NUTS). For each parameter,
Eff.Sample 18 is a crude measure of effective sample size, and Rhat is
the potential 19 scale reduction factor on split chains (at convergence,
Rhat =
1).

## model evaluation (continued)

### compare variations of the basic model to determine which parameters are most meaningful. Deconstruct the model per <https://bookdown.org/connect/#/apps/1850/access> 3 (section 10.2, search “bit by bit”) to calculate the WAIC for each variation of the model

##### references <https://statmodeling.stat.columbia.edu/2015/10/03/comparing-waic-or-loo-or-any-other-predictive-error-measure/> ; <http://www.stat.columbia.edu/~gelman/research/unpublished/loo_stan.pdf>

#### observation: none of the model variations reveal meaningful contributions by any particular parameter as all confidence intervals almost fully overlap. <https://discourse.mc-stan.org/t/brms-loo-compare-interpretation-of-waic-deltas/10318>)

<img src="ampelos_files/figure-gfm/clusterAltModels-1.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterAltModels-2.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterAltModels-3.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterAltModels-4.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterAltModels-5.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterAltModels-6.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterAltModels-7.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterAltModels-8.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterAltModels-9.png" width="33%" />

## model evaluation (continued)

### evaluate the effect of “contact rate” on model prediction for 9 models, 3 clusters across 3 seasonal timeframes. Assume the median spider population per vine varies by seasonal timeframe according to the results above. Calculate the posterior distribution of the expected rate of trapped spiders per vine for high and low contact environments, normalize the difference in these two expected rates and plot the difference. Do the distributions look reasonable?

#### observation: for the first two seasonal timeframes, yes, the number of mcmc iterations seem sufficient as the distributions are single peaked and smooth. (<https://www.rensvandeschoot.com/brms-wambs/> paragraph 4) They also have reasonable bounds (<https://www.rensvandeschoot.com/brms-wambs/> paragraph 6). The model yields excessively broad distributions in the first seasonal timeframe suggesting that the model has broken down at this point.

<img src="ampelos_files/figure-gfm/clusterDiag1-1.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag1-2.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag1-3.png" width="33%" />

## spiders predicted with SNH effects in the model

``` r
print(gg.list[[13]])
```

![](ampelos_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
print(gg.list[[14]])
```

![](ampelos_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
print(gg.list[[15]])
```

![](ampelos_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

## spiders predicted without SNH effects in the model

``` r
print(gg.list[[16]])
```

![](ampelos_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
print(gg.list[[17]])
```

![](ampelos_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
print(gg.list[[18]])
```

![](ampelos_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

## model evaluation (continued)

### what is the impact of parameter joint uncertainty on model prediction?

#### observation: as predictors of trapped spiders, the parameter uncertainty of log(population) and contact rate are negatively correlated during seasonal timeframe 1 and 2 in each cluster. So, for smaller vine populations, SNH contact has a larger effect. These parameters are very mildly negatively correlated during seasonal timeframe 3 for each cluster.

<img src="ampelos_files/figure-gfm/clusterDiag2-1.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-2.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-3.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-4.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-5.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-6.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-7.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-8.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-9.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-10.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-11.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-12.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-13.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-14.png" width="33%" /><img src="ampelos_files/figure-gfm/clusterDiag2-15.png" width="33%" />

## { end model evaluation }

## model retrospective

### the model does not capture the effect of vineyard cultural controls. For example, the vineyard cover crop does not receive the same irrigation support as do the vines, and it is eventually removed by roughly week 28. The Ampelos vineyard management protocol also calls for the application of organic fungicides by foliar spray every 10 days. This effect is not modelled.

### the model does not capture possible environmental effects including those related to the growth and decline of the vineyard canopy.

## further data analysis

### the crab spider is a dominant species in the vineyard. Over the course of the season, how are they distributed along the length of the row?

<img src="ampelos_files/figure-gfm/ridges-1.png" width="50%" /><img src="ampelos_files/figure-gfm/ridges-2.png" width="50%" />

### Is there a difference in the number of crab spiders trapped in the morning compared to the number trapped in the evening?

#### spiders seem more active in the daylight hours (afternoon collection). Cumulative counts also reveal more crab spiders in the control transect for afternoon collection.

<img src="ampelos_files/figure-gfm/population-trends-both-1.png" width="50%" /><img src="ampelos_files/figure-gfm/population-trends-both-2.png" width="50%" /><img src="ampelos_files/figure-gfm/population-trends-both-3.png" width="50%" /><img src="ampelos_files/figure-gfm/population-trends-both-4.png" width="50%" /><img src="ampelos_files/figure-gfm/population-trends-both-5.png" width="50%" /><img src="ampelos_files/figure-gfm/population-trends-both-6.png" width="50%" />

### each of the two transects consists of 3 rows of 10 traps in each row. Is the total insect population relatively uniform among the 3 rows of a transect? Does this uniformity change over time? Compute the Jaccard Index for each week: the index *‘is a statistic used for comparing the similarity and diversity of sample sets.’*

![transect
layout](./images/transectLayout.jpg)

##### Note that *‘… the SMC counts both mutual presences (when an attribute is present in both sets) and mutual absence (when an attribute is absent in both sets) as matches and compares it to the total number of attributes in the universe, whereas the Jaccard index only counts mutual presence as matches and compares it to the number of attributes that have been chosen by at least one of the two sets.’* (<https://en.wikipedia.org/wiki/Jaccard_index>)

#### observation: in-transect crab spider counts exhibit moderate uniformity.

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

<img src="ampelos_files/figure-gfm/similarity-1.png" width="50%" /><img src="ampelos_files/figure-gfm/similarity-2.png" width="50%" />

### are population clusters visually apparent across the length of the transects?

#### observation: for example, data for week 24 and week 30

<img src="ampelos_files/figure-gfm/overheadCompare-1.png" width="50%" /><img src="ampelos_files/figure-gfm/overheadCompare-2.png" width="50%" />

### is there a difference in the arthropod abundance and diversity between the two transects?

<img src="ampelos_files/figure-gfm/diversity-1.png" width="50%" /><img src="ampelos_files/figure-gfm/diversity-2.png" width="50%" />

### species count table

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

Halictus.sp….3.part..native.bee.

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

Agapostemon.sp….green..native.bee.

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

Osmia.sp…native.bee.

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

pentamonidae…stinkBug.

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

<img src="ampelos_files/figure-gfm/speciesMatrixOak-1.png" width="50%" /><img src="ampelos_files/figure-gfm/speciesMatrixOak-2.png" width="50%" />

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

``` r
sessionInfo()
```

    ## R version 3.6.1 (2019-07-05)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Mojave 10.14.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] ggcorrplot_0.1.3      reshape2_1.4.3        coda_0.19-3          
    ##  [4] StanHeaders_2.18.1-10 Rcpp_1.0.1            ade4_1.7-13          
    ##  [7] kableExtra_1.1.0      knitr_1.23            forcats_0.4.0        
    ## [10] stringr_1.4.0         purrr_0.3.2           readr_1.3.1          
    ## [13] tidyr_0.8.3           tibble_2.1.3          tidyverse_1.2.1      
    ## [16] rlang_0.4.0           gridExtra_2.3         ggridges_0.5.1       
    ## [19] ggplot2_3.2.0         dplyr_0.8.3          
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] colorspace_1.4-1     brms_2.9.0           rsconnect_0.8.15    
    ##  [4] markdown_1.0         base64enc_0.1-3      rstudioapi_0.10     
    ##  [7] rstan_2.19.2         DT_0.7               lubridate_1.7.4     
    ## [10] xml2_1.2.0           bridgesampling_0.7-2 codetools_0.2-16    
    ## [13] shinythemes_1.1.2    zeallot_0.1.0        bayesplot_1.7.0     
    ## [16] jsonlite_1.6         broom_0.5.2          sfsmisc_1.1-4       
    ## [19] shiny_1.3.2          compiler_3.6.1       httr_1.4.0          
    ## [22] backports_1.1.4      assertthat_0.2.1     Matrix_1.2-17       
    ## [25] lazyeval_0.2.2       cli_1.1.0            later_0.8.0         
    ## [28] htmltools_0.3.6      prettyunits_1.0.2    tools_3.6.1         
    ## [31] igraph_1.2.4.1       gtable_0.3.0         glue_1.3.1          
    ## [34] cellranger_1.1.0     vctrs_0.2.0          nlme_3.1-140        
    ## [37] crosstalk_1.0.0      xfun_0.8             ps_1.3.0            
    ## [40] rvest_0.3.4          mime_0.7             miniUI_0.1.1.1      
    ## [43] gtools_3.8.1         MASS_7.3-51.4        zoo_1.8-6           
    ## [46] scales_1.0.0         colourpicker_1.0     hms_0.5.0           
    ## [49] promises_1.0.1       Brobdingnag_1.2-6    parallel_3.6.1      
    ## [52] inline_0.3.15        RColorBrewer_1.1-2   shinystan_2.5.0     
    ## [55] yaml_2.2.0           loo_2.1.0            reshape_0.8.8       
    ## [58] stringi_1.4.3        highr_0.8            dygraphs_1.1.1.6    
    ## [61] pkgbuild_1.0.3       pkgconfig_2.0.2      matrixStats_0.54.0  
    ## [64] evaluate_0.14        lattice_0.20-38      rstantools_1.5.1    
    ## [67] htmlwidgets_1.3      labeling_0.3         tidyselect_0.2.5    
    ## [70] processx_3.4.1       GGally_1.4.0         plyr_1.8.4          
    ## [73] magrittr_1.5         R6_2.4.0             generics_0.0.2      
    ## [76] pillar_1.4.2         haven_2.1.1          withr_2.1.2         
    ## [79] xts_0.11-2           abind_1.4-5          modelr_0.1.4        
    ## [82] crayon_1.3.4         denstrip_1.5.4       rmarkdown_1.14      
    ## [85] readxl_1.3.1         callr_3.3.1          threejs_0.3.1       
    ## [88] digest_0.6.20        webshot_0.5.1        xtable_1.8-4        
    ## [91] httpuv_1.5.1         stats4_3.6.1         munsell_0.5.0       
    ## [94] viridisLite_0.3.0    shinyjs_1.0
