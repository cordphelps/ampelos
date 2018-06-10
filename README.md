CIMIS
================

``` r
library(ggplot2)
#library(dplyr)
#library(RColorBrewer)
#library(ggridges)

ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.7) +
  #scale_colour_brewer(type = "qual", aesthetics = "fill")
  scale_fill_brewer(palette="Set2") +

labs(x = 'DW-Nominate Score',
        y = element_blank(),
        title = 'Polarization of the 113th Senate',
        subtitle = 'Session 113: Dimension 1 - Economic Ideology') +
        xlim(-1,8)
```

![](ampelos_files/figure-markdown_github/unnamed-chunk-1-1.png)
