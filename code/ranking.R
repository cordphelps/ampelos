

rankingDemo <- function() {
  
library(tibble)
library(ggplot2)
library(dplyr)

# (a tibble is not exactly a dataframe...)
intensity.df <- as.data.frame(tibble::tribble(
    ~redSoup, ~greenSoup, ~blueSoup, ~attribute,
    # 
    1, 3, 2, "a1",
    1, 2, 3, "a1",
    3, 3, 1, "a1",
    2, 1, 3, "a1",
    3, 2, 1, "a2",
    2, 2, 1, "a2",
    1, 1, 2, "a2",
    2, 1, 2, "a2"    
    ))

calculateDistance(df=intensity.df, valuesName="redSoup", 
              attName="attribute", normalize=FALSE )

out.df <- rbind(calculateDistance(df=intensity.df, valuesName="redSoup", 
                    attName="attribute", normalize=FALSE ), 
                calculateDistance(df=intensity.df, valuesName="greenSoup", 
                    attName="attribute", normalize=FALSE ),
                calculateDistance(df=intensity.df, valuesName="blueSoup", 
                    attName="attribute", normalize=FALSE )
      )

# sort
out.df <- out.df %>% dplyr::arrange(desc(distance))
#
#   row         a1   a2  distance
#1  greenSoup 2.25  1.5  2.704163
#2  blueSoup  2.25  1.5  2.704163
#3   redSoup  1.75  2.0  2.657536

highToLow.list <- as.list(out.df$row)

}

calculateDistance <- function(df, valuesName, attName, normalize) {
  
  # use two attributes to calculate a 'distance' that can be used to rank 
  # sets of values (presumably other valueColumns from the same source dataframe)
  # 
  # input: df : dataframe
  #        valuesName : (the column name)
  #        attName : columnName measurement attribute used to sub-set the values
  #        normalize : boolean indicating that the values sould be normalized
  #
  # example
  # valueName is a list of engine performance measurements
  # attName indicates the meaning of the value (2 dimensions)
  #
  # valueName     attName  
  #        15     temp
  #        14     temp
  #        16     temp
  #        44     pressure
  #        45     pressure
  #        50     pressure
  #
  # output: 
  
  #
  # usage: calculateRank(df=intensity.df, valuesName="redSoup", attName="attribute", normalize=FALSE )
  #
  # TODO
  # check if the values are numeric
  # check if there is a certain minimum number of values
  # check if the attribute is 2 dimensional
  
  
  # in.df <- df %>% 
    # group_by(!! as.name(attName)) %>%
    #   !! as.name(!! valuesName) = 
    # summarise(mean(!! as.name(valuesName))) %>%
    # select(- !! as.name(attName))
      
  in.df <- df %>%
    select( !! as.name(attName)) %>%
    
  
    
    
  in.df <- as.data.frame(transpose_df(in.df))
  names(in.df) <- c("row", "a1", "a2")
  
  final.df <- in.df %>% 
    mutate(distance=sqrt(a1**2 + a2**2)) 
  
  final.df[1,1] <- valuesName
  
  
    if (FALSE) {
  ggplot(data=final.df, aes(y=distance, x=as.factor(valuesName))) +
    geom_point()
    }
  
  #             row   a1 a2 distance
  # 1 mean(redSoup) 1.75  2 2.657536
  
  
  return(final.df)
  
}

  transpose_df <- function(df) {
    # https://stackoverflow.com/questions/42790219/how-do-i-transpose-a-tibble-in-r
    t_df <- data.table::transpose(df)
    colnames(t_df) <- rownames(df)
    rownames(t_df) <- colnames(df)
    t_df <- t_df %>%
      tibble::rownames_to_column(df = .) %>%
      tibble::as_data_frame(x = .)
    return(t_df)
  }
  
  rankByWeek <- function(df) {
    #
    # for each transect:week, return an ordered list indicating the clusters
    # ranked by 'distanceTenX'
    #
    # input df:  (transect and time are pre-selected, parameters are pre-calculated)
    #
    # > temp.df
    #    transect time cluster week       mean        sd normalMean  normalSD distanceTenX
    # 1   control   pm     cl1   34 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
    # 2   control   pm     cl1   32 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
    # 3   control   pm     cl1   31 0.00000000 0.0000000 0.00000000 0.0000000     0.000000
    # 4   control   pm     cl1   30 0.03703704 0.1924501 0.02941176 0.1538812     1.566667
    #
    # output: (rank is high to low)
    #
    # > clusterOrder.df
    #    week first second third
    # 1    23   cl2    cl3   cl1
    # 2    24   cl3    cl1   cl2
    # 3    25   cl2    cl3   cl1
    # 4    26   cl2    cl3   cl1
    # 5    27   cl2    cl1   cl3
    # 6    28   cl1    cl3   cl2
    # 7    29   cl3    cl2   cl1
    # 8    30   cl3    cl2   cl1
    # 9    31   cl2    cl3   cl1
    # 10   32   cl3    cl1   cl2
    # 11   34   cl3    cl1   cl2
    #
    
    weeks.vector <- getWeeks(df)
    clusterOrder.df <- data.frame()
    
    for (i in 1:length(weeks.vector)) {
      
      clusterDist1 <- df %>% 
        dplyr::filter(cluster=="cl1", week ==  weeks.vector[[i]]) %>%
        dplyr::select(distanceTenX) %>%
        max(.)
    
      clusterDist2 <- df %>% 
        dplyr::filter(cluster=="cl2", week ==  weeks.vector[[i]]) %>%
        dplyr::select(distanceTenX) %>%
        max(.)
    
      clusterDist3 <- df %>% 
        dplyr::filter(cluster=="cl3", week ==  weeks.vector[[i]]) %>%
        dplyr::select(distanceTenX) %>%
       max(.)
      
      clusterStack1.df <- data.frame(cluster="cl1", distanceTenX=clusterDist1)
      clusterStack2.df <- data.frame(cluster="cl2", distanceTenX=clusterDist2)
      clusterStack3.df <- data.frame(cluster="cl3", distanceTenX=clusterDist3)
      clusterStack.df <- rbind(clusterStack1.df, clusterStack2.df, clusterStack3.df)
      #
      # > clusterStack.df
      #   cluster distanceTenX
      # 1     cl1     4.929271
      # 2     cl2     7.236879
      # 3     cl3     5.828317
      
      # sort
      clusterStack.df <- clusterStack.df %>% dplyr::arrange(desc(distanceTenX))
      #
      # > clusterStack.df
      #   cluster distanceTenX
      # 1     cl2     7.236879
      # 2     cl3     5.828317
      # 3     cl1     4.929271
      
      temp.df <- data.frame(week=weeks.vector[[i]], 
                            first=clusterStack.df[[1]][[1]], 
                            second=clusterStack.df[[1]][[2]], 
                            third=clusterStack.df[[1]][[3]])
      #
      # > temp.df
      #   week first second third
      # 1   23   cl2    cl3   cl1
      
      clusterOrder.df <- bind_rows(temp.df, clusterOrder.df)
      
    }
    
    # > clusterOrder.df
    #    week first second third
    # 1    23   cl2    cl3   cl1
    # 2    24   cl3    cl1   cl2
    # 3    25   cl2    cl3   cl1
    # 4    26   cl2    cl3   cl1
    # 5    27   cl2    cl1   cl3
    # 6    28   cl1    cl3   cl2
    # 7    29   cl3    cl2   cl1
    # 8    30   cl3    cl2   cl1
    # 9    31   cl2    cl3   cl1
    # 10   32   cl3    cl1   cl2
    # 11   34   cl3    cl1   cl2
    
    return(clusterOrder.df)
    
    
  }
  
  bubbleClusterRanks <- function(df, t, time) {
    
    # > clusterOrder.df
    #    week first second third
    # 1    23   cl2    cl3   cl1
    # 2    24   cl3    cl1   cl2
    # 3    25   cl2    cl3   cl1
    # 4    26   cl2    cl3   cl1
    # 5    27   cl2    cl1   cl3
    # 6    28   cl1    cl3   cl2
    # 7    29   cl3    cl2   cl1
    # 8    30   cl3    cl2   cl1
    # 9    31   cl2    cl3   cl1
    # 10   32   cl3    cl1   cl2
    # 11   34   cl3    cl1   cl2
    
    #           first second third
    # cl1       
    # cl2
    # cl3
    
    #g.df <- clusterOrder.df %>% 
    g.df <- df %>% 
      dplyr::mutate(
        firstN = ifelse(first=="cl1", 1,
                  ifelse(first=="cl2", 2, 3)
              )) %>%
      dplyr::mutate(
        secondN = ifelse(second=="cl1", 1,
                        ifelse(second=="cl2", 2, 3)
        )) %>%
      dplyr::mutate(
        thirdN = ifelse(third=="cl1", 1,
                        ifelse(third=="cl2", 2, 3)
        )) 
      
    #           week first second third firstN secondN thirdN
    #     1       23    cl2    cl3   cl1      2       3      1
    #     2       24    cl3    cl1   cl2      3       1      2
    #     3       25    cl2    cl3   cl1      2       3      1
    #     4       26    cl2    cl3   cl1      2       3      1
    #     5       27    cl2    cl1   cl3      2       1      3
    #     6       28    cl1    cl3   cl2      1       3      2
    #     7       29    cl3    cl2   cl1      3       2      1
    #     8       30    cl3    cl2   cl1      3       2      1
    #     9       31    cl2    cl3   cl1      2       3      1
    #     10      32    cl3    cl1   cl2      3       1      2
    #     11      34    cl3    cl1   cl2      3       1      2
    #     > 
      
    # re-write the df such that color can be associated with cluster number
    
    # remove extraneous
    g.df <- g.df %>% select(-first, -second, -third) %>%
    # Gather columns into key-value pairs.
     gather(key=week, value=firstN:thirdN)
    
    #    week    week firstN:thirdN
    # 1    23  firstN             2
    # 2    24  firstN             3
    # 3    25  firstN             2
    # 4    26  firstN             2
    # 5    27  firstN             2
    # 6    28  firstN             1
    # 7    29  firstN             3
    # 8    30  firstN             3
    # 9    31  firstN             2
    # 10   32  firstN             3
    # 11   34  firstN             3
    # 12   23 secondN             3
    # 13   24 secondN             1
    # 14   25 secondN             3
    
    # rename columns with base R
    names(g.df)[2:3] = c("alpha", "cluster")
    
    
    colours = c("3" = "blue", "2" = "green", "1" = "red")
    
    ggplot(g.df, aes(x=week, y=alpha)) +
      
      geom_point(aes(fill=as.character(cluster)), shape=23, size=5, show.legend=TRUE) +
      
      labs(title= paste("Relative Population Strength Between Clusters", sep=""), 
           subtitle = paste("transect: ", t, " daytime= ", time, sep=""),
           x="week", y= "cluster relative strength",
           caption=paste("'strength' as a function of mean and SD", sep="")) +
      
      expand_limits(x=c(23,34)) +
      scale_x_continuous(breaks = seq(24, 34, 2)) +

      scale_y_discrete(
        limits=c("thirdN","secondN","firstN"), # low to high
        labels=c("weakest\npresence","mid-range\npresence","strongest\npresence") ) +
      
      theme_bw() + 
      scale_fill_manual(values = colours, 
                        breaks = c("1", "2", "3"),
                        labels = c("cluster 1", "cluster 2", "cluster 3")) +
      
      theme(legend.title = element_blank(),
            legend.spacing.y = unit(0, "mm"), 
            #legend.position=c(.9,.7),
            legend.justification=c(1,0),
            panel.border = element_rect(colour = "black", fill=NA),
            aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black")) 
      
    
    
  }