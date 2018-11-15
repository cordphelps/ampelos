

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