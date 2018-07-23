# https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf
# https://tex.stackexchange.com/questions/152488/suppress-library-comments-from-output-with-knitr



##################### QA #############################


# get all julian
bugs.df %>% select(julian) %>%
   				unique() %>% 
   				.$julian

# QA : bugs.df %>% dplyr::group_by(julian) %>% skim() 
bugs.df %>% dplyr::filter(julian==158) %>% skim() 



# https://stackoverflow.com/questions/22353633/filter-for-complete-cases-in-data-frame-using-dplyr-case-wise-deletion
bugs.df %>% dplyr::filter(complete.cases(.))
bugs.df %>% dplyr::filter(!complete.cases(.))
######################################################


#setwd("./code/thesis/ampelos/")
