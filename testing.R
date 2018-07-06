
# for each day, calculate the number of species observed
bugs.df %>% 
  dplyr::filter(transect=="oakMargin") %>%
  dplyr::group_by(julian) %>% 
  dplyr::summarise(totalBugs=ifelse(Thomisidae..crab.spider. > 0, 1, 0))


tribble(
  ~x, ~y, ~z, ~w, ~v, ~u,
  "a", 175, 0, 1, 0, 0,
  "b", 176, 1, 1, 1, 1,
  "c", 175, 1, 0, 1, 0
) -> test.tbl

test.tbl %>% 
  dplyr::filter(y>170) %>%
  dplyr::group_by(y) %>% 
  dplyr::summarise(totalBugs=sum(z, w, v, u))

test.tbl %>% 
  mutate(newColumn = ifelse(w > 0, 1, 0))


test.tbl %>% 
  +     mutate(newColumn = ifelse(w > 0, 1, 0))
cbind(test.tbl, zeros=t(t(rowSums(test.tbl==0))))
cbind(new.tbl, nonZeros=4-new.tbl$zeros)
