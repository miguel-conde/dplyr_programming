
library(dplyr)

mtcars <- mtcars %>% as_tibble()

glimpse(mtcars)

gr_by_cyl <- mtcars %>% 
  group_by(cyl) %>% 
  summarise(avg_wt_cyl = mean(wt))

gr_by_cyl_disp <- mtcars %>% 
  group_by(cyl, disp) %>% 
  summarise(avg_wt_cyl_disp = mean(wt))

left_join(gr_by_cyl, gr_by_cyl_disp, by = "cyl")

gr_by_cyl_gear <- mtcars %>% 
  group_by(cyl, gear) %>% 
  summarise(avg_wt_cyl_gear = mean(wt))

left_join(gr_by_cyl, gr_by_cyl_gear, by = "cyl")
inner_join(gr_by_cyl, gr_by_cyl_gear, by = "cyl")

source("utils.R")
