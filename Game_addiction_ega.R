install.packages('sna') 
library(tidyverse)
library(EGAnet)

gam_ega <- read_csv2("data-tables/gam_clean.csv")
View(gam_ega)
str(gam_ega)
gam_ega %>% mutate(g_w = factor(g_w,
                            levels = c('1-5 игр', 
                                       '6-10 игр', 
                                       '11-20 игр', 
                                       '20-30 игр', 
                                       'Больше 30 игр', 
                                       'Больше 50 игр'), 
                            ordered = TRUE)) -> gam_ega
gam_ega %>% select(age:pas_total, -c(pas01:pas09, max_rating, a_g, heroes_3)) %>% EGA()
