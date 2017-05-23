###############################################################################


library(sqldf)
library(dplyr)
###############################################################################
# RECODING
###############################################################################
# from the ipoms codebook impums_00006.cbk, manually copy/pasted the 
# codes for RELATED and in the spreadsheet manually added generation signifiers

rel.codes <- read.csv("data-raw/relationship-codes.csv")

armenia <- read.csv.sql("data-raw/ipumsi_00006.csv", 
                      sql = "select * from file where COUNTRY = 51", eol = "\n")



x <- armenia[1:5000,]




x %>%   left_join(rel.codes[,c(1,3)], by = c("RELATED" = "code")) %>%  # ADD GENERATION FROM LOOKUP TABLE
  mutate(RELATED = factor(RELATED,                           # ADD LABELS TO RELATED CODES FROM CODEBOOK 
                              levels = rel.codes$code, 
                              labels = rel.codes$label),
             RELATED = droplevels(RELATED),
         old = ifelse(AGE >= 60, 1, 0)) -> x           


x %>% 
  group_by(SERIAL) %>% 
  mutate(n.gen = length(unique(gen)),
         hh.size = n()) -> x

x %>% 
  ungroup() %>% 
  group_by(SEX, n.gen) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))

