###############################################################################


library(sqldf)
library(dplyr)
library(tidyr)
source("scripts/02-myfunctions.R")
###############################################################################
# IMPORT
###############################################################################
# from the ipoms codebook impums_00006.cbk, manually copy/pasted the 
# codes for RELATED and in the spreadsheet manually added generation signifiers
# import lookup tables for relationships and country codes (these were generated manually from the codebook)
rel.codes <- read.csv("data/relationship-codes.csv")
country.codes <- read.csv("data/countrycodes.csv")
country.codes$country <- as.character(country.codes$country)


# split file into 21 (actually had to do this in two sets, because of some
# sql disk full error, but same thing
for (i in 16:20){
  assign(country.codes[i,2], read.csv.sql("data-raw/ipumsi_00006.csv", 
                                          sql = paste("select * from file where COUNTRY =", country.codes[i,1] ), eol = "\n"))
  write.csv(eval(as.name(country.codes[i,2])), file = paste0("data/", country.codes[i,2],".csv"), row.names = FALSE)
  rm(list = country.codes[i,2])
  }



###############################################################################
## summaries for individual countires
###############################################################################
# add generation number from lookup table, as well as labels for relationships
# then binary for old/young

# group into HH and calculate n. of generations
# hh. size

i = 8
x <- read.csv(paste0("data/", country.codes[i,2],".csv"))



x %>%   
  select(-COUNTRY, -YEAR, -SAMPLE ) %>% 
  left_join(rel.codes[,c(1,3)], by = c("RELATED" = "code")) %>%  # ADD GENERATION FROM LOOKUP TABLE
  mutate(RELATED = factor(RELATED,                           # ADD LABELS TO RELATED CODES FROM CODEBOOK 
                          levels = rel.codes$code, 
                          labels = rel.codes$label),
         RELATED = droplevels(RELATED),
         old = ifelse(AGE >= 60, 1, 0)) -> x           


x %>% 
  group_by(SERIAL) %>% 
  mutate(n.gen = as.numeric(FunNumberGens(gen)),
         hh.size = n(),
         gen.skipped = FunSkippedGen(gen),
         hh.type = ifelse(hh.size == 1, 1,
                          ifelse(gen.skipped == 1, 2, n.gen+2 )), 
         hh.type =  factor(hh.type, 
                           levels = 1:7, 
                           labels = c("single", "skipped", "one gen", 
                                      "two gen", "three gen", "four gen", "five gen")))  -> x



  
  x %>% 
  ungroup() %>% 
  mutate(SEX = factor(SEX, labels = c("male", "female")),
         old = factor(old, labels = c("young", "old"))) %>% 
  group_by(old, SEX, hh.type) %>% 
  summarise(count = n())  %>% 
    mutate(prop = count/sum(count)) %>% 
  unite(category, old, SEX) %>% 
    select(-count) %>% 
    spread(category, prop)  -> y

barplot(as.matrix(y[,2:5]) , legend.text = y[[1]])
