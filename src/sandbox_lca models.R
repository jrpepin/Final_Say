# Restore the object
## generated from line 358 in FS_03_qual analyses.R
lcadata <- readRDS(file = file.path(outDir,"lcadataMultinomTopics.rds"))

## Need matched data
tally <- lcadata %>% 
  group_by(CaseID) %>%
  tally() 

lcadata <- left_join(lcadata, tally)

not_all_na <- function(x) any(!is.na(x))

lca1 <- lcadata %>%
  group_by(CaseID) %>%
  filter(n == 2) %>%
  filter(row_number()==1) %>% 
  select(where(not_all_na))

