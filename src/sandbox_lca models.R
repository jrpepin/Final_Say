# Restore the object
## generated from line 358 in FS_03_qual analyses.R
lcadata <- readRDS(file = file.path(outDir,"lcadataMultinomTopics.rds"))

## Need matched data
tally <- lcadata %>% 
  group_by(CaseID) %>%
  tally() 

lcadata <- left_join(lcadata, tally)

### Function to drop opposite decision variables
not_all_na <- function(x) any(!is.na(x))

### Decision 1
lca1 <- lcadata               %>%
  group_by(CaseID)            %>%
  filter(n == 2)              %>%    # keep matched decisions
  mutate(row = row_number())  %>%
  filter(row==1)              %>%    # keep decision 1 rows
  select(where(not_all_na))   %>%    # keep decision 1 variables
  select(!c(fair2, qual2, 
            aperson, dum2, 
            per2, apref))     %>%
  rename(fair     = fair1,
         qual     = qual1,
         person   = iperson,
         dum      = dum1,
         per      = per1,
         pref     = ipref)    %>%
  mutate(decision = "high")

  #### Remove everything from the second underscore onwards
  colnames(lca1) <- sub("(_[^_]+){1}$", "", colnames(lca1))

### Decision 2
  lca2 <- lcadata               %>%
    group_by(CaseID)            %>%
    filter(n == 2)              %>%    # keep matched decisions
    mutate(row = row_number())  %>%
    filter(row==2)              %>%    # keep decision 2 rows
    select(where(not_all_na))   %>%    # keep decision 2 variables
    select(!c(fair1, qual1,
              iperson, dum1, 
              per1, ipref))     %>%
    rename(fair     = fair2,
           qual     = qual2,
           person   = aperson,
           dum      = dum2,
           per      = per2,
           pref     = apref)    %>%
    mutate(decision = "low")
  
  #### Remove everything from the second underscore onwards
  colnames(lca2) <- sub("(_[^_]+){1}$", "", colnames(lca2))
  
# Append data frames together
data_lca <- rbind(lca1, lca2)  # 7434 matched person decisions

## Arrange column and row order
data_lca <- data_lca                  %>% 
  select(CaseID, decision, fair, dum, per, qual, starts_with("t_"), 
         everything())                %>%
  mutate(CaseID = as.numeric(CaseID)) %>%
  arrange(CaseID)                     %>%
  mutate(CaseID = as.character(CaseID))

# Predict each topic -----------------------------------------------------------

## List of topics
dv <- names(c(select(data_lca %>% ungroup(), starts_with("t_"))))

## Create pdata frames
pdata_m1 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Man higher-earner"),   index = c("CaseID"))
pdata_m2 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Woman higher-earner"), index = c("CaseID"))
pdata_m3 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Equal earners"),       index = c("CaseID"))

## Create the plm functions
plms_mhe <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m1, model = "within")
}

plms_whe <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m2, model = "within")
}

plms_ee  <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m3, model = "within")
}

## Run the fixed effects models (loop over topics as DVs)
fe_mhe   <- lapply(dv, plms_mhe) # Man higher-earner
fe_whe   <- lapply(dv, plms_whe) # Woman higher-earner
fe_ee    <- lapply(dv, plms_ee)  # Equal earners

mods_mhe <- list(fe_mhe[[1]], fe_mhe[[2]], fe_mhe[[3]], fe_mhe[[4]],
                 fe_mhe[[5]], fe_mhe[[6]], fe_mhe[[7]])

mods_whe <- list(fe_whe[[1]], fe_whe[[2]], fe_whe[[3]], fe_whe[[4]],
                 fe_whe[[5]], fe_whe[[6]], fe_whe[[7]])

mods_ee  <- list(fe_ee[[1]],  fe_ee[[2]],  fe_ee[[3]],  fe_ee[[4]],
                 fe_ee[[5]],  fe_ee[[6]],  fe_ee[[7]])

## Average Marginal Effects of the models

### Create the function
give_me_ame <- function(model){
  avg_slopes(model, variables = c("dum"), by = c("decision", "per"))
}

#### estimate the AMEs
ame_mhe  <- lapply(mods_mhe, give_me_ame) 
ame_whe  <- lapply(mods_whe, give_me_ame) 
ame_ee   <- lapply(mods_ee,  give_me_ame) 

#### put them into a dataframe
df_mhe <- bind_rows(ame_mhe, .id = "topic")
df_whe <- bind_rows(ame_whe, .id = "topic")
df_ee  <- bind_rows(ame_ee,  .id = "topic")

df_mhe$relinc <- "Man higher-earner"
df_whe$relinc <- "Woman higher-earner"
df_ee$relinc  <- "Equal earners"

data_ame <- rbind(df_mhe, df_whe, df_ee)  # 7434 matched person decisions


# ------------------------------------------------------------------------------
# not using but might be useful if something doesn't work out.


dv = (c("t_1", "t_2"))

for(i in 1:length(dv)){
  model <- paste("model",i, sep="")       # create an object to hold the models
  m <- plm(as.formula(paste(dv[i],"~ dum + per + decision + (dum * per) + (dum * decision) + 
                              (per * decision) + (dum * per * decision)")), 
           data = pdata_m1, model = "within")
  assign(model,m)                        # assign the model m to the model object 
}