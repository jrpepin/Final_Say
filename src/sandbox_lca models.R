# Restore the object
## generated from line 358 in FS_03_qual analyses.R
##lcadata <- readRDS(file = file.path(outDir,"lcadataMultinomTopics.rds"))


################################################################################
# Topic Modeling Regression Tables
################################################################################

# Predict each topic -----------------------------------------------------------

## List of topics
dv <- names(c(select(data_lca %>% ungroup(), starts_with("t_"))))

## Create pdata frames
pdata_m1 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Man higher-earner"),   
                        index = c("CaseID"))
pdata_m2 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Woman higher-earner"), 
                        index = c("CaseID"))
pdata_m3 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Equal earners"),       
                        index = c("CaseID"))

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

#### add relinc indicator
ame_mhe<- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
       ame_mhe, "Men higher-earner", SIMPLIFY = FALSE)

ame_whe<- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                 ame_whe, "Woman higher-earner", SIMPLIFY = FALSE)

ame_ee<- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                 ame_ee, "Equal earner", SIMPLIFY = FALSE)

# Appendix Table A4 ------------------------------------------------------------
## Relationship of Fairness Rating to Topic Prevalence by Decision-Maker Gender, 
## Decision Type, and Vignette Couple’s Relative Income

p1 <- modelsummary(ame_mhe, shape = decision + model ~ relinc + per,
             gof_map = NA, output = "huxtable") 
p2 <- modelsummary(ame_whe, shape = decision + model ~ relinc + per,
                   gof_map = NA, output = "huxtable")
p3 <- modelsummary(ame_ee, shape = decision + model ~ relinc + per,
                   gof_map = NA, output = "huxtable")
ht <-  cbind(p1, p2, p3)


ht %>%
  select(c("decision", "  ", "Men higher-earner / 0", "Men higher-earner / 1", 
           "Woman higher-earner / 0", "Woman higher-earner / 1",
           "Equal earner / 0", "Equal earner / 1")) %>%
  insert_row(c(" ", " ", 
               "Anthony\nDecides", "Michelle\nDecides",
               "Anthony\nDecides", "Michelle\nDecides", 
               "Anthony\nDecides", "Michelle\nDecides"), after = 1)  %>%
  huxtable::as_flextable() %>%
  delete_rows(i = 1, part = "body") %>%
  add_header_row(values = c("Decision", "Topic", "Men higher-earner", 
                            "Women higher-earner", "Equal earners"),
                 colwidths = c(1, 1, 2, 2, 2), top = TRUE) %>%
  flextable::align(align = "center", part = "header")





# Not working

ames <- c(ame_mhe, ame_whe, ame_ee)

topics <- list("t1" = ames[[1]],  "t2" = ames[[2]],  "t3" = ames[[3]],
               "t4" = ames[[4]],  "t5" = ames[[5]],  "t6" = ames[[6]],  "t7"  = ames[[7]],
               "t1" = ames[[8]],  "t2" = ames[[9]],  "t3" = ames[[10]],
               "t4" = ames[[11]], "t5" = ames[[12]], "t6" = ames[[13]], "t7" = ames[[14]],
               "t1" = ames[[15]], "t2" = ames[[16]], "t3" = ames[[17]],
               "t4" = ames[[18]], "t5" = ames[[19]], "t6" = ames[[20]], "t7" = ames[[21]])

modelsummary(topics, shape = decision + per ~ relinc + model,
             gof_map = NA, output = "gt") 



# For plotting

#### put them into a dataframe
df_mhe <- bind_rows(ame_mhe, .id = "topic")
df_whe <- bind_rows(ame_whe, .id = "topic")
df_ee  <- bind_rows(ame_ee,  .id = "topic")

df_mhe$relinc <- "Man higher-earner"
df_whe$relinc <- "Woman higher-earner"
df_ee$relinc  <- "Equal earners"

data_ame <- rbind(df_mhe, df_whe, df_ee)


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