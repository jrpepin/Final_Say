

### Create the function
pp_t1 <- function(model){
  ggpredict(model[[1]], variables = c("dum"), by = c("decision", "per"))
}

# Topic 1
ggpredict(fe_mheM[[1]], terms = c("dum", "per", "decision")) # men-higher-earner R=Man


pp_mhe  <- lapply(mods_mhe, give_me_ame) 
