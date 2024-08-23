
# create list of pdata frames (for give_me_stata_ame function)
times <- 7
dta_m0  <- replicate(times, pdata_m0,  simplify = FALSE)
dta_m1  <- replicate(times, pdata_m1,  simplify = FALSE)
dta_m2  <- replicate(times, pdata_m2,  simplify = FALSE)
dta_m3  <- replicate(times, pdata_m3,  simplify = FALSE)

dta_m0M <- replicate(times, pdata_m0M, simplify = FALSE)
dta_m1M <- replicate(times, pdata_m1M, simplify = FALSE)
dta_m2M <- replicate(times, pdata_m2M, simplify = FALSE)
dta_m3M <- replicate(times, pdata_m3M, simplify = FALSE)

dta_m0W <- replicate(times, pdata_m0W, simplify = FALSE)
dta_m1W <- replicate(times, pdata_m1W, simplify = FALSE)
dta_m2W <- replicate(times, pdata_m2W, simplify = FALSE)
dta_m3W <- replicate(times, pdata_m3W, simplify = FALSE)

# Create the function
give_me_stata_ame <- function(model, dta){
  avg_slopes(model, variables = c("dum"), by = c("decision", "per"), newdata = dta)
}
  
## estimate the AMEs
ame_all  <- Map(give_me_stata_ame, fe_all,  dta_m0)
ame_mhe  <- Map(give_me_stata_ame, fe_mhe,  dta_m1)
ame_whe  <- Map(give_me_stata_ame, fe_whe,  dta_m2)
ame_ee   <- Map(give_me_stata_ame, fe_ee,   dta_m3)

ame_allM <- Map(give_me_stata_ame, fe_allM, dta_m0M)
ame_mheM <- Map(give_me_stata_ame, fe_mheM, dta_m1M)
ame_wheM <- Map(give_me_stata_ame, fe_wheM, dta_m2M)
ame_eeM  <- Map(give_me_stata_ame, fe_eeM,  dta_m3M)

ame_allW <- Map(give_me_stata_ame, fe_allW, dta_m0M)
ame_mheW <- Map(give_me_stata_ame, fe_mheW, dta_m1M)
ame_wheW <- Map(give_me_stata_ame, fe_wheW, dta_m2M)
ame_eeW  <- Map(give_me_stata_ame, fe_eeW,  dta_m3M)



# avg_slopes(fe_mheM[[1]], variables = c("dum"), by = c("decision", "per"), newdata = pdata_m1M)

avg_slopes(fe_mhe[[1]], variables = c("dum"), by = c("decision", "per"), newdata = pdata_m1)

