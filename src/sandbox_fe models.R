install.packages("plm")
library(plm)

# Table 02. --------------------------------------------------------------------
# Average Marginal Effects of Woman Deciding on Perceptions of Fairness 
# by relative income of vignette couple and decision type

## Prepare data for plm
pdata_m1 <- pdata.frame(femodels %>% 
                               filter(relinc == "Man higher-earner"),
                               index = c("CaseID"))
pdata_m2 <- pdata.frame(femodels %>% 
                               filter(relinc == "Woman higher-earner"),
                               index = c("CaseID"))
pdata_m3 <- pdata.frame(femodels %>% 
                               filter(relinc == "Equal earners"),
                               index = c("CaseID"))

## Run the fixed effects models
plm1 <- plm(dum ~ per * decision, data = pdata_m1, model = "within")
plm2 <- plm(dum ~ per * decision, data = pdata_m2, model = "within")
plm3 <- plm(dum ~ per * decision, data = pdata_m3, model = "within")


## Average Marginal Effects of the models
m1 <- broom.helpers::tidy_avg_slopes(plm1, variables = c("per"), by = "decision")
m2 <- broom.helpers::tidy_avg_slopes(plm2, variables = c("per"), by = "decision")
m3 <- broom.helpers::tidy_avg_slopes(plm3, variables = c("per"), by = "decision")

m1
m2
m3