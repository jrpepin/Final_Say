#-------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_02_quant analyses.R
# Joanna R. Pepin & William J. Scarborough
#-------------------------------------------------------------------------------

# This file analyzes the decision making variables.

################################################################################
# Paper Tables and Figures (quant)
################################################################################

# Table 02. --------------------------------------------------------------------

## Specify reference levels
quantdata$relate   <- relevel(quantdata$relate,   ref = "Never married")
quantdata$raceeth  <- relevel(quantdata$raceeth,  ref = "White")
quantdata$educ     <- relevel(quantdata$educ,     ref = "High school")
quantdata$employ   <- relevel(quantdata$employ,   ref = "Employed")
quantdata$inc      <- relevel(quantdata$inc,      ref = "< than $50,000")
quantdata$earner   <- relevel(quantdata$earner,   ref = "Higher earner")
quantdata$order    <- relevel(quantdata$order,    ref = "Same")

## Wide to long
femodels <- quantdata %>%
  select(CaseID, dum1, dum2, fair1, fair2, per1, per2, 
         relinc, organize, mar, child, dur, item, gender, relate, parent, 
         raceeth, educ, employ, inc, age, activity, order, weight) %>% 
  pivot_longer(                                                                 # long 2 numeric variables
    cols = c(contains('per'), 
             contains('dum')),
    names_to = c("vars", "decision"),
    names_pattern = "(.*)(.)$") %>%
  pivot_wider(                                                                  # back to 2 rows (decisions) per person
    names_from = vars,
    values_from = value) %>%
  pivot_longer(                                                                 # long factor variable
    cols = c("fair1", "fair2"),
    names_to = c("drop1", "drop2"),
    names_pattern = "(.*)(.)$",
    values_to = "fair") %>%
  filter(decision == drop2) %>%                                                 # back to 2 rows (decisions) per person
  select(!c(drop1, drop2))


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
m1 <- avg_slopes(plm1, variables = c("per"), by = "decision")
m2 <- avg_slopes(plm2, variables = c("per"), by = "decision")
m3 <- avg_slopes(plm3, variables = c("per"), by = "decision")

## identify interaction variables
m1$term <- paste(m1$term, m1$decision, sep= ".")
m2$term <- paste(m2$term, m2$decision, sep= ".")
m3$term <- paste(m3$term, m3$decision, sep= ".")

# test equality of coefficients between HIGH & LOW stakes
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
# https://journals.sagepub.com/doi/10.1177/0081175019852763

## Calculate Z scores
MHE   <- (m1[[1,4]] - m1[[2,4]]) / sqrt(m1[[1,5]]^2 + m1[[2,5]]^2)
WHE   <- (m2[[1,4]] - m2[[2,4]]) / sqrt(m2[[1,5]]^2 + m2[[2,5]]^2)
EE    <- (m3[[1,4]] - m3[[2,4]]) / sqrt(m3[[1,5]]^2 + m3[[2,5]]^2)

## Calculate p values
p_MHE <- 2*pnorm(-abs(MHE)) 
p_WHE <- 2*pnorm(-abs(WHE)) 
p_EE  <- 2*pnorm(-abs(EE)) 

message("Man higher-earner p = ",    round(p_MHE,  digits = 3))
message("Woman Higher-Earner p = ",  round(p_WHE,  digits = 3))
message("Equal earners p = ",        round(p_EE,   digits = 3))

new_rows <- data.frame("Significant difference, high vs. low stakes?",
                       "Yes",
                       "Yes",
                       "No")

## List of models
models <- list(
  "Man Higher-Earner"   = m1,
  "Woman Higher-Earner" = m2,
  "Equal Earner"        = m3)

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"   = "Low Stakes")

## Table notes
n <- "Notes: N=7,956 person-decisions. Results calculated from respondent-fixed 
effects linear probability models. Independent models applied by relative income. 
2 tailed tests. Standard errors in parentheses."


## Produce table 02
modelsummary(
  models, 
  coef_map = coef_map,
  gof_map = NA,
  #  exponentiate = TRUE,
  stars = c("*" =.05, "**" = .01, "***" = .001),
  fmt = fmt_decimal(digits = 3, pdigits = 3),
  notes = n,
  add_rows = new_rows,
  output = file.path(outDir, "finalsay_table02.docx"))


# Figure 02. -------------------------------------------------------------------

