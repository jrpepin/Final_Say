#-------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_99_supplement.R
# Joanna R. Pepin & William J. Scarborough
#-------------------------------------------------------------------------------


################################################################################
# Supplementary Materials
################################################################################

# Section A --------------------------------------------------------------------

#Calculate AME for vignette marital & parental status, relationship duration

### Live Together
pdata_m1mar1 <- pdata.frame(femodels %>%
                              filter(relinc == "Man higher-earner"    & 
                                       mar    == "live together"),
                            index = c("CaseID"))

pdata_m2mar1 <- pdata.frame(femodels %>%
                              filter(relinc == "Woman higher-earner"  & 
                                       mar  == "live together"),
                            index = c("CaseID"))

pdata_m3mar1 <- pdata.frame(femodels %>%
                              filter(relinc == "Equal earners"        & 
                                       mar  == "live together"),
                            index = c("CaseID"))

### Married
pdata_m1mar2 <- pdata.frame(femodels %>%
                              filter(relinc == "Man higher-earner"    & 
                                       mar    == "are married"),
                            index = c("CaseID"))

pdata_m2mar2 <- pdata.frame(femodels %>%
                              filter(relinc == "Woman higher-earner"  & 
                                       mar  == "are married"),
                            index = c("CaseID"))

pdata_m3mar2 <- pdata.frame(femodels %>%
                              filter(relinc == "Equal earners"        & 
                                       mar  == "are married"),
                            index = c("CaseID"))

### no children
pdata_m1par1 <- pdata.frame(femodels %>%
                              filter(relinc == "Man higher-earner"    & 
                                     child  == "no children"),
                            index = c("CaseID"))

pdata_m2par1 <- pdata.frame(femodels %>%
                              filter(relinc == "Woman higher-earner"  & 
                                     child  == "no children"),
                            index = c("CaseID"))

pdata_m3par1 <- pdata.frame(femodels %>%
                              filter(relinc == "Equal earners"        & 
                                     child  == "no children"),
                            index = c("CaseID"))

### one child together
pdata_m1par2 <- pdata.frame(femodels %>%
                              filter(relinc == "Man higher-earner"    & 
                                     child  == "one child together"),
                            index = c("CaseID"))

pdata_m2par2 <- pdata.frame(femodels %>%
                              filter(relinc == "Woman higher-earner"  & 
                                     child  == "one child together"),
                            index = c("CaseID"))

pdata_m3par2 <- pdata.frame(femodels %>%
                              filter(relinc == "Equal earners"        & 
                                     child  == "one child together"),
                            index = c("CaseID"))

### 3 years
pdata_m1dur1 <- pdata.frame(femodels %>%
                              filter(relinc == "Man higher-earner"    & 
                                       dur    == "3 years"),
                            index = c("CaseID"))

pdata_m2dur1 <- pdata.frame(femodels %>%
                              filter(relinc == "Woman higher-earner"  & 
                                       dur  == "3 years"),
                            index = c("CaseID"))

pdata_m3dur1 <- pdata.frame(femodels %>%
                              filter(relinc == "Equal earners"        & 
                                       dur  == "3 years"),
                            index = c("CaseID"))

### 7 years
pdata_m1dur2 <- pdata.frame(femodels %>%
                              filter(relinc == "Man higher-earner"    & 
                                       dur    == "7 years"),
                            index = c("CaseID"))

pdata_m2dur2 <- pdata.frame(femodels %>%
                              filter(relinc == "Woman higher-earner"  & 
                                       dur  == "7 years"),
                            index = c("CaseID"))

pdata_m3dur2 <- pdata.frame(femodels %>%
                              filter(relinc == "Equal earners"        & 
                                       dur  == "7 years"),
                            index = c("CaseID"))

## Run the fixed effects models

## Marital status
plm1mar1 <- plm(dum ~ per * decision, data = pdata_m1mar1, model = "within")
plm2mar1 <- plm(dum ~ per * decision, data = pdata_m2mar1, model = "within")
plm3mar1 <- plm(dum ~ per * decision, data = pdata_m3mar1, model = "within")

plm1mar2 <- plm(dum ~ per * decision, data = pdata_m1mar2, model = "within")
plm2mar2 <- plm(dum ~ per * decision, data = pdata_m2mar2, model = "within")
plm3mar2 <- plm(dum ~ per * decision, data = pdata_m3mar2, model = "within")

## Parental status
plm1par1 <- plm(dum ~ per * decision, data = pdata_m1par1, model = "within")
plm2par1 <- plm(dum ~ per * decision, data = pdata_m2par1, model = "within")
plm3par1 <- plm(dum ~ per * decision, data = pdata_m3par1, model = "within")

plm1par2 <- plm(dum ~ per * decision, data = pdata_m1par2, model = "within")
plm2par2 <- plm(dum ~ per * decision, data = pdata_m2par2, model = "within")
plm3par2 <- plm(dum ~ per * decision, data = pdata_m3par2, model = "within")

## Relationship Duration
plm1dur1 <- plm(dum ~ per * decision, data = pdata_m1dur1, model = "within")
plm2dur1 <- plm(dum ~ per * decision, data = pdata_m2dur1, model = "within")
plm3dur1 <- plm(dum ~ per * decision, data = pdata_m3dur1, model = "within")

plm1dur2 <- plm(dum ~ per * decision, data = pdata_m1dur2, model = "within")
plm2dur2 <- plm(dum ~ per * decision, data = pdata_m2dur2, model = "within")
plm3dur2 <- plm(dum ~ per * decision, data = pdata_m3dur2, model = "within")


## Average Marginal Effects of the models
m1_mar1 <- avg_slopes(plm1mar1, variables = c("per"), by = "decision") 
m2_mar1 <- avg_slopes(plm2mar1, variables = c("per"), by = "decision")
m3_mar1 <- avg_slopes(plm3mar1, variables = c("per"), by = "decision")

m1_mar2 <- avg_slopes(plm1mar2, variables = c("per"), by = "decision") 
m2_mar2 <- avg_slopes(plm2mar2, variables = c("per"), by = "decision")
m3_mar2 <- avg_slopes(plm3mar2, variables = c("per"), by = "decision")

m1_par1 <- avg_slopes(plm1par1, variables = c("per"), by = "decision") 
m2_par1 <- avg_slopes(plm2par1, variables = c("per"), by = "decision")
m3_par1 <- avg_slopes(plm3par1, variables = c("per"), by = "decision")

m1_par2 <- avg_slopes(plm1par2, variables = c("per"), by = "decision") 
m2_par2 <- avg_slopes(plm2par2, variables = c("per"), by = "decision")
m3_par2 <- avg_slopes(plm3par2, variables = c("per"), by = "decision")

m1_dur1 <- avg_slopes(plm1dur1, variables = c("per"), by = "decision") 
m2_dur1 <- avg_slopes(plm2dur1, variables = c("per"), by = "decision")
m3_dur1 <- avg_slopes(plm3dur1, variables = c("per"), by = "decision")

m1_dur2 <- avg_slopes(plm1dur2, variables = c("per"), by = "decision") 
m2_dur2 <- avg_slopes(plm2dur2, variables = c("per"), by = "decision")
m3_dur2 <- avg_slopes(plm3dur2, variables = c("per"), by = "decision")


## identify interaction variables for modelsummary
m1_mar1$term <- paste(m1_mar1$term, m1_mar1$decision, sep= ".")
m2_mar1$term <- paste(m2_mar1$term, m2_mar1$decision, sep= ".")
m3_mar1$term <- paste(m3_mar1$term, m3_mar1$decision, sep= ".")

m1_mar2$term <- paste(m1_mar2$term, m1_mar2$decision, sep= ".")
m2_mar2$term <- paste(m2_mar2$term, m2_mar2$decision, sep= ".")
m3_mar2$term <- paste(m3_mar2$term, m3_mar2$decision, sep= ".")

m1_par1$term <- paste(m1_par1$term, m1_par1$decision, sep= ".")
m2_par1$term <- paste(m2_par1$term, m2_par1$decision, sep= ".")
m3_par1$term <- paste(m3_par1$term, m3_par1$decision, sep= ".")

m1_par2$term <- paste(m1_par2$term, m1_par2$decision, sep= ".")
m2_par2$term <- paste(m2_par2$term, m2_par2$decision, sep= ".")
m3_par2$term <- paste(m3_par2$term, m3_par2$decision, sep= ".")

m1_dur1$term <- paste(m1_dur1$term, m1_dur1$decision, sep= ".")
m2_dur1$term <- paste(m2_dur1$term, m2_dur1$decision, sep= ".")
m3_dur1$term <- paste(m3_dur1$term, m3_dur1$decision, sep= ".")

m1_dur2$term <- paste(m1_dur2$term, m1_dur2$decision, sep= ".")
m2_dur2$term <- paste(m2_dur2$term, m2_dur2$decision, sep= ".")
m3_dur2$term <- paste(m3_dur2$term, m3_dur2$decision, sep= ".")


## Test for statistical sig. between high and low stakes decisions
### Calculate Z scores 
MHE_mar1  <- (m1_mar1[[1,4]] - m1_mar1[[2,4]]) / sqrt(m1_mar1[[1,5]]^2 + m1_mar1[[2,5]]^2)
MHE_mar2  <- (m1_mar2[[1,4]] - m1_mar2[[2,4]]) / sqrt(m1_mar2[[1,5]]^2 + m1_mar2[[2,5]]^2)

WHE_mar1  <- (m2_mar1[[1,4]] - m2_mar1[[2,4]]) / sqrt(m2_mar1[[1,5]]^2 + m2_mar1[[2,5]]^2)
WHE_mar2  <- (m2_mar2[[1,4]] - m2_mar2[[2,4]]) / sqrt(m2_mar2[[1,5]]^2 + m2_mar2[[2,5]]^2)

EE_mar1   <- (m3_mar1[[1,4]] - m3_mar1[[2,4]]) / sqrt(m3_mar1[[1,5]]^2 + m3_mar1[[2,5]]^2)
EE_mar2   <- (m3_mar2[[1,4]] - m3_mar2[[2,4]]) / sqrt(m3_mar2[[1,5]]^2 + m3_mar2[[2,5]]^2)

MHE_par1  <- (m1_par1[[1,4]] - m1_par1[[2,4]]) / sqrt(m1_par1[[1,5]]^2 + m1_par1[[2,5]]^2)
MHE_par2  <- (m1_par2[[1,4]] - m1_par2[[2,4]]) / sqrt(m1_par2[[1,5]]^2 + m1_par2[[2,5]]^2)

WHE_par1  <- (m2_par1[[1,4]] - m2_par1[[2,4]]) / sqrt(m2_par1[[1,5]]^2 + m2_par1[[2,5]]^2)
WHE_par2  <- (m2_par2[[1,4]] - m2_par2[[2,4]]) / sqrt(m2_par2[[1,5]]^2 + m2_par2[[2,5]]^2)

EE_par1   <- (m3_par1[[1,4]] - m3_par1[[2,4]]) / sqrt(m3_par1[[1,5]]^2 + m3_par1[[2,5]]^2)
EE_par2   <- (m3_par2[[1,4]] - m3_par2[[2,4]]) / sqrt(m3_par2[[1,5]]^2 + m3_par2[[2,5]]^2)

MHE_dur1  <- (m1_dur1[[1,4]] - m1_dur1[[2,4]]) / sqrt(m1_dur1[[1,5]]^2 + m1_dur1[[2,5]]^2)
MHE_dur2  <- (m1_dur2[[1,4]] - m1_dur2[[2,4]]) / sqrt(m1_dur2[[1,5]]^2 + m1_dur2[[2,5]]^2)

WHE_dur1  <- (m2_dur1[[1,4]] - m2_dur1[[2,4]]) / sqrt(m2_dur1[[1,5]]^2 + m2_dur1[[2,5]]^2)
WHE_dur2  <- (m2_dur2[[1,4]] - m2_dur2[[2,4]]) / sqrt(m2_dur2[[1,5]]^2 + m2_dur2[[2,5]]^2)

EE_dur1   <- (m3_dur1[[1,4]] - m3_dur1[[2,4]]) / sqrt(m3_dur1[[1,5]]^2 + m3_dur1[[2,5]]^2)
EE_dur2   <- (m3_dur2[[1,4]] - m3_dur2[[2,4]]) / sqrt(m3_dur2[[1,5]]^2 + m3_dur2[[2,5]]^2)


### Calculate p values
p_MHE_mar1 <- 2*pnorm(-abs(MHE_mar1)) 
p_MHE_mar2 <- 2*pnorm(-abs(MHE_mar2)) 

p_WHE_mar1 <- 2*pnorm(-abs(WHE_mar1)) 
p_WHE_mar2 <- 2*pnorm(-abs(WHE_mar2)) 

p_EE_mar1  <- 2*pnorm(-abs(EE_mar1)) 
p_EE_mar2  <- 2*pnorm(-abs(EE_mar2)) 

p_MHE_par1 <- 2*pnorm(-abs(MHE_par1)) 
p_MHE_par2 <- 2*pnorm(-abs(MHE_par2)) 

p_WHE_par1 <- 2*pnorm(-abs(WHE_par1)) 
p_WHE_par2 <- 2*pnorm(-abs(WHE_par2)) 

p_EE_par1  <- 2*pnorm(-abs(EE_par1)) 
p_EE_par2  <- 2*pnorm(-abs(EE_par2)) 

p_MHE_dur1 <- 2*pnorm(-abs(MHE_dur1)) 
p_MHE_dur2 <- 2*pnorm(-abs(MHE_dur2)) 

p_WHE_dur1 <- 2*pnorm(-abs(WHE_dur1)) 
p_WHE_dur2 <- 2*pnorm(-abs(WHE_dur2)) 

p_EE_dur1  <- 2*pnorm(-abs(EE_dur1)) 
p_EE_dur2  <- 2*pnorm(-abs(EE_dur2)) 

### Man Higher Earner
message("Cohabit p = ",  round(p_MHE_mar1,  digits = 3)) 
message("Married p = ",  round(p_MHE_mar2,  digits = 3)) 

message("No child p = ", round(p_MHE_par1,  digits = 3)) 
message("Parent p = ",   round(p_MHE_par2,  digits = 3)) 

message("3 years p = ",  round(p_MHE_dur1,  digits = 3)) 
message("7 years p = ",  round(p_MHE_dur2,  digits = 3)) 

### Woman Higher Earner
message("Cohabit p = ",  round(p_WHE_mar1,  digits = 3)) 
message("Married p = ",  round(p_WHE_mar2,  digits = 3)) 

message("No child p = ", round(p_WHE_par1,  digits = 3)) 
message("Parent p = ",   round(p_WHE_par2,  digits = 3)) 

message("3 years p = ",  round(p_WHE_dur1,  digits = 3)) 
message("7 years p = ",  round(p_WHE_dur2,  digits = 3)) 

### Equal Earners
message("Cohabit p = ",  round(p_EE_mar1,   digits = 3)) 
message("Married p = ",  round(p_EE_mar2,   digits = 3)) 

message("No child p = ", round(p_EE_par1,   digits = 3)) 
message("Parent p = ",   round(p_EE_par2,   digits = 3)) 

message("3 years p = ",  round(p_EE_dur1,   digits = 3)) 
message("7 years p = ",  round(p_EE_dur2,   digits = 3)) 


## Test for statistical sig. between cohabit and married (within decision)
  ### Calculate Z scores 
  MHEH_mar  <- (m1_mar1[[1,4]] - m1_mar2[[1,4]]) / sqrt(m1_mar1[[1,5]]^2 + m1_mar2[[1,5]]^2)
  MHEL_mar  <- (m1_mar1[[2,4]] - m1_mar2[[2,4]]) / sqrt(m1_mar1[[2,5]]^2 + m1_mar2[[2,5]]^2)
  
  WHEH_mar  <- (m2_mar1[[1,4]] - m2_mar2[[1,4]]) / sqrt(m2_mar1[[1,5]]^2 + m2_mar2[[1,5]]^2)
  WHEL_mar  <- (m2_mar1[[2,4]] - m2_mar2[[2,4]]) / sqrt(m2_mar1[[2,5]]^2 + m2_mar2[[2,5]]^2)
  
  EEH_mar   <- (m3_mar1[[1,4]] - m3_mar2[[1,4]]) / sqrt(m3_mar1[[1,5]]^2 + m3_mar2[[1,5]]^2)
  EEL_mar   <- (m3_mar1[[2,4]] - m3_mar2[[2,4]]) / sqrt(m3_mar1[[2,5]]^2 + m3_mar2[[2,5]]^2)

  ### Calculate p values
  p_MHEH_mar <- 2*pnorm(-abs(MHEH_mar)) 
  p_MHEL_mar <- 2*pnorm(-abs(MHEL_mar)) 
  
  p_WHEH_mar <- 2*pnorm(-abs(WHEH_mar)) 
  p_WHEL_mar <- 2*pnorm(-abs(WHEL_mar)) 
  
  p_EEH_mar  <- 2*pnorm(-abs(EEH_mar)) 
  p_EEL_mar  <- 2*pnorm(-abs(EEL_mar)) 
  
  ### Man Higher Earner
  message("High-stakes p = ",    round(p_MHEH_mar,  digits = 3))
  message("Low-stakes  p = ",    round(p_MHEL_mar,  digits = 3)) 
  
  ### Woman Higher Earner
  message("High-stakes p = ",    round(p_WHEH_mar,  digits = 3))
  message("Low-stakes  p = ",    round(p_WHEL_mar,  digits = 3)) 
  
  ### Equal Earners
  message("High-stakes p = ",    round(p_EEH_mar,   digits = 3))
  message("Low-stakes  p = ",    round(p_EEL_mar,   digits = 3)) 

## Test for statistical sig. between no child and parent (within decision)
  ### Calculate Z scores 
  MHEH_par  <- (m1_par1[[1,4]] - m1_par2[[1,4]]) / sqrt(m1_par1[[1,5]]^2 + m1_par2[[1,5]]^2)
  MHEL_par  <- (m1_par1[[2,4]] - m1_par2[[2,4]]) / sqrt(m1_par1[[2,5]]^2 + m1_par2[[2,5]]^2)
  
  WHEH_par  <- (m2_par1[[1,4]] - m2_par2[[1,4]]) / sqrt(m2_par1[[1,5]]^2 + m2_par2[[1,5]]^2)
  WHEL_par  <- (m2_par1[[2,4]] - m2_par2[[2,4]]) / sqrt(m2_par1[[2,5]]^2 + m2_par2[[2,5]]^2)
  
  EEH_par   <- (m3_par1[[1,4]] - m3_par2[[1,4]]) / sqrt(m3_par1[[1,5]]^2 + m3_par2[[1,5]]^2)
  EEL_par   <- (m3_par1[[2,4]] - m3_par2[[2,4]]) / sqrt(m3_par1[[2,5]]^2 + m3_par2[[2,5]]^2)
  
  ### Calculate p values
  p_MHEH_par <- 2*pnorm(-abs(MHEH_par)) 
  p_MHEL_par <- 2*pnorm(-abs(MHEL_par)) 
  
  p_WHEH_par <- 2*pnorm(-abs(WHEH_par)) 
  p_WHEL_par <- 2*pnorm(-abs(WHEL_par)) 
  
  p_EEH_par  <- 2*pnorm(-abs(EEH_par)) 
  p_EEL_par  <- 2*pnorm(-abs(EEL_par)) 
  
  ### Man Higher Earner
  message("High-stakes p = ",    round(p_MHEH_par,  digits = 3))
  message("Low-stakes  p = ",    round(p_MHEL_par,  digits = 3)) 
  
  ### Woman Higher Earner
  message("High-stakes p = ",    round(p_WHEH_par,  digits = 3))
  message("Low-stakes  p = ",    round(p_WHEL_par,  digits = 3)) 
  
  ### Equal Earners
  message("High-stakes p = ",    round(p_EEH_par,   digits = 3))
  message("Low-stakes  p = ",    round(p_EEL_par,   digits = 3)) 
  
## Test for statistical sig. between 3 years and 7 years (within decision)
  ### Calculate Z scores 
  MHEH_dur  <- (m1_dur1[[1,4]] - m1_dur2[[1,4]]) / sqrt(m1_dur1[[1,5]]^2 + m1_dur2[[1,5]]^2)
  MHEL_dur  <- (m1_dur1[[2,4]] - m1_dur2[[2,4]]) / sqrt(m1_dur1[[2,5]]^2 + m1_dur2[[2,5]]^2)
  
  WHEH_dur  <- (m2_dur1[[1,4]] - m2_dur2[[1,4]]) / sqrt(m2_dur1[[1,5]]^2 + m2_dur2[[1,5]]^2)
  WHEL_dur  <- (m2_dur1[[2,4]] - m2_dur2[[2,4]]) / sqrt(m2_dur1[[2,5]]^2 + m2_dur2[[2,5]]^2)
  
  EEH_dur   <- (m3_dur1[[1,4]] - m3_dur2[[1,4]]) / sqrt(m3_dur1[[1,5]]^2 + m3_dur2[[1,5]]^2)
  EEL_dur   <- (m3_dur1[[2,4]] - m3_dur2[[2,4]]) / sqrt(m3_dur1[[2,5]]^2 + m3_dur2[[2,5]]^2)
  
  ### Calculate p values
  p_MHEH_dur <- 2*pnorm(-abs(MHEH_dur)) 
  p_MHEL_dur <- 2*pnorm(-abs(MHEL_dur)) 
  
  p_WHEH_dur <- 2*pnorm(-abs(WHEH_dur)) 
  p_WHEL_dur <- 2*pnorm(-abs(WHEL_dur)) 
  
  p_EEH_dur  <- 2*pnorm(-abs(EEH_dur)) 
  p_EEL_dur  <- 2*pnorm(-abs(EEL_dur)) 
  
  ### Man Higher Earner
  message("High-stakes p = ",    round(p_MHEH_dur,  digits = 3))
  message("Low-stakes  p = ",    round(p_MHEL_dur,  digits = 3)) 
  
  ### Woman Higher Earner
  message("High-stakes p = ",    round(p_WHEH_dur,  digits = 3))
  message("Low-stakes  p = ",    round(p_WHEL_dur,  digits = 3)) 
  
  ### Equal Earners
  message("High-stakes p = ",    round(p_EEH_dur,   digits = 3))
  message("Low-stakes  p = ",    round(p_EEL_dur,   digits = 3)) 

  
## Create list for 3 panels
panels_S1 <- list(
  "Man Higher Earner"   = list("Cohabit"      = m1_mar1, "Married" = m1_mar2, 
                               "Not a\nparent" = m1_par1, "Parent"  = m1_par2,
                               "3 years"      = m1_dur1, "7 years" = m1_dur2),
  "Woman Higher Earner" = list("Cohabit"      = m2_mar1, "Married" = m2_mar2,
                               "Not a\nparent" = m2_par1, "Parent"  = m2_par2,
                               "3 years"      = m2_dur1, "7 years" = m2_dur2),
  "Equal Earners"       = list("Cohabit"      = m3_mar1, "Married" = m3_mar2,
                               "Not a\nparent" = m3_par1, "Parent"  = m3_par2,
                               "3 years"      = m3_dur1, "7 years" = m3_dur2))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"   = "Low Stakes")

## Produce Supplementary Table 01
modelsummary(
  panels_S1,
  shape = "rbind",
  coef_map = coef_map,
  gof_map = NA,
  #  exponentiate = TRUE,
  stars = c("*" =.05, "**" = .01, "***" = .001),
  fmt = fmt_decimal(digits = 3, pdigits = 3),
  #  add_rows = rows,
  output = "huxtable") %>%
  insert_row(c("Man Higher Earner",   " ", " ", " ", " ", " ", " "), after = 1)  %>%
  insert_row(c("Woman Higher Earner", " ", " ", " ", " ", " ", " "), after = 6)  %>%
  insert_row(c("Equal Earners",       " ", " ", " ", " ", " ", " "), after = 11) %>%
  insert_row(c('Significant difference, high vs. low stakes?', 
               'Yes', 'No', 'Yes', 'No', "Yes", "No"),               after = 6)  %>%
  insert_row(c('Significant difference, high vs. low stakes?', 
               'No', 'Yes', 'No', 'No', 'No',"No"),                  after = 12) %>%
  insert_row(c('Significant difference, high vs. low stakes?', 
               'No', 'No', 'No', 'No', 'No', "No"),                  after = 18) %>%
  set_top_border(row = c(8, 14), col = everywhere)                %>%
  set_bottom_border(row = c(1,8,14), col = everywhere)            %>%
  set_align(row = c(3, 5, 9, 11, 15, 17), 1, "center")            %>%
  huxtable::as_flextable()                                        %>%
  flextable::footnote(i = 15, j = 1, 
                      value = as_paragraph(c("Statistically significant parental status difference (p < .05)."))) %>%
  add_header_row(
    values = c(" ", "Marital Status", "Parental Status", "Relationship Duration"),
    colwidths = c(1, 2, 2, 2), top = TRUE) %>%
  flextable::align(align = "center", part = "header") %>%
  add_footer_lines("Notes: N=7,956 person-decisions. 3,970 men and 3,986 women. Results calculated from respondent-fixed effects linear probability models. Independent models applied by relative income and respondent gender. There were no statistically significant marital status or relationship duration differences (p < .05). Standard errors in parentheses.") %>%
  save_as_docx(path = file.path(outDir, "finalsay_table_S01.docx"))


# ------------------------------------------------------------------------------
# code that may be useful still but not part of workflow

### export to stata for FE models (Table C)
femodels <- quantdata %>%
  select(CaseID, dum1, dum2, fair1, fair2, per1, per2, 
         relinc, organize, mar, child, dur, high, gender, relate, parent, 
         raceeth, educ, employ, inc, age, low, order, weight)

write_dta(femodels , path = file.path(outDir, "femodels.dta")) 

### -- sensitivity test -- include order of decider*gender of decider
quantdata <- quantdata %>%
  mutate(orderN= case_when (order == "Same"  ~ 0,
                            order == "Mixed" ~ 1))

logit1o <- glm(dum1 ~ per1 * orderN + relinc + organize + mar + child + dur + high + 
                 gender+relate+parent+raceeth+educ+employ+inc+age,
               quantdata, family="binomial")
logit2o <- glm(dum2 ~ per2 * orderN + relinc + organize + mar + child + dur + low +
                 gender+relate+parent+raceeth+educ+employ+inc+age,
               quantdata, family="binomial")

AME1o <- summary(margins(logit1o, 
                         variables = "per1",
                         at = list(orderN= 0:1)))
AME2o <- summary(margins(logit2o, 
                         variables = "per2",
                         at = list(orderN= 0:1)))
AME1o 
AME2o

## Other interactions per reviewer B -------------------------------------------

### interactions
logit5a <- glm(dum1 ~ per1 * child + relinc + organize + mar + dur + high +
                 gender+relate+parent+raceeth+educ+employ+inc+age,
               quantdata, family="binomial")

logit5b <- glm(dum2 ~ per2 * child + relinc + organize + mar + dur + order + low +
                 gender+relate+parent+raceeth+educ+employ+inc+age,
               quantdata, family="binomial")