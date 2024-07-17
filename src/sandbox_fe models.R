#-------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_02_quant analyses.R
# Joanna R. Pepin & William J. Scarborough
#-------------------------------------------------------------------------------

# This file analyzes the decision making variables.

################################################################################
# Paper Tables and Figures (quant)
################################################################################

## Run the fixed effects models

### Full Sample
plm1_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m1, model = "within")
plm2_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m2, model = "within")
plm3_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m3, model = "within")

### Men
plm1M_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m1M, model = "within")
plm2M_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m2M, model = "within")
plm3M_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m3M, model = "within")

### Women
plm1F_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m1F, model = "within")
plm2F_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m2F, model = "within")
plm3F_S1 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m3F, model = "within")

## Average Marginal Effects of the models

m1_S1  <- avg_slopes(plm1_S1, variables = c("per"), by = "decision")             
m2_S1  <- avg_slopes(plm2_S1, variables = c("per"), by = "decision")
m3_S1  <- avg_slopes(plm3_S1, variables = c("per"), by = "decision")

m1M_S1 <- avg_slopes(plm1M_S1, variables = c("per"), by = "decision")           
m2M_S1 <- avg_slopes(plm2M_S1, variables = c("per"), by = "decision")
m3M_S1 <- avg_slopes(plm3M_S1, variables = c("per"), by = "decision")

m1F_S1 <- avg_slopes(plm1F_S1, variables = c("per"), by = "decision")           
m2F_S1 <- avg_slopes(plm2F_S1, variables = c("per"), by = "decision")
m3F_S1 <- avg_slopes(plm3F_S1, variables = c("per"), by = "decision")

## identify interaction variables
m1_S1$term  <- paste(m1_S1$term,  m1_S1$decision, sep= ".")
m2_S1$term  <- paste(m2_S1$term,  m2_S1$decision, sep= ".")
m3_S1$term  <- paste(m3_S1$term,  m3_S1$decision, sep= ".")

m1M_S1$term <- paste(m1M_S1$term, m1M_S1$decision, sep= ".")
m2M_S1$term <- paste(m2M_S1$term, m2M_S1$decision, sep= ".")
m3M_S1$term <- paste(m3M_S1$term, m3M_S1$decision, sep= ".")

m1F_S1$term <- paste(m1F_S1$term, m1F_S1$decision, sep= ".")
m2F_S1$term <- paste(m2F_S1$term, m2F_S1$decision, sep= ".")
m3F_S1$term <- paste(m3F_S1$term, m3F_S1$decision, sep= ".")


## Calculate Z scores
MHE_S1   <- (m1_S1[[1,4]] - m1_S1[[2,4]])   / sqrt(m1_S1[[1,5]]^2 + m1_S1[[2,5]]^2)
WHE_S1   <- (m2_S1[[1,4]] - m2_S1[[2,4]])   / sqrt(m2_S1[[1,5]]^2 + m2_S1[[2,5]]^2)
EE_S1    <- (m3_S1[[1,4]] - m3_S1[[2,4]])   / sqrt(m3_S1[[1,5]]^2 + m3_S1[[2,5]]^2)

MHEM_S1  <- (m1M_S1[[1,4]] - m1M_S1[[2,4]]) / sqrt(m1M_S1[[1,5]]^2 + m1M_S1[[2,5]]^2)
MHEF_S1  <- (m1F_S1[[1,4]] - m1F_S1[[2,4]]) / sqrt(m1F_S1[[1,5]]^2 + m1F_S1[[2,5]]^2)

WHEM_S1  <- (m2M_S1[[1,4]] - m2M_S1[[2,4]]) / sqrt(m2M_S1[[1,5]]^2 + m2M_S1[[2,5]]^2)
WHEF_S1  <- (m2F_S1[[1,4]] - m2F_S1[[2,4]]) / sqrt(m2F_S1[[1,5]]^2 + m2F_S1[[2,5]]^2)

EEM_S1   <- (m3M_S1[[1,4]] - m3M_S1[[2,4]]) / sqrt(m3M_S1[[1,5]]^2 + m3M_S1[[2,5]]^2)
EEF_S1   <- (m3F_S1[[1,4]] - m3F_S1[[2,4]]) / sqrt(m3F_S1[[1,5]]^2 + m3F_S1[[2,5]]^2)

## Calculate p values
p_MHE_S1  <- 2*pnorm(-abs(MHE_S1)) 
p_WHE_S1  <- 2*pnorm(-abs(WHE_S1)) 
p_EE_S1   <- 2*pnorm(-abs(EE_S1)) 

p_MHEM_S1 <- 2*pnorm(-abs(MHEM_S1)) 
p_MHEF_S1 <- 2*pnorm(-abs(MHEF_S1)) 

p_WHEM_S1 <- 2*pnorm(-abs(WHEM_S1)) 
p_WHEF_S1 <- 2*pnorm(-abs(WHEF_S1)) 

p_EEM_S1  <- 2*pnorm(-abs(EEM_S1)) 
p_EEF_S1  <- 2*pnorm(-abs(EEF_S1)) 


## Man Higher Earner
message("All p = ",    round(p_MHE_S1,   digits = 3))
message("Men p = ",    round(p_MHEM_S1,  digits = 3)) 
message("Women p = ",  round(p_MHEF_S1,  digits = 3)) 

## Woman Higher Earner
message("All p = ",    round(p_WHE_S1,   digits = 3))
message("Men p = ",    round(p_WHEM_S1,  digits = 3)) 
message("Women p = ",  round(p_WHEF_S1,  digits = 3)) 

## Equal Earners
message("All p = ",    round(p_EE_S1,    digits = 3))
message("Men p = ",    round(p_EEM_S1,   digits = 3)) 
message("Women p = ",  round(p_EEF_S1,   digits = 3)) 

## Test for statistical sig. between men and women (within decision)
### Calculate Z scores 
MHEH_S1   <- (m1M_S1[[1,4]] - m1F_S1[[1,4]]) / sqrt(m1M_S1[[1,5]]^2 + m1F_S1[[1,5]]^2)
MHEL_S1   <- (m1M_S1[[2,4]] - m1F_S1[[2,4]]) / sqrt(m1M_S1[[2,5]]^2 + m1F_S1[[2,5]]^2)

WHEH_S1   <- (m2M_S1[[1,4]] - m2F_S1[[1,4]]) / sqrt(m2M_S1[[1,5]]^2 + m2F_S1[[1,5]]^2)
WHEL_S1   <- (m2M_S1[[2,4]] - m2F_S1[[2,4]]) / sqrt(m2M_S1[[2,5]]^2 + m2F_S1[[2,5]]^2)

EEH_S1    <- (m3M_S1[[1,4]] - m3F_S1[[1,4]]) / sqrt(m3M_S1[[1,5]]^2 + m3F_S1[[1,5]]^2)
EEL_S1    <- (m3M_S1[[2,4]] - m3F_S1[[2,4]]) / sqrt(m3M_S1[[2,5]]^2 + m3F_S1[[2,5]]^2)

### Calculate p values
p_MHEH_S1  <- 2*pnorm(-abs(MHEH_S1)) 
p_MHEL_S1  <- 2*pnorm(-abs(MHEL_S1)) 

p_WHEH_S1 <- 2*pnorm(-abs(WHEH_S1)) 
p_WHEL_S1 <- 2*pnorm(-abs(WHEL_S1)) 

p_EEH_S1  <- 2*pnorm(-abs(EEH_S1)) 
p_EEL_S1  <- 2*pnorm(-abs(EEL_S1)) 

### Man Higher Earner
message("High-stakes p = ",    round(p_MHEH_S1,  digits = 3))
message("Low-stakes  p = ",    round(p_MHEL_S1,  digits = 3)) 

### Woman Higher Earner
message("High-stakes p = ",    round(p_WHEH_S1,  digits = 3))
message("Low-stakes  p = ",    round(p_WHEL_S1,  digits = 3)) 

### Equal Earners
message("High-stakes p = ",    round(p_EEH_S1,   digits = 3))
message("Low-stakes  p = ",    round(p_EEL_S1,   digits = 3)) 

## Create list for 3 panels
panels_S1 <- list(
  "Man Higher Earner"   = list("All" = m1_S1, "Men" = m1M_S1, "Women" = m1F_S1),
  "Woman Higher Earner" = list("All" = m2_S1, "Men" = m2M_S1, "Women" = m2F_S1),
  "Equal Earners"       = list("All" = m3_S1, "Men" = m3M_S1, "Women" = m3F_S1))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"   = "Low Stakes")

## Produce Table 02
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
  insert_row(c("Man Higher Earner",   " ", " ", " "), after = 1)  %>%
  insert_row(c("Woman Higher Earner", " ", " ", " "), after = 6)  %>%
  insert_row(c("Equal Earners",       " ", " ", " "), after = 11) %>%
  insert_row(c('Significant difference, high vs. low stakes?', 
               'Yes', 'Yes', 'No'),                   after = 6)  %>%
  insert_row(c('Significant difference, high vs. low stakes?', 
               'Yes', 'No', 'Yes'),                  after = 12)  %>%
  insert_row(c('Significant difference, high vs. low stakes?', 
               'No', 'No', 'No'),                    after = 18)  %>%
  set_top_border(row = c(8, 14), col = everywhere)                %>%
  set_bottom_border(row = c(1,8,14), col = everywhere)            %>%
  set_align(row = c(3, 5, 9, 11, 15, 17), 1, "center")            %>%
  huxtable::as_flextable()                                        %>%
  add_footer_lines("Notes: N=7,956 person-decisions. 3,970 men and 3,986 women. Results calculated from respondent-fixed effects linear probability models. Independent models applied by relative income and respondent gender. Standard errors in parentheses. There were no statistically significant gender difference (p < .05).") %>%
  save_as_docx(path = file.path(outDir, "finalsay_tableA3.docx"))
