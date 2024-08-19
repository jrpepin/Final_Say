
### Married
pdata_m1MAR <- pdata.frame(femodels %>% 
                           filter(relinc == "Man higher-earner"     & 
                                    mar == "are married"),
                         index = c("CaseID"))
pdata_m2MAR <- pdata.frame(femodels %>% 
                           filter(relinc == "Woman higher-earner"   & 
                                    mar == "are married"),
                         index = c("CaseID"))
pdata_m3MAR <- pdata.frame(femodels %>% 
                           filter(relinc == "Equal earners"         & 
                                    mar == "are married"),
                         index = c("CaseID"))

### Cohabiting
pdata_m1COH <- pdata.frame(femodels %>% 
                             filter(relinc == "Man higher-earner"   & 
                                      mar == "live together"),
                           index = c("CaseID"))
pdata_m2COH <- pdata.frame(femodels %>% 
                             filter(relinc == "Woman higher-earner" & 
                                      mar == "live together"),
                           index = c("CaseID"))
pdata_m3COH <- pdata.frame(femodels %>% 
                             filter(relinc == "Equal earners"       & 
                                      mar == "live together"),
                           index = c("CaseID"))

### Parents
pdata_m1PAR <- pdata.frame(femodels %>% 
                             filter(relinc == "Man higher-earner"   & 
                                      child == "one child together"),
                           index = c("CaseID"))
pdata_m2PAR <- pdata.frame(femodels %>% 
                             filter(relinc == "Woman higher-earner" & 
                                      child == "one child together"),
                           index = c("CaseID"))
pdata_m3PAR <- pdata.frame(femodels %>% 
                             filter(relinc == "Equal earners"       & 
                                      child == "one child together"),
                           index = c("CaseID"))

### Not parents
pdata_m1NOK <- pdata.frame(femodels %>% 
                             filter(relinc == "Man higher-earner"   & 
                                      child == "no children"),
                           index = c("CaseID"))
pdata_m2NOK <- pdata.frame(femodels %>% 
                             filter(relinc == "Woman higher-earner" & 
                                      child == "no children"),
                           index = c("CaseID"))
pdata_m3NOK <- pdata.frame(femodels %>% 
                             filter(relinc == "Equal earners"       & 
                                      child == "no children"),
                           index = c("CaseID"))

### 7 years
pdata_m1yr7 <- pdata.frame(femodels %>% 
                             filter(relinc == "Man higher-earner"   & 
                                      dur == "7 years"),
                           index = c("CaseID"))
pdata_m2yr7 <- pdata.frame(femodels %>% 
                             filter(relinc == "Woman higher-earner" & 
                                      dur == "7 years"),
                           index = c("CaseID"))
pdata_m3yr7 <- pdata.frame(femodels %>% 
                             filter(relinc == "Equal earners"       & 
                                      dur == "7 years"),
                           index = c("CaseID"))

### 3 years
pdata_m1yr3 <- pdata.frame(femodels %>% 
                             filter(relinc == "Man higher-earner"   & 
                                      dur == "3 years"),
                           index = c("CaseID"))
pdata_m2yr3 <- pdata.frame(femodels %>% 
                             filter(relinc == "Woman higher-earner" & 
                                      dur == "3 years"),
                           index = c("CaseID"))
pdata_m3yr3 <- pdata.frame(femodels %>% 
                             filter(relinc == "Equal earners"       & 
                                      dur == "3 years"),
                           index = c("CaseID"))


## Run the fixed effects models
in_list <- list(pdata_m1MAR, pdata_m2MAR, pdata_m3MAR,
                pdata_m1COH, pdata_m2COH, pdata_m3COH,
                pdata_m1PAR, pdata_m2PAR, pdata_m3PAR,
                pdata_m1NOK, pdata_m2NOK, pdata_m3NOK,
                pdata_m1yr7, pdata_m2yr7, pdata_m3yr7,
                pdata_m1yr3, pdata_m2yr3, pdata_m3yr3)

out_list <- lapply(in_list, function(pdata){
  
  plm <- plm(dum ~ per * decision, data = pdata, model = "within")
  avg_slopes(plm, variables = c("per"), by = "decision")

})

# Define a function to identify person (term) 
replace_based_on_column <- function(df) {
  df$term <- paste(df$term, df$decision, sep= ".")
  return(df)
}

# Apply this function to each data frame in the list
out_list <- lapply(out_list, replace_based_on_column)

# Add list identifiers
out_list[[1]][["relinc"]]   <- "Men higher-earner" 
out_list[[2]][["relinc"]]   <- "Women higher-earner" 
out_list[[3]][["relinc"]]   <- "Equal earners" 
out_list[[4]][["relinc"]]   <- "Men higher-earner" 
out_list[[5]][["relinc"]]   <- "Women higher-earner" 
out_list[[6]][["relinc"]]   <- "Equal earners" 
out_list[[7]][["relinc"]]   <- "Men higher-earner" 
out_list[[8]][["relinc"]]   <- "Women higher-earner" 
out_list[[9]][["relinc"]]   <- "Equal earners" 
out_list[[10]][["relinc"]]  <- "Men higher-earner" 
out_list[[11]][["relinc"]]  <- "Women higher-earner" 
out_list[[12]][["relinc"]]  <- "Equal earners" 
out_list[[13]][["relinc"]]  <- "Men higher-earner" 
out_list[[14]][["relinc"]]  <- "Women higher-earner" 
out_list[[15]][["relinc"]]  <- "Equal earners" 
out_list[[16]][["relinc"]]  <- "Men higher-earner" 
out_list[[17]][["relinc"]]  <- "Women higher-earner" 
out_list[[18]][["relinc"]]  <- "Equal earners" 

out_list[[1]][["mar.par.dur"]]  <- "Married" 
out_list[[2]][["mar.par.dur"]]  <- "Married" 
out_list[[3]][["mar.par.dur"]]  <- "Married" 
out_list[[4]][["mar.par.dur"]]  <- "Cohabiting" 
out_list[[5]][["mar.par.dur"]]  <- "Cohabiting" 
out_list[[6]][["mar.par.dur"]]  <- "Cohabiting" 
out_list[[7]][["mar.par.dur"]]  <- "Parents" 
out_list[[8]][["mar.par.dur"]]  <- "Parents" 
out_list[[9]][["mar.par.dur"]]  <- "Parents" 
out_list[[10]][["mar.par.dur"]] <- "Not parents" 
out_list[[11]][["mar.par.dur"]] <- "Not parents" 
out_list[[12]][["mar.par.dur"]] <- "Not parents" 
out_list[[13]][["mar.par.dur"]] <- "7 years" 
out_list[[14]][["mar.par.dur"]] <- "7 years" 
out_list[[15]][["mar.par.dur"]] <- "7 years" 
out_list[[16]][["mar.par.dur"]] <- "3 years" 
out_list[[17]][["mar.par.dur"]] <- "3 years" 
out_list[[18]][["mar.par.dur"]] <- "3 years" 


## Test for statistical difference between high & low decisions

data_type <- as_tibble(rbind(
  out_list[[1]],  out_list[[2]],  out_list[[3]], 
  out_list[[4]],  out_list[[5]],  out_list[[6]],
  out_list[[7]],  out_list[[8]],  out_list[[9]],
  out_list[[10]], out_list[[11]], out_list[[12]],
  out_list[[13]], out_list[[14]], out_list[[15]],
  out_list[[16]], out_list[[17]], out_list[[18]])) %>%
  mutate(relinc = fct_case_when(
    relinc == "Men higher-earner"   ~ "Men higher-earner",
    relinc == "Women higher-earner" ~ "Women higher-earner",
    relinc == "Equal earners"       ~ "Equal earners" )) %>%
  arrange(relinc) # sort data to test down

output_type <- NULL # create empty df for test results

for (i in seq(1, nrow(data_type), by = 2)) {
  status        <- data_type[i, 15]
  relinc        <- data_type[i, 14]
  stakes        <- data_type[i,  3]
  decider       <- data_type[i,  1]
  estimate      <- ((data_type[i, 4] - data_type[i + 1, 4]) / 
                      sqrt(data_type[i, 5]^2 + data_type[i + 1, 5]^2))
  p             <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  output_type <- rbind(output_type, 
                         data.frame(status, relinc, stakes, decider, estimate, p))
}

output_type <- output_type %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

output_type[!(is.na(output_type$sig)), ] # show only statistically sig. status differences


## Test for statistical difference between relationship statuses
data_status <- data_type %>%
  arrange(relinc, decision) #resort data to test across table

output_status <- NULL # create empty df for test results

for (i in seq(1, nrow(data_status), by = 2)) {
  status        <- data_status[i, 15]
  relinc        <- data_status[i, 14]
  stakes        <- data_status[i,  3]
  decider       <- data_status[i,  1]
  estimate      <- ((data_status[i, 4] - data_status[i + 1, 4]) / 
                      sqrt(data_status[i, 5]^2 + data_status[i + 1, 5]^2))
  p             <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  output_status <- rbind(output_status, 
                         data.frame(status, relinc, stakes, decider, estimate, p))
}

output_status <- output_status %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

output_status[!(is.na(output_status$sig)), ] # show only statistically sig. status differences


## Create list for 3 panels
panels <- list(
  "Man Higher Earner"   = list("Married" = out_list[[1]],  "Cohabiting"  = out_list[[4]], 
                               "Parents" = out_list[[7]],  "Not parents" = out_list[[10]],
                               "7 years" = out_list[[13]], "3 years"     = out_list[[16]]),
  "Woman Higher Earner" = list("Married" = out_list[[2]],  "Cohabiting"  = out_list[[5]], 
                               "Parents" = out_list[[8]],  "Not parents" = out_list[[11]],
                               "7 years" = out_list[[14]], "3 years"     = out_list[[17]]),
  "Equal Earners"       = list("Married" = out_list[[3]],  "Cohabiting"  = out_list[[6]], 
                               "Parents" = out_list[[9]],  "Not parents" = out_list[[12]],
                               "7 years" = out_list[[15]], "3 years"     = out_list[[18]]))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"    = "Low Stakes")

## Produce Table S4
tabS4 <- modelsummary(
  panels,
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
  set_top_border(row = c(7, 12), col = everywhere)                          %>%
  set_bottom_border(row = c(1,7,12), col = everywhere)                      %>%
  set_align(row = c(3, 5, 8, 10, 13, 15), 1, "center")                      %>%
  huxtable::as_flextable()                                                  %>%
  add_footer_lines("Notes: N=7,956 person-decisions. There were no statistically significant differences by decision type or by the relationship indicators (i.e., marital status, parental status, or relationship duration). Results calculated from respondent-fixed effects linear probability models. Independent models applied by relative income and relationship indicators. Standard errors in parentheses.") %>%
  set_table_properties(layout = "autofit") 

tabS4

read_docx() %>% 
  body_add_par(paste("Table S4. Average Marginal Effects of Woman Deciding on Perceptions of Fairness by vignette couple relative income, vignette decision type, and vignette relationship status indicators")) %>% 
  body_add_flextable(value = tabS4) %>% 
  print(target = file.path(outDir, "finalsay_tableS4.docx")) # save table


