#-------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_02_quant analyses.R
# Joanna R. Pepin & William J. Scarborough
#-------------------------------------------------------------------------------

################################################################################
# Paper Tables and Figures (quant)
################################################################################
# This file analyzes the decision making variables.

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
         relinc, organize, mar, child, dur, high, gender, relate, parent, 
         raceeth, educ, employ, inc, age, low, order, weight) %>% 
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
  select(!c(drop1, drop2)) %>%
  mutate(decision = as.factor(decision))


# Table 02. --------------------------------------------------------------------
# Average Marginal Effects of Woman Deciding on Perceptions of Fairness 
# by relative income of vignette couple and decision type

## Prepare data for plm

### All
pdata_m1 <- pdata.frame(femodels %>% 
                          filter(relinc == "Man higher-earner"),   index = c("CaseID"))
pdata_m2 <- pdata.frame(femodels %>% 
                          filter(relinc == "Woman higher-earner"), index = c("CaseID"))
pdata_m3 <- pdata.frame(femodels %>% 
                          filter(relinc == "Equal earners"),       index = c("CaseID"))
### Men
pdata_m1M <- pdata.frame(femodels %>%
                           filter(relinc == "Man higher-earner"   & gender == "Male"),
                         index = c("CaseID"))
pdata_m2M <- pdata.frame(femodels %>% 
                           filter(relinc == "Woman higher-earner" & gender == "Male"),
                         index = c("CaseID"))
pdata_m3M <- pdata.frame(femodels %>% 
                           filter(relinc == "Equal earners"       & gender == "Male"),
                         index = c("CaseID"))
### Women
pdata_m1F <- pdata.frame(femodels %>% 
                           filter(relinc == "Man higher-earner"   & gender == "Female"),
                         index = c("CaseID"))
pdata_m2F <- pdata.frame(femodels %>% 
                           filter(relinc == "Woman higher-earner" & gender == "Female"),
                         index = c("CaseID"))
pdata_m3F <- pdata.frame(femodels %>% 
                           filter(relinc == "Equal earners"       & gender == "Female"),
                         index = c("CaseID"))

## Run the fixed effects models
plm1 <- plm(dum ~ per * decision, data = pdata_m1, model = "within")
plm2 <- plm(dum ~ per * decision, data = pdata_m2, model = "within")
plm3 <- plm(dum ~ per * decision, data = pdata_m3, model = "within")

plm1M <- plm(dum ~ per * decision, data = pdata_m1M, model = "within")
plm2M <- plm(dum ~ per * decision, data = pdata_m2M, model = "within")
plm3M <- plm(dum ~ per * decision, data = pdata_m3M, model = "within")

plm1F <- plm(dum ~ per * decision, data = pdata_m1F, model = "within")
plm2F <- plm(dum ~ per * decision, data = pdata_m2F, model = "within")
plm3F <- plm(dum ~ per * decision, data = pdata_m3F, model = "within")

## Average Marginal Effects of the models
m1 <- avg_slopes(plm1, variables = c("per"), by = "decision") 
m2 <- avg_slopes(plm2, variables = c("per"), by = "decision")
m3 <- avg_slopes(plm3, variables = c("per"), by = "decision")

m1M <- avg_slopes(plm1M, variables = c("per"), by = "decision")
m2M <- avg_slopes(plm2M, variables = c("per"), by = "decision")
m3M <- avg_slopes(plm3M, variables = c("per"), by = "decision")

m1F <- avg_slopes(plm1F, variables = c("per"), by = "decision")
m2F <- avg_slopes(plm2F, variables = c("per"), by = "decision")
m3F <- avg_slopes(plm3F, variables = c("per"), by = "decision")

## identify interaction variables
m1$term <- paste(m1$term, m1$decision, sep= ".")
m2$term <- paste(m2$term, m2$decision, sep= ".")
m3$term <- paste(m3$term, m3$decision, sep= ".")

m1M$term <- paste(m1M$term, m1M$decision, sep= ".")
m2M$term <- paste(m2M$term, m2M$decision, sep= ".")
m3M$term <- paste(m3M$term, m3M$decision, sep= ".")

m1F$term <- paste(m1F$term, m1F$decision, sep= ".")
m2F$term <- paste(m2F$term, m2F$decision, sep= ".")
m3F$term <- paste(m3F$term, m3F$decision, sep= ".")

# test equality of coefficients between HIGH & LOW stakes
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
# https://journals.sagepub.com/doi/10.1177/0081175019852763

## Test for statistical sig. between high and low stakes decisions
### Calculate Z scores 
  MHE    <- (m1[[1,4]] - m1[[2,4]]) / sqrt(m1[[1,5]]^2 + m1[[2,5]]^2)
  MHEM   <- (m1M[[1,4]] - m1M[[2,4]]) / sqrt(m1M[[1,5]]^2 + m1M[[2,5]]^2)
  MHEF   <- (m1F[[1,4]] - m1F[[2,4]]) / sqrt(m1F[[1,5]]^2 + m1F[[2,5]]^2)

  WHE    <- (m2[[1,4]] - m2[[2,4]]) / sqrt(m2[[1,5]]^2 + m2[[2,5]]^2)
  WHEM   <- (m2M[[1,4]] - m2M[[2,4]]) / sqrt(m2M[[1,5]]^2 + m2M[[2,5]]^2)
  WHEF   <- (m2F[[1,4]] - m2F[[2,4]]) / sqrt(m2F[[1,5]]^2 + m2F[[2,5]]^2)
 
  EE     <- (m3[[1,4]] - m3[[2,4]]) / sqrt(m3[[1,5]]^2 + m3[[2,5]]^2)
  EEM    <- (m3M[[1,4]] - m3M[[2,4]]) / sqrt(m3M[[1,5]]^2 + m3M[[2,5]]^2)
  EEF    <- (m3F[[1,4]] - m3F[[2,4]]) / sqrt(m3F[[1,5]]^2 + m3F[[2,5]]^2)

### Calculate p values
  p_MHE  <- 2*pnorm(-abs(MHE)) 
  p_MHEM <- 2*pnorm(-abs(MHEM)) 
  p_MHEF <- 2*pnorm(-abs(MHEF)) 

  p_WHE  <- 2*pnorm(-abs(WHE)) 
  p_WHEM <- 2*pnorm(-abs(WHEM)) 
  p_WHEF <- 2*pnorm(-abs(WHEF)) 

  p_EE   <- 2*pnorm(-abs(EE)) 
  p_EEM  <- 2*pnorm(-abs(EEM)) 
  p_EEF  <- 2*pnorm(-abs(EEF)) 

### Man Higher Earner
  message("All p = ",    round(p_MHE,   digits = 3))
  message("Men p = ",    round(p_MHEM,  digits = 3)) 
  message("Women p = ",  round(p_MHEF,  digits = 3)) 

### Woman Higher Earner
  message("All p = ",    round(p_WHE,   digits = 3))
  message("Men p = ",    round(p_WHEM,  digits = 3)) 
  message("Women p = ",  round(p_WHEF,  digits = 3)) 

### Equal Earners
  message("All p = ",    round(p_EE,    digits = 3))
  message("Men p = ",    round(p_EEM,   digits = 3)) 
  message("Women p = ",  round(p_EEF,   digits = 3)) 

## Test for statistical sig. between men and women (within decision)
### Calculate Z scores 
  MHEH   <- (m1M[[1,4]] - m1F[[1,4]]) / sqrt(m1M[[1,5]]^2 + m1F[[1,5]]^2)
  MHEL   <- (m1M[[2,4]] - m1F[[2,4]]) / sqrt(m1M[[2,5]]^2 + m1F[[2,5]]^2)
  
  WHEH   <- (m2M[[1,4]] - m2F[[1,4]]) / sqrt(m2M[[1,5]]^2 + m2F[[1,5]]^2)
  WHEL   <- (m2M[[2,4]] - m2F[[2,4]]) / sqrt(m2M[[2,5]]^2 + m2F[[2,5]]^2)
  
  EEH    <- (m3M[[1,4]] - m3F[[1,4]]) / sqrt(m3M[[1,5]]^2 + m3F[[1,5]]^2)
  EEL    <- (m3M[[2,4]] - m3F[[2,4]]) / sqrt(m3M[[2,5]]^2 + m3F[[2,5]]^2)
  
### Calculate p values
  p_MHEH  <- 2*pnorm(-abs(MHEH)) 
  p_MHEL  <- 2*pnorm(-abs(MHEL)) 

  p_WHEH <- 2*pnorm(-abs(WHEH)) 
  p_WHEL <- 2*pnorm(-abs(WHEL)) 
  
  p_EEH  <- 2*pnorm(-abs(EEH)) 
  p_EEL  <- 2*pnorm(-abs(EEL)) 
  
### Man Higher Earner
  message("High-stakes p = ",    round(p_MHEH,  digits = 3))
  message("Low-stakes  p = ",    round(p_MHEL,  digits = 3)) 
  
### Woman Higher Earner
  message("High-stakes p = ",    round(p_WHEH,  digits = 3))
  message("Low-stakes  p = ",    round(p_WHEL,  digits = 3)) 
  
### Equal Earners
  message("High-stakes p = ",    round(p_EEH,   digits = 3))
  message("Low-stakes  p = ",    round(p_EEL,   digits = 3)) 

## Create list for 3 panels
  panels <- list(
    "Man Higher Earner"   = list("All" = m1, "Men" = m1M, "Women" = m1F),
    "Woman Higher Earner" = list("All" = m2, "Men" = m2M, "Women" = m2F),
    "Equal Earners"       = list("All" = m3, "Men" = m3M, "Women" = m3F))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"   = "Low Stakes")

## Produce Table 02
modelsummary(
  panels,
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
  flextable::footnote(i = 11, j = 1, 
                      value = as_paragraph(c("Statistically significant gender difference (p < .05)."))) %>%
  add_footer_lines("Notes: N=7,956 person-decisions. 3,970 men and 3,986 women. Results calculated from respondent-fixed effects linear probability models. Independent models applied by relative income and respondent gender. Standard errors in parentheses.") %>%
  save_as_docx(path = file.path(outDir, "finalsay_table02.docx"))

# Figure 02. -------------------------------------------------------------------

## Create predicted probabilities datesets
pp1M <- avg_predictions(plm1M, by = c("per", "decision"))
pp1M$relinc <- "Man higher-earner"
pp1M$gender <- "Men"
pp1F <- avg_predictions(plm1F, by = c("per", "decision"))
pp1F$relinc <- "Man higher-earner"
pp1F$gender <- "Women"
pp2M <- avg_predictions(plm2M, by = c("per", "decision"))
pp2M$relinc <- "Woman higher-earner"
pp2M$gender <- "Men"
pp2F <- avg_predictions(plm2F, by = c("per", "decision"))
pp2F$relinc <- "Woman higher-earner"
pp2F$gender <- "Women"
pp3M <- avg_predictions(plm3M, by = c("per", "decision"))
pp3M$relinc <- "Equal earners"
pp3M$gender <- "Men"
pp3F <- avg_predictions(plm3F, by = c("per", "decision"))
pp3F$relinc <- "Equal earners"
pp3F$gender <- "Women"

## Combine and clean the datasets
data_fig2 <- do.call("rbind", list(pp1M, pp1F, pp2M, pp2F, pp3M, pp3F))

## tidy the data frame
data_fig2 <- data_fig2 %>% 
  mutate( 
    per = fct_case_when(
      per   == 0   ~ "He\ndecided",
      per   == 1   ~ "She\ndecided"),
    decision = fct_case_when(
      decision == "1" ~ "High\nstakes",
      decision == "2" ~ "Low\nstakes"),
    relinc = fct_case_when(
      relinc == "Man higher-earner" ~ "Man higher-earner",
      relinc == "Woman higher-earner" ~ "Woman higher-earner",
      relinc == "Equal earners" ~ "Equal earners"))

fig2 <- data_fig2 %>%
  ggplot(aes(x = per, y = estimate, fill = gender)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                stat="identity", position=position_dodge(.7), color="#707070") +
  geom_text(position = position_dodge(width = .7),
            vjust = -1,
            aes(label=sprintf("%1.0f%%", estimate*100))) +
  facet_grid(decision ~ relinc,
             scales="free",
             space = "free",
             switch = "y") +
  scale_fill_grey() +
  theme_minimal(12) +
  theme(legend.position     = "bottom",
        panel.grid.major.x  = element_blank(),
        strip.text          = element_text(face = "bold"),
        strip.text.y.left   = element_text(angle = 0),
        axis.text.y         = element_blank(),  #remove y axis labels
        axis.ticks.y        = element_blank(),  #remove y axis ticks
        plot.subtitle       = element_text(face = "italic", color = "#707070"),
        plot.caption        = element_text(face = "italic", color = "#707070"),
        plot.title          = ggtext::element_markdown(),
        plot.title.position = "plot") +
  scale_y_continuous(labels=scales::percent, limits = c(0, .88)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = "Respondents' gender",
        title    = "Predicted percent of respondents who rated the decision as somewhat or very fair",
        subtitle = "By decision type, vignette couples' relative income and decision-maker gender, and respondent gender",
        caption  = "Predicted percentages calculated from respondent-fixed effects linear probability models (see Appendix Table A3). 
        Independent models applied by vignette couple’s relative income and respondent gender.") 

fig2

ggsave(filename = file.path(figDir, "fig2.png"), fig2, 
       width=9, height=6, units="in", dpi=300, bg = "white")

################################################################################
# Appendix (quant)
################################################################################

# Appendix Table 1 -------------------------------------------------------------
## Descriptive Statistics of Respondent Characteristics

## Create weighted data 
data_A1 <- quantdata %>%
  select("gender", "relate", "parent", "raceeth", 
         "educ", "employ", "inc", "age")

tabA1 <- data_A1 %>%
  tbl_summary(
    label = list(gender  ~ "Women",
                 relate  ~ "Relationship Status",
                 parent  ~ "Parent",
                 raceeth ~ "Respondent race/ethnicity",
                 educ    ~ "Educational attainment",
                 employ  ~ "Employment status",
                 inc     ~ "Household income > $50,000",
                 age     ~ "Respondent age"),
    type  = list(gender  ~ "dichotomous",
                 parent  ~ "dichotomous",
                 inc     ~ "dichotomous"),
    value = list(gender  = "Female",
                 parent  = "Parent",
                 inc  = "> than $50,000"))  %>%
  modify_header(
    label = '**Variable**',
    stat_0 = '**N = 3,978**') %>%
  as_flex_table() 

tabA1 # show table

## https://mran.microsoft.com/snapshot/2017-12-11/web/packages/officer/vignettes/word.html
read_docx() %>% 
  body_add_par("Table A1. Sample Characteristics") %>% 
  body_add_flextable(value = tabA1) %>% 
  print(target = file.path(outDir, "finalsay_tableA1.docx"))


# Appendix Table 2 -------------------------------------------------------------
## Bivariate Statistics of Perceptions of Fairness in Decision Making 
## by Type of Decision (N = 3,978)

data_A2 <- quantdata %>%
  # Create long data for high/low vars
  pivot_longer(
    cols = c(dum1, dum2),
    names_to = "type",
    values_to = "fairness") %>%
  # Add a gender of decider variable
  mutate(
    person = case_when(
      (type  == "dum1"   &  iperson == "Michelle") |
      (type  == "dum2"   &  aperson == "Michelle") ~ "Woman",
      (type  == "dum1"   &  iperson == "Anthony")  |
      (type  == "dum2"   &  aperson == "Anthony")  ~ "Man")) %>%
  # Create long data for vignette manipulation
  pivot_longer(
    cols = c(relinc, person, mar, child, dur), # removed organize
    names_to = "variable",
    values_to = "level") %>%
  # Keep vignette variables
  select("CaseID", "weight", "type", "variable", "level", "fairness")

## Re order variable manipulations
data_A2$variable <- factor(data_A2$variable, 
                            levels  = c("relinc", "person", "organize",
                                        "mar", "child", "dur"), 
                            ordered = FALSE)

## Create summary data
tabA2 <- data_A2 %>%
  group_by(type, variable, level) %>%
  summarize(mean = mean(fairness, na.rm = TRUE),
            sd = sd(fairness, na.rm = TRUE)) %>%
  subset(select = -c(variable)) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  select(level, mean_dum1, sd_dum1, mean_dum2, sd_dum2)

tabA2 <- tabA2 %>%
mutate(
    cat = case_when(
      tabA2$level == "Man higher-earner"     |
      tabA2$level == "Woman higher-earner"   |
      tabA2$level == "Equal earners"       ~ "Relative Earnings",
      tabA2$level == "Woman"                 |
      tabA2$level == "Man"                 ~ "Gender of Decider",
      tabA2$level == "Shared"                |
      tabA2$level == "Separate"              |
      tabA2$level == "Both"                ~ "Financial Allocation Strategy",
      tabA2$level == "live together"         |
      tabA2$level == "are married"         ~ "Marital Status",
      tabA2$level == "no children"           |
      tabA2$level == "one child together"  ~ "Parental Status",
      tabA2$level == "3 years"               |
      tabA2$level == "7 years"             ~ "Relationship Duration"))

## Create Flextable
tabA2 <- as_grouped_data(x = tabA2, groups = c("cat"), columns = NULL) # Group by vignette condition

tabA2 <- tabA2 %>%
  flextable::as_flextable(hide_grouplabel = TRUE) %>%
  add_header_row(values = c("", "High Stakes", "Low Stakes"), colwidths = c(1, 2, 2)) %>%
  flextable::align(i = 1, align = "center", part = "header") %>%
  colformat_double(digits = 2) %>%
  set_header_labels(level = "Vignette Variables",
                    mean_dum1 = "M",
                    sd_dum1   = "SD", 
                    mean_dum2 = "M",
                    sd_dum2   = "SD" ) %>% 
  autofit() %>%
  padding(i=c(2:4,6:7,9:10,12:13, 15:16), j=1, padding.left=25) %>%
  add_footer(level = "Note: Perception of fairness measured as 0 (not fair) or 1 (fair).") %>%
  merge_at(j = 1:5, part = "footer")

num <-nrow(quantdata) #number of observations

tabA2 # show table

read_docx() %>% 
  body_add_par(paste("Table 02. Weighted Bivariate Statistics of Perceptions of Fairness in Decision Making 
               by Type of Decision (N = ", num,")", sep="")) %>% 
  body_add_flextable(value = tabA2) %>% 
  print(target = file.path(outDir, "finalsay_tableA2.docx")) # save table

# Appendix Table A3 ------------------------------------------------------------
## Results of Fixed Effects Models Using Continuous Outcome 
## to Measure Perception of Fairness

## Run the fixed effects models

### Full Sample
plm1_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m1, model = "within")
plm2_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m2, model = "within")
plm3_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m3, model = "within")

### Men
plm1M_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m1M, model = "within")
plm2M_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m2M, model = "within")
plm3M_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m3M, model = "within")

### Women
plm1F_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m1F, model = "within")
plm2F_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m2F, model = "within")
plm3F_A3 <- plm(as_numeric(fair) ~ per * decision, data = pdata_m3F, model = "within")

## Average Marginal Effects of the models

m1_A3  <- avg_slopes(plm1_A3, variables = c("per"), by = "decision")             
m2_A3  <- avg_slopes(plm2_A3, variables = c("per"), by = "decision")
m3_A3  <- avg_slopes(plm3_A3, variables = c("per"), by = "decision")

m1M_A3 <- avg_slopes(plm1M_A3, variables = c("per"), by = "decision")           
m2M_A3 <- avg_slopes(plm2M_A3, variables = c("per"), by = "decision")
m3M_A3 <- avg_slopes(plm3M_A3, variables = c("per"), by = "decision")

m1F_A3 <- avg_slopes(plm1F_A3, variables = c("per"), by = "decision")           
m2F_A3 <- avg_slopes(plm2F_A3, variables = c("per"), by = "decision")
m3F_A3 <- avg_slopes(plm3F_A3, variables = c("per"), by = "decision")

## identify interaction variables
m1_A3$term  <- paste(m1_A3$term,  m1_A3$decision, sep= ".")
m2_A3$term  <- paste(m2_A3$term,  m2_A3$decision, sep= ".")
m3_A3$term  <- paste(m3_A3$term,  m3_A3$decision, sep= ".")

m1M_A3$term <- paste(m1M_A3$term, m1M_A3$decision, sep= ".")
m2M_A3$term <- paste(m2M_A3$term, m2M_A3$decision, sep= ".")
m3M_A3$term <- paste(m3M_A3$term, m3M_A3$decision, sep= ".")

m1F_A3$term <- paste(m1F_A3$term, m1F_A3$decision, sep= ".")
m2F_A3$term <- paste(m2F_A3$term, m2F_A3$decision, sep= ".")
m3F_A3$term <- paste(m3F_A3$term, m3F_A3$decision, sep= ".")


## Calculate Z scores
MHE_A3   <- (m1_A3[[1,4]] - m1_A3[[2,4]])   / sqrt(m1_A3[[1,5]]^2 + m1_A3[[2,5]]^2)
WHE_A3   <- (m2_A3[[1,4]] - m2_A3[[2,4]])   / sqrt(m2_A3[[1,5]]^2 + m2_A3[[2,5]]^2)
EE_A3    <- (m3_A3[[1,4]] - m3_A3[[2,4]])   / sqrt(m3_A3[[1,5]]^2 + m3_A3[[2,5]]^2)

MHEM_A3  <- (m1M_A3[[1,4]] - m1M_A3[[2,4]]) / sqrt(m1M_A3[[1,5]]^2 + m1M_A3[[2,5]]^2)
MHEF_A3  <- (m1F_A3[[1,4]] - m1F_A3[[2,4]]) / sqrt(m1F_A3[[1,5]]^2 + m1F_A3[[2,5]]^2)

WHEM_A3  <- (m2M_A3[[1,4]] - m2M_A3[[2,4]]) / sqrt(m2M_A3[[1,5]]^2 + m2M_A3[[2,5]]^2)
WHEF_A3  <- (m2F_A3[[1,4]] - m2F_A3[[2,4]]) / sqrt(m2F_A3[[1,5]]^2 + m2F_A3[[2,5]]^2)

EEM_A3   <- (m3M_A3[[1,4]] - m3M_A3[[2,4]]) / sqrt(m3M_A3[[1,5]]^2 + m3M_A3[[2,5]]^2)
EEF_A3   <- (m3F_A3[[1,4]] - m3F_A3[[2,4]]) / sqrt(m3F_A3[[1,5]]^2 + m3F_A3[[2,5]]^2)

## Calculate p values
p_MHE_A3  <- 2*pnorm(-abs(MHE_A3)) 
p_WHE_A3  <- 2*pnorm(-abs(WHE_A3)) 
p_EE_A3   <- 2*pnorm(-abs(EE_A3)) 

p_MHEM_A3 <- 2*pnorm(-abs(MHEM_A3)) 
p_MHEF_A3 <- 2*pnorm(-abs(MHEF_A3)) 

p_WHEM_A3 <- 2*pnorm(-abs(WHEM_A3)) 
p_WHEF_A3 <- 2*pnorm(-abs(WHEF_A3)) 

p_EEM_A3  <- 2*pnorm(-abs(EEM_A3)) 
p_EEF_A3  <- 2*pnorm(-abs(EEF_A3)) 


## Man Higher Earner
message("All p = ",    round(p_MHE_A3,   digits = 3))
message("Men p = ",    round(p_MHEM_A3,  digits = 3)) 
message("Women p = ",  round(p_MHEF_A3,  digits = 3)) 

## Woman Higher Earner
message("All p = ",    round(p_WHE_A3,   digits = 3))
message("Men p = ",    round(p_WHEM_A3,  digits = 3)) 
message("Women p = ",  round(p_WHEF_A3,  digits = 3)) 

## Equal Earners
message("All p = ",    round(p_EE_A3,    digits = 3))
message("Men p = ",    round(p_EEM_A3,   digits = 3)) 
message("Women p = ",  round(p_EEF_A3,   digits = 3)) 

## Test for statistical sig. between men and women (within decision)
### Calculate Z scores 
MHEH_A3   <- (m1M_A3[[1,4]] - m1F_A3[[1,4]]) / sqrt(m1M_A3[[1,5]]^2 + m1F_A3[[1,5]]^2)
MHEL_A3   <- (m1M_A3[[2,4]] - m1F_A3[[2,4]]) / sqrt(m1M_A3[[2,5]]^2 + m1F_A3[[2,5]]^2)

WHEH_A3   <- (m2M_A3[[1,4]] - m2F_A3[[1,4]]) / sqrt(m2M_A3[[1,5]]^2 + m2F_A3[[1,5]]^2)
WHEL_A3   <- (m2M_A3[[2,4]] - m2F_A3[[2,4]]) / sqrt(m2M_A3[[2,5]]^2 + m2F_A3[[2,5]]^2)

EEH_A3    <- (m3M_A3[[1,4]] - m3F_A3[[1,4]]) / sqrt(m3M_A3[[1,5]]^2 + m3F_A3[[1,5]]^2)
EEL_A3    <- (m3M_A3[[2,4]] - m3F_A3[[2,4]]) / sqrt(m3M_A3[[2,5]]^2 + m3F_A3[[2,5]]^2)

### Calculate p values
p_MHEH_A3  <- 2*pnorm(-abs(MHEH_A3)) 
p_MHEL_A3  <- 2*pnorm(-abs(MHEL_A3)) 

p_WHEH_A3 <- 2*pnorm(-abs(WHEH_A3)) 
p_WHEL_A3 <- 2*pnorm(-abs(WHEL_A3)) 

p_EEH_A3  <- 2*pnorm(-abs(EEH_A3)) 
p_EEL_A3  <- 2*pnorm(-abs(EEL_A3)) 

### Man Higher Earner
message("High-stakes p = ",    round(p_MHEH_A3,  digits = 3))
message("Low-stakes  p = ",    round(p_MHEL_A3,  digits = 3)) 

### Woman Higher Earner
message("High-stakes p = ",    round(p_WHEH_A3,  digits = 3))
message("Low-stakes  p = ",    round(p_WHEL_A3,  digits = 3)) 

### Equal Earners
message("High-stakes p = ",    round(p_EEH_A3,   digits = 3))
message("Low-stakes  p = ",    round(p_EEL_A3,   digits = 3)) 

## Create list for 3 panels
panels_A3 <- list(
  "Man Higher Earner"   = list("All" = m1_A3, "Men" = m1M_A3, "Women" = m1F_A3),
  "Woman Higher Earner" = list("All" = m2_A3, "Men" = m2M_A3, "Women" = m2F_A3),
  "Equal Earners"       = list("All" = m3_A3, "Men" = m3M_A3, "Women" = m3F_A3))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"    = "Low Stakes")

## Produce Table A3
modelsummary(
  panels_A3,
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

# Appendix Figure A. -----------------------------------------------------------
## Fairness Evaluation by high/low Presented to Respondent

data_figA <- quantdata %>%
  select("CaseID", "high", "low", "fair1", "fair2") %>%
  # Create long data for high/low fairness vars
  pivot_longer(
    cols = c(fair1, fair2),
    names_to = "drop",
    values_to = "fairness") %>%
  # Create long data for high/low decision vars
  pivot_longer(
    cols = c(high, low),
    names_to = "type",
    values_to = "category") %>%
  # remove duplicates
  filter((drop == "fair1" & type == "high") |
         (drop == "fair2" & type == "low")) %>%
  select(-c("drop")) %>%
  # create percentage data
  group_by(type, category) %>%
  count(category = factor(category), fairness = factor(fairness)) %>% 
  mutate(pct = prop.table(n)) %>%
  ungroup()

data_figA$type[data_figA$type == "high"] <-"High\nstakes"
data_figA$type[data_figA$type == "low"] <-"Low\nstakes"

data_figA$type <- factor(data_figA$type, 
                         levels  = c("High\nstakes", "Low\nstakes"), 
                         ordered = FALSE)

figA <- data_figA %>%
  ggplot(aes(x = category, y = pct, fill = fct_rev(fairness))) +
  geom_col(position = "fill",
           width = 0.6) +
  facet_grid(type ~ .,
             scales="free",
             space = "free",
             switch = "y") +
  geom_text(aes(label=sprintf("%1.0f%%", pct*100)),
            position=position_stack(vjust=0.5),
            colour = "white",
            size = 3) +
  coord_flip()+
  scale_fill_grey() +
  theme_minimal(12) +
  theme(legend.position      = "top",
        legend.justification = c(1, 0),
        panel.grid.major.x   = element_blank(),
        strip.placement      = 'outside',
        strip.text.y         = element_text(face = "bold"),
        strip.text.y.left    = element_text(angle = 0),
        plot.title           = element_text(face = "bold"),
        plot.title.position  = "plot",
        plot.subtitle       = element_text(face = "italic", color = "#707070"),
        plot.caption        = element_text(face = "italic", color = "#707070")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Fairness evaluation for high and low stake decisions",
        subtitle = "How fair do you think the decision was?")

figA   

ggsave(filename = file.path(figDir, "figA.png"), figA, 
       width=6, height=4, units="in", dpi=300, bg = 'white')

