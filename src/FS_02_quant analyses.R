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
  pivot_longer(                                                               # long 2 numeric variables
    cols = c(contains('per'), 
             contains('dum')),
    names_to = c("vars", "decision"),
    names_pattern = "(.*)(.)$") %>%
  pivot_wider(                                                                # back to 2 rows (decisions) per person
    names_from = vars,
    values_from = value) %>%
  pivot_longer(                                                               # long factor variable
    cols = c("fair1", "fair2"),
    names_to = c("drop1", "drop2"),
    names_pattern = "(.*)(.)$",
    values_to = "fair") %>%
  filter(decision == drop2) %>%                                               # back to 2 rows (decisions) per person
  select(!c(drop1, drop2)) 

# Table 02. --------------------------------------------------------------------
# Average Marginal Effects of Woman Deciding on Perceptions of Fairness 
# by relative income of vignette couple and decision type

## Prepare data for plm
list_pdata <- list(
  ### All
  pdata_m1 <- pdata.frame(femodels %>% 
                            filter(relinc == "Man higher-earner"),   
                          index = c("CaseID")),
  pdata_m2 <- pdata.frame(femodels %>% 
                            filter(relinc == "Woman higher-earner"), 
                          index = c("CaseID")),
  pdata_m3 <- pdata.frame(femodels %>% 
                            filter(relinc == "Equal earners"),       
                          index = c("CaseID")),
  ### Men
  pdata_m1M <- pdata.frame(femodels %>%
                             filter(relinc == "Man higher-earner"   & gender == "Male"),
                           index = c("CaseID")),
  pdata_m2M <- pdata.frame(femodels %>% 
                             filter(relinc == "Woman higher-earner" & gender == "Male"),
                           index = c("CaseID")),
  pdata_m3M <- pdata.frame(femodels %>% 
                             filter(relinc == "Equal earners"       & gender == "Male"),
                           index = c("CaseID")),
  ### Women
  pdata_m1F <- pdata.frame(femodels %>% 
                             filter(relinc == "Man higher-earner"   & gender == "Female"),
                           index = c("CaseID")),
  pdata_m2F <- pdata.frame(femodels %>% 
                             filter(relinc == "Woman higher-earner" & gender == "Female"),
                           index = c("CaseID")),
  pdata_m3F <- pdata.frame(femodels %>% 
                             filter(relinc == "Equal earners"       & gender == "Female"),
                           index = c("CaseID")))

rm(list = ls()[grep("^pdata_", ls())]) # clean up global environment

# Run PLM models  
list_plm <- lapply(list_pdata, function(pdata){
  plm <- plm(dum ~ per * decision, data = pdata, model = "within")
})

# create average marginal effects
list_ame <- lapply(list_plm, function(plm){
  avg_slopes(plm, variables = c("per"), by = "decision")
})

# Define a function to identify person (term) 
replace_based_on_column <- function(df) {
  df$term <- paste(df$term, df$decision, sep= ".")
  return(df)
}

# Apply this function to each data frame in the list
list_ame <- lapply(list_ame, replace_based_on_column)

# Add list identifiers
list_ame[[1]][["relinc"]]   <- "Men higher-earner" 
list_ame[[2]][["relinc"]]   <- "Women higher-earner" 
list_ame[[3]][["relinc"]]   <- "Equal earners" 
list_ame[[4]][["relinc"]]   <- "Men higher-earner" 
list_ame[[5]][["relinc"]]   <- "Women higher-earner" 
list_ame[[6]][["relinc"]]   <- "Equal earners" 
list_ame[[7]][["relinc"]]   <- "Men higher-earner" 
list_ame[[8]][["relinc"]]   <- "Women higher-earner" 
list_ame[[9]][["relinc"]]   <- "Equal earners" 

list_ame[[1]][["gender"]]  <- "All" 
list_ame[[2]][["gender"]]  <- "All" 
list_ame[[3]][["gender"]]  <- "All" 
list_ame[[4]][["gender"]]  <- "Men" 
list_ame[[5]][["gender"]]  <- "Men" 
list_ame[[6]][["gender"]]  <- "Men" 
list_ame[[7]][["gender"]]  <- "Women" 
list_ame[[8]][["gender"]]  <- "Women" 
list_ame[[9]][["gender"]]  <- "Women" 

# test equality of coefficients between HIGH & LOW stakes
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
# https://journals.sagepub.com/doi/10.1177/0081175019852763

data_type <- as_tibble(rbind(
  list_ame[[1]],  list_ame[[2]],  list_ame[[3]], 
  list_ame[[4]],  list_ame[[5]],  list_ame[[6]],
  list_ame[[7]],  list_ame[[8]],  list_ame[[9]])) %>%
  mutate(relinc = fct_case_when(
    relinc == "Men higher-earner"   ~ "Men higher-earner",
    relinc == "Women higher-earner" ~ "Women higher-earner",
    relinc == "Equal earners"       ~ "Equal earners" )) %>%
  arrange(relinc) # sort data to test down

output_type <- NULL # create empty df for test results

for (i in seq(1, nrow(data_type), by = 2)) {
  gender        <- data_type[i, 15]
  relinc        <- data_type[i, 14]
  stakes        <- data_type[i,  3]
  decider       <- data_type[i,  1]
  estimate      <- ((data_type[i, 4] - data_type[i + 1, 4]) / 
                      sqrt(data_type[i, 5]^2 + data_type[i + 1, 5]^2))
  p             <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  output_type <- rbind(output_type, 
                       data.frame(gender, relinc, stakes, decider, estimate, p))
}

output_type <- output_type %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

output_type[!(is.na(output_type$sig)), ] # show only statistically sig. status differences


## Test for statistical sig. between men and women (within decision)
data_gender <- data_type %>%
  filter(gender != "All") %>%
  arrange(relinc, decision) #resort data to test across table

output_gender <- NULL # create empty df for test results

for (i in seq(1, nrow(data_gender), by = 2)) {
  gender        <- data_gender[i, 15]
  relinc        <- data_gender[i, 14]
  stakes        <- data_gender[i,  3]
  decider       <- data_gender[i,  1]
  estimate      <- ((data_gender[i, 4] - data_gender[i + 1, 4]) / 
                      sqrt(data_gender[i, 5]^2 + data_gender[i + 1, 5]^2))
  p             <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  output_gender <- rbind(output_gender, 
                         data.frame(gender, relinc, stakes, decider, estimate, p))
}

output_gender <- output_gender %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

output_gender[!(is.na(output_gender$sig)), ] # show only statistically sig. gender differences

output_gender <- output_gender %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

output_gender[!(is.na(output_gender$sig)), ] # show only statistically sig. gender differences

## Create list for 3 panels
panels <- list(
  "Man Higher Earner"   = list("All"   = list_ame[[1]], 
                               "Men"   = list_ame[[4]], 
                               "Women" = list_ame[[7]]),
  "Woman Higher Earner" = list("All"   = list_ame[[2]], 
                               "Men"   = list_ame[[5]], 
                               "Women" = list_ame[[8]]),
  "Equal Earners"       = list("All"   = list_ame[[3]], 
                               "Men"   = list_ame[[6]], 
                               "Women" = list_ame[[9]]))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"    = "Low Stakes")

## Produce Table 02
tab02 <- modelsummary(
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
  set_table_properties(layout = "autofit") 

tab02

read_docx() %>% 
  body_add_par(paste("Table 02. Average Marginal Effects of Woman Deciding on Perceptions of Fairness 
by vignette couple relative income, vignette decision type, and respondent gender")) %>% 
  body_add_flextable(value = tab02) %>% 
  print(target = file.path(outDir, "finalsay_table02.docx")) # save table


# Figure 02. -------------------------------------------------------------------

## Create predicted probabilities datesets
pp1M <- avg_predictions(list_plm[[4]], by = c("per", "decision"))
pp1M$relinc <- "Man higher-earner"
pp1M$gender <- "Men"
pp1F <- avg_predictions(list_plm[[7]], by = c("per", "decision"))
pp1F$relinc <- "Man higher-earner"
pp1F$gender <- "Women"
pp2M <- avg_predictions(list_plm[[5]], by = c("per", "decision"))
pp2M$relinc <- "Woman higher-earner"
pp2M$gender <- "Men"
pp2F <- avg_predictions(list_plm[[8]], by = c("per", "decision"))
pp2F$relinc <- "Woman higher-earner"
pp2F$gender <- "Women"
pp3M <- avg_predictions(list_plm[[6]], by = c("per", "decision"))
pp3M$relinc <- "Equal earners"
pp3M$gender <- "Men"
pp3F <- avg_predictions(list_plm[[9]], by = c("per", "decision"))
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
        caption  = "Predicted percentages calculated from respondent-fixed effects linear probability models (see Table 2). 
        Independent models applied by vignette couple’s relative income and respondent gender.") 

fig2

ggsave(filename = file.path(figDir, "fig2.png"), fig2, 
       width=9, height=6, units="in", dpi=300, bg = "white")

################################################################################
# SUPPLEMENTARY MATERIALS (quant)
################################################################################

# Supplementary Table 1 --------------------------------------------------------
## Descriptive Statistics of Respondent Characteristics

## Create weighted data 
data_S1 <- quantdata %>%
  select("gender", "relate", "parent", "raceeth", 
         "educ", "employ", "inc", "age")

tabS1 <- data_S1 %>%
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

tabS1 # show table

## https://mran.microsoft.com/snapshot/2017-12-11/web/packages/officer/vignettes/word.html
read_docx() %>% 
  body_add_par("Table S1. Sample Characteristics") %>% 
  body_add_flextable(value = tabS1) %>% 
  print(target = file.path(outDir, "finalsay_tableS1.docx"))


# Supplementary Table 2 --------------------------------------------------------
## Bivariate Statistics of Perceptions of Fairness in Decision Making 
## by Type of Decision (N = 3,978)

data_S2 <- quantdata %>%
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
data_S2$variable <- factor(data_S2$variable, 
                           levels  = c("relinc", "person", "organize",
                                       "mar", "child", "dur"), 
                           ordered = FALSE)

## Create summary data
tabS2 <- data_S2 %>%
  group_by(type, variable, level) %>%
  summarize(mean = mean(fairness, na.rm = TRUE),
            sd = sd(fairness, na.rm = TRUE)) %>%
  subset(select = -c(variable)) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  select(level, mean_dum1, sd_dum1, mean_dum2, sd_dum2)

tabS2 <- tabS2 %>%
  mutate(
    cat = case_when(
      tabS2$level == "Man higher-earner"     |
        tabS2$level == "Woman higher-earner"   |
        tabS2$level == "Equal earners"       ~ "Relative Earnings",
      tabS2$level == "Woman"                 |
        tabS2$level == "Man"                 ~ "Gender of Decider",
      tabS2$level == "Shared"                |
        tabS2$level == "Separate"              |
        tabS2$level == "Both"                ~ "Financial Allocation Strategy",
      tabS2$level == "live together"         |
        tabS2$level == "are married"         ~ "Marital Status",
      tabS2$level == "no children"           |
        tabS2$level == "one child together"  ~ "Parental Status",
      tabS2$level == "3 years"               |
        tabS2$level == "7 years"             ~ "Relationship Duration"))

## Create Flextable
tabS2 <- as_grouped_data(x = tabS2, groups = c("cat"), columns = NULL) # Group by vignette condition

tabS2 <- tabS2 %>%
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

tabS2 # show table

read_docx() %>% 
  body_add_par(paste("Table S2. Bivariate Statistics of Perceptions of Fairness in Decision Making by Type of Decision (N = ", num,")", sep="")) %>% 
  body_add_flextable(value = tabS2) %>% 
  print(target = file.path(outDir, "finalsay_tableS2.docx")) # save table


# Supplementary Table S3 -------------------------------------------------------
## Results of Fixed Effects Models Using Continuous Outcome 
## to Measure Perception of Fairness

# Run PLM models and create average marginal effects
list_ame_S3 <- lapply(list_pdata, function(pdata){
  
  plm <- plm(as_numeric(fair) ~ per * decision, data = pdata, model = "within")
  avg_slopes(plm, variables = c("per"), by = "decision", newdata = pdata)
  
})

# Apply function to each data frame in the list
list_ame_S3 <- lapply(list_ame_S3, replace_based_on_column)

# Add list identifiers
list_ame_S3[[1]][["relinc"]]   <- "Men higher-earner" 
list_ame_S3[[2]][["relinc"]]   <- "Women higher-earner" 
list_ame_S3[[3]][["relinc"]]   <- "Equal earners" 
list_ame_S3[[4]][["relinc"]]   <- "Men higher-earner" 
list_ame_S3[[5]][["relinc"]]   <- "Women higher-earner" 
list_ame_S3[[6]][["relinc"]]   <- "Equal earners" 
list_ame_S3[[7]][["relinc"]]   <- "Men higher-earner" 
list_ame_S3[[8]][["relinc"]]   <- "Women higher-earner" 
list_ame_S3[[9]][["relinc"]]   <- "Equal earners" 

list_ame_S3[[1]][["gender"]]  <- "All" 
list_ame_S3[[2]][["gender"]]  <- "All" 
list_ame_S3[[3]][["gender"]]  <- "All" 
list_ame_S3[[4]][["gender"]]  <- "Men" 
list_ame_S3[[5]][["gender"]]  <- "Men" 
list_ame_S3[[6]][["gender"]]  <- "Men" 
list_ame_S3[[7]][["gender"]]  <- "Women" 
list_ame_S3[[8]][["gender"]]  <- "Women" 
list_ame_S3[[9]][["gender"]]  <- "Women" 

# test equality of coefficients between HIGH & LOW stakes
data_type_S3 <- as_tibble(rbind(
  list_ame_S3[[1]],  list_ame_S3[[2]],  list_ame_S3[[3]], 
  list_ame_S3[[4]],  list_ame_S3[[5]],  list_ame_S3[[6]],
  list_ame_S3[[7]],  list_ame_S3[[8]],  list_ame_S3[[9]])) %>%
  mutate(relinc = fct_case_when(
    relinc == "Men higher-earner"   ~ "Men higher-earner",
    relinc == "Women higher-earner" ~ "Women higher-earner",
    relinc == "Equal earners"       ~ "Equal earners" )) %>%
  arrange(relinc) # sort data to test down

output_type_S3 <- NULL # create empty df for test results

for (i in seq(1, nrow(data_type_S3), by = 2)) {
  gender        <- data_type_S3[i, 15]
  relinc        <- data_type_S3[i, 14]
  stakes        <- data_type_S3[i,  3]
  decider       <- data_type_S3[i,  1]
  estimate      <- ((data_type_S3[i, 4] - data_type_S3[i + 1, 4]) / 
                      sqrt(data_type_S3[i, 5]^2 + data_type_S3[i + 1, 5]^2))
  p             <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  output_type_S3 <- rbind(output_type_S3, 
                          data.frame(gender, relinc, stakes, decider, estimate, p))
}

output_type_S3 <- output_type_S3 %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

output_type_S3[!(is.na(output_type_S3$sig)), ] # show only statistically sig. status differences


## Test for statistical sig. between men and women (within decision)
data_gender_S3 <- data_type_S3 %>%
  filter(gender != "All") %>%
  arrange(relinc, decision) #resort data to test across table

output_gender_S3 <- NULL # create empty df for test results

for (i in seq(1, nrow(data_gender_S3), by = 2)) {
  gender        <- data_gender_S3[i, 15]
  relinc        <- data_gender_S3[i, 14]
  stakes        <- data_gender_S3[i,  3]
  decider       <- data_gender_S3[i,  1]
  estimate      <- ((data_gender_S3[i, 4] - data_gender_S3[i + 1, 4]) / 
                      sqrt(data_gender_S3[i, 5]^2 + data_gender_S3[i + 1, 5]^2))
  p             <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  output_gender_S3 <- rbind(output_gender_S3, 
                            data.frame(gender, relinc, stakes, decider, estimate, p))
}

output_gender_S3 <- output_gender_S3 %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

output_gender_S3[!(is.na(output_gender_S3$sig)), ] # show only statistically sig. gender differences

output_gender_S3 <- output_gender_S3 %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

output_gender_S3[!(is.na(output_gender_S3$sig)), ] # show only statistically sig. gender differences

## Create list for 3 panels
panels_S3 <- list(
  "Man Higher Earner"   = list("All"   = list_ame_S3[[1]], 
                               "Men"   = list_ame_S3[[4]], 
                               "Women" = list_ame_S3[[7]]),
  "Woman Higher Earner" = list("All"   = list_ame_S3[[2]], 
                               "Men"   = list_ame_S3[[5]], 
                               "Women" = list_ame_S3[[8]]),
  "Equal Earners"       = list("All"   = list_ame_S3[[3]], 
                               "Men"   = list_ame_S3[[6]], 
                               "Women" = list_ame_S3[[9]]))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"    = "Low Stakes")

## Produce Table S3
tabS3 <- modelsummary(
  panels_S3,
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
  add_footer_lines("Notes: N=7,956 person-decisions. 3,970 men and 3,986 women. Results calculated from respondent-fixed effects linear probability models. Independent models applied by relative income and respondent gender. Standard errors in parentheses. There were no statistically significant gender difference (p < .05).")

tabS3
num <-nrow(quantdata) #number of observations

read_docx() %>% 
  body_add_par(paste("Table S3. Marginal Effects of Woman Deciding on Perceptions of Fairness by Relative Income of Vignette Couple and Decision Type", sep="")) %>% 
  body_add_flextable(value = tabS3) %>% 
  print(target = file.path(outDir, "finalsay_tableS3.docx")) # save table


# Supplementary Table S4. ------------------------------------------------------

## Prepare data for plm
list_pdata_mar.par.dur <- list(
  
  ### Married
  pdata_m1MAR <- pdata.frame(femodels %>% 
                               filter(relinc == "Man higher-earner"     & 
                                        mar == "are married"),
                             index = c("CaseID")),
  pdata_m2MAR <- pdata.frame(femodels %>% 
                               filter(relinc == "Woman higher-earner"   & 
                                        mar == "are married"),
                             index = c("CaseID")),
  pdata_m3MAR <- pdata.frame(femodels %>% 
                               filter(relinc == "Equal earners"         & 
                                        mar == "are married"),
                             index = c("CaseID")),
  
  ### Cohabiting
  pdata_m1COH <- pdata.frame(femodels %>% 
                               filter(relinc == "Man higher-earner"   & 
                                        mar == "live together"),
                             index = c("CaseID")),
  pdata_m2COH <- pdata.frame(femodels %>% 
                               filter(relinc == "Woman higher-earner" & 
                                        mar == "live together"),
                             index = c("CaseID")),
  pdata_m3COH <- pdata.frame(femodels %>% 
                               filter(relinc == "Equal earners"       & 
                                        mar == "live together"),
                             index = c("CaseID")),
  
  ### Parents
  pdata_m1PAR <- pdata.frame(femodels %>% 
                               filter(relinc == "Man higher-earner"   & 
                                        child == "one child together"),
                             index = c("CaseID")),
  pdata_m2PAR <- pdata.frame(femodels %>% 
                               filter(relinc == "Woman higher-earner" & 
                                        child == "one child together"),
                             index = c("CaseID")),
  pdata_m3PAR <- pdata.frame(femodels %>% 
                               filter(relinc == "Equal earners"       & 
                                        child == "one child together"),
                             index = c("CaseID")),
  
  ### Not parents
  pdata_m1NOK <- pdata.frame(femodels %>% 
                               filter(relinc == "Man higher-earner"   & 
                                        child == "no children"),
                             index = c("CaseID")),
  pdata_m2NOK <- pdata.frame(femodels %>% 
                               filter(relinc == "Woman higher-earner" & 
                                        child == "no children"),
                             index = c("CaseID")),
  pdata_m3NOK <- pdata.frame(femodels %>% 
                               filter(relinc == "Equal earners"       & 
                                        child == "no children"),
                             index = c("CaseID")),
  
  ### 7 years
  pdata_m1yr7 <- pdata.frame(femodels %>% 
                               filter(relinc == "Man higher-earner"   & 
                                        dur == "7 years"),
                             index = c("CaseID")),
  pdata_m2yr7 <- pdata.frame(femodels %>% 
                               filter(relinc == "Woman higher-earner" & 
                                        dur == "7 years"),
                             index = c("CaseID")),
  pdata_m3yr7 <- pdata.frame(femodels %>% 
                               filter(relinc == "Equal earners"       & 
                                        dur == "7 years"),
                             index = c("CaseID")),
  
  ### 3 years
  pdata_m1yr3 <- pdata.frame(femodels %>% 
                               filter(relinc == "Man higher-earner"   & 
                                        dur == "3 years"),
                             index = c("CaseID")),
  pdata_m2yr3 <- pdata.frame(femodels %>% 
                               filter(relinc == "Woman higher-earner" & 
                                        dur == "3 years"),
                             index = c("CaseID")),
  pdata_m3yr3 <- pdata.frame(femodels %>% 
                               filter(relinc == "Equal earners"       & 
                                        dur == "3 years"),
                             index = c("CaseID"))
)

rm(list = ls()[grep("^pdata_", ls())]) # clean up global environment


list_ame_S4 <- lapply(list_pdata_mar.par.dur, function(pdata){
  
  plm <- plm(dum ~ per * decision, data = pdata, model = "within")
  avg_slopes(plm, variables = c("per"), by = "decision", newdata = pdata)
  
})


# Apply this function to each data frame in the list
list_ame_S4 <- lapply(list_ame_S4, replace_based_on_column)

# Add list identifiers
list_ame_S4[[1]][["relinc"]]   <- "Men higher-earner" 
list_ame_S4[[2]][["relinc"]]   <- "Women higher-earner" 
list_ame_S4[[3]][["relinc"]]   <- "Equal earners" 
list_ame_S4[[4]][["relinc"]]   <- "Men higher-earner" 
list_ame_S4[[5]][["relinc"]]   <- "Women higher-earner" 
list_ame_S4[[6]][["relinc"]]   <- "Equal earners" 
list_ame_S4[[7]][["relinc"]]   <- "Men higher-earner" 
list_ame_S4[[8]][["relinc"]]   <- "Women higher-earner" 
list_ame_S4[[9]][["relinc"]]   <- "Equal earners" 
list_ame_S4[[10]][["relinc"]]  <- "Men higher-earner" 
list_ame_S4[[11]][["relinc"]]  <- "Women higher-earner" 
list_ame_S4[[12]][["relinc"]]  <- "Equal earners" 
list_ame_S4[[13]][["relinc"]]  <- "Men higher-earner" 
list_ame_S4[[14]][["relinc"]]  <- "Women higher-earner" 
list_ame_S4[[15]][["relinc"]]  <- "Equal earners" 
list_ame_S4[[16]][["relinc"]]  <- "Men higher-earner" 
list_ame_S4[[17]][["relinc"]]  <- "Women higher-earner" 
list_ame_S4[[18]][["relinc"]]  <- "Equal earners" 

list_ame_S4[[1]][["mar.par.dur"]]  <- "Married" 
list_ame_S4[[2]][["mar.par.dur"]]  <- "Married" 
list_ame_S4[[3]][["mar.par.dur"]]  <- "Married" 
list_ame_S4[[4]][["mar.par.dur"]]  <- "Cohabiting" 
list_ame_S4[[5]][["mar.par.dur"]]  <- "Cohabiting" 
list_ame_S4[[6]][["mar.par.dur"]]  <- "Cohabiting" 
list_ame_S4[[7]][["mar.par.dur"]]  <- "Parents" 
list_ame_S4[[8]][["mar.par.dur"]]  <- "Parents" 
list_ame_S4[[9]][["mar.par.dur"]]  <- "Parents" 
list_ame_S4[[10]][["mar.par.dur"]] <- "Not parents" 
list_ame_S4[[11]][["mar.par.dur"]] <- "Not parents" 
list_ame_S4[[12]][["mar.par.dur"]] <- "Not parents" 
list_ame_S4[[13]][["mar.par.dur"]] <- "7 years" 
list_ame_S4[[14]][["mar.par.dur"]] <- "7 years" 
list_ame_S4[[15]][["mar.par.dur"]] <- "7 years" 
list_ame_S4[[16]][["mar.par.dur"]] <- "3 years" 
list_ame_S4[[17]][["mar.par.dur"]] <- "3 years" 
list_ame_S4[[18]][["mar.par.dur"]] <- "3 years" 


## Test for statistical difference between high & low decisions

data_type_S4 <- as_tibble(rbind(
  list_ame_S4[[1]],  list_ame_S4[[2]],  list_ame_S4[[3]], 
  list_ame_S4[[4]],  list_ame_S4[[5]],  list_ame_S4[[6]],
  list_ame_S4[[7]],  list_ame_S4[[8]],  list_ame_S4[[9]],
  list_ame_S4[[10]], list_ame_S4[[11]], list_ame_S4[[12]],
  list_ame_S4[[13]], list_ame_S4[[14]], list_ame_S4[[15]],
  list_ame_S4[[16]], list_ame_S4[[17]], list_ame_S4[[18]])) %>%
  mutate(relinc = fct_case_when(
    relinc == "Men higher-earner"   ~ "Men higher-earner",
    relinc == "Women higher-earner" ~ "Women higher-earner",
    relinc == "Equal earners"       ~ "Equal earners" )) %>%
  arrange(relinc) # sort data to test down

output_type_S4 <- NULL # create empty df for test results

for (i in seq(1, nrow(data_type_S4), by = 2)) {
  status        <- data_type_S4[i, 15]
  relinc        <- data_type_S4[i, 14]
  stakes        <- data_type_S4[i,  3]
  decider       <- data_type_S4[i,  1]
  estimate      <- ((data_type_S4[i, 4] - data_type_S4[i + 1, 4]) / 
                      sqrt(data_type_S4[i, 5]^2 + data_type_S4[i + 1, 5]^2))
  p             <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  output_type_S4 <- rbind(output_type_S4, 
                          data.frame(status, relinc, stakes, decider, estimate, p))
}

output_type_S4 <- output_type_S4 %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

# show only statistically sig. decision differences
output_type_S4[!(is.na(output_type_S4$sig)), ] 


## Test for statistical difference between relationship statuses
data_status_S4 <- data_type_S4 %>%
  arrange(relinc, decision) #resort data to test across table

output_status_S4 <- NULL # create empty df for test results

for (i in seq(1, nrow(data_status_S4), by = 2)) {
  status        <- data_status_S4[i, 15]
  relinc        <- data_status_S4[i, 14]
  stakes        <- data_status_S4[i,  3]
  decider       <- data_status_S4[i,  1]
  estimate      <- ((data_status_S4[i, 4] - data_status_S4[i + 1, 4]) / 
                      sqrt(data_status_S4[i, 5]^2 + data_status_S4[i + 1, 5]^2))
  p             <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  output_status_S4 <- rbind(output_status_S4, 
                            data.frame(status, relinc, stakes, decider, estimate, p))
}

output_status_S4 <- output_status_S4 %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

# show only statistically sig. status differences
output_status_S4[!(is.na(output_status_S4$sig)), ] 


## Create list for 3 panels
panels_S4 <- list(
  "Man Higher Earner"   = list("Married" = list_ame_S4[[1]],  "Cohabiting"  = list_ame_S4[[4]], 
                               "Parents" = list_ame_S4[[7]],  "Not parents" = list_ame_S4[[10]],
                               "7 years" = list_ame_S4[[13]], "3 years"     = list_ame_S4[[16]]),
  "Woman Higher Earner" = list("Married" = list_ame_S4[[2]],  "Cohabiting"  = list_ame_S4[[5]], 
                               "Parents" = list_ame_S4[[8]],  "Not parents" = list_ame_S4[[11]],
                               "7 years" = list_ame_S4[[14]], "3 years"     = list_ame_S4[[17]]),
  "Equal Earners"       = list("Married" = list_ame_S4[[3]],  "Cohabiting"  = list_ame_S4[[6]], 
                               "Parents" = list_ame_S4[[9]],  "Not parents" = list_ame_S4[[12]],
                               "7 years" = list_ame_S4[[15]], "3 years"     = list_ame_S4[[18]]))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"    = "Low Stakes")

## Produce Table S4
tabS4 <- modelsummary(
  panels_S4,
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
               'No', 'Yes', 'No', 'Yes', 'No', 'Yes'),               after = 6)  %>%
  insert_row(c('Significant difference, high vs. low stakes?', 
               'No', 'No', 'No', 'No', 'No', 'No'),                  after = 12) %>%
  insert_row(c('Significant difference, high vs. low stakes?', 
               'No', 'No', 'No', 'No', 'Yes', 'No'),                 after = 18) %>%
  set_top_border(row = c(8, 14), col = everywhere)                %>%
  set_bottom_border(row = c(1,8,14), col = everywhere)            %>%
  set_align(row = c(3, 5, 9, 11, 15, 17), 1, "center")            %>%
  huxtable::as_flextable()                                        %>%
  flextable::footnote(i = 15, j = 1, 
                      value = as_paragraph(c("Statistically significant parental status difference (p < .05)."))) %>%
  add_footer_lines("Notes: N=7,956 person-decisions. Results calculated from respondent-fixed effects linear probability models. Independent models applied by relative income and relationship indicators. Standard errors in parentheses.") %>%
  set_table_properties(layout = "autofit") 

tabS4

read_docx() %>% 
  body_add_par(paste("Table S4. Average Marginal Effects of Woman Deciding on Perceptions of Fairness by vignette couple relative income, vignette decision type, and vignette relationship status indicators")) %>% 
  body_add_flextable(value = tabS4) %>% 
  print(target = file.path(outDir, "finalsay_tableS4.docx")) # save table


# Supplementary Figure 1. ------------------------------------------------------
## Fairness Evaluation by high/low Presented to Respondent

data_figS1 <- quantdata %>%
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

data_figS1$type[data_figS1$type == "high"] <-"High\nstakes"
data_figS1$type[data_figS1$type == "low"] <-"Low\nstakes"

data_figS1$type <- factor(data_figS1$type, 
                          levels  = c("High\nstakes", "Low\nstakes"), 
                          ordered = FALSE)

figS1 <- data_figS1 %>%
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

figS1   

ggsave(filename = file.path(figDir, "figS1.png"), figS1, 
       width=6, height=4, units="in", dpi=300, bg = 'white')