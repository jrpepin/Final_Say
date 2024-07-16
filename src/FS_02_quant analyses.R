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

## Prepare data for plm

### Male respondents
pdata_m1M <- pdata.frame(femodels %>% 
                           filter(relinc == "Man higher-earner" &
                                    gender == "Male"),
                         index = c("CaseID"))
pdata_m2M <- pdata.frame(femodels %>% 
                           filter(relinc == "Woman higher-earner" &
                                    gender == "Male"),
                         index = c("CaseID"))
pdata_m3M <- pdata.frame(femodels %>% 
                           filter(relinc == "Equal earners" &
                                    gender == "Male"),
                         index = c("CaseID"))

### Female respondents
pdata_m1F <- pdata.frame(femodels %>% 
                           filter(relinc == "Man higher-earner" &
                                    gender == "Female"),
                         index = c("CaseID"))
pdata_m2F <- pdata.frame(femodels %>% 
                           filter(relinc == "Woman higher-earner" &
                                    gender == "Female"),
                         index = c("CaseID"))
pdata_m3F <- pdata.frame(femodels %>% 
                           filter(relinc == "Equal earners" &
                                    gender == "Female"),
                         index = c("CaseID"))

## Run the fixed effects models
plm1M <- plm(dum ~ per * decision, data = pdata_m1M, model = "within")
plm2M <- plm(dum ~ per * decision, data = pdata_m2M, model = "within")
plm3M <- plm(dum ~ per * decision, data = pdata_m3M, model = "within")

plm1F <- plm(dum ~ per * decision, data = pdata_m1F, model = "within")
plm2F <- plm(dum ~ per * decision, data = pdata_m2F, model = "within")
plm3F <- plm(dum ~ per * decision, data = pdata_m3F, model = "within")

## Create predicted probabilities datesets
pp1M <- ggpredict(plm1M, terms = c("per", "decision"))
pp1M$relinc <- "Man higher-earner"
pp1M$gender <- "Men"
pp1F <- ggpredict(plm1F, terms = c("per", "decision"))
pp1F$relinc <- "Man higher-earner"
pp1F$gender <- "Women"
pp2M <- ggpredict(plm2M, terms = c("per", "decision"))
pp2M$relinc <- "Woman higher-earner"
pp2M$gender <- "Men"
pp2F <- ggpredict(plm2F, terms = c("per", "decision"))
pp2F$relinc <- "Woman higher-earner"
pp2F$gender <- "Women"
pp3M <- ggpredict(plm3M, terms = c("per", "decision"))
pp3M$relinc <- "Equal earners"
pp3M$gender <- "Men"
pp3F <- ggpredict(plm3F, terms = c("per", "decision"))
pp3F$relinc <- "Equal earners"
pp3F$gender <- "Women"

## Combine and clean the datasets
data_fig2 <- do.call("rbind", list(pp1M, pp1F, pp2M, pp2F, pp3M, pp3F))

data_fig2$x <-factor(data_fig2$x)
levels(data_fig2$x)[levels(data_fig2$x)=="0"] <- "He\ndecided"
levels(data_fig2$x)[levels(data_fig2$x)=="1"] <- "She\ndecided"
data_fig2$x    <- factor(data_fig2$x, 
                         levels  = c("He\ndecided", "She\ndecided"), 
                         ordered = FALSE)
data_fig2$group <-factor(data_fig2$group)
levels(data_fig2$group)[levels(data_fig2$group)=="1"] <- "High\nstakes"
levels(data_fig2$group)[levels(data_fig2$group)=="2"] <- "Low\nstakes"
data_fig2$group <- factor(data_fig2$group, 
                          levels  = c("High\nstakes", "Low\nstakes"), 
                          ordered = FALSE)
data_fig2$relinc <- factor(data_fig2$relinc,
                           levels = c("Man higher-earner", 
                                      "Woman higher-earner", 
                                      "Equal earners"))

fig2 <- data_fig2 %>%
  ggplot(aes(x = x, y = predicted, fill = gender)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  #  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
  #                stat="identity", position=position_dodge(.7), color="#ADB5BD") +
  geom_text(position = position_dodge(width = .7),
            vjust = -0.5,
            aes(label=sprintf("%1.0f%%", predicted*100))) +
  facet_grid(group ~ relinc,
             scales="free",
             space = "free",
             switch = "y") +
  scale_fill_grey() +
  theme_minimal(12) +
  theme(legend.position     = "right",
        panel.grid.major.x  = element_blank(),
        strip.text          = element_text(face = "bold"),
        strip.text.y.left   = element_text(angle = 0),
        axis.text.y         = element_blank(),  #remove y axis labels
        axis.ticks.y        = element_blank(),  #remove y axis ticks
        plot.subtitle       = element_text(face = "italic", color = "#707070"),
        plot.caption        = element_text(face = "italic", color = "#707070"),
        plot.title          = ggtext::element_markdown(),
        plot.title.position = "plot") +
  scale_y_continuous(labels=scales::percent, limits = c(0, .8)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = "Respondents'\ngender",
        title    = "Predicted percent of respondents who said the decision was somewhat or very fair",
        subtitle = "By decision type, vignette couples' relative income and decision-maker gender, and respondent gender",
        caption  = "Predicted percentages calculated from respondent-fixed effects linear probability models (see Appendix Table A3). 
        Independent models applied by vignette couple’s relative income and respondent gender.") 

fig2

ggsave(filename = file.path(figDir, "fig2.png"), fig2, 
       width=9, height=6, units="in", dpi=300, bg = "white")


################################################################################
# Supplementary Materials (quant)
################################################################################

# Appendix Table 1 -------------------------------------------------------------
## Weighted Descriptive Statistics of Respondent Characteristics

## Create weighted data 
data_A1 <- quantdata %>%
  select("weight", "gender", "relate", "parent", "raceeth", 
         "educ", "employ", "inc", "age")

data_A1_Svy <- svydesign(ids = ~1, weights = ~ weight, data = data_A1)

tabA1 <- data_A1_Svy %>%
  tbl_svysummary(
    include = !c(weight),
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
  body_add_par("Table A. Weighted Sample Characteristics") %>% 
  body_add_flextable(value = tabA1) %>% 
  print(target = file.path(outDir, "finalsay_tableA1.docx"))


# Appendix Table 2 -------------------------------------------------------------
## Bivariate Statistics of Perceptions of Fairness in Decision Making 
## by Type of Decision (N = 3,978)

data_A2 <- quantdata %>%
  # Create long data for item/activity vars
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

## Set as weighted survey data
data_A2_Svy <- data_A2 %>% 
  srvyr::as_survey_design(id = CaseID,
                          weights = weight)
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

## Average Marginal Effects of the models
m1M <- avg_slopes(plm1M, variables = c("per"), by = "decision")
m2M <- avg_slopes(plm2M, variables = c("per"), by = "decision")
m3M <- avg_slopes(plm3M, variables = c("per"), by = "decision")

m1F <- avg_slopes(plm1F, variables = c("per"), by = "decision")
m2F <- avg_slopes(plm2F, variables = c("per"), by = "decision")
m3F <- avg_slopes(plm3F, variables = c("per"), by = "decision")

## identify interaction variables
m1M$term <- paste(m1M$term, m1M$decision, sep= ".")
m2M$term <- paste(m2M$term, m2M$decision, sep= ".")
m3M$term <- paste(m3M$term, m3M$decision, sep= ".")

m1F$term <- paste(m1F$term, m1F$decision, sep= ".")
m2F$term <- paste(m2F$term, m2F$decision, sep= ".")
m3F$term <- paste(m3F$term, m3F$decision, sep= ".")

# test equality of coefficients between HIGH & LOW stakes
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
# https://journals.sagepub.com/doi/10.1177/0081175019852763

## Calculate Z scores
MHEM   <- (m1M[[1,4]] - m1M[[2,4]]) / sqrt(m1M[[1,5]]^2 + m1M[[2,5]]^2)
MHEF   <- (m1F[[1,4]] - m1F[[2,4]]) / sqrt(m1F[[1,5]]^2 + m1F[[2,5]]^2)

WHEM   <- (m2M[[1,4]] - m2M[[2,4]]) / sqrt(m2M[[1,5]]^2 + m2M[[2,5]]^2)
WHEF   <- (m2F[[1,4]] - m2F[[2,4]]) / sqrt(m2F[[1,5]]^2 + m2F[[2,5]]^2)

EEM    <- (m3M[[1,4]] - m3M[[2,4]]) / sqrt(m3M[[1,5]]^2 + m3M[[2,5]]^2)
EEF    <- (m3F[[1,4]] - m3F[[2,4]]) / sqrt(m3F[[1,5]]^2 + m3F[[2,5]]^2)

## Calculate p values
p_MHEM <- 2*pnorm(-abs(MHEM)) 
p_MHEF <- 2*pnorm(-abs(MHEF)) 

p_WHEM <- 2*pnorm(-abs(WHEM)) 
p_WHEF <- 2*pnorm(-abs(WHEF)) 

p_EEM  <- 2*pnorm(-abs(EEM)) 
p_EEF  <- 2*pnorm(-abs(EEF)) 

## Man Higher Earner
message("Men p = ",    round(p_MHEM,  digits = 3)) 
message("Women p = ",  round(p_MHEF,  digits = 3)) 

## Woman Higher Earner
message("Men p = ",    round(p_WHEM,  digits = 3)) 
message("Women p = ",  round(p_WHEF,  digits = 3)) 

## Equal Earners
message("Men p = ",    round(p_EEM,   digits = 3)) 
message("Women p = ",  round(p_EEF,   digits = 3)) 

## Create model list for 2 panels
panels <- list(
  "Man Higher Earner" = list(
    "Men" = m1M, "Women" = m1F),
  "Woman Higher Earner" = list(
    "Men" = m2M, "Women" = m2F),
  "Equal Earners" = list(
    "Men" = m3M, "Women" = m3F))

## Create pretty labels
coef_map <- c(
  "per.1"    = "High Stakes",
  "per.2"   = "Low Stakes")


## Produce Appendix Table 03

library(huxtable)
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
  insert_row(c("Man Higher Earner",   " ", " "), after = 1) %>%
  insert_row(c("Woman Higher Earner", " ", " "), after = 6) %>%
  insert_row(c("Equal Earners", " ", " "),       after = 11) %>%
  insert_row(c('Significant difference, high vs. low stakes?', 'Yes', 'No'), 
             after = 6) %>%
  insert_row(c('Significant difference, high vs. low stakes?', 'No', 'Yes'), 
             after = 12) %>%
  insert_row(c('Significant difference, high vs. low stakes?', 'No', 'No'), 
             after = 18) %>%
  set_top_border(row = c(8, 14), col = everywhere) %>%
  set_bottom_border(row = c(1,8,14), col = everywhere) %>%
  set_align(row = c(3, 5, 9, 11, 15, 17), 1, "center") %>%
  huxtable::as_flextable() %>%
  flextable::footnote(i = 11, j = 1, 
                      value = as_paragraph(c("Statistically significant gender difference (p < .05)."))) %>%
  add_footer_lines("Notes: N=7,956 person-decisions. 3,970 men and 3,986 women. Results calculated from respondent-fixed effects linear probability models. Independent models applied by relative income and respondent gender. Standard errors in parentheses.") %>%
  save_as_docx(path = file.path(outDir, "finalsay_tableA3.docx"))



### export to stata for FE models (Table C)
femodels <- quantdata %>%
  select(CaseID, dum1, dum2, fair1, fair2, per1, per2, 
         relinc, organize, mar, child, dur, item, gender, relate, parent, 
         raceeth, educ, employ, inc, age, activity, order, weight)

write_dta(femodels , path = file.path(outDir, "femodels.dta")) 

### -- sensitivity test -- include order of decider*gender of decider
quantdata <- quantdata %>%
  mutate(orderN= case_when (order == "Same"  ~ 0,
                            order == "Mixed" ~ 1))

logit1o <- glm(dum1 ~ per1 * orderN + relinc + organize + mar + child + dur + item + 
                 gender+relate+parent+raceeth+educ+employ+inc+age,
               quantdata, family="binomial")
logit2o <- glm(dum2 ~ per2 * orderN + relinc + organize + mar + child + dur + activity +
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
logit5a <- glm(dum1 ~ per1 * child + relinc + organize + mar + dur + item +
                 gender+relate+parent+raceeth+educ+employ+inc+age,
               quantdata, family="binomial")

logit5b <- glm(dum2 ~ per2 * child + relinc + organize + mar + dur + order + activity +
                 gender+relate+parent+raceeth+educ+employ+inc+age,
               quantdata, family="binomial")




# Appendix Figure A. -----------------------------------------------------------
## Fairness Evaluation by Item/Activity Presented to Respondent

data_figA <- quantdata %>%
  select("CaseID", "item", "activity", "fair1", "fair2") %>%
  # Create long data for item/activity fairness vars
  pivot_longer(
    cols = c(fair1, fair2),
    names_to = "drop",
    values_to = "fairness") %>%
  # Create long data for item/activity decision vars
  pivot_longer(
    cols = c(item, activity),
    names_to = "type",
    values_to = "category") %>%
  # remove duplicates
  filter((drop == "fair1" & type == "item") |
           (drop == "fair2" & type == "activity")) %>%
  select(-c("drop")) %>%
  # create percentage data
  group_by(type, category) %>%
  count(category = factor(category), fairness = factor(fairness)) %>% 
  mutate(pct = prop.table(n)) %>%
  ungroup()

data_figA$type[data_figA$type == "item"] <-"purchase"
data_figA$type <- factor(data_figA$type, 
                         levels  = c("purchase", "activity"), 
                         ordered = FALSE)


figA <- data_figA %>%
  ggplot(aes(x = category, y = pct, fill = fairness)) +
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
  scale_fill_manual(values = c("#18BC9C", "#3498DB", "#F39C12", "#E74C3C")) +
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
        title    = "Fairness evaluation by purchase & activity",
        subtitle = "How fair do you think the decision was?")

figA   

ggsave(filename = file.path(figDir, "figA.png"), figA, 
       width=6, height=4, units="in", dpi=300, bg = 'white')



# # Appendix Figure B. ---------------------------------------------------------
# 
# ## Create predicted probabilities datesets
# pp1   <- ggeffect(logit1, terms = "per1")
# pp2   <- ggeffect(logit2, terms = "per2")
# 
# # https://github.com/easystats/insight/issues/451 <- delta??
# 
# pp1$type <- "item"
# pp2$type <- "activity"
# 
# data_figB = merge(pp1, pp2, all = TRUE)
# head(data_figB)
# 
# data_figB$x[data_figB$x==1] <- "She decided"
# data_figB$x[data_figB$x==0]  <- "He decided"
# data_figB$type <- factor(data_figB$type, levels = c("item", "activity"), ordered = FALSE)
# 
# figB <- data_figB %>%
#   ggplot(aes(x = x, y = predicted, fill = x)) +
#   geom_col(width = 0.6, position = position_dodge(0.7)) +
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
#                 stat="identity", position=position_dodge(.7), color="#ADB5BD") +
#   geom_text(position = position_dodge(width = .7),
#             vjust = -0.5,
#             aes(label=sprintf("%1.0f%%", predicted*100))) +
#   facet_wrap(~ type) +
#   scale_fill_manual(values = c("#18BC9C", "#F39C12")) +
#   theme_minimal() +
#   theme(legend.position     = "top",
#         panel.grid.major.x  = element_blank(),
#         plot.title          = element_text(face = "bold"),
#         plot.title.position = "plot",
#         plot.subtitle       = element_text(face = "italic", color = "#707070"),
#         plot.caption        = element_text(face = "italic", color = "#707070")) +
#   scale_y_continuous(labels=scales::percent, limits = c(0, .8)) +
#   labs( x        = " ", 
#         y        = " ", 
#         fill     = " ",
#         title    = "Gender differences in perceptions of fairness \nin decision-making for items and activities.",
#         subtitle = "% of respondents who said the decision was somewhat or very fair...",
#         caption  = "Predicted percentages adjust for other vignette manipulations and respondent demographic characteristics.") 
# 
# figB
# 
# ggsave(filename = file.path(figDir, "figB.png"), figB, width=5, height=5, units="in", dpi=300)


