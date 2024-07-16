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

# Appendix Table A3 ------------------------------------------------------------

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

## Table notes
nA3 <- "Notes: N=7,956 person-decisions. 3,970 men and 3,986 women. 
Results calculated from respondent-fixed effects linear probability models. 
Independent models applied by relative income and respondent gender. 
Standard errors in parentheses."


## Produce Appendix Table 03
rows <- tribble(~term,          ~Men,  ~Women,
                'Significant difference, high vs. low stakes?', 'Yes', 'No')

attr(rows, 'position') <- c(4)

# https://michaeltopper1.github.io/panelsummary/articles/panelsummary.html
modelsummary(
  panels,
  shape = "rbind",
  coef_map = coef_map,
  gof_map = NA,
  #  exponentiate = TRUE,
  stars = c("*" =.05, "**" = .01, "***" = .001),
  fmt = fmt_decimal(digits = 3, pdigits = 3),
  add_rows = rows,
  notes = nA3,
  output = "gt") 

tab %>%
  tab_footnote(footnote = md("Statistically significant gender difference (p < .05)."),
               locations = cells_body(rows = 7, columns = 1))


# Figure 02. -------------------------------------------------------------------

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
                           levels = c("Man higher-earner", "Woman higher-earner", "Equal earners"))

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

