#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_02_quant analyses.R
# Joanna R. Pepin & William J. Scarborough
#------------------------------------------------------------------------------------

# This file analyzes the decision making variables.
safe <- data
data <- safe

#####################################################################################
# Paper Tables
#####################################################################################

# Table 1 ---------------------------------------------------------------------------
## Table 1. Hypotheses about Perceptions of Decision-making (No table generated.)


# Table 2 ---------------------------------------------------------------------------
## Weighted Descriptive Statistics of Respondent Characteristics

## Create weighted data 
tab2data <- data %>%
  select("weight", "gender", "relate", "parent", "raceeth", 
         "educ", "employ", "incdum", "age")

tab2Svy <- svydesign(ids = ~1, weights = ~ weight, data = tab2data)

tab2Svy %>%
  tbl_svysummary(
    label = list(gender  ~ "Women",
                 relate  ~ "Relationship Status",
                 parent  ~ "Parent",
                 raceeth ~ "Respondent race/ethnicity",
                 educ    ~ "Educational attainment",
                 employ  ~ "Employment status",
                 incdum  ~ "Household income > $50,000",
                 age     ~ "Respondent age"),
    type  = list(gender  ~ "dichotomous",
                 parent  ~ "dichotomous",
                 incdum  ~ "dichotomous"),
    value = list(gender  = "Female",
                 parent  = "Parent",
                 incdum  = "> than $50,000"))  %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Sample descriptives are weighted.*"))



tab2 <- tab2Svy %>%
  tbl_svysummary(
    label = list(gender  ~ "Women",
                 relate  ~ "Relationship Status",
                 parent  ~ "Parent",
                 raceeth ~ "Respondent race/ethnicity",
                 educ    ~ "Educational attainment",
                 employ  ~ "Employment status",
                 incdum  ~ "Household income > $50,000",
                 age     ~ "Respondent age"),
    type  = list(gender  ~ "dichotomous",
                 parent  ~ "dichotomous",
                 incdum  ~ "dichotomous"),
    value = list(gender  = "Female",
                 parent  = "Parent",
                 incdum  = "> than $50,000"))  %>%
  as_flex_table() 

## https://mran.microsoft.com/snapshot/2017-12-11/web/packages/officer/vignettes/word.html
read_docx() %>% 
  body_add_par("Table 02. Sample Characteristics") %>% 
  body_add_flextable(value = tab2) %>% 
  print(target = file.path(outDir, "finalsay_table02.docx"))


# Table 3 ---------------------------------------------------------------------------

## Weighted Bi-variate Statistics of Perceptions of Fairness in Decision Making by Type of Decision

data_long <- data %>%
  pivot_longer(
    cols = c(idum, adum),
    names_to = "type",
    values_to = "fairness"
  )

data_long <- data_long %>%
  mutate(
    person = case_when(
      (type  == "idum"   &  iperson == "Michelle") |
      (type  == "adum"   &  aperson == "Michelle") ~ "Woman",
      (type  == "idum"   &  iperson == "Anthony")  |
      (type  == "adum"   &  aperson == "Anthony")  ~ "Man",
    ))

data_long <- data_long %>%
  pivot_longer(
    cols = c(relinc, person, organize, mar, child, dur),
    names_to = "variable",
    values_to = "level"
  )

tab3data <- data_long %>%
  select("CaseID", "weight", "type", "variable", "level", "fairness")

tab3Svy <- tab3data %>% 
  srvyr::as_survey_design(id = CaseID,
                          weights = weight)

tab3sum <- tab3Svy %>%
  group_by(type, variable, level) %>%
  summarize(mean = survey_mean(fairness, na.rm = TRUE),
            sd = sd(fairness, na.rm = TRUE)) %>%
  subset(select = -c(variable, mean_se)) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  select(level, mean_idum, sd_idum, mean_adum, sd_adum)

tab3sum %>%
  kbl(digits=2, col.names = c("Vignette Variables", "$M$", "$SD$", "$M$", "$SD$")) %>%
  kable_styling() %>%
  pack_rows("Parent", 1, 2) %>%
  pack_rows("Duration", 3, 4) %>%
  pack_rows("Marital Status", 5, 6) %>%
  add_header_above(c(" " = 1, "Item" = 2, "Activity" = 2))
  
  
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Row_indentation
# https://haozhu233.github.io/kableExtra/awesome_table_in_html.html#Grouped_Columns__Rows


# Figure 2. --------------------------------------------------------------------------

## Create the multinomial models with ordinal year
data$year <- as.factor(data$year) 

## Specify reference levels
data$racesex  <- relevel(data$racesex,  ref = "White men")
data$momed    <- relevel(data$momed,    ref = "COMPLETED HIGH SCHOOL")
data$momemp   <- relevel(data$momemp,   ref = "YES, ALL OR NEARLY ALL OF THE TIME")
data$famstru  <- relevel(data$famstru,  ref = "Both Mother & Father")
data$religion <- relevel(data$religion, ref = "RARELY")
data$region   <- relevel(data$region,   ref = "Northeast")

## Run the models
library(nnet)
library(ggeffects)
m1 <- multinom(ifair ~ relinc + iperson + gender, data, weights = weight)
m2 <- multinom(afair ~ relinc + aperson + gender, data, weights = weight)

logit1 <- glm(idum ~ iperson + relinc + marpar + dur + 
              gender + age + educ + employ + relate + incdum + raceeth + relfreq + religion + region + parent, 
              data, weights = weight, family="binomial")

logit2 <- glm(adum ~ aperson + relinc + marpar + dur + 
              gender + age + educ + employ + relate + incdum + raceeth + relfreq + religion + region + parent, 
              data, weights = weight, family="binomial")

## Create predicted probabilities datesets
pm1   <- ggeffect(logit1, terms = c("iperson", "relinc"))
pm2   <- ggeffect(logit2, terms = c("aperson", "relinc"))

pm1$type <- "item"
pm2$type <- "activity"

mdata = merge(pm1, pm2, all = TRUE)
head(mdata)

levels(mdata$x)[levels(mdata$x)=="Michelle"] <- "She decided"
levels(mdata$x)[levels(mdata$x)=="Anthony"]  <- "He decided"

fig2 <- mdata %>%
  filter(type == "item") %>%
  ggplot(aes(x = x, y = predicted, fill = group)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                stat="identity", position=position_dodge(.7), color="#ADB5BD") +
  geom_text(position = position_dodge(width = .7),
            vjust = -0.5, 
            size=5,
            aes(label=sprintf("%1.0f%%", predicted*100))) +
  scale_fill_jrp() +
  theme_minimal() +
  theme(legend.position    = "bottom",
        panel.grid.major.x = element_blank(),
        plot.title       = element_text(size = 26, face = "bold"),
        plot.subtitle    = element_text(size = 22, face = "italic", color = "#707070"),
        axis.text.x      = element_text(size = 20),
        axis.text.y      = element_text(size = 20),
        legend.text      = element_text(size = 20),
        plot.caption     = element_text(size = 16, face = "italic", color = "#707070")) +
  scale_y_continuous(labels=percent, limits = c(0, .8)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Women's 'final say' on life items were more likely to be\nviewed as fair.",
        subtitle = "% of respondents who said the decision was somewhat or very fair...",
        caption  = "Predicted percentages adjust for other vignette manipulations and respondent demographic characteristics.") 

fig2

ggsave(filename = file.path(figDir, "fig2.png"), fig2, width=11, height=6, units="in", dpi=300)


fig3 <- mdata %>%
  filter(type == "activity") %>%
  ggplot(aes(x = x, y = predicted, fill = group)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                stat="identity", position=position_dodge(.7), color="#ADB5BD") +
  geom_text(position = position_dodge(width = .7),
            vjust = -0.5, 
            size=5,
            aes(label=sprintf("%1.0f%%", predicted*100))) +
  scale_fill_jrp() +
  theme_minimal() +
  theme(legend.position    = "bottom",
        panel.grid.major.x = element_blank(),
        plot.title       = element_text(size = 26, face = "bold"),
        plot.subtitle    = element_text(size = 22, face = "italic", color = "#707070"),
        axis.text.x      = element_text(size = 20),
        axis.text.y      = element_text(size = 20),
        legend.text      = element_text(size = 20),
        plot.caption     = element_text(size = 16, face = "italic", color = "#707070")) +
  scale_y_continuous(labels=percent, limits = c(0, .8)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Men's 'final say' on leisure activities were more likely to be\nviewed as fair.",
        subtitle = "% of respondents who said the decision was somewhat or very fair...",
        caption  = "Predicted percentages adjust for other vignette manipulations and respondent demographic characteristics.") 

fig3

ggsave(filename = file.path(figDir, "fig3.png"), fig3, width=11, height=6, units="in", dpi=300)

