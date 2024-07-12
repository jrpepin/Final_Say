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



### export to stata for FE models (Table C)
femodels <- quantdata %>%
  select(CaseID, dum1, dum2, fair1, fair2, per1, per2, 
         relinc, organize, mar, child, dur, item, gender, relate, parent, 
         raceeth, educ, employ, inc, age, activity, order, weight)

write_dta(femodels , path = file.path(outDir, "femodels.dta")) 

## Wide to long
femodels <- femodels %>% 
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
filter(decision == drop2) %>%                                                   # back to 2 rows (decisions) per person
select(!c(drop1, drop2))

## Run the models

logit1 <- glm(dum1 ~ per1 + relinc + organize + mar + child + dur + item +
                gender+relate+parent+raceeth+educ+employ+inc+age,
              quantdata, family="binomial")

logit2 <- glm(dum2 ~ per2 + relinc + organize + mar + child + dur + order + activity +
                gender+relate+parent+raceeth+educ+employ+inc+age,
              quantdata, family="binomial")

### interactions
logit3 <- glm(dum1 ~ per1 * relinc + organize + mar + child + dur + item +
                gender+relate+parent+raceeth+educ+employ+inc+age,
              quantdata, family="binomial")

logit4 <- glm(dum2 ~ per2 * relinc + organize + mar + child + dur + order + activity +
                gender+relate+parent+raceeth+educ+employ+inc+age,
              quantdata, family="binomial")

## Average marginal effects
### https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html
### https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html

## Panel A ---------------------------------------------------------------------
AME_log1  <- summary(margins(logit1,  variables = c("per1", "relinc", "gender")))
AME_log2  <- summary(margins(logit2,  variables = c("per2", "relinc", "gender")))

summary(margins(logit1, 
                variables = c("gender", "relate", "parent", 
                              "raceeth", "educ", "inc", "age")))
summary(margins(logit2, 
                variables = c("gender", "relate", "parent", 
                              "raceeth", "educ", "inc", "age", "order")))

# test equality of coefficients between Item & Activity
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
z_RgenderA <- (AME_log1[[1,2]] - AME_log2[[1,2]]) / sqrt(AME_log1[[1,3]]^2 + AME_log2[[1,3]]^2)
z_deciderA <- (AME_log1[[2,2]] - AME_log2[[2,2]]) / sqrt(AME_log1[[2,3]]^2 + AME_log2[[2,3]]^2)
z_equalA   <- (AME_log1[[3,2]] - AME_log2[[3,2]]) / sqrt(AME_log1[[3,3]]^2 + AME_log2[[3,3]]^2)
z_fmoreA   <- (AME_log1[[4,2]] - AME_log2[[4,2]]) / sqrt(AME_log1[[4,3]]^2 + AME_log2[[4,3]]^2)

p_RgenderA <- 2*pnorm(-abs(z_RgenderA)) 
p_deciderA <- 2*pnorm(-abs(z_deciderA)) 
p_equalA   <- 2*pnorm(-abs(z_equalA)) 
p_fmoreA   <- 2*pnorm(-abs(z_fmoreA)) 

AME_log1
AME_log2

print(paste("Equal test of equality: p =", round(p_equalA, digits = 3)))
print(paste("Woman Higher-Earner test of equality: p =", round(p_fmoreA, digits = 3)))
print(paste("Decider test of equality: p =", round(p_deciderA, digits = 3)))
print(paste("Respondents' Gender test of equality: p =", round(p_RgenderA, digits = 3)))

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

## Panel B ---------------------------------------------------------------------

### Item ***********************************************************************
AME3 <- summary(margins(logit3, 
                        variables = "relinc",
                        at = list(per1 = 0:1)))
AME3

## test difference between coefficients
### create Z scores
z_equalI <- (AME3[[1,3]] - AME3[[2,3]]) / sqrt(AME3[[1,4]]^2 + AME3[[2,4]]^2)
z_womanI <- (AME3[[3,3]] - AME3[[4,3]]) / sqrt(AME3[[3,4]]^2 + AME3[[4,4]]^2)
z_equalI
z_womanI
### create p values
p_equalI  <- 2*pnorm(-abs(z_equalI))
p_womanI  <- 2*pnorm(-abs(z_womanI))

### report p values
print(paste("(ITEM) test of equality-- Equal Earners * gender: p =", 
            round(p_equalI, digits = 3)))
print(paste("(ITEM) test of equality-- Woman Higher-Earner * gender: p =", 
            round(p_womanI, digits = 3)))

### Activity *******************************************************************
AME4 <- summary(margins(logit4, 
                        variables = "relinc",
                        at = list(per2 = 0:1)))
AME4

## test difference between coefficients
### create Z scores
z_equalA  <- (AME4[[1,3]] - AME4[[2,3]]) / sqrt(AME4[[1,4]]^2 + AME4[[2,4]]^2)
z_womanA  <- (AME4[[3,3]] - AME4[[4,3]]) / sqrt(AME4[[3,4]]^2 + AME4[[4,4]]^2)
z_equalA
z_womanA

### create p values
p_equalA  <- 2*pnorm(-abs(z_equalA))
p_womanA  <- 2*pnorm(-abs(z_womanA))

### report p values
print(paste("(ACTIVITY) test of equality-- Equal Earners * gender: p =", 
            round(p_equalA, digits = 3)))
print(paste("(ACTIVITY) test of equality-- Woman Higher-Earner * gender: p =", 
            round(p_womanA, digits = 3)))


### test equality of coefficients between Item & Activity **********************

## Z scores
z_equal_MAN_AB  <- (AME3[[1,3]] - AME4[[1,3]]) / sqrt(AME3[[1,4]]^2 + AME4[[1,4]]^2)
z_equal_FEM_AB  <- (AME3[[2,3]] - AME4[[2,3]]) / sqrt(AME3[[2,4]]^2 + AME4[[2,4]]^2)

z_lower_MAN_AB  <- (AME3[[3,3]] - AME4[[3,3]]) / sqrt(AME3[[3,4]]^2 + AME4[[3,4]]^2)
z_lower_FEM_AB  <- (AME3[[4,3]] - AME4[[4,3]]) / sqrt(AME3[[4,4]]^2 + AME4[[4,4]]^2)

z_equal_MAN_AB
z_equal_FEM_AB
z_lower_MAN_AB
z_lower_FEM_AB

## p values
p_lower_MAN_AB  <- 2*pnorm(-abs(z_lower_MAN_AB)) 
p_equal_MAN_AB  <- 2*pnorm(-abs(z_equal_MAN_AB)) 

p_lower_FEM_AB  <- 2*pnorm(-abs(z_lower_FEM_AB)) 
p_equal_FEM_AB  <- 2*pnorm(-abs(z_equal_FEM_AB)) 

print(paste("(ITEM VS ACT) Man * Equal Earners test of equality: p =", 
            round(p_equal_MAN_AB, digits = 3)))
print(paste("(ITEM VS ACT) Female * Equal Earners test of equality: p =", 
            round(p_equal_FEM_AB, digits = 3)))

print(paste("(ITEM VS ACT) Man * Lower-Earner test of equality: p =", 
            round(p_lower_MAN_AB, digits = 3)))
print(paste("(ITEM VS ACT) Female * Lower-Earner test of equality: p =", 
            round(p_lower_FEM_AB, digits = 3)))


## Panel C. Other interactions per reviewer B ----------------------------------

### interactions
logit5a <- glm(dum1 ~ per1 * child + relinc + organize + mar + dur + item +
                gender+relate+parent+raceeth+educ+employ+inc+age,
              quantdata, family="binomial")

logit5b <- glm(dum2 ~ per2 * child + relinc + organize + mar + dur + order + activity +
                gender+relate+parent+raceeth+educ+employ+inc+age,
              quantdata, family="binomial")




# Figure 2. --------------------------------------------------------------------
# (*Figure 1 is the coherence plot generated in FS_03_qual analyses)
### https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html
### https://github.com/easystats/insight/issues/451 <- delta??
### https://data.library.virginia.edu/a-beginners-guide-to-marginal-effects/

## Create predicted probabilities datesets
pp3   <- ggeffect(logit3, terms = c("per1", "relinc", "gender"))
pp4   <- ggeffect(logit4, terms = c("per2", "relinc", "gender"))

pp3$type <- "High\nstakes"
pp4$type <- "Low\nstakes"

data_fig2 = merge(pp3, pp4, all = TRUE)
head(data_fig2)

data_fig2$x <-factor(data_fig2$x)
levels(data_fig2$x)[levels(data_fig2$x)=="1"] <- "She\ndecided"
levels(data_fig2$x)[levels(data_fig2$x)=="0"] <- "He\ndecided"

data_fig2$x    <- factor(data_fig2$x, 
                         levels  = c("She\ndecided", "He\ndecided"), 
                         ordered = FALSE)
data_fig2$type <- factor(data_fig2$type, 
                         levels  = c("High\nstakes", "Low\nstakes"), 
                         ordered = FALSE)

levels(data_fig2$facet)[levels(data_fig2$facet)=="Male"]   <- "Men"
levels(data_fig2$facet)[levels(data_fig2$facet)=="Female"] <- "Women"

qualitative_hcl(4, palette = "Dark 3") # show color hex codes

fig2 <- data_fig2 %>%
  ggplot(aes(x = x, y = predicted, fill = facet)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                stat="identity", position=position_dodge(.7), color="#ADB5BD") +
  geom_text(position = position_dodge(width = .7),
            vjust = -0.5,
            aes(label=sprintf("%1.0f%%", predicted*100))) +
  facet_grid(type ~ group,
             scales="free",
             space = "free",
             switch = "y") +
  scale_fill_grey() +
  #  scale_fill_discrete_qualitative(palette = "Dark 3") +
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
        title    = "Perceptions of decision-making by decision type and relative earnings",
        subtitle = "Predicted % of respondents who said the decision was somewhat or very fair",
        caption  = "Predicted percentages calculated from Table 2 logistic regression models interacting relative income and gender of decider.\nPresented by respondent gender with 95% confidence intervals.") 

fig2

ggsave(filename = file.path(figDir, "fig2.png"), fig2, 
       width=9, height=6, units="in", dpi=300, bg = "white")

################################################################################
# Appendix Tables and Figures (quant)
################################################################################

# Table A ----------------------------------------------------------------------
## Weighted Descriptive Statistics of Respondent Characteristics

## Create weighted data 
tabAdata <- quantdata %>%
  select("weight", "gender", "relate", "parent", "raceeth", 
         "educ", "employ", "inc", "age")

tabASvy <- svydesign(ids = ~1, weights = ~ weight, data = tabAdata)

tabA <- tabASvy %>%
  tbl_svysummary(
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

tabA # show table

## https://mran.microsoft.com/snapshot/2017-12-11/web/packages/officer/vignettes/word.html
read_docx() %>% 
  body_add_par("Table A. Weighted Sample Characteristics") %>% 
  body_add_flextable(value = tabA) %>% 
  print(target = file.path(outDir, "finalsay_tableA.docx"))


# Table B ----------------------------------------------------------------------
## Weighted Bi-variate Statistics of Perceptions of Fairness in Decision Making by Type of Decision

tabBdata <- quantdata %>%
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
    cols = c(relinc, person, organize, mar, child, dur),
    names_to = "variable",
    values_to = "level") %>%
  # Keep vignette variables
  select("CaseID", "weight", "type", "variable", "level", "fairness")

## Re order variable manipulations
tabBdata$variable <- factor(tabBdata$variable, 
                            levels  = c("relinc", "person", "organize",
                                        "mar", "child", "dur"), 
                            ordered = FALSE)
## Set as weighted survey data
tabBSvy <- tabBdata %>% 
  srvyr::as_survey_design(id = CaseID,
                          weights = weight)
## Create summary data
tabBsum <- tabBSvy %>%
  group_by(type, variable, level) %>%
  summarize(mean = survey_mean(fairness, na.rm = TRUE),
            sd = sd(fairness, na.rm = TRUE)) %>%
  subset(select = -c(variable, mean_se)) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  select(level, mean_dum1, sd_dum1, mean_dum2, sd_dum2)

tabBsum <- tabBsum %>%
  mutate(
    cat = case_when(
      tabBsum$level == "Man higher-earner"     |
        tabBsum$level == "Woman higher-earner" |
        tabBsum$level == "Equal earners"       ~ "Relative Earnings",
      tabBsum$level == "Woman"                 |
        tabBsum$level == "Man"                 ~ "Gender of Decider",
      tabBsum$level == "Shared"                |
        tabBsum$level == "Separate"            |
        tabBsum$level == "Both"                ~ "Financial Allocation Strategy",
      tabBsum$level == "live together"         |
        tabBsum$level == "are married"         ~ "Marital Status",
      tabBsum$level == "no children"           |
        tabBsum$level == "one child together"  ~ "Parental Status",
      tabBsum$level == "3 years"               |
        tabBsum$level == "7 years"             ~ "Relationship Duration"))

## Create Flextable
tabBsum <- as_grouped_data(x = tabBsum, groups = c("cat"), columns = NULL) # Group by vignette condition

tabB <- tabBsum %>%
  flextable::as_flextable(hide_grouplabel = TRUE) %>%
  add_header_row(values = c("", "Purchase", "Activity"), colwidths = c(1, 2, 2)) %>%
  flextable::align(i = 1, align = "center", part = "header") %>%
  colformat_double(digits = 2) %>%
  set_header_labels(level = "Vignette Variables",
                    mean_dum1 = "M",
                    sd_dum1   = "SD", 
                    mean_dum2 = "M",
                    sd_dum2   = "SD" ) %>% 
  autofit() %>%
  padding(i=c(2:4,6:7,9:11,13:14,16:17,19:20), j=1, padding.left=25) %>%
  add_footer(level = "Note: Descriptive statistics include survey weights.\n
             Range is from 0 (not fair) to 1 (fair).") %>%
  merge_at(j = 1:5, part = "footer")

num <-nrow(quantdata) #number of observations

tabB # show table

read_docx() %>% 
  body_add_par(paste("Table 02. Weighted Bivariate Statistics of Perceptions of Fairness in Decision Making 
               by Type of Decision (N = ", num,")", sep="")) %>% 
  body_add_flextable(value = tabB) %>% 
  print(target = file.path(outDir, "finalsay_tableB.docx")) # save table


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


