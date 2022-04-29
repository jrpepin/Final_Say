#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_02_quant analyses.R
# Joanna R. Pepin & William J. Scarborough
#------------------------------------------------------------------------------------

# This file analyzes the decision making variables.

#####################################################################################
# Paper Tables and Figures (quant)
#####################################################################################

# Table 1 ---------------------------------------------------------------------------
## Table 1. Hypotheses about Perceptions of Decision-making (No table generated.)

# Table 2 ---------------------------------------------------------------------------
## Weighted Bi-variate Statistics of Perceptions of Fairness in Decision Making by Type of Decision

tab2data <- quantdata %>%
  # Create long data for item/activity vars
  pivot_longer(
    cols = c(idum, adum),
    names_to = "type",
    values_to = "fairness") %>%
  # Add a gender of decider variable
  mutate(
    person = case_when(
      (type  == "idum"   &  iperson == "Michelle") |
      (type  == "adum"   &  aperson == "Michelle") ~ "Woman",
      (type  == "idum"   &  iperson == "Anthony")  |
      (type  == "adum"   &  aperson == "Anthony")  ~ "Man")) %>%
  # Create long data for vignette manipulation
  pivot_longer(
    cols = c(relinc, person, organize, mar, child, dur),
    names_to = "variable",
    values_to = "level") %>%
  # Keep vignette variables
  select("CaseID", "weight", "type", "variable", "level", "fairness")

## Re order variable manipulations
tab2data$variable <- factor(tab2data$variable, 
                            levels = c("relinc", "person", "organize",
                                       "mar", "child", "dur"), ordered = FALSE)
## Set as weighted survey data
tab2Svy <- tab2data %>% 
  srvyr::as_survey_design(id = CaseID,
                          weights = weight)
## Create summary data
tab2sum <- tab2Svy %>%
  group_by(type, variable, level) %>%
  summarize(mean = survey_mean(fairness, na.rm = TRUE),
            sd = sd(fairness, na.rm = TRUE)) %>%
  subset(select = -c(variable, mean_se)) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  select(level, mean_idum, sd_idum, mean_adum, sd_adum)

tab2sum <- tab2sum %>%
  mutate(
    cat = case_when(
      tab2sum$level == "Man higher-earner"     |
        tab2sum$level == "Woman higher-earner" |
        tab2sum$level == "Equal earners"       ~ "Relative Earnings",
      tab2sum$level == "Woman"                 |
        tab2sum$level == "Man"                 ~ "Gender of Decider",
      tab2sum$level == "Shared"                |
        tab2sum$level == "Separate"            |
        tab2sum$level == "Both"                ~ "Financial Allocation Strategy",
      tab2sum$level == "live together"         |
        tab2sum$level == "are married"         ~ "Marital Status",
      tab2sum$level == "no children"           |
        tab2sum$level == "one child together"  ~ "Parental Status",
      tab2sum$level == "3 years"               |
        tab2sum$level == "7 years"             ~ "Relationship Duration"))

## Create Flextable
tab2sum <- as_grouped_data(x = tab2sum, groups = c("cat"), columns = NULL) # Group by vignette condition

tab2 <- tab2sum %>%
  flextable::as_flextable(hide_grouplabel = TRUE) %>%
  add_header_row(values = c("", "Item", "Activity"), colwidths = c(1, 2, 2)) %>%
  flextable::align(i = 1, align = "center", part = "header") %>%
  colformat_double(digits = 2) %>%
  set_header_labels(level = "Vignette Variables",
                    mean_idum = "M",
                    sd_idum   = "SD", 
                    mean_adum = "M",
                    sd_adum   = "SD" ) %>% 
  autofit() %>%
  padding(i=c(2:4,6:7,9:11,13:14,16:17,19:20), j=1, padding.left=25) %>%
  add_footer(level = "Note: Descriptive statistics include survey weights.\n
             Range is from 0 (not fair) to 1 (fair).") %>%
  merge_at(j = 1:5, part = "footer")

num <-nrow(quantdata) #number of observations

tab2 # show table

read_docx() %>% 
  body_add_par(paste("Table 02. Weighted Bivariate Statistics of Perceptions of Fairness in Decision Making 
               by Type of Decision (N = ", num,")", sep="")) %>% 
  body_add_flextable(value = tab2) %>% 
  print(target = file.path(outDir, "finalsay_table02.docx")) # save table


# Table 3. --------------------------------------------------------------------------

## Specify reference levels
quantdata$relate   <- relevel(quantdata$relate,   ref = "Never married")
quantdata$raceeth  <- relevel(quantdata$raceeth,  ref = "White")
quantdata$educ     <- relevel(quantdata$educ,     ref = "High school")
quantdata$employ   <- relevel(quantdata$employ,   ref = "Employed")
quantdata$incdum   <- relevel(quantdata$incdum,   ref = "< than $50,000")
quantdata$earner   <- relevel(quantdata$earner,   ref = "Higher earner")
quantdata$perI     <- as.numeric(quantdata$iperson == "Michelle") # create dummy variables
quantdata$perA     <- as.numeric(quantdata$aperson == "Michelle") # create dummy variables


## Run the models

logit1 <- glm(idum ~ iperson + relinc + organize + mar + child + dur + 
              gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, weights = weight, family="binomial")

logit2 <- glm(adum ~ aperson + relinc + organize + mar + child + dur + 
              gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, weights = weight, family="binomial")

logit3 <- glm(idum ~ perI * earner + organize + mar + child + dur + 
              gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, weights = weight, family="binomial")

logit4 <- glm(adum ~ perA * earner + organize + mar + child + dur + 
              gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, weights = weight, family="binomial")


## Average marginal effects
### https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html
### https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html

## Panel A ------------------------------------------------------------------------------------
AME_log1 <- summary(margins(logit1, variables = c("iperson", "relinc")))
AME_log2 <- summary(margins(logit2, variables = c("aperson", "relinc")))

# test equality of coefficients between Item & Activity
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
z_genderA <- (AME_log1[[1,2]] - AME_log2[[1,2]]) / sqrt(AME_log1[[1,3]]^2 + AME_log2[[1,3]]^2)
# z_bothA   <- (AME_log1[[2,2]] - AME_log2[[2,2]]) / sqrt(AME_log1[[2,3]]^2 + AME_log2[[2,3]]^2)
# z_sepA    <- (AME_log1[[3,2]] - AME_log2[[3,2]]) / sqrt(AME_log1[[3,3]]^2 + AME_log2[[3,3]]^2)
z_equalA  <- (AME_log1[[4,2]] - AME_log2[[4,2]]) / sqrt(AME_log1[[4,3]]^2 + AME_log2[[4,3]]^2)
z_fmoreA  <- (AME_log1[[5,2]] - AME_log2[[5,2]]) / sqrt(AME_log1[[5,3]]^2 + AME_log2[[5,3]]^2)

p_genderA <- 2*pnorm(-abs(z_genderA)) 
# p_bothA   <- 2*pnorm(-abs(z_bothA)) 
# p_sepA    <- 2*pnorm(-abs(z_sepA)) 
p_equalA  <- 2*pnorm(-abs(z_equalA)) 
p_fmoreA  <- 2*pnorm(-abs(z_fmoreA)) 

AME_log1
AME_log2

print(paste("Gender test of equality: p =", round(p_genderA, digits = 3)))
print(paste("Woman Higher-Earner test of equality: p =", round(p_fmoreA, digits = 3)))
print(paste("Equal test of equality: p =", round(p_equalA, digits = 3)))
# print(paste("Separate test of equality: p =", round(p_sepA, digits = 3)))
# print(paste("Both test of equality: p =", round(p_bothA, digits = 3)))

##  alternative test -- test coefficients instead of marginal effects
    # coef_log1 <- coef(summary(logit1))[[2,1]] ## gender coefficients
    # sd_log1   <- coef(summary(logit1))[[2,2]]
    # coef_log2 <- coef(summary(logit2))[[2,1]] ## gender standard errors
    # sd_log2   <- coef(summary(logit2))[[2,2]]
    # 
    # z_gend_coef <- (coef_log1 - coef_log2) / (sd_log1^2 + sd_log2^2)^(1/2)
    # p_gend_coef <- 2*pnorm(-abs(z_gend_coef)) 
    # p_gend_coef


## Panel B ------------------------------------------------------------------------------------
### Activity **********************************************************************************
AME3 <- summary(margins(logit3, 
                variables = "perI",
                 at = list(perI = 0:1, 
                           earner = c("Higher earner", "Lower earner", "Equal earners"))))

## test difference between coefficients
### create Z scores
z_higherI <- (AME3[[1,4]] - AME3[[4,4]]) / sqrt(AME3[[1,5]]^2 + AME3[[4,5]]^2)
z_lowerI  <- (AME3[[2,4]] - AME3[[5,4]]) / sqrt(AME3[[2,5]]^2 + AME3[[5,5]]^2)
z_equalI  <- (AME3[[3,4]] - AME3[[6,4]]) / sqrt(AME3[[3,5]]^2 + AME3[[6,5]]^2)

### create p values
p_higherI <- 2*pnorm(-abs(z_higherI))
p_lowerI  <- 2*pnorm(-abs(z_lowerI))
p_equalI  <- 2*pnorm(-abs(z_equalI))

### report p values
print(paste("(ITEM) test of equality-- Higher-Earner * gender: p =", round(p_higherI, digits = 3)))
print(paste("(ITEM) test of equality-- Lower-Earner * gender: p =", round(p_lowerI, digits = 3)))
print(paste("(ITEM) test of equality-- Equal Earners * gender: p =", round(p_equalI, digits = 3)))


### Item **********************************************************************************
AME4 <- summary(margins(logit4, 
                variables = "perA",
                at = list(perA = 0:1, 
                          earner = c("Higher earner", "Lower earner", "Equal earners"))))

## test difference between coefficients
### create Z scores
z_higherA <- (AME4[[1,4]] - AME4[[4,4]]) / sqrt(AME4[[1,5]]^2 + AME4[[4,5]]^2)
z_lowerA  <- (AME4[[2,4]] - AME4[[5,4]]) / sqrt(AME4[[2,5]]^2 + AME4[[5,5]]^2)
z_equalA  <- (AME4[[3,4]] - AME4[[6,4]]) / sqrt(AME4[[3,5]]^2 + AME4[[6,5]]^2)

### create p values
p_higherA <- 2*pnorm(-abs(z_higherA))
p_lowerA  <- 2*pnorm(-abs(z_lowerA))
p_equalA  <- 2*pnorm(-abs(z_equalA))

### report p values
print(paste("(ACTIVITY) test of equality-- Higher-Earner * gender: p =", round(p_higherA, digits = 3)))
print(paste("(ACTIVITY) test of equality-- Lower-Earner * gender: p =", round(p_lowerA, digits = 3)))
print(paste("(ACTIVITY) test of equality-- Equal Earners * gender: p =", round(p_equalA, digits = 3)))


### test equality of coefficients between Item & Activity **********************************************************************************

## Z scores
z_higher_MAN_AB <- (AME3[[1,4]] - AME4[[1,4]]) / sqrt(AME3[[1,5]]^2 + AME4[[1,5]]^2)
z_lower_MAN_AB  <- (AME3[[2,4]] - AME4[[2,4]]) / sqrt(AME3[[2,5]]^2 + AME4[[2,5]]^2)
z_equal_MAN_AB  <- (AME3[[3,4]] - AME4[[3,4]]) / sqrt(AME3[[3,5]]^2 + AME4[[3,5]]^2)

z_higher_FEM_AB <- (AME3[[4,4]] - AME4[[4,4]]) / sqrt(AME3[[4,5]]^2 + AME4[[4,5]]^2)
z_lower_FEM_AB  <- (AME3[[5,4]] - AME4[[5,4]]) / sqrt(AME3[[5,5]]^2 + AME4[[5,5]]^2)
z_equal_FEM_AB  <- (AME3[[6,4]] - AME4[[6,4]]) / sqrt(AME3[[6,5]]^2 + AME4[[6,5]]^2)

## p values
p_higher_MAN_AB <- 2*pnorm(-abs(z_higher_MAN_AB)) 
p_lower_MAN_AB  <- 2*pnorm(-abs(z_lower_MAN_AB)) 
p_equal_MAN_AB  <- 2*pnorm(-abs(z_equal_MAN_AB)) 

p_higher_FEM_AB <- 2*pnorm(-abs(z_higher_FEM_AB)) 
p_lower_FEM_AB  <- 2*pnorm(-abs(z_lower_FEM_AB)) 
p_equal_FEM_AB  <- 2*pnorm(-abs(z_equal_FEM_AB)) 

print(paste("(ITEM VS ACT) Man * Higher-Earner test of equality: p =", round(p_higher_MAN_AB, digits = 3)))
print(paste("(ITEM VS ACT) Man * Lower-Earner test of equality: p =", round(p_lower_MAN_AB, digits = 3)))
print(paste("(ITEM VS ACT) Man * Equal Earners test of equality: p =", round(p_equal_MAN_AB, digits = 3)))

print(paste("(ITEM VS ACT) Female * Higher-Earner test of equality: p =", round(p_higher_FEM_AB, digits = 3)))
print(paste("(ITEM VS ACT) Female * Lower-Earner test of equality: p =", round(p_lower_FEM_AB, digits = 3)))
print(paste("(ITEM VS ACT) Female * Equal Earners test of equality: p =", round(p_equal_FEM_AB, digits = 3)))


# Figure 1. --------------------------------------------------------------------------
### https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html
### https://github.com/easystats/insight/issues/451 <- delta??
### https://data.library.virginia.edu/a-beginners-guide-to-marginal-effects/

## Create predicted probabilities datesets
pp3   <- ggeffect(logit3, terms = c("perI", "earner"))
pp4   <- ggeffect(logit4, terms = c("perA", "earner"))

pp3$type <- "item"
pp4$type <- "activity"

data_fig1 = merge(pp3, pp4, all = TRUE)
head(data_fig1)

data_fig1$x <-factor(data_fig1$x)
levels(data_fig1$x)[levels(data_fig1$x)=="1"] <- "She decided"
levels(data_fig1$x)[levels(data_fig1$x)=="0"] <- "He decided"

data_fig1$x    <- factor(data_fig1$x, levels = c("She decided", "He decided"), ordered = FALSE)
data_fig1$type <- factor(data_fig1$type, levels = c("item", "activity"), ordered = FALSE)

fig1 <- data_fig1 %>%
  ggplot(aes(x = x, y = predicted, fill = x)) +
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
  scale_fill_manual(values = c("#18BC9C", "#F39C12")) +
  theme_minimal() +
  theme(legend.position     = "none",
        panel.grid.major.x  = element_blank(),
        strip.text          = element_text(face = "bold"),
        strip.text.y.left   = element_text(angle = 0),
        axis.text.y         = element_blank(),  #remove y axis labels
        axis.ticks.y        = element_blank(),  #remove y axis ticks
        plot.subtitle       = element_text(face = "italic", color = "#707070"),
        plot.caption        = element_text(face = "italic", color = "#707070"),
        plot.title          = element_markdown(face = "bold"),
        plot.title.position = "plot") +
  scale_y_continuous(labels=scales::percent, limits = c(0, .8)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Perceptions of <span style = 'color: #18BC9C;'>women's</span> and <span style = 'color: #F39C12;'>men's</span> 
        decision-making about <p>items and activities.",
        subtitle = "% of respondents who said the decision was somewhat or very fair",
        caption  = "Predicted percentages adjust for vignette manipulations and respondent demographic characteristics.") 

fig1

ggsave(filename = file.path(figDir, "fig1.png"), fig1, width=6, height=6, units="in", dpi=300)

#####################################################################################
# Appendix Tables and Figures (quant)
#####################################################################################

# Table A ---------------------------------------------------------------------------
## Weighted Descriptive Statistics of Respondent Characteristics

## Create weighted data 
tabAdata <- quantdata %>%
  select("weight", "gender", "relate", "parent", "raceeth", 
         "educ", "employ", "incdum", "age")

tabASvy <- svydesign(ids = ~1, weights = ~ weight, data = tabAdata)

tabA <- tabASvy %>%
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
  modify_header(
    update = list(
      label ~ "**Variable**",
      stat_0 ~ "**Overall** N = {N_unweighted}")) %>%
  as_flex_table() 

tabA # show table

## https://mran.microsoft.com/snapshot/2017-12-11/web/packages/officer/vignettes/word.html
read_docx() %>% 
  body_add_par("Table A. Weighted Sample Characteristics") %>% 
  body_add_flextable(value = tabA) %>% 
  print(target = file.path(outDir, "finalsay_tableA.docx"))

# Appendix Figure A. --------------------------------------------------------------------------
## Experimental Design (No figure generated)

# Appendix Figure B. --------------------------------------------------------------------------
## Fairness Evaluation by Item/Activity Presented to Respondent

data_figB <- quantdata %>%
  select("CaseID", "item", "activity", "ifair", "afair") %>%
  # Create long data for item/activity fairness vars
  pivot_longer(
    cols = c(ifair, afair),
    names_to = "drop",
    values_to = "fairness") %>%
  # Create long data for item/activity decision vars
  pivot_longer(
    cols = c(item, activity),
    names_to = "type",
    values_to = "category") %>%
  # remove duplicates
  filter((drop == "ifair" & type == "item") |
         (drop == "afair" & type == "activity")) %>%
  select(-c("drop")) %>%
  # create percentage data
  group_by(type, category) %>%
  count(category = factor(category), fairness = factor(fairness)) %>% 
  mutate(pct = prop.table(n)) %>%
  ungroup()

data_figB$type <- factor(data_figB$type, levels = c("item", "activity"), ordered = FALSE)


figB <- data_figB %>%
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
  theme_minimal() +
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
        title    = "Fairness evaluation by item & activity presented to respondent",
        subtitle = "How fair do you think the decision was?")
  
figB   
  
ggsave(filename = file.path(figDir, "figB.png"), figB, width=6, height=4, units="in", dpi=300)


# Appendix Figure C. --------------------------------------------------------------------------

## Create predicted probabilities datesets
pp1   <- ggeffect(logit1, terms = "iperson")
pp2   <- ggeffect(logit2, terms = "aperson")

# https://github.com/easystats/insight/issues/451 <- delta??

pp1$type <- "item"
pp2$type <- "activity"

data_figC = merge(pp1, pp2, all = TRUE)
head(data_figC)

levels(data_figC$x)[levels(data_figC$x)=="Michelle"] <- "She decided"
levels(data_figC$x)[levels(data_figC$x)=="Anthony"]  <- "He decided"
data_figC$type <- factor(data_figC$type, levels = c("item", "activity"), ordered = FALSE)


figC <- data_figC %>%
  ggplot(aes(x = x, y = predicted, fill = x)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                stat="identity", position=position_dodge(.7), color="#ADB5BD") +
  geom_text(position = position_dodge(width = .7),
            vjust = -0.5,
            aes(label=sprintf("%1.0f%%", predicted*100))) +
  facet_wrap(~ type) +
  scale_fill_manual(values = c("#18BC9C", "#F39C12")) +
  theme_minimal() +
  theme(legend.position     = "top",
        panel.grid.major.x  = element_blank(),
        plot.title          = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.subtitle       = element_text(face = "italic", color = "#707070"),
        plot.caption        = element_text(face = "italic", color = "#707070")) +
  scale_y_continuous(labels=scales::percent, limits = c(0, .8)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Gender differences in perceptions of fairness \nin decision-making for items and activities.",
        subtitle = "% of respondents who said the decision was somewhat or very fair...",
        caption  = "Predicted percentages adjust for other vignette manipulations and respondent demographic characteristics.") 

figC

ggsave(filename = file.path(figDir, "figC.png"), figC, width=5, height=5, units="in", dpi=300)


# Appendix Figure D. --------------------------------------------------------------------------

##  DROPPED FROM STUDY ENTIRELY
# logit5 <- glm(idum ~ iperson * relinc * organize + mar + child + dur + 
#                 gender+relate+parent+raceeth+educ+employ+incdum+age,
#               quantdata, weights = weight, family="binomial")
# 
# logit6 <- glm(adum ~ aperson * relinc * organize + mar + child + dur + 
#                 gender+relate+parent+raceeth+educ+employ+incdum+age,
#               quantdata, weights = weight, family="binomial")
# 
# 
# pp5   <- ggeffect(logit5, terms = c("iperson", "relinc", "organize"))
# pp6   <- ggeffect(logit6, terms = c("aperson", "relinc", "organize"))
# 
# pp5$type <- "item"
# pp6$type <- "activity"
# 
# data_figD = merge(pp5, pp6, all = TRUE)
# head(data_figD)
# 
# levels(data_figD$x)[levels(data_figD$x)=="Michelle"] <- "She decided"
# levels(data_figD$x)[levels(data_figD$x)=="Anthony"]  <- "He decided"
# data_figD$x <- factor(data_figD$x, levels = c("He decided", "She decided"), ordered = FALSE)
# data_figD$type <- factor(data_figD$type, levels = c("item", "activity"), ordered = FALSE)
# data_figD$group <- factor(data_figD$group, levels = c("Equal earners", "Woman higher-earner", "Man higher-earner"), ordered = FALSE)
# 
# 
# figD <- data_figD %>%
#   ggplot(aes(x = x, y = predicted, fill = group)) +
#   geom_col(width = 0.6, position = position_dodge(0.7)) +
#   facet_grid(type ~ facet) +
#   facet_grid(type ~ facet,
#              scales="free",
#              space = "free",
#              switch = "y") +
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
#                 stat="identity", position=position_dodge(.7), color="#ADB5BD") +
#   coord_flip() +
#   geom_text(position = position_dodge(width = .7),
#             hjust = 1.5,
#             aes(label=sprintf("%1.0f%%", predicted*100)),
#             colour = "white",
#             size = 3) +
#   scale_fill_manual(values = c("#18BC9C", "#E74C3C", "#3498DB")) +
#   theme_minimal() +
#   theme(legend.position     = "bottom",
#         panel.grid.major.x  = element_blank(),
#         plot.title          = element_text(face = "bold"),
#         plot.title.position = "plot",
#         plot.subtitle       = element_text(face = "italic", color = "#707070"),
#         plot.caption        = element_text(face = "italic", color = "#707070"),
#         strip.text          = element_text(face = "bold"),
#         strip.text.y.left   = element_text(angle = 0),
#         strip.placement     = "outside") +
#   scale_y_continuous(labels = percent, limits = c(0, .85)) +
#   labs( x        = " ", 
#         y        = " ", 
#         fill     = " ",
#         title    = "Perceptions of fairness about item and activity decisions \n by gender, relative earnings, and allocation system",
#         subtitle = "Adjusted predicted % who said the decision was somewhat or very fair...",
#         caption  = "Predicted percentages adjust for other vignette manipulations\nand respondent demographic characteristics.") +
#   guides(fill = guide_legend(reverse = TRUE))
# 
# figD
# 
# ggsave(filename = file.path(figDir, "figD.png"), figD, width=6, height=6, units="in", dpi=300)
