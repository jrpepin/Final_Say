library(modelsummary)

modelsummary::modelsummary(
  list("Model 1" = logit1, "Model 2" = logit2),
  stars = c("*" =.05, "**" = .01, "***" = .001)
)

## Create predicted probabilities datesets
pp3   <- ggeffect(logit3, terms = c("perI", "relinc", "gender"))
pp4   <- ggeffect(logit4, terms = c("perA", "relinc", "gender"))

pp3$type <- "purchase"
pp4$type <- "activity"

data_fig2 = merge(pp3, pp4, all = TRUE)
head(data_fig2)

data_fig2$x <-factor(data_fig2$x)
levels(data_fig2$x)[levels(data_fig2$x)=="1"] <- "She\ndecided"
levels(data_fig2$x)[levels(data_fig2$x)=="0"] <- "He\ndecided"

data_fig2$x    <- factor(data_fig2$x, 
                         levels  = c("She\ndecided", "He\ndecided"), 
                         ordered = FALSE)
data_fig2$type <- factor(data_fig2$type, 
                         levels  = c("purchase", "activity"), 
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
  scale_fill_discrete_qualitative(palette = "Dark 3") +
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
        fill     = "Respondent's\ngender",
        title    = "Perceptions of decision-making about purchases and activities <p> by decision type and relative earnings",
        subtitle = "Predicted % of respondents who said the decision was somewhat or very fair",
        caption  = "Predicted percentages calculated from logistic regression models interacting relative income and gender of decider reported in Table 2.\nPresented by respondent gender with 95% confidence intervals.") 

fig2


### REPLACE WORDCLOUDS
library(treemapify)

clouddata %>%
  ggplot(aes(area = phi, label = word, fill = as.factor(phi), subgroup = word)) +
  geom_treemap() + 
  geom_treemap_text(color = "white") +
#  geom_treemap_subgroup_border(color = "white") +
  facet_wrap(~topic, ncol = 2) +
  scale_fill_grey() +
  theme_minimal(12) +
  theme(legend.position     = "none") +
  labs(title    = "Highest-ranking word stems per topic", 
       subtitle = "Tiles are weighted by probability of being found in topic")


# Table 02 ---------------------------------------------------------------------

## Run the models

### Key IVs
logit1 <- glm(idum ~ perI + relinc + organize + mar + child + dur + item +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, family="binomial")

logit2 <- glm(adum ~ perA + relinc + organize + mar + child + dur + order + activity +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, family="binomial")

### Interactions
logit3 <- glm(idum ~ perI * relinc + organize + mar + child + dur + item +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, family="binomial")

logit4 <- glm(adum ~ perA * relinc + organize + mar + child + dur + order + activity +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, family="binomial")

## Calculate AMEs
m1 <- avg_slopes(logit1, variables = c("relinc", "perI", "gender"))
m2 <- avg_slopes(logit2, variables = c("relinc", "perA", "gender"))
m3 <- avg_slopes(logit3, variables = c("relinc"), by = "perI")
m4 <- avg_slopes(logit4, variables = c("relinc"), by = "perA")

# test equality of coefficients between HIGH & LOW stakes
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
# https://journals.sagepub.com/doi/10.1177/0081175019852763
z_equal   <- (m1[[3,3]] - m2[[3,3]]) / sqrt(m1[[3,4]]^2 + m2[[3,4]]^2)
z_fmore   <- (m1[[4,3]] - m2[[4,3]]) / sqrt(m1[[4,4]]^2 + m2[[4,4]]^2)
z_Rgender <- (m1[[1,3]] - m2[[1,3]]) / sqrt(m1[[1,4]]^2 + m2[[1,4]]^2)
z_decider <- (m1[[2,3]] - m2[[2,3]]) / sqrt(m1[[2,4]]^2 + m2[[2,4]]^2)

p_equal   <- 2*pnorm(-abs(z_equal)) 
p_fmore   <- 2*pnorm(-abs(z_fmore)) 
p_Rgender <- 2*pnorm(-abs(z_Rgender)) 
p_decider <- 2*pnorm(-abs(z_decider)) 

# Test of Equality between models (pvalues)
message("Equal earners: p = ",       round(p_equal,     digits = 3))
message("Woman Higher-Earner: p = ", round(p_fmore,     digits = 3))
message("Decider: p = ",             round(p_decider,   digits = 3))
message("Respondents' Gender: p = ", round(p_Rgender,   digits = 3))

# test equality between models with interactions
z_equalH   <- (m3[[1,4]] - m4[[1,4]]) / sqrt(m3[[1,5]]^2 + m4[[1,5]]^2)
z_equalS   <- (m3[[2,4]] - m4[[2,4]]) / sqrt(m3[[2,5]]^2 + m4[[2,5]]^2)

z_fmoreH   <- (m3[[3,4]] - m4[[3,4]]) / sqrt(m3[[3,5]]^2 + m4[[3,5]]^2)
z_fmoreS   <- (m3[[4,4]] - m4[[4,4]]) / sqrt(m3[[4,5]]^2 + m4[[4,5]]^2)

p_equalH   <- 2*pnorm(-abs(z_equalH)) 
p_equalS   <- 2*pnorm(-abs(z_equalS)) 
p_fmoreH   <- 2*pnorm(-abs(z_fmoreH)) 
p_fmoreS   <- 2*pnorm(-abs(z_fmoreS)) 

# Test of Equality pvalues
message("Equal earners: He decides p = ",       round(p_equalH,   digits = 3))
message("Equal earners: She decides p = ",      round(p_equalS,   digits = 3))
message("Woman Higher-Earner: He decidesp = ",  round(p_fmoreH,   digits = 3))
message("Woman Higher-Earner: She decidesp = ", round(p_fmoreS,   digits = 3))

## Combine labels for table
m1$term <- paste(m1$term, m1$contrast, sep= ".")
m2$term <- paste(m2$term, m2$contrast, sep= ".")
m3$term <- paste(m3$term, m3$contrast, m3$perI, sep= ".")
m4$term <- paste(m4$term, m4$contrast, m4$perA, sep= ".")

## Create model list for 2 panels
panels <- list(
  "Effect of Relative Income and Gender of Decider" = list(
    "High Stakes" = m1, "Low Stakes" = m2),
  "Effect of Relative Income by Gender of Decider" = list(
    "High Stakes" = m3, "Low Stakes" = m4))

## Create pretty labels
coef_mapC <- c(
  "relinc.mean(Equal earners) - mean(Man higher-earner)"         = "Equal earners",
  "relinc.mean(Woman higher-earner) - mean(Man higher-earner)"   = "Woman higher-earner",
  "perI.mean(1) - mean(0)"                                       = "Decider: Woman",
  "perA.mean(1) - mean(0)"                                       = "Decider: Woman",
  "gender.mean(Female) - mean(Male)"                             = "Respondent's Gender: Woman",
  "relinc.mean(Equal earners) - mean(Man higher-earner).0"       = "Equal earners: He decides",
  "relinc.mean(Equal earners) - mean(Man higher-earner).1"       = "Equal earners: She decides",
  "relinc.mean(Woman higher-earner) - mean(Man higher-earner).0" = "Woman higher-earner: He decides",
  "relinc.mean(Woman higher-earner) - mean(Man higher-earner).1" = "Woman higher-earner: She decides")

## Produce table
modelsummary(
  panels,
  shape = "rbind",
  coef_map = coef_mapC,
  gof_map = NA,
  #  exponentiate = TRUE,
  stars = c("*" =.05, "**" = .01, "***" = .001),
  fmt = fmt_decimal(digits = 3, pdigits = 3),
  notes = reference)



## Appendix Table 2 for reviewer -----------------------------------------------

### Gender Interactions
m_HighMar <- glm(idum ~ perI * mar + relinc + organize + child + dur + item +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, family="binomial")

m_HighPar <- glm(idum ~ perI * child + relinc + organize + mar + dur + item +
                   gender+relate+parent+raceeth+educ+employ+incdum+age,
                 quantdata, family="binomial")

m_HighDur <- glm(idum ~ perI * dur + relinc + organize + mar + child + item +
                   gender+relate+parent+raceeth+educ+employ+incdum+age,
                 quantdata, family="binomial")

m_LowMar <- glm(adum ~ perA * mar + relinc + organize + child + dur + order + activity +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, family="binomial")

m_LowPar <- glm(adum ~ perA * child + relinc + organize + mar + dur + order + activity +
                   gender+relate+parent+raceeth+educ+employ+incdum+age,
                 quantdata, family="binomial")

m_LowDur <- glm(adum ~ perA * dur + relinc + organize + mar + child + order + activity +
                  gender+relate+parent+raceeth+educ+employ+incdum+age,
                quantdata, family="binomial")


## Calculate AMEs
AME_HighMar <- avg_slopes(m_HighMar, variables = c("mar"),   by = "perI")
AME_HighPar <- avg_slopes(m_HighPar, variables = c("child"), by = "perI")
AME_HighDur <- avg_slopes(m_HighDur, variables = c("dur"),   by = "perI")

AME_LowMar <- avg_slopes(m_LowMar,   variables = c("mar"),   by = "perA")
AME_LowPar <- avg_slopes(m_LowPar,   variables = c("child"), by = "perA")
AME_LowDur <- avg_slopes(m_LowDur,   variables = c("dur"),   by = "perA")

## Combine labels for table
AME_HighMar$term  <- paste(AME_HighMar$term, AME_HighMar$contrast, AME_HighMar$perI, sep= ".")
AME_HighPar$term  <- paste(AME_HighPar$term, AME_HighPar$contrast, AME_HighPar$perI, sep= ".")
AME_HighDur$term  <- paste(AME_HighDur$term, AME_HighDur$contrast, AME_HighDur$perI, sep= ".")

AME_LowMar$term   <- paste(AME_LowMar$term,  AME_LowMar$contrast,  AME_LowMar$perA,  sep= ".")
AME_LowPar$term   <- paste(AME_LowPar$term,  AME_LowPar$contrast,  AME_LowPar$perA,  sep= ".")
AME_LowDur$term   <- paste(AME_LowDur$term,  AME_LowDur$contrast,  AME_LowDur$perA,  sep= ".")

## Create model list for 2 panels
panels <- list(
  "Married (relative to living together)" = list(
    "High Stakes" = AME_HighMar, "Low Stakes" = AME_LowMar),
  "Parent (relative to not parenting)" = list(
    "High Stakes" = AME_HighPar, "Low Stakes" = AME_LowPar),
  "7 year relationship (relative to 3 years)"= list(
    "High Stakes" = AME_HighDur, "Low Stakes" = AME_LowDur))

## Create pretty labels
coef_mapA <- c(
  "mar.mean(live together) - mean(are married).0"                = "He Decides",
  "mar.mean(live together) - mean(are married).1"                = "She Decides",
  "child.mean(one child together) - mean(no children).0"         = "He Decides",
  "child.mean(one child together) - mean(no children).1"         = "She Decides",
  "dur.mean(7 years) - mean(3 years).0"                          = "He Decides",
  "dur.mean(7 years) - mean(3 years).1"                          = "She Decides")

## Produce table
modelsummary(
  panels,
  shape = "rbind",
  coef_map = coef_mapA,
  gof_map = NA,
  #  exponentiate = TRUE,
  stars = c("*" =.05, "**" = .01, "***" = .001),
  fmt = fmt_decimal(digits = 3, pdigits = 3),
  notes = reference)

