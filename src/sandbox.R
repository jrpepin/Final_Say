library(modelsummary)

modelsummary::modelsummary(
  list("Model 1" = logit1, "Model 2" = logit2),
  stars = c("*" =.05, "**" = .01, "***" = .001)
)

library(performance)
test_performance(logit1, logit2)
test_bf(logit1, logit2)
test_vuong(logit1, logit2) # nonnest2::vuongtest(m1, m2)



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
