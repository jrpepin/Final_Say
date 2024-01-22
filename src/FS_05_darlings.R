# OLS REGRESSIONS ----------------------------------------------------------------
# lcaSvy <- svydesign(ids = ~1, weights = ~ weight, data = lcadata)
# 
# ## item regressions
# equal_i  <- svyglm(t_1_item ~ iperson + earner + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# happy_i  <- svyglm(t_2_item ~ iperson + earner + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# gender_i <- svyglm(t_3_item ~ iperson + earner + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# money_i  <- svyglm(t_4_item ~ iperson + earner + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# 
# ## activity regressions
# 
# equal_a  <- svyglm(t_1_act ~ aperson + earner + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# happy_a  <- svyglm(t_2_act ~ aperson + earner + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# gender_a <- svyglm(t_3_act ~ aperson + earner + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# money_a <- svyglm(t_4_act ~ aperson + earner + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# ADD BACK IN LATER


## FIGURE 5 -------------------------------------------------------------------------------------------------
mn_item <- multinom(top_i ~ iperson * relinc + organize + mar + child + dur + item + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

mn_act  <- multinom(top_a ~ aperson * relinc + organize + mar + child + dur + order + activity + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

pur.pp  <- ggeffect(mn_item , terms = c("iperson", "relinc"))
act.pp  <- ggeffect(mn_act ,  terms = c("aperson", "relinc"))

pur.pp$type <- "purchase"
act.pp$type <- "activity"

data_fig56 <- rbind(pur.pp, act.pp)

levels(data_fig56$x)[levels(data_fig56$x)=="Michelle"] <- "She decided"
levels(data_fig56$x)[levels(data_fig56$x)=="Anthony"]  <- "He decided"
data_fig56$x <- factor(data_fig56$x, levels = c("She decided", "He decided"), ordered = FALSE)
data_fig56$type <- factor(data_fig56$type, levels = c("purchase", "activity"), ordered = FALSE)

data_fig56$earnings <- factor(data_fig56$group, levels = c("Equal earners", "Woman higher-earner", "Man higher-earner"), ordered = FALSE)

## Pretty topic labels
data_fig56$response.level[data_fig56$response.level == "Assured.Acquiescence"]  <- "Assured Acquiescence"
data_fig56$response.level[data_fig56$response.level == "Happy.Wife.Happy.Life"] <- "Man Has\nFinal Say"
data_fig56$response.level[data_fig56$response.level == "Man.Has.Final.Say"]     <- "Practical Efficiency"
data_fig56$response.level[data_fig56$response.level == "Money.Matters"]         <- "Happy Wife\nHappy Life"
data_fig56$response.level[data_fig56$response.level == "Practical.Efficiency"]  <- "Taking Turns"
data_fig56$response.level[data_fig56$response.level == "Taking.Turns"]          <- "Money Matters"
data_fig56$response.level[data_fig56$response.level == "Work.Together"]         <- "Work Together"

data_fig56$response.level <- factor(data_fig56$response.level, 
                                    levels = c("Practical Efficiency", "Assured Acquiescence", 
                                               "Money Matters", "Work Together",
                                               "Taking Turns", "Man Has\nFinal Say", "Happy Wife\nHappy Life"))

fig5 <- data_fig56 %>%
  filter(type == "purchase") %>% # exclude activity because all CI overlap
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 3.5, shape=21, alpha = 0.9) +
  facet_grid(response.level ~ . , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, .8), breaks = c(.0, .2, .4, .6, .8)) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme(axis.text.x      = element_text(vjust = 0.5, hjust=1, size = 10),
        strip.text.x     = element_text(face = "bold", size = 10),
        strip.text.y     = element_text(angle = 0, size = 10, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.title       = element_text(size = 10), 
        axis.text        = element_text(size = 8), 
        plot.margin      = unit(c(.1,.5,.1,.5),"cm"),
        legend.position  = "top",
        plot.title.position = "plot",
        axis.line        = element_line(size = 4, colour = "white"),
        panel.spacing=unit(.5, "lines"),
        panel.grid.major.y = element_line(colour="#f2f2f2", size=7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Topic probability among perceived fair decisions on purchases",
        subtitle = "by vignette gender and relative earnings")

fig5

ggsave(filename = file.path(figDir, "fig5.png"), fig5, width=6.5, height=8, units="in", dpi=300)

## FIGURE 6 -------------------------------------------------------------------------------------------------

fig6 <- data_fig56 %>%
  filter(type == "activity") %>% # change to activities
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 3.5, shape=21, alpha = 0.9) +
  facet_grid(response.level ~ . , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, 1), breaks = c(.0, .2, .4, .6, .8, 1)) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme(axis.text.x      = element_text(vjust = 0.5, hjust=1, size = 10),
        strip.text.x     = element_text(face = "bold", size = 10),
        strip.text.y     = element_text(angle = 0, size = 10, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.title       = element_text(size = 10), 
        axis.text        = element_text(size = 8), 
        plot.margin      = unit(c(.1,.5,.1,.5),"cm"),
        legend.position  = "top",
        plot.title.position = "plot",
        axis.line        = element_line(size = 4, colour = "white"),
        panel.spacing=unit(.5, "lines"),
        panel.grid.major.y = element_line(colour="#f2f2f2", size=7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Topic probability among perceived fair decisions on activities",
        subtitle = "by vignette gender and relative earnings")

fig6

ggsave(filename = file.path(figDir, "fig6.png"), fig6, width=6.5, height=8, units="in", dpi=300)



##### new figure


data_fig56 %>%
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 3.5, shape=21, alpha = 0.9) +
  facet_grid(response.level ~ type , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, .5), breaks = c(.0, .25, .5)) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme(axis.text.x      = element_text(vjust = 0.5, hjust=1, size = 10),
        strip.text.x     = element_text(face = "bold", size = 10),
        strip.text.y     = element_text(angle = 0, size = 10, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.title       = element_text(size = 10), 
        axis.text        = element_text(size = 8), 
        plot.margin      = unit(c(.1,.5,.1,.5),"cm"),
        legend.position  = "top",
        plot.title.position = "plot",
        axis.line        = element_line(size = 4, colour = "white"),
        panel.spacing=unit(.5, "lines"),
        panel.grid.major.y = element_line(colour="#f2f2f2", size=7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Topic probability among perceived fair decisions on activities",
        subtitle = "by vignette gender and relative earnings")

# ------------------------------------------------------------------------------


mn_item <- multinom(top_i ~ iperson * relinc + organize + mar + child + dur + item + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

ggpredict(mn_item , terms = c("iperson", "relinc")) %>%
  as_tibble() %>%
  print



gge

library(marginaleffects)
mfx <- slopes(mn_item, condition = list("iperson", "relinc" = "Man higher-earner"))


library(margins)
ame <- marginal_effects(mn_item, data = subset(lcadata, iperson == "Anthony"), variables = c("relinc"))
ame <- marginal_effects(mn_item, variables = c("relinc"))


library(effects)

effects.obj <- Effect(c("iperson", "relinc"), mn_item)

cis = with(effects.obj, cbind(x, prob, upper.prob, lower.prob))  # Generate a tidy data frame with predictions, lower and upper confidence intervals

cis %>%
  pivot_longer(cols = starts_with(c("prob", "U.prob", "L.prob"))) %>%
  separate(name, into=c('type', 'response.level'), sep="*.prob.X")
  
ci.fnc = function(effects.obj) {
  col = syms(names(effects.obj$x)) # Capture the name of the effects term
  cis = with(effects.obj, cbind(x, prob, upper.prob, lower.prob)) %>% # Generate a tidy data frame with predictions, lower and upper confidence intervals
    pivot_longer(cols=-col) %>% 
    separate(name, into=c('type', 'response.level'), sep="prob.X") %>% 
    mutate(response.level=gsub("\\.", " ", response.level)) %>% 
    mutate(type=gsub("\\.", "", type)) %>% 
    spread(type, value)
  return(cis)
}


pptopic = ci.fnc(effects.obj) 

