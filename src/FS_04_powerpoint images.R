fig2_pp <- data_fig1 %>%
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
  scale_fill_manual(values=c("#F27575", "#2EA5D7")) +
  theme_minimal(20) +
  theme(legend.position     = "none",
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
        fill     = " ",
        title    = " ") 

fig2_pp

ggsave(filename = file.path(figDir, "fig2_pp.png"), fig2_pp, width=12, height=6, units="in", dpi=300)


fig2_pp1 <- data_fig1 %>%
  filter(type == "purchase") %>%
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
  scale_fill_manual(values=c("#F27575", "#2EA5D7")) +
  theme_minimal(20) +
  theme(legend.position     = "none",
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
        fill     = " ",
        title    = " ") 

fig2_pp1

ggsave(filename = file.path(figDir, "fig2_pp1.png"), fig2_pp1, width=12, height=4, units="in", dpi=300)

## Wordcloud

## Create wordcloud

clouddata$rank <- as.numeric(clouddata$rank)

clouddata <- clouddata %>% # top 5 indicator
  mutate(top5 = case_when(
    rank <= 5 ~ "yes",
    TRUE      ~ "no"))

fig3 <- clouddata %>%
  ggplot(aes(label = word, size = phi, color = top5)) + 
  geom_text_wordcloud(rm_outside = TRUE, 
                      max_steps = 1,
                      grid_size = 1, 
                      eccentricity = .9) +
  facet_wrap(~topic, ncol = 2) +
  scale_size_area(max_size = 14) +
  scale_colour_manual(values = c("#566472", "#E16A86")) +
  theme_minimal(12) +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.1, "lines")) +
  labs(title    = "Word clouds with highest-ranking word stems per topic", 
       subtitle = "Word stems weighted by probability of being found in topic")

fig3

ggsave(file.path(figDir, "fig3.png"), fig3, height = 6, width = 6, units="in",dpi = 300, bg = 'white')



fig3_pp1 <- clouddata %>%
  filter(topic == "Topic 1: Equality or Bust") %>%
  ggplot(aes(label = word, size = phi, color = top5)) + 
  geom_text_wordcloud(rm_outside = TRUE, 
                      max_steps = 1,
                      grid_size = 1, 
                      eccentricity = .9) +
  scale_size_area(max_size = 14) +
  scale_colour_manual(values = c("#566472", "#F27575")) +
  theme_minimal() +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.1, "lines")) +
  labs(title    = "Topic 1: Equality or Bust")

fig3_pp1

ggsave(file.path(figDir, "fig3_pp1.png"), fig3_pp1, height = 6, width = 6, units="in",dpi = 300)

fig3_pp2 <- clouddata %>%
  filter(topic == "Topic 2: Man Has Final Say") %>%
  ggplot(aes(label = word, size = phi, color = top5)) + 
  geom_text_wordcloud(rm_outside = TRUE, 
                      max_steps = 1,
                      grid_size = 1, 
                      eccentricity = .9) +
  scale_size_area(max_size = 14) +
  scale_colour_manual(values = c("#566472", "#F27575")) +
  theme_minimal() +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.1, "lines")) +
  labs(title    = "Topic 2: Man Has Final Say")

fig3_pp2

ggsave(file.path(figDir, "fig3_pp2.png"), fig3_pp2, height = 6, width = 6, units="in",dpi = 300)

fig3_pp3 <- clouddata %>%
  filter(topic == "Topic 3: Money Matters") %>%
  ggplot(aes(label = word, size = phi, color = top5)) + 
  geom_text_wordcloud(rm_outside = TRUE, 
                      max_steps = 1,
                      grid_size = 1, 
                      eccentricity = .9) +
  scale_size_area(max_size = 14) +
  scale_colour_manual(values = c("#566472", "#F27575")) +
  theme_minimal() +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.1, "lines")) +
  labs(title    = "Topic 3: Money Matters")

fig3_pp3

ggsave(file.path(figDir, "fig3_pp3.png"), fig3_pp3, height = 6, width = 6, units="in",dpi = 300)


fig3_pp4 <- clouddata %>%
  filter(topic == "Topic 4: Happy Wife, Happy Life") %>%
  ggplot(aes(label = word, size = phi, color = top5)) + 
  geom_text_wordcloud(rm_outside = TRUE, 
                      max_steps = 1,
                      grid_size = 1, 
                      eccentricity = .9) +
  scale_size_area(max_size = 14) +
  scale_colour_manual(values = c("#566472", "#F27575")) +
  theme_minimal() +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.1, "lines")) +
  labs(title    = "Topic 4: Happy Wife, Happy Life")

fig3_pp4

ggsave(file.path(figDir, "fig3_pp4.png"), fig3_pp4, height = 6, width = 6, units="in",dpi = 300)


fig3_pp2 <- clouddata %>%
  ggplot(aes(label = word, size = phi, color = top5)) + 
  geom_text_wordcloud(rm_outside = TRUE, 
                      max_steps = 1,
                      grid_size = 1, 
                      eccentricity = .9) +
  facet_wrap(~topic, ncol = 2) +
  scale_size_area(max_size = 24) +
  scale_colour_manual(values = c("#566472", "#F27575")) +
  theme_minimal(24) +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.1, "lines"))

fig3_pp2

ggsave(file.path(figDir, "fig3_pp2.png"), fig3_pp2, height = 7, width = 10, units="in", dpi = 300)


# Figure 4

fig4_pp <- data_fig4 %>%
  ggplot(aes(fill=topic, y=decision, x=prop)) + 
  geom_bar(position=position_fill(reverse = TRUE), stat="identity") +
  geom_text(aes(label = weights::rd(prop, digits =2)), 
            position = position_fill(reverse = TRUE, vjust = .5),
            size = 8) +
  theme_minimal(24) +
  scale_fill_manual(values=c("#F27575", "#2EA5D7", "#EFA937", "#51E0CE")) +
  theme(legend.position = "top",
        axis.text.y = element_text(face="bold"),
        plot.title.position = "plot") +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ")

fig4_pp

ggsave(filename = file.path(figDir, "fig4_pp.png"), fig4_pp, width=11, height=6, units="in", dpi=300)

## FIGURE 5

fig5_pp <- data_fig56 %>%
  filter(type == "purchase") %>% # exclude activity because all CI overlap
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 3.5, shape=21, alpha = 0.9) +
  facet_grid(response.level ~ . , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, 1), breaks = c(.0, .2, .4, .6, .8, 1)) +
  scale_fill_manual(values=c("#F27575", "#2EA5D7")) +
  theme_minimal(20) +
  theme(axis.text.x      = element_text(vjust = 0.5, hjust=1, size = 16),
        strip.text.x     = element_text(face = "bold", size = 16),
        strip.text.y     = element_text(angle = 0, size = 16, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.title       = element_text(size = 16), 
        axis.text        = element_text(size = 16), 
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
        fill     = " ")

fig5_pp

ggsave(filename = file.path(figDir, "fig5_pp.png"), fig5_pp, width=11.5, height=6.5, units="in", dpi=300)


## FIGURE 5

fig6_pp <- data_fig56 %>%
  filter(type == "activity") %>% # exclude activity because all CI overlap
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 3.5, shape=21, alpha = 0.9) +
  facet_grid(response.level ~ . , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, 1), breaks = c(.0, .2, .4, .6, .8, 1)) +
  scale_fill_manual(values=c("#F27575", "#2EA5D7")) +
  theme_minimal(20) +
  theme(axis.text.x      = element_text(vjust = 0.5, hjust=1, size = 16),
        strip.text.x     = element_text(face = "bold", size = 16),
        strip.text.y     = element_text(angle = 0, size = 16, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.title       = element_text(size = 16), 
        axis.text        = element_text(size = 16), 
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
        fill     = " ")

fig6_pp

ggsave(filename = file.path(figDir, "fig6_pp.png"), fig6_pp, width=11.5, height=6.5, units="in", dpi=300)
