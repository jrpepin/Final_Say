## Create high stakes plot
p3 <- data_fig5 %>%
  filter(gender != "All" & earner == "All earners" & 
           fair  == "Fair" & stakes == "High") %>%
  ggplot(aes(x = estimate, y = gender, fill = gender)) +
  geom_col(width = 0.8, position="stack") +
  facet_grid(rows   = vars(reorder(topic, -estimate)),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y") +
  theme_minimal(13) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        strip.text.y        = element_blank(),
        legend.position     = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_fill_grey(name = " ") +
  labs(title     = "Predicted topic prevalence for decisions rated as fair by decision type, vignette couples' relative income\nand decision-maker gender, and respondent gender\n ",
       x        = " ", 
       y        = " ",
       subtitle = "High-stakes decisions")

## Create low stakes plot
p4 <- data_fig5 %>%
  filter(gender != "All" & earner == "All earners" & 
           fair  == "Fair" & stakes == "Low") %>%
  ggplot(aes(x = estimate, y = gender, fill = gender)) +
  geom_col(width = 0.8, position="stack") +
  #  geom_point() +
  facet_grid(rows   = vars(reorder(topic, -estimate)),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y") +
  theme_minimal(13) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        #        axis.text.y         = element_blank(),
        strip.text.y        = element_blank(),
        legend.position     = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_fill_grey(name = " ") +
  labs( x        = " ", 
        y        = " ",
        subtitle = "Low-stakes decisions")

## Create the combined plot
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4) 
g_fig5 <- rbind(g3, g4, size = "first")
g_fig5$widths <- unit.pmax(g3$widths, g4$widths)
grid.newpage()
grid.draw(g_fig5)

### save Figure 5
png(file.path(figDir, "fig5.png"), 
    width = 850, height = 580, pointsize=16) 
grid.draw(g_fig5) 
dev.off()




#####################################


## Create high stakes plot
data_fig5 %>%
  filter(gender != "All" & earner != "All earners" & 
           fair  == "Fair" & stakes == "High") %>%
  ggplot(aes(x = estimate, y = gender, fill = gender)) +
  geom_col(width = 0.7, position="stack") +
  facet_grid(reorder(topic, -estimate) ~  earner + decider, 
             space  = "free",
             switch = "y") +
  theme_minimal(13) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        strip.text.y        = element_blank(),
        axis.text.x         = element_blank(),
        panel.grid          = element_blank(),
        legend.position     = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_fill_grey(name = " ") +
  labs(title     = "Predicted topic prevalence for decisions rated as fair by decision type, vignette couples' relative income\nand decision-maker gender, and respondent gender\n ",
       x        = " ", 
       y        = " ",
       subtitle = "High-stakes decisions")

##########################################

df <- data_fig5 %>%
  filter(gender != "All" & earner != "All earners") %>%
  # use whole numbers
  mutate(estimate = estimate * 100) %>%
  pivot_wider(names_from = gender, 
              values_from = c(estimate, std.error, statistic, p.value, s.value, 
                              conf.low, conf.high, newest)) %>%
  # compute the gap
  mutate(gap=estimate_Men-estimate_Women) %>% 
  # find the maximum value by decider
  group_by(fair, stakes, topic, earner, decider) %>%
  mutate(max=max(estimate_Men, estimate_Women)) %>% 
  ungroup() %>%
  pivot_longer(
    cols = estimate_Men:newest_Women,
    names_to = c("statistic", "gender"),
    names_sep = "_",
    values_to = "value") %>%
  pivot_wider(names_from = statistic,
              values_from = value) %>%
  mutate(estimate = round(estimate, digits =2),
         max = round(max, digits = 2))


df %>%
  filter(fair  == "Fair" & stakes == "High") %>%
  ggplot(aes(x = estimate, y = gender, color = forcats::fct_rev(earner), position_dodge(.5))) +
  geom_line(aes(group  =gender), color="#E7E7E7", linewidth=3.5) + 
  geom_point(aes(color =decider),  size =3) +
  facet_grid(rows   = vars(reorder(topic, -estimate)),  
             cols   = vars(earner), 
             space  = "free",
             switch = "y") +
  # data callout
  geom_text(data = df %>% filter(fair  == "Fair" & stakes == "High" &
                                   estimate == max), 
            aes(label=round(estimate, digits = 0), color=decider),
            size=3.25, nudge_x= 2) +
  geom_text(data = df %>% filter(fair  == "Fair" & stakes == "High" &
                                   estimate != max), 
            aes(label=round(estimate, digits = 0), color=decider),
            size=3.25, nudge_x= -2) +
  theme_minimal(13) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        strip.text.y        = element_blank(),
        panel.grid          = element_blank(),
        axis.text.x         = element_blank(),
        legend.position     = "top") +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_color_grey(name = " ") +
  xlim(0, 50) +
  labs(title     = "Predicted topic prevalence for decisions rated as fair by decision type, vignette couples' relative income\nand decision-maker gender, and respondent gender\n ",
       x        = " ", 
       y        = " ",
       subtitle = "High-stakes decisions")