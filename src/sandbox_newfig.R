
## Create HIGH stakes stacked bar plots ----------------------------------------
df_Hbar <- data_fig5 %>%
  filter(earner != "All earners" & fair  == "Fair" & gender != "All" &
           stakes == "High") %>%
  # Updates the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) 
  

pHH_bar <- df_Hbar %>%
  filter(decider == "He decided") %>%
  ggplot(aes(x = estimate, y = gender, fill = forcats::fct_rev(earner))) +
  geom_col(width = 0.9, position=position_stack()) +
  geom_text(aes(label = label,
                color = earner == "Men higher-earner"), size=2.5, 
            position = position_stack(vjust = .5)) +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y") +
  theme_minimal(12) +
  theme(strip.text.y.left   = element_text(angle = 0),
        axis.text.x         = element_blank(),
        panel.grid          = element_blank(),
        strip.placement.y   = "outside",
        legend.position     = "none",
        plot.title.position = "plot",
        plot.subtitle       = element_text(face = "bold"),
        plot.margin = margin(t = 0,    # Top margin
                             r = 0,    # Right margin
                             b = 0,    # Bottom margin
                             l = 0)) + # Left margin
  xlim(0, .85) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_grey(name = " ") +
  labs(subtitle = "High-stakes", 
       x        = " ", 
       y        = " ") 

pHS_bar <- df_Hbar %>%
  filter(decider == "She decided") %>%
  ggplot(aes(x = estimate, y = gender, fill = forcats::fct_rev(earner))) +
  geom_col(width = 0.9, position=position_stack()) +
  geom_text(aes(label = label,
                color = earner == "Men higher-earner"), size=2.5, 
            position = position_stack(vjust = .5)) +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y") +
  theme_minimal(12) +
  theme(strip.text.y        = element_blank(),
        axis.text.x         = element_blank(),
        axis.text.y         = element_blank(), # remove this for pHS
        panel.grid          = element_blank(),
        legend.position     = "none",
        plot.margin = margin(t = 0,    # Top margin
                             r = 0,    # Right margin
                             b = 0,    # Bottom margin
                             l = 0)) + # Left margin
  xlim(0, .85) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_fill_grey(name = " ") +
  labs(title    = " ", 
       x        = " ", 
       y        = " ") 

## Create average theta strip for HIGH stakes plots ----------------------------
df_Havg <- data_fig5 %>%
  filter(earner == "All earners" & fair  == "Fair" & gender == "All" &
           stakes == "High") %>%
  # Updates the factor levels
  mutate(topic = factor(topic,
                        levels = c("Accommodate",
                                   "Money Matters",
                                   "Man Has Final Say",
                                   "Consensus",
                                   "Balanced Sacrifice",
                                   "Decision History",
                                   "Happy Wife, Happy Life")))

pHH_avg <- df_Havg %>% 
  filter(decider == "He decided") %>%
  ggplot(aes(x=estimate,y=gender)) +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y",
             labeller = as_labeller(c(`He decided` = "Avg.\ntheta",
                                      `She decided` = "Avg.\ntheta"))) +
  geom_text(aes(x=0, label=label), position = position_stack(),
            fontface="bold",
            size=3.25) +
  theme_void() +
  theme(
    plot.margin = margin(l=0, r=0, b=0, t=0), #otherwise it adds too much space
    panel.background = element_rect(fill="grey90", color="grey90"),
    strip.text.y = element_blank(), 
    legend.position = "none") 
  
pHS_avg <- df_Havg %>% 
  filter(decider == "She decided") %>%
  ggplot(aes(x=estimate,y=gender)) +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y",
             labeller = as_labeller(c(`He decided` = "Avg.\ntheta",
                                      `She decided` = "Avg.\ntheta"))) +
  geom_text(aes(x=0, label=label), position = position_stack(),
            fontface="bold",
            size=3.25) +
  theme_void() +
  theme(
    plot.margin = margin(l=0, r=0, b=0, t=0), #otherwise it adds too much space
    panel.background = element_rect(fill="grey90", color="grey90"),
    strip.text.y = element_blank(), 
    legend.position = "none") 

## Create LOW stakes stacked bar plots -----------------------------------------
df_Lbar <- data_fig5 %>%
  filter(earner != "All earners" & fair  == "Fair" & gender != "All" &
           stakes == "Low") %>%
  # Updates the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) 


pLH_bar <- df_Lbar %>%
  filter(decider == "He decided") %>%
  ggplot(aes(x = estimate, y = gender, fill = forcats::fct_rev(earner))) +
  geom_col(width = 0.9, position=position_stack()) +
  geom_text(aes(label = label,
                color = earner == "Men higher-earner"), size=2.5, 
            position = position_stack(vjust = .5)) +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y") +
  theme_minimal(12) +
  theme(strip.text.y.left   = element_text(angle = 0),
        axis.text.x         = element_blank(),
        panel.grid          = element_blank(),
        strip.placement.y   = "outside",
        legend.position     = "bottom",
        plot.title.position = "plot",
        plot.subtitle       = element_text(face = "bold"),
        plot.margin = margin(t = 0,    # Top margin
                             r = 0,    # Right margin
                             b = 0,    # Bottom margin
                             l = 0)) + # Left margin
  xlim(0, .85) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_grey(name = " ") +
  labs(subtitle    = "Low-stakes", 
       x        = " ", 
       y        = " ") 

pLS_bar <- df_Lbar %>%
  filter(decider == "She decided") %>%
  ggplot(aes(x = estimate, y = gender, fill = forcats::fct_rev(earner))) +
  geom_col(width = 0.9, position=position_stack()) +
  geom_text(aes(label = label,
                color = earner == "Men higher-earner"), size=2.5, 
            position = position_stack(vjust = .5)) +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y") +
  theme_minimal(12) +
  theme(strip.text.y        = element_blank(),
        axis.text.x         = element_blank(),
        axis.text.y         = element_blank(), # remove this for pHS
        panel.grid          = element_blank(),
        legend.position     = "bottom",
        plot.margin = margin(t = 0,    # Top margin
                             r = 0,    # Right margin
                             b = 0,    # Bottom margin
                             l = 0)) + # Left margin
  xlim(0, .85) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_fill_grey(name = " ") +
  labs(title    = " ", 
       x        = " ", 
       y        = " ") 

## Create average theta strip for LOW stakes plots -----------------------------
df_Lavg <- data_fig5 %>%
  filter(earner == "All earners" & fair  == "Fair" & gender == "All" &
           stakes == "Low") %>%
  # Updates the factor levels
  mutate(topic = factor(topic,
                        levels = c("Accommodate",
                                   "Balanced Sacrifice",
                                   "Happy Wife, Happy Life",
                                   "Decision History",
                                   "Consensus",
                                   "Man Has Final Say",
                                   "Money Matters")))

pLH_avg <- df_Lavg %>% 
  filter(decider == "He decided") %>%
  ggplot(aes(x=estimate,y=gender)) +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y",
             labeller = as_labeller(c(`He decided` = "Avg.\ntheta",
                                      `She decided` = "Avg.\ntheta"))) +
  geom_text(aes(x=0, label=label), position = position_stack(),
            fontface="bold",
            size=3.25) +
  theme_void() +
  theme(
    plot.margin = margin(l=0, r=0, b=0, t=0), #otherwise it adds too much space
    panel.background = element_rect(fill="grey90", color="grey90"),
    strip.text.y = element_blank(), 
    legend.position = "none") 

pLS_avg <- df_Lavg %>% 
  filter(decider == "She decided") %>%
  ggplot(aes(x=estimate,y=gender)) +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y",
             labeller = as_labeller(c(`He decided` = "Avg.\ntheta",
                                      `She decided` = "Avg.\ntheta"))) +
  geom_text(aes(x=0, label=label), position = position_stack(),
            fontface="bold",
            size=3.25) +
  theme_void() +
  theme(
    plot.margin = margin(l=0, r=0, b=0, t=0), #otherwise it adds too much space
    panel.background = element_rect(fill="grey90", color="grey90"),
    strip.text.y = element_blank(), 
    legend.position = "none") 


# Combine HIGH & LOW stakes charts ---------------------------------------------
layout <- c(
  area(l=0,  r=45, t=0, b=1), # defines the main figure area
  area(l=45, r=49, t=0, b=1), # defines the gap figure area
  area(l=51, r=96, t=0, b=1), 
  area(l=96, r=97, t=0, b=1))

# Show the layout to make sure it looks as it should
plot(layout)

## Combine HIGH stakes charts 
p3_high <- pHH_bar + pHH_avg + pHS_bar + pHS_avg + 
  plot_layout(design = layout) 

p3_high

## Combine LOW stakes charts 
p4_low <- pLH_bar + pLH_avg + pLS_bar + pLS_avg +
  plot_layout(design = layout, guides = "collect") & theme(legend.position = "bottom")

p4_low

## Combine all plots
fig5 <- p3_high / p4_low +
  plot_annotation(
    title = "Predicted topic prevalence for decisions rated as fair",
    subtitle = "by decision type, vignette decision-maker gender and relative income, and respondent gender")

fig5

ggsave(filename = file.path(figDir, "fig5.png"), fig5, 
       width=9, height=7, units="in", dpi=300, bg = "white")


################################################################################

# Wrangle the data some more
df5 <- data_fig5 %>%
  filter(gender != "All") %>%
  pivot_wider(names_from = gender, 
              values_from = c(estimate, std.error, statistic, p.value, s.value, 
                              conf.low, conf.high)) %>%
  # compute the gap
  mutate(gap=round(estimate_Men-estimate_Women, digits = 2)) %>% 
  # find the maximum value by decider
  group_by(fair, stakes, topic, earner, decider) %>%
  mutate(max=max(estimate_Men, estimate_Women),
         gap_gender_max=if_else(estimate_Men==max, "M","W"),
         gap_label = paste0("+", abs(gap), gap_gender_max) %>% 
           #turns into factor to bake in the order
           fct_inorder()) %>% 
  ungroup() %>%
  pivot_longer(
    cols = estimate_Men:conf.high_Women,
    names_to = c("statistic", "gender"),
    names_sep = "_",
    values_to = "value") %>%
  pivot_wider(names_from = statistic,
              values_from = value) %>%
  mutate(estimate = round(estimate, digits =2),
         max = round(max, digits = 2),
         gender = fct_rev(gender))




df5 %>%
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