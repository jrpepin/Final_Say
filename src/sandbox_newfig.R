

df <- data_fig5 %>%
  filter(gender != "All" & earner != "All earners") %>%
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
  ggplot(aes(x = estimate, y = decider, color = forcats::fct_rev(earner), position_dodge(.5))) +
  geom_line(aes(group=decider), color="#E7E7E7", linewidth=3.5) + 
  geom_point(aes(color=gender), size=3) +
  facet_grid(rows   = vars(reorder(topic, -estimate)),  
             cols   = vars(earner), 
             space  = "free",
             switch = "y") +
  # data callout
  geom_text(data = df %>% filter(fair  == "Fair" & stakes == "High" &
                                   estimate == max), aes(label=estimate, color=gender),
            size=3.25,
            nudge_x= .02) +
  theme_minimal(13) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        #        axis.text.y         = element_blank(),
        strip.text.y        = element_blank(),
        legend.position     = "none") +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_color_grey(name = " ") +
  labs(title     = "Predicted topic prevalence for decisions rated as fair by decision type, vignette couples' relative income\nand decision-maker gender, and respondent gender\n ",
       x        = " ", 
       y        = " ",
       subtitle = "High-stakes decisions")