

### Create the function
pp <- function(model){
  ggpredict(model, terms = c("dum", "per", "decision"))
}

#### estimate the predicted probabilities
pp_mhe   <- lapply(fe_mhe,  pp) # men-higher-earner R=ALL
pp_mheM  <- lapply(fe_mheM, pp) # men-higher-earner R=Man
pp_mheW  <- lapply(fe_mheW, pp) # men-higher-earner R=Woman

pp_whe   <- lapply(fe_whe,  pp) # women-higher-earner R=ALL
pp_wheM  <- lapply(fe_wheM, pp) # women-higher-earner R=Man
pp_wheW  <- lapply(fe_wheW, pp) # women-higher-earner R=Woman

pp_ee   <- lapply(fe_ee,  pp)   # equal earners R=ALL
pp_eeM  <- lapply(fe_eeM, pp)   # equal earners R=Man
pp_eeW  <- lapply(fe_eeW, pp)   # equal earners R=Woman


#### add relinc indicator
pp_mhe  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_mhe,  "Men higher-earner",   SIMPLIFY = FALSE)

pp_whe  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_whe,  "Women higher-earner", SIMPLIFY = FALSE)

pp_ee  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                 pp_ee,    "Equal earner",       SIMPLIFY = FALSE)

pp_mheM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   pp_mheM, "Men higher-earner",   SIMPLIFY = FALSE)

pp_wheM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   pp_wheM, "Women higher-earner", SIMPLIFY = FALSE)

pp_eeM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_eeM,   "Equal earner",       SIMPLIFY = FALSE)

pp_mheW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   pp_mheW, "Men higher-earner",   SIMPLIFY = FALSE)

pp_wheW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   pp_wheW, "Women higher-earner", SIMPLIFY = FALSE)

pp_eeW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_eeW,   "Equal earner",       SIMPLIFY = FALSE)


#### add R gender indicator
pp_mhe  <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_mhe,  "All",   SIMPLIFY = FALSE)

pp_whe  <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_whe,  "All",   SIMPLIFY = FALSE)

pp_ee  <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                 pp_ee,    "All",   SIMPLIFY = FALSE)

pp_mheM <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_mheM, "Men",   SIMPLIFY = FALSE)

pp_wheM <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_wheM, "Men",   SIMPLIFY = FALSE)

pp_eeM <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                 pp_eeM,   "Men",   SIMPLIFY = FALSE)

pp_mheW <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_mheW, "Women", SIMPLIFY = FALSE)

pp_wheW <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_wheW, "Women", SIMPLIFY = FALSE)

pp_eeW <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                 pp_eeW,   "Women", SIMPLIFY = FALSE)

#### put them into a dataframe
df_mhe  <- bind_rows(pp_mhe,  .id = "topic")
df_whe  <- bind_rows(pp_whe,  .id = "topic")
df_ee   <- bind_rows(pp_ee,   .id = "topic")

df_mheM <- bind_rows(pp_mheM, .id = "topic")
df_wheM <- bind_rows(pp_wheM, .id = "topic")
df_eeM  <- bind_rows(pp_eeM,  .id = "topic")

df_mheW <- bind_rows(pp_mheW, .id = "topic")
df_wheW <- bind_rows(pp_wheW, .id = "topic")
df_eeW  <- bind_rows(pp_eeW,  .id = "topic")


data_fig5 <- as_tibble(rbind(df_mhe, df_whe, df_ee, df_mheM, df_wheM, df_eeM, df_mheW, df_wheW, df_eeW))

data_fig5 <- data_fig5 %>% 
  mutate( 
    topic = fct_case_when(
      topic == "3" ~ "Practical Efficiency",
      topic == "1" ~ "Give & Take",
      topic == "6" ~ "Money Matters",
      topic == "7" ~ "Work Together",
      topic == "5" ~ "Taking Turns",
      topic == "2" ~ "Man Has Final Say",
      topic == "4" ~ "Happy Wife, Happy Life"),
    decider = fct_case_when(
      group == 0   ~ "He decided",
      group == 1   ~ "She decided"),
    fair = fct_case_when(
      x     == 1   ~ "Fair",
      x     == 0   ~ "Unfair"),
    newest = case_when(
      fair == "Fair"   ~ predicted,
      fair == "Unfair" ~ -predicted),
    stakes = fct_case_when(
      facet == "high" ~ "High",
      facet == "low"  ~ "Low"),
    earner = fct_case_when(
      relinc == "Men higher-earner"    ~ "Men higher-earner",
      relinc == "Women higher-earner"  ~ "Women higher-earner",
      relinc == "Equal earner"         ~ "Equal earners")) %>%
  select(!c(x, group, facet))


data_fig5 %>%
  filter(gender == "All" & stakes == "High") %>%
  ggplot(aes(x = predicted, y = decider, fill = fair)) +
  geom_col(width = 0.8, position="stack") +
#  geom_point() +
  facet_grid(rows   = vars(topic),
             cols   = vars(earner),
             space  = "free",
             switch = "y") +
  theme_minimal(13) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        strip.text.y        = element_blank(),
        legend.position     = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_grey()

## clear that bars add to 1, but unreadable in black & white
data_fig5 %>%
  filter(gender == "All") %>%
  ggplot(aes(x = predicted, y = decider, fill = topic)) +
  geom_col(width = 0.8, position="stack") +
  #  geom_point() +
  facet_grid(rows   = vars(stakes, fair),
             cols   = vars(earner),
             space  = "free",
             switch = "y") +
  theme_minimal(13) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        strip.text.y        = element_blank(),
        legend.position     = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_grey()


data_fig5 %>%
  filter(gender == "All" & fair == "Fair") %>%
  ggplot(aes(x = predicted, y = earner)) +
  geom_line() +
  geom_point(aes(color = decider), size = 2) +
  facet_grid(rows   = vars(topic),
             cols   = vars(stakes),
             space  = "free",
             switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Predicted probability of topic usage for high and low stakes decisions",
        subtitle = "by vignette gender and relative earnings") +
  theme_minimal() +
  theme(axis.text.x         = element_text(size = 10),
        strip.text.x        = element_text(face = "bold", size = 10),
        strip.text.y        = element_text(angle = 0, size = 10),
        strip.text.y.left   = element_text(angle = 0),
        axis.title          = element_text(size = 10), 
        axis.text           = element_text(size = 8), 
        plot.margin         = unit(c(.1,.5,.1,.5),"cm"),
        legend.position     = "top",
        plot.title.position = "plot",
        axis.line           = element_line(size = 4, colour = "white"),
        panel.spacing       = unit(.5, "lines"),
        panel.grid.major.y  = element_line(colour="#f2f2f2", size=7),
        panel.grid.minor.x  = element_blank(),
        panel.border        = element_blank())
  
