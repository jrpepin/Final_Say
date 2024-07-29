

# Topic 1
ggpredict(fe_mheM[[1]], terms = c("dum", "per", "decision")) 


### Create the function
pp <- function(model){
  ggpredict(model, terms = c("dum", "per", "decision"))
}

#### estimate the predicted probabilities
pp_mheM  <- lapply(fe_mheM, pp) # men-higher-earner R=Man
pp_mheW  <- lapply(fe_mheW, pp) # men-higher-earner R=Woman

pp_wheM  <- lapply(fe_wheM, pp) # women-higher-earner R=Man
pp_wheW  <- lapply(fe_wheW, pp) # women-higher-earner R=Woman

pp_eeM  <- lapply(fe_eeM, pp)   # equal earners R=Man
pp_eeW  <- lapply(fe_eeW, pp)   # equal earners R=Woman


#### add relinc indicator
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
df_mheM <- bind_rows(pp_mheM, .id = "topic")
df_wheM <- bind_rows(pp_wheM, .id = "topic")
df_eeM  <- bind_rows(pp_eeM,  .id = "topic")

df_mheW <- bind_rows(pp_mheW, .id = "topic")
df_wheW <- bind_rows(pp_wheW, .id = "topic")
df_eeW  <- bind_rows(pp_eeW,  .id = "topic")


data_fig5 <- as_tibble(rbind(df_mheM, df_wheM, df_eeM, df_mheW, df_wheW, df_eeW))

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
      group == 1   ~ "She decided",
      group == 0   ~ "He decided"),
    fair = fct_case_when(
      x     == 0   ~ "Unfair",
      x     == 1   ~ "Fair"),
    stakes = fct_case_when(
      facet == "high" ~ "High",
      facet == "low"  ~ "low"),
    earner = fct_case_when(
      relinc == "Men higher-earner"    ~ "Men higher-earner",
      relinc == "Women higher-earner"  ~ "Women higher-earner",
      relinc == "Equal earner"         ~ "Equal earners")) %>%
  select(!c(x, group, facet))

