# Restore the object
## generated from line 358 in FS_03_qual analyses.R
##lcadata <- readRDS(file = file.path(outDir,"lcadataMultinomTopics.rds"))


################################################################################
# Topic Modeling Regression Tables
################################################################################

# Predict each topic -----------------------------------------------------------

## List of topics
dv <- names(c(select(data_lca %>% ungroup(), starts_with("t_"))))

## Create pdata frames
pdata_m1 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Man higher-earner"),   
                        index = c("CaseID"))
pdata_m2 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Woman higher-earner"), 
                        index = c("CaseID"))
pdata_m3 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Equal earners"),       
                        index = c("CaseID"))

## Create the plm functions
plms_mhe <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m1, model = "within")
}

plms_whe <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m2, model = "within")
}

plms_ee  <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m3, model = "within")
}

## Run the fixed effects models (loop over topics as DVs)
fe_mhe   <- lapply(dv, plms_mhe) # Man higher-earner
fe_whe   <- lapply(dv, plms_whe) # Woman higher-earner
fe_ee    <- lapply(dv, plms_ee)  # Equal earners

mods_mhe <- list(fe_mhe[[1]], fe_mhe[[2]], fe_mhe[[3]], fe_mhe[[4]],
                 fe_mhe[[5]], fe_mhe[[6]], fe_mhe[[7]])

mods_whe <- list(fe_whe[[1]], fe_whe[[2]], fe_whe[[3]], fe_whe[[4]],
                 fe_whe[[5]], fe_whe[[6]], fe_whe[[7]])

mods_ee  <- list(fe_ee[[1]],  fe_ee[[2]],  fe_ee[[3]],  fe_ee[[4]],
                 fe_ee[[5]],  fe_ee[[6]],  fe_ee[[7]])

## Average Marginal Effects of the models

### Create the function
give_me_ame <- function(model){
  avg_slopes(model, variables = c("dum"), by = c("decision", "per"))
}

#### estimate the AMEs
ame_mhe  <- lapply(mods_mhe, give_me_ame) 
ame_whe  <- lapply(mods_whe, give_me_ame) 
ame_ee   <- lapply(mods_ee,  give_me_ame) 

#### add relinc indicator
ame_mhe<- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
       ame_mhe, "Men higher-earner", SIMPLIFY = FALSE)

ame_whe<- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                 ame_whe, "Woman higher-earner", SIMPLIFY = FALSE)

ame_ee<- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                 ame_ee, "Equal earner", SIMPLIFY = FALSE)

# Appendix Table A4 ------------------------------------------------------------
## Relationship of Fairness Rating to Topic Prevalence by Decision-Maker Gender, 
## Decision Type, and Vignette Couple’s Relative Income

p1 <- modelsummary(ame_mhe, shape = decision + model ~ relinc + per,
             gof_map = NA, output = "huxtable") 
p2 <- modelsummary(ame_whe, shape = decision + model ~ relinc + per,
                   gof_map = NA, output = "huxtable")
p3 <- modelsummary(ame_ee, shape = decision + model ~ relinc + per,
                   gof_map = NA, output = "huxtable")
data_tableA4 <-  cbind(p1, p2, p3)


## control docx formatting output
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar())


data_tableA4 %>%
  select(c("decision", "  ", "Men higher-earner / 0", "Men higher-earner / 1", 
           "Woman higher-earner / 0", "Woman higher-earner / 1",
           "Equal earner / 0", "Equal earner / 1")) %>%
  insert_row(c(" ", " ", 
               "Anthony\nDecides", "Michelle\nDecides",
               "Anthony\nDecides", "Michelle\nDecides", 
               "Anthony\nDecides", "Michelle\nDecides"), after = 1)  %>%
  huxtable::as_flextable() %>%
  delete_rows(i = 1, part = "body") %>%
  add_header_row(values = c("Decision", "Topic", "Men higher-earner", 
                            "Women higher-earner", "Equal earners"),
                 colwidths = c(1, 1, 2, 2, 2), top = TRUE) %>%
  flextable::align(align = "center", part = "header") %>%
  add_footer_lines("Notes: N=7,956 person-decisions. Coefficients are the marginal effects of perceiving a decision as fair on topic prevalence (theta) in respondents' open-ended explanations, calculated from respondent-level fixed effects models with interaction between decision-maker gender and perception of fairness. Independent models applied by relative income. * p < .05, ** p < .01, *** p < .001; 2 tailed tests. Standard errors in parentheses.") %>%
  set_table_properties(layout = "autofit") %>%
  save_as_docx(path = file.path(outDir, "finalsay_tableA4.docx"))


# Figure 4 ---------------------------------------------------------------------

#### put them into a dataframe
df_mhe <- bind_rows(ame_mhe, .id = "topic")
df_whe <- bind_rows(ame_whe, .id = "topic")
df_ee  <- bind_rows(ame_ee,  .id = "topic")

data_ame <- rbind(df_mhe, df_whe, df_ee)

data_fig4 <- data_ame %>% # label topics
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
      per   == 1   ~ "She decided",
      per   == 0   ~ "He decided"),
    earner = fct_case_when(
      relinc == "Men higher-earner"    ~ "Men higher-earner",
      relinc == "Women higher-earner"  ~ "Women higher-earner",
      relinc == "Equal earner"         ~ "Equal earners"))

p1 <- data_fig4 %>%
  filter(decision == "high") %>%
  ggplot(aes(x = estimate, y = decider, fill = decider)) +
  geom_col(width = 0.8, position = position_dodge(0.7)) +
  geom_text(aes(x = estimate + .01 * sign(estimate), 
                label = ifelse(p.value < .05, "*", " ")), 
            position = position_dodge(width = 0.9), 
            size = 3.5 , angle = 90) +
  facet_grid(reorder(topic, -estimate) ~ earner,
             space = "free",
             switch = "y") +
  theme_minimal(13) +
  scale_fill_grey(name = " ") +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  scale_x_continuous(limits = c(-.22, .15)) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        axis.text.x=element_blank(),
        legend.position     = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( title = "Average marginal effects of topic prevalence\nby vignette couple's relative income, decision-maker gender, and decision type",
        x        = " ", 
        y        = " ",
        subtitle = "High-stakes decisions")

p2 <- data_fig4 %>%
  filter(decision == "low") %>%
  ggplot(aes(x = estimate, y = decider, fill = decider)) +
  geom_col(width = 0.8, position = position_dodge(0.7)) +
  geom_text(aes(x = estimate + .01 * sign(estimate), 
                label = ifelse(p.value < .05, "*", " ")), 
            position = position_dodge(width = 0.9), 
            size = 3.5 , angle = 90) +
  facet_grid(reorder(topic, -estimate) ~ earner,
             space = "free",
             switch = "y") +
  theme_minimal(13) +
  scale_fill_grey(name = " ") +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  scale_x_continuous(limits = c(-.22, .15)) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        strip.text.x = element_blank(),
        legend.position     = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( x        = " ", 
        y        = " ",
        subtitle = "Low-stakes decisions")

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1, g2, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths)
grid.newpage()
grid.draw(g)

### save Figure 4
png(file.path(figDir, "fig4.png"), 
    width = 850, height = 580, pointsize=16) 
grid.draw(g) 
dev.off()


# Not working

ames <- c(ame_mhe, ame_whe, ame_ee)

topics <- list("t1" = ames[[1]],  "t2" = ames[[2]],  "t3" = ames[[3]],
               "t4" = ames[[4]],  "t5" = ames[[5]],  "t6" = ames[[6]],  "t7"  = ames[[7]],
               "t1" = ames[[8]],  "t2" = ames[[9]],  "t3" = ames[[10]],
               "t4" = ames[[11]], "t5" = ames[[12]], "t6" = ames[[13]], "t7" = ames[[14]],
               "t1" = ames[[15]], "t2" = ames[[16]], "t3" = ames[[17]],
               "t4" = ames[[18]], "t5" = ames[[19]], "t6" = ames[[20]], "t7" = ames[[21]])

modelsummary(topics, shape = decision + per ~ relinc + model,
             gof_map = NA, output = "gt") 



# For plotting

#### put them into a dataframe
df_mhe <- bind_rows(ame_mhe, .id = "topic")
df_whe <- bind_rows(ame_whe, .id = "topic")
df_ee  <- bind_rows(ame_ee,  .id = "topic")

df_mhe$relinc <- "Man higher-earner"
df_whe$relinc <- "Woman higher-earner"
df_ee$relinc  <- "Equal earners"

data_ame <- rbind(df_mhe, df_whe, df_ee)


# ------------------------------------------------------------------------------
# not using but might be useful if something doesn't work out.


dv = (c("t_1", "t_2"))

for(i in 1:length(dv)){
  model <- paste("model",i, sep="")       # create an object to hold the models
  m <- plm(as.formula(paste(dv[i],"~ dum + per + decision + (dum * per) + (dum * decision) + 
                              (per * decision) + (dum * per * decision)")), 
           data = pdata_m1, model = "within")
  assign(model,m)                        # assign the model m to the model object 
}