#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_03_qual analyses.R
# William J. Scarborough & Joanna R. Pepin
#------------------------------------------------------------------------------------

# This file analyzes the qualitative responses.
summary(qualdataFULL)

# generating corpus
Corpusprep <-data.frame(
  doc_id=qualdataFULL$longid,
  text=qualdataFULL$qual
  ,stringsAsFactors=F)
str(Corpusprep)
corpus = VCorpus(DataframeSource(Corpusprep))

# cleaning corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, 
                 c("house", "apartment", "apt", "apt.", 
                   "mattress", "sleep", "bed", 
                   "vacation", "travel", 
                   "weekend", "activities", "activity", 
                   "movie", "movies", "see", "watch", 
                   "anthony", "anthonys", "michelle", "michelles", "someone",
                   "decision", "decisions", "decide", "decides", "decided", 
                   "make", "makes",
                   "one", "like", "likes", "liked", "want", "wants",
                   "opinion",
                   "get", "just", "can", "cant" , 
                   "making",
                   "made", 
                   "doesnt", "dont", 
                   "since", "think", "thinks", "thinking", 
                   "maybe", 
                   "need", "needs",
                   "agree", "agrees", "agreement",   
                   "fair", "unfair" 
                 ))

corpus <- tm_map(corpus, removeWords, stopwords::stopwords('english')) 

corpusSTEMMED <- tm_map(corpus, stemDocument)

writeLines(as.character(corpus[[15]])) # used for text analysis
writeLines(as.character(corpusSTEMMED [[15]])) # used for text analysis
qualdataFULL$qual[15] # compared to this for check 

#document-term matrix
mat <- DocumentTermMatrix(corpus)
matSTEMMED <- DocumentTermMatrix(corpusSTEMMED )
matSTEMMED

#document-term matrix to different format for fitlda
m <-  Matrix::sparseMatrix(i=mat$i, 
                           j=mat$j, 
                           x=mat$v, 
                           dims=c(mat$nrow, mat$ncol),
                           dimnames = mat$dimnames)
mSTEMMED <-  Matrix::sparseMatrix(i=matSTEMMED $i, 
                                  j=matSTEMMED $j, 
                                  x=matSTEMMED $v, 
                                  dims=c(matSTEMMED $nrow, matSTEMMED $ncol),
                                  dimnames = matSTEMMED $dimnames)

################################################################################
# Fitting Model
################################################################################
# Fit a 1 through 20 LDA models and Compare Coherence
k_list <- seq(1,20, by=1)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  set.seed(376)
  lda <- FitLdaModel(dtm = mSTEMMED ,
                     k = k, 
                     iterations = 1000, 
                     burnin = 100,
                     optimize_alpha = TRUE, 
                     beta = .05, 
                     calc_likelihood = FALSE,
                     calc_coherence = TRUE,
                     calc_r2 = FALSE,
                     cpus = 16)
  lda$k <- k
  lda
}, export= ls(), # c("m"), 
cpus = 4) 

# Get average coherence for each model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) 
                              mean(x$coherence)), 
                            stringsAsFactors = FALSE)
coherence_mat
plot(coherence_mat, type = "o")
write_xlsx(coherence_mat, path = file.path(outDir, "coherence.xlsx"))


# Figure 1. Coherence plot -----------------------------------------------------

fig1 <- coherence_mat %>%
  ggplot(aes(x = k, y = coherence)) +
  geom_line() +
  geom_point() +
  geom_point(data=coherence_mat %>% filter(k == 7),
             pch=21,
             size=4) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:20) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs( x        = "Number of Topics", 
        y        = "Coherence", 
        title    = "Average probabilistic coherence for 1-20 topics",
        caption  = "Note: The circled point represents the selected LDA model with seven topics.") 

fig1

ggsave(filename = file.path(figDir, "fig1.png"), fig1, 
       width=6, height=4, units="in", dpi=300, bg = 'white')


## Selected 7 Topic Model ------------------------------------------------------
set.seed(376)
model <- FitLdaModel(dtm             = mSTEMMED, 
                     k               = 7,    
                     iterations      = 1000, 
                     burnin          = 100, 
                     optimize_alpha  = TRUE,
                     beta            = .05, 
                     calc_likelihood = FALSE,
                     calc_coherence  = TRUE,
                     calc_r2         = FALSE,
                     cpus            = 16)

coherence = round(model$coherence, 3)
coherence
names(model)
mean(model$coherence)


## Assigning probabilities of topics to observations (theta)
set.seed(376)
theta <- data.frame(predict(model, mSTEMMED,
                            iterations = 1000, 
                            burnin     = 100))   %>%
  tibble::rownames_to_column("longid") %>%
  mutate(longid = as_numeric(longid))

head(theta) # probability

## create wide data
lcadata <- left_join(qualdataFULL, theta) %>%
  select(-c(qual, wN, same, topic, fair)) %>%
  pivot_wider(names_from  = x, 
              values_from = c(t_1, t_2, t_3, t_4, t_5, t_6, t_7))


lcadata <- left_join(lcadata, data) ## Join tables

## Need matched data
tally <- lcadata %>% 
  group_by(CaseID) %>%
  tally() 

lcadata <- left_join(lcadata, tally)

### Function to drop opposite decision variables
not_all_na <- function(x) any(!is.na(x))

### Decision 1
lca1 <- lcadata               %>%
  group_by(CaseID)            %>%
  filter(n == 2)              %>%    # keep matched decisions
  mutate(row = row_number())  %>%
  filter(row==1)              %>%    # keep decision 1 rows
  select(where(not_all_na))   %>%    # keep decision 1 variables
  select(!c(fair2, qual2, 
            aperson, dum2, 
            per2))     %>%
  rename(fair     = fair1,
         qual     = qual1,
         person   = iperson,
         dum      = dum1,
         per      = per1)    %>%
  mutate(decision = "high")

#### Remove everything from the second underscore onwards
colnames(lca1) <- sub("(_[^_]+){1}$", "", colnames(lca1))

### Decision 2
lca2 <- lcadata               %>%
  group_by(CaseID)            %>%
  filter(n == 2)              %>%    # keep matched decisions
  mutate(row = row_number())  %>%
  filter(row==2)              %>%    # keep decision 2 rows
  select(where(not_all_na))   %>%    # keep decision 2 variables
  select(!c(fair1, qual1,
            iperson, dum1, 
            per1))     %>%
  rename(fair     = fair2,
         qual     = qual2,
         person   = aperson,
         dum      = dum2,
         per      = per2)    %>%
  mutate(decision = "low")

#### Remove everything from the second underscore onwards
colnames(lca2) <- sub("(_[^_]+){1}$", "", colnames(lca2))

# Append data frames together
data_lca <- rbind(lca1, lca2)  # 7434 matched person decisions

data_lca$decision <- as.factor(data_lca$decision)

## Arrange column and row order
data_lca <- data_lca                  %>% 
  select(CaseID, decision, fair, dum, per, qual, starts_with("t_"), 
         everything())                %>%
  mutate(CaseID = as.numeric(CaseID)) %>%
  arrange(CaseID)                     %>%
  mutate(CaseID = as.character(CaseID))

## Get topic frequency -- Average theta!
data_lca %>%
  ungroup %>%
  summarise_at(vars(t_1:t_7), mean, na.rm = TRUE)

## Table of top words
model$top_terms  <- GetTopTerms(phi = model$phi, M = 25)
t(model$top_terms)

topterms <- data.frame(model$top_terms) %>%
  rownames_to_column("rank") %>%
  pivot_longer(cols = starts_with("t_"), 
               names_to = "topic", values_to = "word")

## Table of Phi, which is where top words come from.
phi <- data.frame(model$phi)

phi <- data.frame(t(phi[-1])) %>%
  rownames_to_column("word")  %>%
  pivot_longer(!word, names_to = "topic", values_to = "phi")


# Figure 3. Top Words ----------------------------------------------------------

## Combine topterms and phi values
data_fig3 <- left_join(topterms, phi) %>%
  dplyr::arrange(desc(phi))

### topic order is based on Average theta

### function to keep factor order
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
} 

data_fig3 <- data_fig3 %>% # label topics
  mutate(topic = fct_case_when(
    topic == "t_3" ~ "Topic 1:\nPractical Efficiency (.19)",
    topic == "t_1" ~ "Topic 2:\nGive & Take (.18)",
    topic == "t_6" ~ "Topic 3:\nMoney Matters (.16)",
    topic == "t_7" ~ "Topic 4:\nWork Together (.16)",
    topic == "t_5" ~ "Topic 5:\nTaking Turns (.12)",
    topic == "t_2" ~ "Topic 6:\nMan Has Final Say (.10)",
    topic == "t_4" ~ "Topic 7:\nHappy Wife, Happy Life (.09)"), ordered = F)


## Create bargraphs
fig3 <- data_fig3 %>%
  filter(as.numeric(rank) < 11) %>%
  filter(!is.na(phi)) %>%
  ggplot(aes(x = phi, y = reorder_within(word, phi, topic))) +
  geom_col(width = 0.6) +
  facet_wrap(~ topic, ncol = 2,
             scales="free_y") +
  scale_fill_grey() +
  scale_y_reordered() +
  scale_x_continuous(breaks = c(".000" = 0.000, ".025" = 0.025, 
                                ".050" = 0.050, ".075" = 0.075)) +
  theme_minimal(12) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Highest-ranking word stems per topic",
        subtitle = "Probability of being found in topic (phi)")

fig3

ggsave(file.path(figDir, "fig3.png"), fig3, 
       height = 8, width = 6, units="in",dpi = 300, bg = 'white')



################################################################################
# Topic Modeling Regression Tables
################################################################################

# Predict each topic -----------------------------------------------------------

## List of topics
dv <- names(c(select(data_lca %>% ungroup(), starts_with("t_"))))

## Prepare data for plm

### All
pdata_m1 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Man higher-earner"),   
                        index = c("CaseID"))
pdata_m2 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Woman higher-earner"), 
                        index = c("CaseID"))
pdata_m3 <- pdata.frame(data_lca %>% 
                          filter(relinc == "Equal earners"),       
                        index = c("CaseID"))

### Men
pdata_m1M <- pdata.frame(data_lca %>% 
                          filter(relinc == "Man higher-earner"   & gender == "Male"),   
                        index = c("CaseID"))
pdata_m2M <- pdata.frame(data_lca %>% 
                          filter(relinc == "Woman higher-earner" & gender == "Male"), 
                        index = c("CaseID"))
pdata_m3M <- pdata.frame(data_lca %>% 
                          filter(relinc == "Equal earners"       & gender == "Male"),       
                        index = c("CaseID"))

### Women
pdata_m1W <- pdata.frame(data_lca %>% 
                           filter(relinc == "Man higher-earner"   & gender == "Female"),   
                         index = c("CaseID"))
pdata_m2W <- pdata.frame(data_lca %>% 
                           filter(relinc == "Woman higher-earner" & gender == "Female"), 
                         index = c("CaseID"))
pdata_m3W <- pdata.frame(data_lca %>% 
                           filter(relinc == "Equal earners"       & gender == "Female"),       
                         index = c("CaseID"))

## Create the plm functions

### All
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

### Men
plms_mheM <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m1M, model = "within")
}

plms_wheM <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m2M, model = "within")
}

plms_eeM  <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m3M, model = "within")
}

### Women
plms_mheW <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m1W, model = "within")
}

plms_wheW <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m2W, model = "within")
}

plms_eeW  <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m3W, model = "within")
}


## Run the fixed effects models (loop over topics as DVs)
fe_mhe    <- lapply(dv, plms_mhe)  # Man higher-earner
fe_whe    <- lapply(dv, plms_whe)  # Woman higher-earner
fe_ee     <- lapply(dv, plms_ee)   # Equal earners

fe_mheM   <- lapply(dv, plms_mheM) # Man higher-earner
fe_wheM   <- lapply(dv, plms_wheM) # Woman higher-earner
fe_eeM    <- lapply(dv, plms_eeM)  # Equal earners

fe_mheW   <- lapply(dv, plms_mheW) # Man higher-earner
fe_wheW   <- lapply(dv, plms_wheW) # Woman higher-earner
fe_eeW    <- lapply(dv, plms_eeW)  # Equal earners


## Average Marginal Effects of the models

### Create the function
give_me_ame <- function(model){
  avg_slopes(model, variables = c("dum"), by = c("decision", "per"))
}

#### estimate the AMEs
ame_mhe  <- lapply(fe_mhe, give_me_ame) 
ame_whe  <- lapply(fe_whe, give_me_ame) 
ame_ee   <- lapply(fe_ee,  give_me_ame) 

ame_mheM <- lapply(fe_mheM, give_me_ame) 
ame_wheM <- lapply(fe_wheM, give_me_ame) 
ame_eeM  <- lapply(fe_eeM,  give_me_ame) 

ame_mheW <- lapply(fe_mheW, give_me_ame) 
ame_wheW <- lapply(fe_wheW, give_me_ame) 
ame_eeW  <- lapply(fe_eeW,  give_me_ame)

#### add relinc indicator
ame_mhe  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_mhe,  "Men higher-earner",   SIMPLIFY = FALSE)

ame_whe  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_whe,  "Women higher-earner", SIMPLIFY = FALSE)

ame_ee   <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_ee,    "Equal earner",       SIMPLIFY = FALSE)

ame_mheM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_mheM, "Men higher-earner",   SIMPLIFY = FALSE)

ame_wheM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_wheM, "Women higher-earner", SIMPLIFY = FALSE)

ame_eeM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_eeM,   "Equal earner",       SIMPLIFY = FALSE)

ame_mheW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_mheW, "Men higher-earner",   SIMPLIFY = FALSE)

ame_wheW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_wheW, "Women higher-earner", SIMPLIFY = FALSE)

ame_eeW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                   ame_eeW,   "Equal earner",       SIMPLIFY = FALSE)

# Figure 4 ---------------------------------------------------------------------

#### put them into a dataframe
df_mhe <- bind_rows(ame_mhe, .id = "topic")
df_whe <- bind_rows(ame_whe, .id = "topic")
df_ee  <- bind_rows(ame_ee,  .id = "topic")

data_fig4 <- as_tibble(rbind(df_mhe, df_whe, df_ee))

data_fig4 <- data_fig4 %>% 
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
  scale_x_continuous(limits = c(-.22, .22)) +
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
  scale_x_continuous(limits = c(-.22, .22)) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        strip.text.x = element_blank(),
        legend.position     = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( x        = " ", 
        y        = " ",
        subtitle = "Low-stakes decisions")

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2) ## this is cutting off data?!?!?!
g <- rbind(g1, g2, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths)
grid.newpage()
grid.draw(g)

### save Figure 4
png(file.path(figDir, "fig4.png"), 
    width = 850, height = 580, pointsize=16) 
grid.draw(g) 
dev.off()


# Figure 5 ---------------------------------------------------------------------

## Create predicted probabilities datesets
pp1M <- ggpredict(plm1M, terms = c("per", "decision"))
pp1M$relinc <- "Man higher-earner"
pp1M$gender <- "Men"
pp1F <- ggpredict(plm1F, terms = c("per", "decision"))
pp1F$relinc <- "Man higher-earner"
pp1F$gender <- "Women"










#### put them into a dataframe -- WRONG -- THIS IS THE AME NOT THE PP
df_mheM <- bind_rows(ame_mheM, .id = "topic")
df_wheM <- bind_rows(ame_wheM, .id = "topic")
df_eeM  <- bind_rows(ame_eeM,  .id = "topic")

df_mheW <- bind_rows(ame_mheW, .id = "topic")
df_wheW <- bind_rows(ame_wheW, .id = "topic")
df_eeW  <- bind_rows(ame_eeW,  .id = "topic")

data_ameM <- rbind(df_mheM, df_wheM, df_eeM)
data_ameW <- rbind(df_mheW, df_wheW, df_eeW)

data_ameM$gender <- "Men"
data_ameW$gender <- "Women"

data_fig5 <- rbind(data_ameM, data_ameW)

data_fig5 <- data_fig5 %>% # label topics
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



################################################################################
# Appendix (quant)
################################################################################

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











## OLD FIGURE 5 ----------------------------------------------------------------

## Create an object with predicted probabilities
### https://community.rstudio.com/t/plotting-confidence-intervals-with-ggeffects-for-multinom/54354
effects.item <- effects::Effect(c("iperson", "relinc"), mn_item)
effects.act  <- effects::Effect(c("aperson", "relinc"), mn_act)

## Generate a tidy data frame with predictions, lower and upper confidence intervals
pur.pp = with(effects.item, cbind(x, prob, upper.prob, lower.prob))  
act.pp = with(effects.act, cbind(x, prob, upper.prob, lower.prob))  

pur.pp <- pur.pp %>%
  pivot_longer(cols = starts_with(c("prob", "U.prob", "L.prob"))) %>%
  separate(name, into=c('type', 'response.level'), sep="prob.") %>%
  mutate(type = case_when(
    type == "U." ~ "conf.high",
    type == "L." ~ "conf.low",
    TRUE         ~ "predicted")) %>%
  mutate(response.level=gsub("\\.", " ", response.level)) %>%
  spread(type, value) %>%
  rename(x = iperson) %>%
  mutate(type = "Purchase")

act.pp <- act.pp %>%
  pivot_longer(cols = starts_with(c("prob", "U.prob", "L.prob"))) %>%
  separate(name, into=c('type', 'response.level'), sep="prob.") %>%
  mutate(type = case_when(
    type == "U." ~ "conf.high",
    type == "L." ~ "conf.low",
    TRUE         ~ "predicted")) %>%
  mutate(response.level=gsub("\\.", " ", response.level)) %>%
  spread(type, value) %>%
  rename(x = aperson) %>%
  mutate(type = "Activity")

data_fig5 <- rbind(pur.pp, act.pp) 

levels(data_fig5$x)[levels(data_fig5$x)=="Michelle"] <- "She decided"
levels(data_fig5$x)[levels(data_fig5$x)=="Anthony"]  <- "He decided"

data_fig5$x        <- factor(data_fig5$x, levels = c("She decided", "He decided"), ordered = FALSE)
data_fig5$type     <- factor(data_fig5$type, levels = c("Purchase", "Activity"), ordered = FALSE)
data_fig5$earnings <- factor(data_fig5$relinc, levels = c("Man higher-earner", "Woman higher-earner", "Equal earners"), ordered = FALSE)

data_fig5$response.level[data_fig5$response.level == "Practical Efficiency"]    <- "Practical\nEfficiency"
data_fig5$response.level[data_fig5$response.level == "Give & Take"]    <- "Assured\nAcquiescence"
data_fig5$response.level[data_fig5$response.level == "Money Matters"]           <- "Money\nMatters"
data_fig5$response.level[data_fig5$response.level == "Work Together"]           <- "Work\nTogether"
data_fig5$response.level[data_fig5$response.level == "Taking Turns"]            <- "Taking\nTurns"
data_fig5$response.level[data_fig5$response.level == "Man Has Final Say"]       <- "Man Has\nFinal Say"
data_fig5$response.level[data_fig5$response.level == "Happy Wife Happy Life"]   <- "Happy Wife\nHappy Life"

data_fig5$response.level <- factor(data_fig5$response.level, 
                                    levels = c("Practical\nEfficiency", 
                                               "Assured\nAcquiescence", 
                                               "Money\nMatters", 
                                               "Work\nTogether",
                                               "Taking\nTurns", 
                                               "Man Has\nFinal Say", 
                                               "Happy Wife\nHappy Life"))

p1_fig5 <- data_fig5 %>%
  filter(response.level == "Practical\nEfficiency" | response.level == "Assured\nAcquiescence" |
         response.level == "Money\nMatters" | response.level =="Work\nTogether") %>%
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 2.5, shape=21, alpha = 0.7) +
  facet_grid(response.level ~ type , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, .5), breaks = c(.0, .25, .5), labels = c(".0", ".25", ".5")) +
  scale_fill_grey() +
  #  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme(axis.text.x         = element_text(size = 10),
        strip.text.x        = element_text(face = "bold", size = 10),
        strip.text.y        = element_text(angle = 0, size = 10, face = "bold"),
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
        panel.border        = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Predicted probability of topic usage for purchase and activity decisions",
        subtitle = "by vignette gender and relative earnings")

p1_fig5


p2_fig5 <- data_fig5 %>%
  filter(response.level == "Taking\nTurns" | response.level == "Man Has\nFinal Say" |
           response.level == "Happy Wife\nHappy Life") %>%
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 2.5, shape=21, alpha = 0.7) +
  facet_grid(response.level ~ type , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, .2), breaks = c(.0, .1, .2), labels = c(".0", ".1", ".2")) +
  scale_fill_grey() +
#  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme(axis.text.x         = element_text(size = 10),
        strip.text.x        = element_blank(),
        strip.text.y        = element_text(angle = 0, size = 10, face = "bold"),
        strip.text.y.left   = element_text(angle = 0),
        axis.title          = element_text(size = 10), 
        axis.text           = element_text(size = 8), 
        plot.margin         = unit(c(.1,.5,.1,.5),"cm"),
        legend.position     = "none",
        plot.title.position = "plot",
        axis.line           = element_line(size = 4, colour = "white"),
        panel.spacing       = unit(.5, "lines"),
        panel.grid.major.y  = element_line(colour="#f2f2f2", size=7),
        panel.grid.minor.x  = element_blank(),
        panel.border        = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        caption  = "Predicted probabilities calculated from multinomial regression models\nreported in Appendix Table A4. Presented with 95% confidence intervals.")

p2_fig5

### Put it all together
fig5 <- ggarrange(p1_fig5, p2_fig5,
               ncol = 1, nrow = 2, align = "v", heights = c(.6, .4))
fig5

ggsave(filename = file.path(figDir, "fig5.png"), fig5, width=6.5, height=8, units="in", dpi=300, bg = 'white')
