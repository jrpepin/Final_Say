#-------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_03_qual analyses.R
# William J. Scarborough & Joanna R. Pepin
#-------------------------------------------------------------------------------

# This file analyzes the qualitative responses.
summary(qualdataFULL)

# generating corpus
Corpusprep <-data.frame(
  doc_id=qualdataFULL$longid,
  text=qualdataFULL$qual
  ,stringsAsFactors=F)
str(Corpusprep) # Compactly display the internal structure of an R object
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

writeLines(as.character(corpus[[15]]))         # used for text analysis
writeLines(as.character(corpusSTEMMED [[15]])) # used for text analysis
qualdataFULL$qual[15]                          # compared to this for check 

#document-term matrix
mat        <- DocumentTermMatrix(corpus)
matSTEMMED <- DocumentTermMatrix(corpusSTEMMED)
matSTEMMED

# document-term matrix to different format for fitlda
m       <-  Matrix::sparseMatrix(i=mat$i,
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
# Fit a 1 through 20 LDA models and compare coherence
k_list <- seq(1,20, by=1)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  set.seed(376)
  lda <- FitLdaModel(dtm             = mSTEMMED,
                     k               = k, 
                     iterations      = 1000, 
                     burnin          = 100,
                     optimize_alpha  = TRUE, 
                     beta            = .05, 
                     calc_likelihood = FALSE,
                     calc_coherence  = TRUE,
                     calc_r2         = FALSE,
                     cpus            = 16)
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


################################################################################
# Selected 7 Topic Model 
################################################################################

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

## Assigning probabilities of topics to observations (theta)
set.seed(376)
theta <- data.frame(predict(model, mSTEMMED,
                            iterations = 1000, 
                            burnin     = 100))   %>%
  tibble::rownames_to_column("longid") %>%
  mutate(longid = as_numeric(longid))

head(theta) # probability

### topic frequency -- Average theta!
theta_sum <- theta %>% 
  summarise_at(vars(!c(longid)), mean, na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "topic", values_to = "theta")

# Figure 3. Top Words ----------------------------------------------------------

## Combine topterms and phi values
data_fig3 <- left_join(topterms, phi) %>%
  dplyr::arrange(desc(phi))

data_fig3 <- data_fig3 %>% # label topics -- topic order is based on mean theta
  mutate(topic = fct_case_when(
    topic == "t_3" ~ "Topic 1:\nAccommodate (.19)",
    topic == "t_1" ~ "Topic 2:\nBalanced Sacrifice (.18)",
    topic == "t_7" ~ "Topic 3:\nConsensus (.16)",
    topic == "t_6" ~ "Topic 4:\nMoney Matters (.16)",
    topic == "t_5" ~ "Topic 5:\nDecision History (.12)",
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

## export to Excel/Stata
#library(foreign)
#write.dta(data_lca %>% select(!c(qual)), file = file.path(outDir, "data_lca.dta"))
#write_csv(data_lca, file = file.path(outDir, "data_lca.csv"))

## Check topic frequency -- Average theta!
data_lca %>%
  ungroup %>%
  summarise_at(vars(t_1:t_7), mean, na.rm = TRUE)

# Predict each topic -----------------------------------------------------------

## List of topics
dv <- names(c(select(data_lca %>% ungroup(), starts_with("t_"))))

## Prepare data for plm

### All

pdata_m0 <- pdata.frame(data_lca,                                  ## all relinc
                        index = c("CaseID"))
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
pdata_m0M <- pdata.frame(data_lca %>% 
                           filter(gender == "Male"),               ## all relinc
                         index = c("CaseID"))
pdata_m1M <- pdata.frame(data_lca %>% 
                           filter(gender == "Male" & relinc == "Man higher-earner"),   
                         index = c("CaseID"))
pdata_m2M <- pdata.frame(data_lca %>% 
                           filter(gender == "Male" & relinc == "Woman higher-earner"), 
                         index = c("CaseID"))
pdata_m3M <- pdata.frame(data_lca %>% 
                           filter(gender == "Male" & relinc == "Equal earners"),       
                         index = c("CaseID"))

### Women
pdata_m0W <- pdata.frame(data_lca %>% 
                           filter(gender == "Female"),              # all relinc
                         index = c("CaseID"))
pdata_m1W <- pdata.frame(data_lca %>% 
                           filter(gender == "Female" & relinc == "Man higher-earner"),   
                         index = c("CaseID"))
pdata_m2W <- pdata.frame(data_lca %>% 
                           filter(gender == "Female" & relinc == "Woman higher-earner"), 
                         index = c("CaseID"))
pdata_m3W <- pdata.frame(data_lca %>% 
                           filter(gender == "Female" & relinc == "Equal earners"),       
                         index = c("CaseID"))

## Create the plm functions

### All
plms_all <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m0, model = "within")
}

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
plms_allM <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m0M, model = "within")
}

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
plms_allW <- function(dv){
  formula <- as.formula(paste(dv, "~ dum + per + decision + (dum * per) + (dum * decision) + (per * decision) + (dum * per * decision)"))
  plm(formula, data = pdata_m0W, model = "within")
}

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
fe_all    <- lapply(dv, plms_all)  # All earners
fe_mhe    <- lapply(dv, plms_mhe)  # Man higher-earner
fe_whe    <- lapply(dv, plms_whe)  # Woman higher-earner
fe_ee     <- lapply(dv, plms_ee)   # Equal earners

fe_allM   <- lapply(dv, plms_allM) # All earners
fe_mheM   <- lapply(dv, plms_mheM) # Man higher-earner
fe_wheM   <- lapply(dv, plms_wheM) # Woman higher-earner
fe_eeM    <- lapply(dv, plms_eeM)  # Equal earners

fe_allW   <- lapply(dv, plms_allW) # All earners
fe_mheW   <- lapply(dv, plms_mheW) # Man higher-earner
fe_wheW   <- lapply(dv, plms_wheW) # Woman higher-earner
fe_eeW    <- lapply(dv, plms_eeW)  # Equal earners

# create list of pdata frames (for functions)
times <- 7
dta_m0  <- replicate(times, pdata_m0,  simplify = FALSE)
dta_m1  <- replicate(times, pdata_m1,  simplify = FALSE)
dta_m2  <- replicate(times, pdata_m2,  simplify = FALSE)
dta_m3  <- replicate(times, pdata_m3,  simplify = FALSE)

dta_m0M <- replicate(times, pdata_m0M, simplify = FALSE)
dta_m1M <- replicate(times, pdata_m1M, simplify = FALSE)
dta_m2M <- replicate(times, pdata_m2M, simplify = FALSE)
dta_m3M <- replicate(times, pdata_m3M, simplify = FALSE)

dta_m0W <- replicate(times, pdata_m0W, simplify = FALSE)
dta_m1W <- replicate(times, pdata_m1W, simplify = FALSE)
dta_m2W <- replicate(times, pdata_m2W, simplify = FALSE)
dta_m3W <- replicate(times, pdata_m3W, simplify = FALSE)


# Figure 4 ---------------------------------------------------------------------

# Create the Average Marginal Effects of the models function
give_me_stata_ame <- function(model, dta){
  avg_slopes(model, variables = c("dum"), by = c("decision", "per"), newdata = dta)
}

## estimate the AMEs
ame_all  <- Map(give_me_stata_ame, fe_all,  dta_m0)
ame_mhe  <- Map(give_me_stata_ame, fe_mhe,  dta_m1)
ame_whe  <- Map(give_me_stata_ame, fe_whe,  dta_m2)
ame_ee   <- Map(give_me_stata_ame, fe_ee,   dta_m3)

ame_allM <- Map(give_me_stata_ame, fe_allM, dta_m0M)
ame_mheM <- Map(give_me_stata_ame, fe_mheM, dta_m1M)
ame_wheM <- Map(give_me_stata_ame, fe_wheM, dta_m2M)
ame_eeM  <- Map(give_me_stata_ame, fe_eeM,  dta_m3M)

ame_allW <- Map(give_me_stata_ame, fe_allW, dta_m0M)
ame_mheW <- Map(give_me_stata_ame, fe_mheW, dta_m1M)
ame_wheW <- Map(give_me_stata_ame, fe_wheW, dta_m2M)
ame_eeW  <- Map(give_me_stata_ame, fe_eeW,  dta_m3M)


## add relinc indicator
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

## put them into a data frame
df_mhe <- bind_rows(ame_mhe, .id = "topic")
df_whe <- bind_rows(ame_whe, .id = "topic")
df_ee  <- bind_rows(ame_ee,  .id = "topic")

data_fig4 <- as_tibble(rbind(df_mhe, df_whe, df_ee))

## tidy the data frame
data_fig4 <- data_fig4 %>% 
  mutate( 
    topic = fct_case_when(
      topic == "3" ~ "Accommodate",
      topic == "1" ~ "Balanced Sacrifice",
      topic == "7" ~ "Consensus",
      topic == "6" ~ "Money Matters",
      topic == "5" ~ "Decision History",
      topic == "2" ~ "Man Has Final Say",
      topic == "4" ~ "Happy Wife, Happy Life"),
    decider = fct_case_when(
      per   == 1   ~ "She decided",
      per   == 0   ~ "He decided"),
    earner = fct_case_when(
      relinc == "Men higher-earner"    ~ "Men higher-earner",
      relinc == "Women higher-earner"  ~ "Women higher-earner",
      relinc == "Equal earner"         ~ "Equal earners"))

## create high stakes plot
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
  scale_x_continuous(limits = c(-.3, .3)) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        axis.text.x=element_blank(),
        legend.position     = "none") +
  guides(fill    = guide_legend(reverse = TRUE)) +
  labs(title     = "Average marginal effects of fairness rating to topic prevalence\nby vignette couple's relative income, decision-maker gender, and decision type\n",
       x        = " ", 
       y        = " ",
       subtitle = "High-stakes decisions")

## create low stakes plot
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
  scale_x_continuous(limits = c(-.3, .3)) +
  theme(plot.title.position = "plot",
        strip.text.y.left   = element_text(angle = 0),
        strip.text.x = element_blank(),
        legend.position     = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( x        = " ", 
        y        = " ",
        subtitle = "Low-stakes decisions",
        caption = "Positive coefficients = more frequently used when rated fair
        Negative coefficients = more frequently used when rated unfair 
        * = p < 0.05 significant marginal effect of fairness evaluation on topic prevalence")

## combine the plots
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2) ## this is cutting off data?!?!?!
g_fig4 <- rbind(g1, g2, size = "first")
g_fig4$widths <- unit.pmax(g1$widths, g2$widths)
grid.newpage()
grid.draw(g_fig4)

## save Figure 4
png(file.path(figDir, "fig4.png"), 
    width = 850, height = 580, pointsize=16) 
grid.draw(g_fig4) 
dev.off()


# Figure 5 ---------------------------------------------------------------------

### Create the Predicted Probabilities of the models function
give_me_pp  <- function(model, dta){
  avg_predictions(model, by = c("dum", "per", "decision"), newdata = dta)
}

## estimate the predicted probabilities
pp_all   <- Map(give_me_pp, fe_all,   dta_m0)  # all earners R=ALL
pp_allM  <- Map(give_me_pp, fe_allM,  dta_m0M) # all earners R=Man
pp_allW  <- Map(give_me_pp, fe_allW,  dta_m0W) # all earners R=Woman

pp_mhe   <- Map(give_me_pp, fe_mhe,   dta_m1)  # men-higher-earner R=ALL
pp_mheM  <- Map(give_me_pp, fe_mheM,  dta_m1M) # men-higher-earner R=Man
pp_mheW  <- Map(give_me_pp, fe_mheW,  dta_m1W) # men-higher-earner R=Woman

pp_whe   <- Map(give_me_pp, fe_whe,   dta_m2)  # women-higher-earner R=ALL
pp_wheM  <- Map(give_me_pp, fe_wheM,  dta_m2M) # women-higher-earner R=Man
pp_wheW  <- Map(give_me_pp, fe_wheW,  dta_m2W) # women-higher-earner R=Woman

pp_ee    <- Map(give_me_pp, fe_ee,    dta_m3)  # equal earners R=ALL
pp_eeM   <- Map(give_me_pp, fe_eeM,   dta_m3M) # equal earners R=Man
pp_eeW   <- Map(give_me_pp, fe_eeW,   dta_m3W) # equal earners R=Woman

## add relinc indicator

pp_all  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_all,  "All earners",         SIMPLIFY = FALSE)

pp_mhe  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_mhe,  "Men higher-earner",   SIMPLIFY = FALSE)

pp_whe  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_whe,  "Women higher-earner", SIMPLIFY = FALSE)

pp_ee   <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_ee,    "Equal earner",        SIMPLIFY = FALSE)

pp_allM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_allM, "All earners",         SIMPLIFY = FALSE)

pp_mheM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_mheM, "Men higher-earner",   SIMPLIFY = FALSE)

pp_wheM <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_wheM, "Women higher-earner", SIMPLIFY = FALSE)

pp_eeM  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_eeM,   "Equal earner",        SIMPLIFY = FALSE)

pp_allW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_allW, "All earners",         SIMPLIFY = FALSE)

pp_mheW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_mheW, "Men higher-earner",   SIMPLIFY = FALSE)

pp_wheW <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_wheW, "Women higher-earner", SIMPLIFY = FALSE)

pp_eeW  <- mapply(function(x, y) "[<-"(x, "relinc", value = y) ,
                  pp_eeW,   "Equal earner",        SIMPLIFY = FALSE)

## add R gender indicator
pp_all  <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_all,  "All",   SIMPLIFY = FALSE)

pp_mhe  <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_mhe,  "All",   SIMPLIFY = FALSE)

pp_whe  <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_whe,  "All",   SIMPLIFY = FALSE)

pp_ee   <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_ee,    "All",   SIMPLIFY = FALSE)

pp_allM <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_allM, "Men",   SIMPLIFY = FALSE)

pp_mheM <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_mheM, "Men",   SIMPLIFY = FALSE)

pp_wheM <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_wheM, "Men",   SIMPLIFY = FALSE)

pp_eeM  <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_eeM,   "Men",   SIMPLIFY = FALSE)

pp_allW <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_allW, "Women", SIMPLIFY = FALSE)

pp_mheW <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_mheW, "Women", SIMPLIFY = FALSE)

pp_wheW <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_wheW, "Women", SIMPLIFY = FALSE)

pp_eeW  <- mapply(function(x, y) "[<-"(x, "gender", value = y) ,
                  pp_eeW,   "Women", SIMPLIFY = FALSE)

## put them into a data frame
df_all  <- bind_rows(pp_all,  .id = "topic")
df_mhe  <- bind_rows(pp_mhe,  .id = "topic")
df_whe  <- bind_rows(pp_whe,  .id = "topic")
df_ee   <- bind_rows(pp_ee,   .id = "topic")

df_allM <- bind_rows(pp_allM, .id = "topic")
df_mheM <- bind_rows(pp_mheM, .id = "topic")
df_wheM <- bind_rows(pp_wheM, .id = "topic")
df_eeM  <- bind_rows(pp_eeM,  .id = "topic")

df_allW <- bind_rows(pp_allW, .id = "topic")
df_mheW <- bind_rows(pp_mheW, .id = "topic")
df_wheW <- bind_rows(pp_wheW, .id = "topic")
df_eeW  <- bind_rows(pp_eeW,  .id = "topic")

data_fig5 <- as_tibble(rbind(df_all,  df_mhe,  df_whe,  df_ee, 
                             df_allM, df_mheM, df_wheM, df_eeM, 
                             df_allW, df_mheW, df_wheW, df_eeW))
## Tidy the new data frame
data_fig5 <- data_fig5 %>% 
  mutate( 
    topic = fct_case_when(
      topic    == "3" ~ "Accommodate",
      topic    == "1" ~ "Balanced\nSacrifice",
      topic    == "7" ~ "Consensus",
      topic    == "6" ~ "Money\nMatters",
      topic    == "5" ~ "Decision\nHistory",
      topic    == "2" ~ "Man Has\nFinal Say",
      topic    == "4" ~ "Happy Wife\nHappy Life"),
    decider    = fct_case_when(
      per      == 0   ~ "He decided",
      per      == 1   ~ "She decided"),
    fair       = fct_case_when(
      dum      == 1   ~ "Fair",
      dum      == 0   ~ "Unfair"),
    stakes     = fct_case_when(
      decision == "high"  ~ "High",
      decision == "low"   ~ "Low"),
    earner     = fct_case_when(
      relinc   == "All earners"          ~ "All earners",
      relinc   == "Men higher-earner"    ~ "Men higher-earner",
      relinc   == "Women higher-earner"  ~ "Women higher-earner",
      relinc   == "Equal earner"         ~ "Equal earners"),
    gender     = fct_case_when(
      gender   == "All"   ~ "All",
      gender   == "Women" ~ "Women",
      gender   == "Men"   ~ "Men"),
    label      = format(round(estimate, digits=2), nsmall = 2)) %>%
  select(!c(per, dum, decision, relinc)) %>%
  arrange(fair, topic, stakes, decider, earner)

data_fig5$label <- sub(".", "", data_fig5$label)

## Test respondent gender differences ------------------------------------------

gen_input <- data_fig5 %>% 
  filter(earner != "All earners" & gender != "All")
  
gen_output <- NULL # create empty df for test results

for (i in seq(1, nrow(gen_input), by = 2)) {
  fair      <- gen_input[i, 11]
  topic     <- gen_input[i, 1]
  decider   <- gen_input[i, 10]
  stakes    <- gen_input[i, 12]
  earner    <- gen_input[i, 13]
  estimate  <- ((gen_input[i, 2] - gen_input[i + 1, 2]) / 
                    sqrt(gen_input[i, 3]^2 + gen_input[i + 1, 3]^2))
  p           <- round(2*pnorm(-abs(as.numeric(estimate))), digits = 3)
  gen_output  <- rbind(gen_output, data.frame(fair, topic, decider, stakes, earner, estimate, p))
}

gen_output <- gen_output %>%
  arrange(fair, stakes, topic, earner, decider) %>%
  mutate(sig = case_when(
    p   < .001   ~ "***",
    p   < .01    ~ "**",
    p   < .05    ~ "*",
    TRUE         ~ NA_character_))

gen_output[!(is.na(gen_output$sig)), ] # show only statistically sig. gender differences

# Combine results with fig data
gen_output$gender <- "Men"
names(gen_output)[names(gen_output) == "estimate"] <- "t.test"

data_fig5 <- left_join(data_fig5, gen_output) %>%
  mutate(gender = fct_case_when(
    gender   == "All"   ~ "All",
    gender   == "Women" ~ "Women",
    gender   == "Men"   ~ "Men")) %>% 
  unite("label", label, sig, na.rm = TRUE, sep = "", remove = FALSE)

## Create HIGH stakes stacked bar plots ----------------------------------------
 
pHH_bar <- data_fig5 %>%
  filter(gender != "All"  & earner != "All earners" & 
         stakes == "High" & fair   == "Fair") %>%
  # Orders the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) %>%
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

pHS_bar <- data_fig5 %>%
  filter(gender != "All"  & earner != "All earners" & 
           stakes == "High" & fair   == "Fair") %>%
  # Orders the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) %>%
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
                                   "Money\nMatters",
                                   "Man Has\nFinal Say",
                                   "Consensus",
                                   "Balanced\nSacrifice",
                                   "Decision\nHistory",
                                   "Happy Wife\nHappy Life")))

pHH_avg <- df_Havg %>% 
  filter(decider == "He decided") %>%
  ggplot(aes(x=estimate,y=gender)) +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y",
             labeller = as_labeller(c(`He decided` = "Mean",
                                      `She decided` = "Mean"))) +
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
             labeller = as_labeller(c(`He decided` = "Mean",
                                      `She decided` = "Mean"))) +
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

pLH_bar <- data_fig5 %>%
  filter(gender != "All"  & earner != "All earners" & 
           stakes == "Low" & fair   == "Fair") %>%
  # Orders the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) %>%
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
        legend.justification='left',
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

pLS_bar <- data_fig5 %>%
  filter(gender != "All"  & earner != "All earners" & 
           stakes == "Low" & fair   == "Fair") %>%
  # Orders the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) %>%
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
        legend.justification='left',
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
                                   "Balanced\nSacrifice",
                                   "Happy Wife\nHappy Life",
                                   "Decision\nHistory",
                                   "Consensus",
                                   "Man Has\nFinal Say",
                                   "Money\nMatters")))

pLH_avg <- df_Lavg %>% 
  filter(decider == "He decided") %>%
  ggplot(aes(x=estimate,y=gender)) +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y",
             labeller = as_labeller(c(`He decided` = "Mean",
                                      `She decided` = "Mean"))) +
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
             labeller = as_labeller(c(`He decided` = "Mean",
                                      `She decided` = "Mean"))) +
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
  area(l=44, r=50, t=0, b=1), # defines the gap figure area
  area(l=53, r=98, t=0, b=1), 
  area(l=98, r=99, t=0, b=1))

# Show the layout to make sure it looks as it should
plot(layout)

## Combine HIGH stakes charts 
p3_high <- pHH_bar + pHH_avg + pHS_bar + pHS_avg + 
  plot_layout(design = layout) 

p3_high

## Combine LOW stakes charts 
p4_low <- pLH_bar + pLH_avg + pLS_bar + pLS_avg +
  plot_layout(design = layout, guides = "collect") & theme(legend.position = "bottom", legend.justification='left')

p4_low

## Combine all plots
fig5 <- p3_high / p4_low +
  plot_annotation(
    title = "Predicted topic prevalence for decisions rated as fair",
    subtitle = "by decision type, vignette decision-maker gender and relative income, and respondent gender",
    caption = "Mean indicates the average prevalence of each topic for the pooled sample based on the average theta.
    Asterisks denote men's topic prevalence was statistically significantly different from women respondents. 
    * = p < .05, ** = p < .01 *** = p < .001")

fig5

ggsave(filename = file.path(figDir, "fig5.png"), fig5, 
       width=6.5, height=9, units="in", dpi=300, bg = "white")


################################################################################
# SUPPLEMENTARY MATERIALS (qual)
################################################################################

# Supplementary Table S5 -------------------------------------------------------
## Relationship of Fairness Rating to Topic Prevalence by Decision-Maker Gender, 
## Decision Type, and Vignette Couple’s Relative Income

s1 <- modelsummary(ame_mhe, shape = decision + model ~ relinc + per,
                   gof_map = NA, stars = c("*"=.05, "**"=.01, "***"=0.001), output = "huxtable") 
s2 <- modelsummary(ame_whe, shape = decision + model ~ relinc + per,
                   gof_map = NA, stars = c("*"=.05, "**"=.01, "***"=0.001), output = "huxtable")
s3 <- modelsummary(ame_ee, shape = decision + model ~ relinc + per,
                   gof_map = NA, stars = c("*"=.05, "**"=.01, "***"=0.001), output = "huxtable")
data_tableS5 <-  cbind(s1, s2, s3)
data_tableS5 <- data_tableS5[-c(30), ] # remove duplication p value notations
data_tableS5[2, 2] = "High-stakes"
data_tableS5[16, 2] = "Low-stakes"


data_tableS5 <- data_tableS5 %>%
  rename(topic = 3) %>%
  mutate( 
    topic = fct_case_when(
      topic    == "(3)" ~ "Accommodate",
      topic    == "(1)" ~ "Balanced Sacrifice",
      topic    == "(7)" ~ "Consensus",
      topic    == "(6)" ~ "Money Matters",
      topic    == "(5)" ~ "Decision History",
      topic    == "(2)" ~ "Man Has Final Say",
      topic    == "(4)" ~ "Happy Wife, Happy Life"))


## control docx formatting output
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar())


tabS5 <- data_tableS5 %>%
  select(c("decision", "topic", "Men higher-earner / 0", "Men higher-earner / 1", 
           "Women higher-earner / 0", "Women higher-earner / 1",
           "Equal earner / 0", "Equal earner / 1")) %>%
  insert_row(c(" ", " ", 
               "Anthony\nDecides", "Michelle\nDecides",
               "Anthony\nDecides", "Michelle\nDecides", 
               "Anthony\nDecides", "Michelle\nDecides"), after = 1)  %>%
  set_bottom_border(row = c(1,2, 30), col = everywhere)        %>%
  huxtable::as_flextable() %>%
  delete_rows(i = 1, part = "body") %>%
  add_header_row(values = c("Decision", "Topic", "Men higher-earner", 
                            "Women higher-earner", "Equal earners"),
                 colwidths = c(1, 1, 2, 2, 2), top = TRUE) %>%
  flextable::align(align = "center", part = "header")      %>%
  add_footer_lines("Notes: N=7,956 person-decisions. Coefficients are the marginal effects of perceiving a decision as fair on topic prevalence (theta) in respondents' open-ended explanations, calculated from respondent-level fixed effects models with interaction between decision-maker gender and perception of fairness. Independent models applied by relative income. * p < .05, ** p < .01, *** p < .001; 2 tailed tests. Standard errors in parentheses.") %>%
  set_table_properties(layout = "autofit") 

tabS5

read_docx() %>% 
  body_add_par(paste("Table S5. Relationship of Fairness Rating to Topic Prevalence by Decision-Maker Gender, Decision Type, and Vignette Couple’s Relative Income")) %>% 
  body_add_flextable(value = tabS5) %>% 
  print(target = file.path(outDir, "finalsay_tableS5.docx")) # save table


# Supplementary Table S6 -------------------------------------------------------
## Average Probabilistic Coherence for LDA Models with Independent Topic Models 
## by Decision-Type

# Setup separate data files
qualdataHIGH <- qualdataFULL %>%
  filter(x == "qual1")

qualdataLOW <- qualdataFULL %>%
  filter(x == "qual2")

# document-term matrix to different format for fitlda
in_list  <- list(qualdataHIGH, qualdataLOW)
out_list <- lapply(in_list, function(df){
  
  # generating corpus
  Corpusprep <-data.frame(
    doc_id=df$longid,
    text=df$qual
    ,stringsAsFactors=F)
  
  corpus = VCorpus(DataframeSource(Corpusprep))
  
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
  
  #document-term matrix
  matSTEMMED <- DocumentTermMatrix(corpusSTEMMED)
  
  #document-term matrix to different format for fitlda
  mSTEMMED <-  Matrix::sparseMatrix(i=matSTEMMED $i, 
                                    j=matSTEMMED $j, 
                                    x=matSTEMMED $v, 
                                    dims=c(matSTEMMED $nrow, matSTEMMED $ncol),
                                    dimnames = matSTEMMED $dimnames)
})


# Fit a 1 through 20 LDA models and compare coherence
in_list2  <- list(out_list[[1]], out_list[[2]])
out_list2 <- lapply(in_list2, function(mSTEMMED){
  
  # Fit a 1 through 20 LDA models
  k_list <- seq(1,20, by=1)
  
  model_list <- TmParallelApply(X = k_list, FUN = function(k){
    set.seed(376)
    lda <- FitLdaModel(dtm             = mSTEMMED,
                       k               = k, 
                       iterations      = 1000, 
                       burnin          = 100,
                       optimize_alpha  = TRUE, 
                       beta            = .05, 
                       calc_likelihood = FALSE,
                       calc_coherence  = TRUE,
                       calc_r2         = FALSE,
                       cpus            = 16)
    lda$k <- k
    lda
  }, export= ls(), # c("m"), 
  cpus = 4) 
  
  # Get average coherence for each model
  coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                              coherence = sapply(model_list, function(x) 
                                mean(x$coherence)), 
                              stringsAsFactors = FALSE)
  
})

names(out_list2) <- c("High-stakes", "Low-stakes") # Rename lists 

out_list2$`High-stakes`[["stakes"]] <- "High-stakes" # Add list identifier
out_list2$`Low-stakes`[["stakes"]]  <- "Low-stakes"

new_df <- as_tibble(rbind(out_list2$`High-stakes`,  out_list2$`Low-stakes`))


tabS6 <- new_df %>%
  pivot_wider(names_from = stakes, values_from = coherence) %>%
  filter(k < 11) %>%
  rename("Number of Topics" = k) %>%
  flextable::flextable() %>%
  colformat_double(digits = 4) %>%
  add_footer_lines("Note: Shaded cells represent best fitting model by decision type.") %>%
  set_table_properties(layout = "autofit") %>%
  flextable::align(i = NULL, j = NULL, align = "center", part = "all") %>%
  add_header_row(values = c(" ", "Decision Type"),
                 colwidths = c(1, 2), top = TRUE)

tabS6

## Note shading cells doesn't work, so must do that manually (sorry)
read_docx() %>% 
  body_add_par(paste("Table S6. Average Probabilistic Coherence for LDA Models with Independent Topic Models by Decision-Type")) %>% 
  body_add_flextable(value = tabS6) %>% 
  print(target = file.path(outDir, "finalsay_tableS6.docx")) # save table

# Supplementary Table S7 -------------------------------------------------------
## Highest-ranking Word Stems Per Topics, 
## Independent LDA on High-Stakes and Low-Stakes Decisions

nested_list <- list(
  list(m = out_list[[1]], k = 7),
  list(m = out_list[[2]], k = 8))

out_list3 <- lapply(nested_list, function(matrix){
  
  set.seed(376)
  
  model <- FitLdaModel(dtm             = matrix$m, 
                       k               = matrix$k,    
                       iterations      = 1000, 
                       burnin          = 100, 
                       optimize_alpha  = TRUE,
                       beta            = .05, 
                       calc_likelihood = FALSE,
                       calc_coherence  = TRUE,
                       calc_r2         = FALSE,
                       cpus            = 16)
  
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
  
  ## Assigning probabilities of topics to observations (theta)
  set.seed(376)
  theta <- data.frame(predict(model, mSTEMMED,
                              iterations = 1000, 
                              burnin     = 100))   %>%
    tibble::rownames_to_column("longid") %>%
    mutate(longid = as_numeric(longid))
  
  head(theta) # probability
  
  ### topic frequency -- Average theta!
  theta_sum <- theta %>% 
    summarise_at(vars(!c(longid)), mean, na.rm = TRUE) %>%
    pivot_longer(everything(), names_to = "topic", values_to = "theta")
  
  ## Combine topterms and phi values
  words <- left_join(topterms, phi) %>%
    left_join(., theta_sum) 
  
})

## Combine list into a tidy df
names(out_list3) <- c("High", "Low") # Rename lists 
out_list3$`High`[["stakes"]] <- "High-stakes" # Add list identifier
out_list3$`Low`[["stakes"]]  <- "Low-stakes"

data_tabS7 <- as_tibble(rbind(out_list3$`High`,  out_list3$`Low`)) %>%
  arrange(stakes, topic, desc(phi)) %>%
  mutate(rank = as.numeric(rank)) %>%
  filter(rank < 11) %>%
  mutate(theta = round(theta,digits =2),
         new_topic = fct_case_when(
           (topic == "t_5" & stakes == "High-stakes") |
             (topic == "t_6" & stakes == "Low-stakes")  ~ "Topic 1",
           (topic == "t_3" & stakes == "High-stakes") |
             (topic == "t_7" & stakes == "Low-stakes")  ~ "Topic 2",
           (topic == "t_7" & stakes == "High-stakes") |
             (topic == "t_4" & stakes == "Low-stakes")  ~ "Topic 3",
           (topic == "t_1" & stakes == "High-stakes") |
             (topic == "t_2" & stakes == "Low-stakes")  ~ "Topic 4",
           (topic == "t_4" & stakes == "High-stakes") |
             (topic == "t_5" & stakes == "Low-stakes")  ~ "Topic 5",
           (topic == "t_6" & stakes == "High-stakes") | 
             (topic == "t_3" & stakes == "Low-stakes") ~ "Topic 6",
           (topic == "t_2" & stakes == "High-stakes") |
             (topic == "t_8" & stakes == "Low-stakes")    ~ "Topic 7",
           (topic == "t_1" & stakes == "Low-stakes")    ~ "Topic 8")) %>%
  select(c("new_topic", "word", "stakes", "rank", "theta")) %>%
  arrange(stakes, new_topic, rank)


tabS7 <- data_tabS7  %>%
  select(c("new_topic", "word", "stakes", "rank")) %>%
  pivot_wider(names_from = c(new_topic), values_from = word,
              names_sep = " ",) %>%
  add_row(stakes = "High-stakes",  
          "Topic 1" = "Consensus (.22)",
          "Topic 2" = "Accommodate (.13)",
          "Topic 3" = "Decision History (.14)",
          "Topic 4" = "Balanced Sacrifice (.10)",
          "Topic 5" = "Happy Wife Happy Life (.10)",
          "Topic 6" = "Money Matters (.20)",
          "Topic 7" = "Man Has Final Say (.10)",
          .before = 1) %>%
  add_row(stakes = "Low-stakes", 
          "Topic 1" = "Consensus (.19)",
          "Topic 2" = "Accommodate (.21)",
          "Topic 3" = "Decision History (.11)",
          "Topic 4" = "Balanced Sacrifice (.06)",
          "Topic 5" = "Happy Wife Happy Life (.10)",
          "Topic 6" = "Cooperate (.15)",
          "Topic 7" = "Happy Woman Happy Life (.11)",
          "Topic 8" = "Decision Downplay (.07)",
          .before = 12) %>%
  select(c("stakes", "rank", "Topic 1", "Topic 2", "Topic 3", "Topic 4",  
           "Topic 5", "Topic 6", "Topic 7", "Topic 8"))

tabS7 <- as_grouped_data(x = tabS7, groups = c("stakes"), columns = NULL) # Group by vignette condition

tabS7 <- tabS7 %>%
  flextable::as_flextable(hide_grouplabel = TRUE) 

tabS7

read_docx() %>% 
  body_add_par(paste("Table S7. Highest-ranking Word Stems Per Topics, Independent LDA on High-Stakes and Low-Stakes Decisions")) %>% 
  body_add_flextable(value = tabS7) %>% 
  print(target = file.path(outDir, "finalsay_tableS7.docx")) # save table

# Supplement Figure S2 ---------------------------------------------------------

## Predicted probability of topic for decisions rated somewhat or very unfair\n
## by decision type, vignette couples' relative income and decision-maker gender, 
## and respondent gender\n 

## Create HIGH stakes stacked bar plots ----------------------------------------

pHH_bar <- data_fig5 %>%
  filter(gender != "All"  & earner != "All earners" & 
           stakes == "High" & fair   == "Unfair") %>%
  # Orders the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) %>%
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
  xlim(0, 1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_grey(name = " ") +
  labs(subtitle = "High-stakes", 
       x        = " ", 
       y        = " ") 

pHS_bar <- data_fig5 %>%
  filter(gender != "All"  & earner != "All earners" & 
           stakes == "High" & fair   == "Unfair") %>%
  # Orders the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) %>%
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
  xlim(0, 1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_fill_grey(name = " ") +
  labs(title    = " ", 
       x        = " ", 
       y        = " ") 

## Create average theta strip for HIGH stakes plots ----------------------------
df_Havg <- data_fig5 %>%
  filter(earner == "All earners" & fair  == "Unfair" & gender == "All" &
           stakes == "High") %>%
  # Updates the factor levels
  mutate(topic = factor(topic,
                        levels = c("Consensus",
                                   "Money\nMatters",
                                   "Balanced\nSacrifice",
                                   "Accommodate",
                                   "Decision\nHistory",
                                   "Man Has\nFinal Say",
                                   "Happy Wife\nHappy Life")))

pHH_avg <- df_Havg %>% 
  filter(decider == "He decided") %>%
  ggplot(aes(x=estimate,y=gender)) +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y",
             labeller = as_labeller(c(`He decided` = "Mean",
                                      `She decided` = "Mean"))) +
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
             labeller = as_labeller(c(`He decided` = "Mean",
                                      `She decided` = "Mean"))) +
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

pLH_bar <- data_fig5 %>%
  filter(gender != "All"  & earner != "All earners" & 
           stakes == "Low" & fair   == "Unfair") %>%
  # Orders the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) %>%
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
        legend.justification='left',
        plot.title.position = "plot",
        plot.subtitle       = element_text(face = "bold"),
        plot.margin = margin(t = 0,    # Top margin
                             r = 0,    # Right margin
                             b = 0,    # Bottom margin
                             l = 0)) + # Left margin
  xlim(0, 1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_grey(name = " ") +
  labs(subtitle    = "Low-stakes", 
       x        = " ", 
       y        = " ") 

pLS_bar <- data_fig5 %>%
  filter(gender != "All"  & earner != "All earners" & 
           stakes == "Low" & fair   == "Unfair") %>%
  # Orders the factor levels
  mutate(topic = fct_reorder(topic, .x = estimate, .fun = mean, .desc = TRUE)) %>%
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
        legend.justification='left',
        plot.margin = margin(t = 0,    # Top margin
                             r = 0,    # Right margin
                             b = 0,    # Bottom margin
                             l = 0)) + # Left margin
  xlim(0, 1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(position = "right") +
  scale_fill_grey(name = " ") +
  labs(title    = " ", 
       x        = " ", 
       y        = " ") 

## Create average theta strip for LOW stakes plots -----------------------------
df_Lavg <- data_fig5 %>%
  filter(earner == "All earners" & fair  == "Unfair" & gender == "All" &
           stakes == "Low") %>%
  # Updates the factor levels
  mutate(topic = factor(topic,
                        levels = c("Balanced\nSacrifice",
                                   "Decision\nHistory",
                                   "Accommodate",
                                   "Consensus",
                                   "Money\nMatters",
                                   "Happy Wife\nHappy Life",
                                   "Man Has\nFinal Say")))

pLH_avg <- df_Lavg %>% 
  filter(decider == "He decided") %>%
  ggplot(aes(x=estimate,y=gender)) +
  facet_grid(rows   = vars(topic),  
             cols   = vars(decider), 
             space  = "free",
             switch = "y",
             labeller = as_labeller(c(`He decided` = "Mean",
                                      `She decided` = "Mean"))) +
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
             labeller = as_labeller(c(`He decided` = "Mean",
                                      `She decided` = "Mean"))) +
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
  area(l=44, r=50, t=0, b=1), # defines the gap figure area
  area(l=53, r=98, t=0, b=1), 
  area(l=98, r=99, t=0, b=1))

# Show the layout to make sure it looks as it should
plot(layout)

## Combine HIGH stakes charts 
p5_high <- pHH_bar + pHH_avg + pHS_bar + pHS_avg + 
  plot_layout(design = layout) 

p5_high

## Combine LOW stakes charts 
p6_low <- pLH_bar + pLH_avg + pLS_bar + pLS_avg +
  plot_layout(design = layout, guides = "collect") & theme(legend.position = "bottom", legend.justification='left')

p6_low

## Combine all plots
figS2 <- p5_high / p6_low +
  plot_annotation(
    title = "Predicted topic prevalence for decisions rated as unfair",
    subtitle = "by decision type, vignette decision-maker gender and relative income, and respondent gender",
    caption = "Mean indicates the average prevalence of each topic for the pooled sample based on the average theta.
    Asterisks denote men's topic prevalence was statistically significantly different from women respondents. 
    * = p < .05, ** = p < .01 *** = p < .001")

figS2

ggsave(filename = file.path(figDir, "figS2.png"), figS2, 
       width=6.5, height=9, units="in", dpi=300, bg = "white")