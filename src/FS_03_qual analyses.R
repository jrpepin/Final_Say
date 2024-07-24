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

## Assigning observations probabilities of topics
set.seed(376)
assign <- predict(model, mSTEMMED,
                  iterations = 1000, 
                  burnin     = 100)

head(assign) # assign

## create tibble
assignments <- data.frame(assign)
assignments <- tibble::rownames_to_column(assignments, "longid") 
assignments$longid <- as_numeric(assignments$longid)
head(assignments)
write_xlsx(assignments,  path = file.path(outDir, "assignments.xlsx"))


## create wide data
assign <- left_join(qualdataFULL, assignments) %>%
  select(-c(qual, wN, same, topic, fair)) %>%
  pivot_wider(names_from  = x, 
              values_from = c(t_1, t_2, t_3, t_4, t_5, t_6, t_7))

colnames(assign) <- sub("_qual1", "_item", colnames(assign))
colnames(assign) <- sub("_qual2", "_act", colnames(assign))

lcadata <- left_join(assign, data) ## Join tables


## Assign topics
groups_i <- c("t_1_item", "t_2_item", "t_3_item", "t_4_item", 
              "t_5_item", "t_6_item", "t_7_item")
lcadata$top_i <- max.col(lcadata[groups_i], "first") #tie breakers to 1st class
lcadata$top_i <- as.factor(lcadata$top_i)

groups_a <- c("t_1_act", "t_2_act", "t_3_act", "t_4_act", 
              "t_5_act", "t_6_act", "t_7_act")
lcadata$top_a <- max.col(lcadata[groups_a], "first") #tie breakers to 1st class
lcadata$top_a <- as.factor(lcadata$top_a)

lcadata <- lcadata %>% 
  select(CaseID, fair1, fair2, qual1, top_i, qual2, top_a, 
         longid, everything()) # reorder columns

## Get topic frequency
freq_i <- table(lcadata$top_i)  # frequency of each topic for items
freq_a <- table(lcadata$top_a)  # frequency of each topic for activities
freqs  <- data.frame(t(rbind(freq_i, freq_a))) 
freqs  <- tibble::rownames_to_column(freqs, "topic")
freqs$row_sum <- rowSums(freqs[ , c(2,3)], na.rm=TRUE)
freqs <- freqs %>%
  mutate(total=sum(row_sum),
         prop=percent(row_sum/total, accuracy = 1))
freqs

# Rename topics
levels(lcadata$top_i)[levels(lcadata$top_i)=="1"] <- "Give & Take"
levels(lcadata$top_i)[levels(lcadata$top_i)=="2"] <- "Man Has Final Say"
levels(lcadata$top_i)[levels(lcadata$top_i)=="3"] <- "Practical Efficiency"
levels(lcadata$top_i)[levels(lcadata$top_i)=="4"] <- "Happy Wife Happy Life"
levels(lcadata$top_i)[levels(lcadata$top_i)=="5"] <- "Taking Turns"
levels(lcadata$top_i)[levels(lcadata$top_i)=="6"] <- "Money Matters"
levels(lcadata$top_i)[levels(lcadata$top_i)=="7"] <- "Work Together"

lcadata$top_i <- factor(lcadata$top_i, 
                        levels = c("Practical Efficiency", 
                                   "Give & Take", 
                                   "Money Matters", 
                                   "Work Together",
                                   "Taking Turns", 
                                   "Man Has Final Say", 
                                   "Happy Wife Happy Life"))

levels(lcadata$top_a)[levels(lcadata$top_a)=="1"] <- "Give & Take"
levels(lcadata$top_a)[levels(lcadata$top_a)=="2"] <- "Man Has Final Say"
levels(lcadata$top_a)[levels(lcadata$top_a)=="3"] <- "Practical Efficiency"
levels(lcadata$top_a)[levels(lcadata$top_a)=="4"] <- "Happy Wife Happy Life"
levels(lcadata$top_a)[levels(lcadata$top_a)=="5"] <- "Taking Turns"
levels(lcadata$top_a)[levels(lcadata$top_a)=="6"] <- "Money Matters"
levels(lcadata$top_a)[levels(lcadata$top_a)=="7"] <- "Work Together"

lcadata$top_a <- factor(lcadata$top_a, 
                        levels = c("Practical Efficiency", 
                                   "Give & Take", 
                                   "Money Matters", 
                                   "Work Together",
                                   "Taking Turns",
                                   "Man Has Final Say", 
                                   "Happy Wife Happy Life"))

#creating predictor for respondents' preferred decision-maker
lcadata<- lcadata%>%
  mutate(
    ipref = case_when(
      iperson== "Michelle" & dum1 == 1       			~ "Prefer Michelle",
      iperson== "Anthony"  & dum1 == 0       			~ "Prefer Michelle",
      iperson== "Michelle" & dum1 == 0       			~ "Prefer Anthony",
      iperson== "Anthony"  & dum1 == 1       			~ "Prefer Anthony",
    ))

lcadata<- lcadata%>%
  mutate(
    apref = case_when(
      aperson== "Michelle" & dum2 == 1       			~ "Prefer Michelle",
      aperson== "Anthony"  & dum2 == 0       			~ "Prefer Michelle",
      aperson== "Michelle" & dum2 == 0       			~ "Prefer Anthony",
      aperson== "Anthony"  & dum2 == 1       			~ "Prefer Anthony",
    ))
table(lcadata$apref )
table(lcadata$dum2)
table(lcadata$top_i)

#outputing to dta for multinom table in Stata because we don't know how to do it yet in R :D
write_dta(lcadata, path = file.path(outDir, "lcadataMultinomTopics.dta")) 

# Save data to a file so I can start troubleshooting from here
saveRDS(lcadata, file = file.path(outDir,"lcadataMultinomTopics.rds"))

## FIGURE 4 --------------------------------------------------------------------

data_fig4 <- lcadata %>%
  select(CaseID, top_i, top_a) %>%
  pivot_longer(!CaseID, names_to = "decision", values_to = "topic") %>%
  drop_na(topic) %>%
  group_by(decision, topic) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

data_fig4$decision[data_fig4$decision == "top_i"] <- "Purchase"
data_fig4$decision[data_fig4$decision == "top_a"] <- "Activity"

fig4 <- data_fig4 %>%
  ggplot(aes(fill=topic, y=decision, x=prop)) + 
  geom_bar(position=position_fill(reverse = TRUE), stat="identity") +
  geom_text(aes(label = weights::rd(prop, digits =2)), 
            position = position_fill(reverse = TRUE, 
                                     vjust = .5), color = "white") +
  theme_minimal(12) +
  scale_fill_grey() +
#  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top",
        axis.text.y = element_text(face="bold"),
        plot.title.position = "plot") +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Topic probability by decision type")

fig4

ggsave(filename = file.path(figDir, "fig4.png"), fig4, 
       width=8, height=5, units="in", dpi=300, bg = 'white')

# MULTINOMIALS -----------------------------------------------------------------
### Purchase
mn_item <- multinom(top_i ~ iperson * relinc + organize + mar + child + dur + item + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata)

### Activity
mn_act  <- multinom(top_a ~ aperson * relinc + organize + mar + child + dur + order + activity + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata)

## Table A4 --------------------------------------------------------------------
### Purchase
mep_A4 <- marginaleffects::avg_slopes(mn_item, type = "probs", 
                                      variables = c("iperson"), by = "relinc")
mep_A4 <- as_tibble(mep_A4) %>%
  select(group, relinc, estimate, std.error, p.value) %>%
  mutate(estimate=round(estimate, digits = 2),
         std.error=round(std.error, digits = 2),
         std.error=round(std.error, digits = 3))
kable(mep_A4, "simple")

### Activity
mea_A4 <- marginaleffects::avg_slopes(mn_act, type = "probs", 
                                      variables = c("aperson"), by = "relinc")
mea_A4 <- as_tibble(mea_A4) %>%
  select(group, relinc, estimate, std.error, p.value) %>%
  mutate(estimate=round(estimate, digits = 2),
         std.error=round(std.error, digits = 2),
         std.error=round(std.error, digits = 3))
kable(mea_A4, "simple")

## FIGURE 5 --------------------------------------------------------------------

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


## Table A5 --------------------------------------------------------------------
### Purchase (same multinom from Table A4)
mep_A5 <- marginaleffects::avg_slopes(mn_item, type = "probs", 
                                      variables = c("iperson"), by = "gender")
mep_A5 <- as_tibble(mep_A5) %>%
  select(group, gender, estimate, std.error, p.value) %>%
  mutate(estimate=round(estimate, digits = 2),
         std.error=round(std.error, digits = 2),
         std.error=round(std.error, digits = 3))
kable(mep_A5, "simple")

### Activity (same multinom from Table A4)
mea_A5 <- marginaleffects::avg_slopes(mn_act, type = "probs", 
                                      variables = c("aperson"), by = "gender")
mea_A5 <- as_tibble(mea_A5) %>%
  select(group, gender, estimate, std.error, p.value) %>%
  mutate(estimate=round(estimate, digits = 2),
         std.error=round(std.error, digits = 2),
         std.error=round(std.error, digits = 3))
kable(mea_A5, "simple")


## Table A6 --------------------------------------------------------------------

lcadata$ipref <- ifelse(lcadata$ipref == "Prefer Anthony", 1, 0)
lcadata$apref <- ifelse(lcadata$apref == "Prefer Anthony", 1, 0)

### Purchase
logitP_A6 <- glm(ipref ~ top_i + relinc + organize + mar + child + dur + item +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              lcadata, family="binomial")

summary(margins(logitP_A6, variables = c("top_i")))

### Activity
logitA_A6 <- glm(apref ~ top_a + relinc + organize + mar + child + dur + item +
                   gender+relate+parent+raceeth+educ+employ+incdum+age,
                 lcadata, family="binomial")

summary(margins(logitA_A6, variables = c("top_a")))
