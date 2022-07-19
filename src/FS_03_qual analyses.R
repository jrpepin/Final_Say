#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_03_qual analyses.R
# William J. Scarborough & Joanna R. Pepin
#------------------------------------------------------------------------------------

# This file analyzes the qualitative responses.
summary(qualdata)

# generating corpus
Corpusprep <-data.frame(
  doc_id=qualdata$longid,
  text=qualdata$qual
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

corpus <- tm_map(corpus, removeWords, stopwords::stopwords('english')) # or should we use tm::stopwords?

corpusSTEMMED <- tm_map(corpus, stemDocument)

writeLines(as.character(corpus[[15]])) # used for text analysis
writeLines(as.character(corpusSTEMMED [[15]])) # used for text analysis
qualdata$qual[15] # compared to this for check

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

#####################################################################################
# Fitting Model
#####################################################################################
# Fit a 1 through 20 LDA models and Compare Coherence
k_list <- seq(1,20, by=1)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  set.seed(376)
  lda <- FitLdaModel(dtm = mSTEMMED ,
                     k = k, 
                     iterations = 1000, 
                     burnin = 100,
                     optimize_alpha = TRUE, 
                     beta = .01, 
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
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
coherence_mat
plot(coherence_mat, type = "o")
write_xlsx(coherence_mat, path = file.path(outDir, "coherence.xlsx"))


# Figure 1. Coherence plot ----------------------------------------------------------

fig1 <- coherence_mat %>%
  ggplot(aes(x = k, y = coherence)) +
  geom_line() +
  geom_point() +
  geom_point(data=coherence_mat %>% filter(k == 4),
             pch=21,
             size=4) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:20) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs( x        = "Number of Topics", 
        y        = "Coherence", 
        title    = "Probabilistic average coherence for 1-20 topics",
        caption  = "Note: The circled point represents the selected LDA model with five topics.") 

fig1

ggsave(filename = file.path(figDir, "fig1.png"), fig1, width=6, height=4, units="in", dpi=300)


#####################################################################################
# Selected 4 Topic Model
#####################################################################################
set.seed(376)
model <- FitLdaModel(dtm             = mSTEMMED, 
                     k               = 4,    
                     iterations      = 1000, 
                     burnin          = 100,
                     optimize_alpha  = TRUE,
                     beta            = .01,
                     calc_likelihood = FALSE,
                     calc_coherence  = TRUE,
                     calc_r2         = FALSE,
                     cpus            = 16)

coherence = round(model$coherence, 3)
coherence
names(model)
mean(model$coherence)

# Table 03. Top Words ---------------------------------------------------------------
model$top_terms  <- GetTopTerms(phi = model$phi, M = 50)
t(model$top_terms)
topterms         <- data.frame(model$top_terms)
topterms         <- tibble::rownames_to_column(topterms, "rank") 

top4terms <- topterms %>%
  select(rank, t_1, t_3, t_4, t_2) %>% 
  rename("Equality or Bust"       = "t_1",
         "Man Has Final Say"      = "t_3",
         "Money Matters"          = "t_4",
         "Happy Wife, Happy Life" = "t_2")

write_xlsx(top4terms,  path = file.path(outDir, "Table03_topterms4topicfair.xlsx"))

# Figure 3. Word Cloud  -----------------------------------------------------------

## Table of Phi, which is where top words come from. Used to plot words in word clouds or dot plots
phi<-model$phi
phi<-data.frame(phi)
write_xlsx(phi,  path = file.path(outDir, "phi4topicfair.xlsx"))

## Combine topterms and phi values
df1 <- topterms %>%
  pivot_longer(cols = starts_with("t_"), names_to = "topic", values_to = "word")

df2 <- data.frame(t(phi[-1]))
df2 <- tibble::rownames_to_column(df2, "word")

df2 <- df2 %>%
  pivot_longer(!word, names_to = "topic", values_to = "phi")

clouddata <- left_join(df1, df2) %>%
  dplyr::arrange(desc(phi))

# Clean dataset
clouddata <- clouddata %>% # label topics
  mutate(topic = case_when(
    topic == "t_1" ~ "Topic 1: Equality or Bust",
    topic == "t_3" ~ "Topic 2: Man Has Final Say",
    topic == "t_4" ~ "Topic 3: Money Matters",
    topic == "t_2" ~ "Topic 4: Happy Wife, Happy Life"))


clouddata$topic <- factor(clouddata$topic,
                          levels = c("Topic 1: Equality or Bust",
                                     "Topic 2: Man Has Final Say",
                                     "Topic 3: Money Matters",
                                     "Topic 4: Happy Wife, Happy Life"), 
                          ordered = FALSE)

clouddata$rank <- as.numeric(clouddata$rank)

clouddata <- clouddata %>% # top 5 indicator
  mutate(top5 = case_when(
    rank <= 5 ~ "yes",
    TRUE      ~ "no"))

## Create wordcloud

fig3 <- clouddata %>%
  ggplot(aes(label = word, size = phi, color = top5)) + 
  geom_text_wordcloud(rm_outside = TRUE, 
                      max_steps = 1,
                      grid_size = 1, 
                      eccentricity = .9) +
  facet_wrap(~topic, ncol = 2) +
  scale_size_area(max_size = 14) +
  scale_colour_manual(values = c("#566472", "#E16A86")) +
  theme_minimal() +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.1, "lines")) +
  labs(title    = "Word clouds with highest-ranking word stems per topic", 
       subtitle = "Word stems weighted by probability of being found in topic")

fig3

ggsave(file.path(figDir, "fig3.png"), fig3, height = 6, width = 6, units="in",dpi = 300)


#####################################################################################
# Topic Modeling Regression Tables
#####################################################################################

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
str(assignments )
head(assignments)
write_xlsx(assignments,  path = file.path(outDir, "assignments.xlsx"))


## create wide data
assign <- left_join(qualdata, assignments) %>%
  select(-c(qual, wN, same, topic, fair, longid)) %>%
  pivot_wider(names_from = x, values_from = c(t_1, t_2, t_3, t_4))

colnames(assign) <- sub("_qual1", "_item", colnames(assign))
colnames(assign) <- sub("_qual2", "_act", colnames(assign))

lcadata <- left_join(assign, data) ## Join tables

# OLS REGRESSIONS ----------------------------------------------------------------
# lcaSvy <- svydesign(ids = ~1, weights = ~ weight, data = lcadata)
# 
# ## item regressions
# equal_i  <- svyglm(t_1_item ~ iperson + earner + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# happy_i  <- svyglm(t_2_item ~ iperson + earner + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# gender_i <- svyglm(t_3_item ~ iperson + earner + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# money_i  <- svyglm(t_4_item ~ iperson + earner + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# 
# ## activity regressions
# 
# equal_a  <- svyglm(t_1_act ~ aperson + earner + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# happy_a  <- svyglm(t_2_act ~ aperson + earner + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# gender_a <- svyglm(t_3_act ~ aperson + earner + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# money_a <- svyglm(t_4_act ~ aperson + earner + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 



# MULTI-NOMIAL REGRESSIONS --------------------------------------------------------

## Assign topics
groups_i <- c("t_1_item", "t_2_item", "t_3_item", "t_4_item")
lcadata$top_i <- max.col(lcadata[groups_i], "first") #tie breakers go to first class
lcadata$top_i <- as.factor(lcadata$top_i)

groups_a <- c("t_1_act", "t_2_act", "t_3_act", "t_4_act")
lcadata$top_a <- max.col(lcadata[groups_a], "first") #tie breakers go to first class
lcadata$top_a <- as.factor(lcadata$top_a)

lcadata <- lcadata %>% 
  select(CaseID, ifair, afair, qual1, top_i, qual2, top_a, everything()) # reorder columns

table(lcadata$top_i)  # frequency of each topic for items
table(lcadata$top_a)  # frequency of each topic for activities

# Rename topics
levels(lcadata$top_i)[levels(lcadata$top_i)=="1"] <- "Equality or Bust"
levels(lcadata$top_i)[levels(lcadata$top_i)=="3"] <- "Man Has\nFinal Say"
levels(lcadata$top_i)[levels(lcadata$top_i)=="4"] <- "Money Matters"
levels(lcadata$top_i)[levels(lcadata$top_i)=="2"] <- "Happy Wife\nHappy Life"


lcadata$top_i <- factor(lcadata$top_i, levels = c("Equality or Bust", "Man Has\nFinal Say", "Money Matters",  "Happy Wife\nHappy Life"))


levels(lcadata$top_a)[levels(lcadata$top_a)=="1"] <- "Equality or Bust"
levels(lcadata$top_a)[levels(lcadata$top_a)=="3"] <- "Man Has\nFinal Say"
levels(lcadata$top_a)[levels(lcadata$top_a)=="4"] <- "Money Matters"
levels(lcadata$top_a)[levels(lcadata$top_a)=="2"] <- "Happy Wife\nHappy Life"


lcadata$top_a <- factor(lcadata$top_a, levels = c("Equality or Bust", "Man Has\nFinal Say", "Money Matters",  "Happy Wife\nHappy Life"))

## FIGURE 4 -------------------------------------------------------------------------------------------------
table(lcadata$top_i)  # frequency of each topic for purchases
table(lcadata$top_a)  # frequency of each topic for activities

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
  geom_text(aes(label = weights::rd(prop, digits =2)), position = position_fill(reverse = TRUE, vjust = .5)) +
  theme_minimal() +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top",
        axis.text.y = element_text(face="bold"),
        plot.title.position = "plot") +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Topic prevalence by decision type")

fig4

ggsave(filename = file.path(figDir, "fig4.png"), fig4, width=8, height=5, units="in", dpi=300)

## FIGURE 5 -------------------------------------------------------------------------------------------------
mn_item <- multinom(top_i ~ iperson * relinc + organize + mar + child + dur + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

mn_act  <- multinom(top_a ~ iperson * relinc + organize + mar + child + dur + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

pur.pp <- ggeffect(mn_item , terms = c("iperson", "relinc"))
act.pp  <- ggeffect(mn_act , terms = c("iperson", "relinc"))

pur.pp$type <- "purchase"
act.pp$type <- "activity"

data_fig56 <- rbind(pur.pp, act.pp)

levels(data_fig56$x)[levels(data_fig56$x)=="Michelle"] <- "She decided"
levels(data_fig56$x)[levels(data_fig56$x)=="Anthony"]  <- "He decided"
data_fig56$x <- factor(data_fig56$x, levels = c("She decided", "He decided"), ordered = FALSE)
data_fig56$type <- factor(data_fig56$type, levels = c("purchase", "activity"), ordered = FALSE)

data_fig56$earnings <- factor(data_fig56$group, levels = c("Equal earners", "Woman higher-earner", "Man higher-earner"), ordered = FALSE)

## Pretty topic labels
data_fig56$response.level[data_fig56$response.level == "Equality.or.Bust"]        <- "Equality or Bust"
data_fig56$response.level[data_fig56$response.level == "Man.Has.Final.Say"]     <- "Man Has\nFinal Say"
data_fig56$response.level[data_fig56$response.level == "Money.Matters"]           <- "Money Matters"
data_fig56$response.level[data_fig56$response.level == "Happy.Wife.Happy.Life"]   <- "Happy Wife\nHappy Life"

data_fig56$response.level <- factor(data_fig56$response.level, 
                                   levels = c("Equality or Bust", "Man Has\nFinal Say", "Money Matters",  "Happy Wife\nHappy Life", ordered = FALSE))

fig5 <- data_fig56 %>%
  filter(type == "purchase") %>% # exclude activity because all CI overlap
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 3.5, shape=21, alpha = 0.9) +
  facet_grid(response.level ~ . , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, .8), breaks = c(.0, .2, .4, .6, .8)) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme(axis.text.x      = element_text(vjust = 0.5, hjust=1, size = 10),
        strip.text.x     = element_text(face = "bold", size = 10),
        strip.text.y     = element_text(angle = 0, size = 10, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.title       = element_text(size = 10), 
        axis.text        = element_text(size = 8), 
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
        fill     = " ",
        title    = "Topic Prevalence Among Perceived Fair Decisions on Purchases",
        subtitle = "by vignette gender and relative earnings")

fig5

ggsave(filename = file.path(figDir, "fig5.png"), fig5, width=6.5, height=8, units="in", dpi=300)

## FIGURE 6 -------------------------------------------------------------------------------------------------

fig6 <- data_fig56 %>%
  filter(type == "activity") %>% # change to activities
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 3.5, shape=21, alpha = 0.9) +
  facet_grid(response.level ~ . , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, 1), breaks = c(.0, .2, .4, .6, .8, 1)) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme(axis.text.x      = element_text(vjust = 0.5, hjust=1, size = 10),
        strip.text.x     = element_text(face = "bold", size = 10),
        strip.text.y     = element_text(angle = 0, size = 10, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.title       = element_text(size = 10), 
        axis.text        = element_text(size = 8), 
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
        fill     = " ",
        title    = "Topic Prevalence Among Perceived Fair Decisions on Activities",
        subtitle = "by vignette gender and relative earnings")

fig6

ggsave(filename = file.path(figDir, "fig6.png"), fig6, width=6.5, height=8, units="in", dpi=300)