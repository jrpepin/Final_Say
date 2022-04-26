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
                   "anthony", "anthonys", "michelle", "michelles", 
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
                     beta = .0001, 
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


# Figure 2. Coherence plot ----------------------------------------------------------

fig2 <- coherence_mat %>%
  ggplot(aes(x = k, y = coherence)) +
  geom_line() +
  geom_point() +
  geom_point(data=coherence_mat %>% filter(k == 9),
             pch=21,
             size=4) +
  theme_minimal() +
  scale_x_continuous(breaks = NULL) +
  labs( x        = "Topics", 
        y        = "Coherence", 
        title    = "Probabilistic average coherence for 1-20 topics",
        caption  = "Note. The circled point represents the selected LDA model with nine topics.") 
  
fig2

ggsave(filename = file.path(figDir, "fig2.png"), fig2, width=6, height=4, units="in", dpi=300)


#####################################################################################
# Selected 6 Topic Model
#####################################################################################
set.seed(376)
model <- FitLdaModel(dtm             = mSTEMMED, 
                     k               = 9,    
                     iterations      = 1000, 
                     burnin          = 100,
                     optimize_alpha  = TRUE,
                     beta            = .0001,
                     calc_likelihood = FALSE,
                     calc_coherence  = TRUE,
                     calc_r2         = FALSE,
                     cpus            = 16)

coherence = round(model$coherence, 3)
coherence
names(model)
mean(model$coherence)

# Table 4. Top Words ---------------------------------------------------------------
model$top_terms  <- GetTopTerms(phi = model$phi, M = 50)
t(model$top_terms)
topterms         <- data.frame(model$top_terms)
topterms         <- tibble::rownames_to_column(topterms, "rank") 
topterms$rank    <- as_numeric(topterms$rank)

topterms <- topterms %>%
  rename("Happy Wife, Happy Life" = "t_1",
         "Money Matters"          = "t_2",
         "Gender Essentialism"    = "t_3",
         "Equality or Bust"       = "t_4",
         "Communication"          = "t_5")

write_xlsx(topterms,  path = file.path(outDir, "Table04_topterms5topicfair.xlsx"))

# Figure 3. Word Cloud  -----------------------------------------------------------

## Table of Phi, which is where top words come from. Used to plot words in word clouds or dot plots
phi<-model$phi
phi<-data.frame(phi)
write_xlsx(phi,  path = file.path(outDir, "phi5topicfair.xlsx"))

## Combine topterms and phi values
df1 <- topterms %>%
  pivot_longer(cols = -rank, names_to = "topic", values_to = "word")

df2 <- data.frame(t(phi[-1]))
df2 <- tibble::rownames_to_column(df2, "word")

df2 <- df2 %>%
  pivot_longer(!word, names_to = "topic", values_to = "phi")

df2 <- df2 %>%
  mutate(
    topic = case_when(
      topic == "t_1"    ~ "Happy Wife, Happy Life",
      topic == "t_2"    ~ "Money Matters",
      topic == "t_3"    ~ "Gender Essentialism",
      topic == "t_4"    ~ "Equality or Bust",
      topic == "t_5"    ~ "Communication",
      TRUE              ~  NA_character_ 
    ))

clouddata <- left_join(df1, df2) %>%
  dplyr::arrange(desc(phi))

## Clean dataset
clouddata <- clouddata %>% # label topics
   mutate(topic = case_when(
      topic == "Happy Wife, Happy Life" ~ "Topic 1: Happy Wife, Happy Life",
      topic == "Money Matters"          ~ "Topic 2: Money Matters",
      topic == "Gender Essentialism"    ~ "Topic 3: Gender Essentialism",
      topic == "Equality or Bust"       ~ "Topic 4: Equality or Bust",
      topic == "Communication"          ~ "Topic 5: Communication"))

clouddata$topic <- factor(clouddata$topic, 
                           levels = c("Topic 1: Happy Wife, Happy Life", 
                                      "Topic 2: Money Matters", 
                                      "Topic 3: Gender Essentialism",
                                      "Topic 4: Equality or Bust",
                                      "Topic 5: Communication"), ordered = FALSE)

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
  scale_colour_manual(values = c("#566472", "#E74C3C")) +
  theme_minimal() +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.2, "lines")) +
  labs(title    = "Word clouds with highest-ranking word stems per topic", 
       subtitle = "Word stems weighted by probability of being found in topic")

fig3

ggsave(file.path(figDir, "fig3.png"), fig3, height = 9, width = 6, units="in",dpi = 300)


#####################################################################################
# Topic Modeling Regression Tables
#####################################################################################

## Assigning docs to topics with probabilities
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


## create wide data
assign <- left_join(qualdata, assignments) %>%
  select(-c(qual, wN, same, topic, fair, longid)) %>%
  pivot_wider(names_from = x, values_from = c(t_1, t_2, t_3, t_4, t_5))

colnames(assign) <- sub("_qual1", "_item", colnames(assign))
colnames(assign) <- sub("_qual2", "_act", colnames(assign))

lcadata <- left_join(assign, data) ## Join tables

# OLS REGRESSIONS ----------------------------------------------------------------
# lcaSvy <- svydesign(ids = ~1, weights = ~ weight, data = lcadata)
# 
# ## item regressions
# man_i   <- svyglm(t_1_item ~ iperson + relinc + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# give_i  <- svyglm(t_2_item ~ iperson + relinc + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# turns_i <- svyglm(t_3_item ~ iperson + relinc + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# happy_i <- svyglm(t_4_item ~ iperson + relinc + organize + mar + child + dur + 
#                   gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# choos_i <- svyglm(t_5_item ~ iperson + relinc + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# money_i <- svyglm(t_6_item ~ iperson + relinc + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# ## activity regressions
# 
# man_a   <- svyglm(t_1_act ~ aperson + relinc + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# give_a  <- svyglm(t_2_act ~ aperson + relinc + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# turns_a <- svyglm(t_3_act ~ aperson + relinc + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# happy_a <- svyglm(t_4_act ~ aperson + relinc + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# choos_a <- svyglm(t_5_act ~ aperson + relinc + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)
# 
# money_a <- svyglm(t_6_act ~ aperson + relinc + organize + mar + child + dur + 
#                     gender+relate+parent+raceeth+educ+employ+incdum+age, lcaSvy)

# MULTI-NOMIAL REGRESSIONS --------------------------------------------------------

## Assign topics
groups_i <- c("t_1_item", "t_2_item", "t_3_item", "t_4_item", "t_5_item")
lcadata$top_i <- max.col(lcadata[groups_i], "first") #tie breakers go to first class
lcadata$top_i <- as.factor(lcadata$top_i)

groups_a <- c("t_1_act", "t_2_act", "t_3_act", "t_4_act", "t_5_act")
lcadata$top_a <- max.col(lcadata[groups_a], "first") #tie breakers go to first class
lcadata$top_a <- as.factor(lcadata$top_a)

lcadata <- lcadata %>% 
  select(CaseID, ifair, afair, qual1, top_i, qual2, top_a, everything()) # reorder columns

table(lcadata$top_i)  # frequency of each topic for items
table(lcadata$top_a)  # frequency of each topic for activities

# topic == "t_1" ~ "Topic 5: Man has Final Say",
# topic == "t_2" ~ "Topic 3: Give & Take",
# topic == "t_3" ~ "Topic 1: Taking Turns",
# topic == "t_4" ~ "Topic 6: Happy Wife, Happy Life",
# topic == "t_5" ~ "Topic 4: Some's Gotta Choose",
# topic == "t_6" ~ "Topic 2: Money Matters",

## Rename topics 
# levels(lcadata$top_i)[levels(lcadata$top_i)=="1"] <- "Man has Final Say" 
# levels(lcadata$top_i)[levels(lcadata$top_i)=="2"] <- "Give & Take" 
# levels(lcadata$top_i)[levels(lcadata$top_i)=="3"] <- "Taking Turns" 
# levels(lcadata$top_i)[levels(lcadata$top_i)=="4"] <- "Happy Wife, Happy Life"
# levels(lcadata$top_i)[levels(lcadata$top_i)=="5"] <- "Some's Gotta Choose"
# levels(lcadata$top_i)[levels(lcadata$top_i)=="6"] <- "Money Matters"
# lcadata$top_i <- factor(lcadata$top_i, levels = c("Taking Turns", "Money Matters", "Give & Take", 
#                                                   "Some's Gotta Choose", "Man has Final Say", "Happy Wife, Happy Life"))
# 
# levels(lcadata$top_a)[levels(lcadata$top_a)=="1"] <- "Man has Final Say" 
# levels(lcadata$top_a)[levels(lcadata$top_a)=="2"] <- "Give & Take" 
# levels(lcadata$top_a)[levels(lcadata$top_a)=="3"] <- "Taking Turns" 
# levels(lcadata$top_a)[levels(lcadata$top_a)=="4"] <- "Happy Wife, Happy Life"
# levels(lcadata$top_a)[levels(lcadata$top_a)=="5"] <- "Some's Gotta Choose"
# levels(lcadata$top_a)[levels(lcadata$top_a)=="6"] <- "Money Matters"
# lcadata$top_a <- factor(lcadata$top_a, levels = c("Taking Turns", "Money Matters", "Give & Take", 
#                                                   "Some's Gotta Choose", "Man has Final Say", "Happy Wife, Happy Life"))


mn_item <- multinom(top_i ~ iperson * relinc + organize + mar + child + dur + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

mn_act  <- multinom(top_a ~ iperson * relinc + organize + mar + child + dur + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

item.pp <- ggeffect(mn_item , terms = c("iperson", "relinc"))
act.pp  <- ggeffect(mn_act , terms = c("iperson", "relinc"))

item.pp$type <- "item"
act.pp$type <- "activity"

data_fig4 <- rbind(item.pp, act.pp)

levels(data_fig4$x)[levels(data_fig4$x)=="Michelle"] <- "She decided"
levels(data_fig4$x)[levels(data_fig4$x)=="Anthony"]  <- "He decided"
data_fig4$type <- factor(data_fig4$type, levels = c("item", "activity"), ordered = FALSE)

## create comparable relative earnings categories
data_fig4 <- data_fig4 %>%
  mutate(
    earnings = case_when(
      x == "He decided"  & group   == "Man higher-earner"    ~ "and is HIGHER earner",
      x == "He decided"  & group   == "Woman higher-earner"  ~ "and is LOWER earner",
      x == "She decided" & group   == "Man higher-earner"    ~ "and is LOWER earner",
      x == "She decided" & group   == "Woman higher-earner"  ~ "and is HIGHER earner",
      TRUE ~ "and is equal earner"))
      
data_fig4$earnings <- factor(data_fig4$earnings, levels = c("and is HIGHER earner", "and is LOWER earner", "and is equal earner"), ordered = FALSE)

## Pretty topic labels
# data_fig4$response.level[data_fig4$response.level == "Taking.Turns"]            <- "Taking Turns"
# data_fig4$response.level[data_fig4$response.level == "Money.Matters"]           <- "Money Matters"
# data_fig4$response.level[data_fig4$response.level == "Give...Take"]             <- "Give & Take"
# data_fig4$response.level[data_fig4$response.level == "Some.s.Gotta.Choose"]     <- "Some's Gotta Choose"
# data_fig4$response.level[data_fig4$response.level == "Man.has.Final.Say"]       <- "Man has Final Say"
# data_fig4$response.level[data_fig4$response.level == "Happy.Wife..Happy.Life"]  <- "Happy Wife, Happy Life"
# 
# data_fig4$response.level <- factor(data_fig4$response.level, 
#                                    levels = c("Taking Turns", "Money Matters", 
#                                               "Give & Take", "Some's Gotta Choose", 
#                                               "Man has Final Say", "Happy Wife, Happy Life"), 
#                                    ordered = FALSE)

# data_fig4$response.level <- factor(data_fig4$response.level, levels=rev(levels(data_fig4$response.level)))

fig4 <- data_fig4 %>%
  filter(type == "item") %>% # exclude activity because all CI overlap
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_col(width=.6, position = position_dodge(width=.95)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), 
                color="#ADB5BD", position = position_dodge(width=.95), width=.6) +
  facet_grid(response.level ~ .,
             switch = "y") +
  scale_fill_manual(values = c("#18BC9C", "#F39C12")) +
  theme_minimal() +
  theme(legend.position      = "top",
#        legend.justification = c(0, 1),
        panel.grid.major.x   = element_blank(),
        strip.placement      = 'outside',
        strip.text.y         = element_text(face = "bold"),
        strip.text.x         = element_text(face = "bold"),
        strip.text.y.left    = element_text(angle = 0),
        plot.title           = element_text(face = "bold"),
        plot.title.position  = "plot",
        plot.subtitle       = element_text(face = "italic", color = "#707070"),
        plot.caption        = element_text(face = "italic", color = "#707070")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Topic prevalence among decisions perceived as fair ",
        subtitle = "by vignette gender and relative earnings")

fig4

ggsave(filename = file.path(figDir, "fig4.png"), fig4, width=6, height=8, units="in", dpi=300)

## ------Figure 4 alternative

data_fig4 %>%
  filter(type == "item") %>% # exclude activity because all CI overlap
  ggplot(aes(x = predicted, y = response.level, fill = x)) +
  geom_point(size = 3.5, shape=21, alpha = 0.9) +
  facet_grid(earnings ~ . , scales = "free", space = "free") +
  scale_y_discrete(limits=rev) +
  theme_minimal() +
  theme(axis.text.x      = element_text(vjust = 0.5, hjust=1, size = 10),
        strip.text.x     = element_text(face = "bold", size = 10),
        strip.text.y     = element_text(angle = 0, size = 10, face = "bold"),
        axis.title       = element_text(size = 10), 
        axis.text        = element_text(size = 8), 
        plot.margin      = unit(c(.1,.5,.1,.5),"cm"),
        legend.position  = "top",
        plot.title.position = "plot",
        axis.line        = element_line(size = 4, colour = "white"),
        panel.spacing=unit(.5, "lines"),
        panel.grid.major = element_line(colour="#f2f2f2", size=5),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Topic Prevalence Among Perceived Fair Decisions ",
        subtitle = "by vignette gender and relative earnings")

  