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

corpus <- tm_map(corpus, removeWords, stopwords::stopwords('english')) # or should we use tm::stopwords?

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

ggsave(filename = file.path(figDir, "fig1.png"), fig1, width=6, height=4, units="in", dpi=300, bg = 'white')


#####################################################################################
# Selected 7 Topic Model
#####################################################################################
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

# Figure 3. Word Cloud  -----------------------------------------------------------

## Table of top words
model$top_terms  <- GetTopTerms(phi = model$phi, M = 50)
t(model$top_terms)
topterms         <- data.frame(model$top_terms)
topterms         <- tibble::rownames_to_column(topterms, "rank") 

head(topterms)
topterms

top7terms <- topterms %>%
  select(rank, t_1, t_2, t_3, t_4, t_5, t_6, t_7) %>% 
  rename("Assured Acquiescence"  = "t_1",
         "Man has Final Say"     = "t_2",
         "Practical Efficiency"  = "t_3",
         "Happy Wife Happy Life" = "t_4",
         "Taking Turns"          = "t_5",
         "Money Matters"         = "t_6", 
         "Work Together"         = "t_7")

write_xlsx(top7terms ,  path = file.path(outDir, "Table03_topterms7topicfair.xlsx"))

## Table of Phi, which is where top words come from. Used to plot words in word clouds or dot plots
phi<-model$phi
phi<-data.frame(phi)
write_xlsx(phi,  path = file.path(outDir, "phi7topic.xlsx"))

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

## topic order is based on frequency of topics for items + frequency of topics for activities
clouddata <- clouddata %>% # label topics
  mutate(topic = case_when(
    topic == "t_1" ~ "Topic 2: Assured Acquiescence",
    topic == "t_2" ~ "Topic 6: Man Has Final Say",
    topic == "t_3" ~ "Topic 1: Practical Efficiency",
    topic == "t_4" ~ "Topic 7: Happy Wife, Happy Life",
    topic == "t_5" ~ "Topic 5: Taking Turns",
    topic == "t_6" ~ "Topic 3: Money Matters",
    topic == "t_7" ~ "Topic 4: Work Together"))

clouddata$topic <- factor(clouddata$topic,
                          levels = c("Topic 1: Practical Efficiency",
                                     "Topic 2: Assured Acquiescence",
                                     "Topic 3: Money Matters",
                                     "Topic 4: Work Together",
                                     "Topic 5: Taking Turns",
                                     "Topic 6: Man Has Final Say",
                                     "Topic 7: Happy Wife, Happy Life"), 
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
  theme_minimal(12) +
  theme(strip.text.x         = element_text(face="bold.italic"),
        panel.spacing        = unit(1.1, "lines")) +
  labs(title    = "Word clouds with highest-ranking word stems per topic", 
       subtitle = "Word stems weighted by probability of being found in topic")

fig3

ggsave(file.path(figDir, "fig3.png"), fig3, height = 6, width = 6, units="in",dpi = 300, bg = 'white')


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
head(assignments)
write_xlsx(assignments,  path = file.path(outDir, "assignments.xlsx"))


## create wide data
assign <- left_join(qualdataFULL, assignments) %>%
  select(-c(qual, wN, same, topic, fair)) %>%
  pivot_wider(names_from = x, values_from = c(t_1, t_2, t_3, t_4, t_5, t_6, t_7))

colnames(assign) <- sub("_qual1", "_item", colnames(assign))
colnames(assign) <- sub("_qual2", "_act", colnames(assign))

lcadata <- left_join(assign, data) ## Join tables


# MULTI-NOMIAL REGRESSIONS --------------------------------------------------------

## Assign topics
groups_i <- c("t_1_item", "t_2_item", "t_3_item", "t_4_item", "t_5_item", "t_6_item", "t_7_item")
lcadata$top_i <- max.col(lcadata[groups_i], "first") #tie breakers go to first class
lcadata$top_i <- as.factor(lcadata$top_i)

groups_a <- c("t_1_act", "t_2_act", "t_3_act", "t_4_act", "t_5_act", "t_6_act", "t_7_act")
lcadata$top_a <- max.col(lcadata[groups_a], "first") #tie breakers go to first class
lcadata$top_a <- as.factor(lcadata$top_a)

lcadata <- lcadata %>% 
  select(CaseID, ifair, afair, qual1, top_i, qual2, top_a, longid, everything()) # reorder columns

table(lcadata$top_i)  # frequency of each topic for items
table(lcadata$top_a)  # frequency of each topic for activities

# Rename topics
levels(lcadata$top_i)[levels(lcadata$top_i)=="1"] <- "Assured Acquiescence"
levels(lcadata$top_i)[levels(lcadata$top_i)=="2"] <- "Man Has Final Say"
levels(lcadata$top_i)[levels(lcadata$top_i)=="3"] <- "Practical Efficiency"
levels(lcadata$top_i)[levels(lcadata$top_i)=="4"] <- "Happy Wife Happy Life"
levels(lcadata$top_i)[levels(lcadata$top_i)=="5"] <- "Taking Turns"
levels(lcadata$top_i)[levels(lcadata$top_i)=="6"] <- "Money Matters"
levels(lcadata$top_i)[levels(lcadata$top_i)=="7"] <- "Work Together"


lcadata$top_i <- factor(lcadata$top_i, levels = c("Practical Efficiency", "Assured Acquiescence", "Money Matters", "Work Together",
                                                  "Taking Turns", "Man Has Final Say", "Happy Wife Happy Life"))

levels(lcadata$top_a)[levels(lcadata$top_a)=="1"] <- "Assured Acquiescence"
levels(lcadata$top_a)[levels(lcadata$top_a)=="2"] <- "Man Has Final Say"
levels(lcadata$top_a)[levels(lcadata$top_a)=="3"] <- "Practical Efficiency"
levels(lcadata$top_a)[levels(lcadata$top_a)=="4"] <- "Happy Wife Happy Life"
levels(lcadata$top_a)[levels(lcadata$top_a)=="5"] <- "Taking Turns"
levels(lcadata$top_a)[levels(lcadata$top_a)=="6"] <- "Money Matters"
levels(lcadata$top_a)[levels(lcadata$top_a)=="7"] <- "Work Together"

lcadata$top_a <- factor(lcadata$top_a, levels = c("Practical Efficiency", "Assured Acquiescence", "Money Matters", "Work Together",
                                                  "Taking Turns", "Man Has Final Say", "Happy Wife Happy Life"))

table(lcadata$top_i)  # frequency of each topic for items
table(lcadata$top_a)  # frequency of each topic for activities

#creating predictor for respondents' preferred decision-maker
lcadata<- lcadata%>%
  mutate(
    ipref = case_when(
      iperson== "Michelle" & idum == 1       			~ "Prefer Michelle",
      iperson== "Anthony"  & idum == 0       			~ "Prefer Michelle",
      iperson== "Michelle" & idum == 0       			~ "Prefer Anthony",
      iperson== "Anthony"  & idum == 1       			~ "Prefer Anthony",
    ))

lcadata<- lcadata%>%
  mutate(
    apref = case_when(
      aperson== "Michelle" & adum == 1       			~ "Prefer Michelle",
      aperson== "Anthony"  & adum == 0       			~ "Prefer Michelle",
      aperson== "Michelle" & adum == 0       			~ "Prefer Anthony",
      aperson== "Anthony"  & adum == 1       			~ "Prefer Anthony",
    ))
table(lcadata$apref )
table(lcadata$adum)
table(lcadata$top_i)

#outputing to dta for multinom table in Stata because we don't know how to do it in R :D
write_dta(lcadata, path = file.path(outDir, "lcadataMultinomTopics.dta")) 

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
  geom_text(aes(label = weights::rd(prop, digits =2)), position = position_fill(reverse = TRUE, vjust = .5), color = "white") +
  theme_minimal(12) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top",
        axis.text.y = element_text(face="bold"),
        plot.title.position = "plot") +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Topic probability by decision type")

fig4

ggsave(filename = file.path(figDir, "fig4.png"), fig4, width=8, height=5, units="in", dpi=300, bg = 'white')

## FIGURE 5 -------------------------------------------------------------------------------------------------
mn_item <- multinom(top_i ~ iperson * relinc + organize + mar + child + dur + item + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

mn_act  <- multinom(top_a ~ aperson * relinc + organize + mar + child + dur + order + activity + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age, data = lcadata, weights = weight)

# https://cran.r-project.org/web/packages/huxtable/vignettes/huxreg.html <- make pretty tables in the future


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
  mutate(type = "purchase")

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
  mutate(type = "activity")

data_fig5 <- rbind(pur.pp, act.pp) 

levels(data_fig5$x)[levels(data_fig5$x)=="Michelle"] <- "She decided"
levels(data_fig5$x)[levels(data_fig5$x)=="Anthony"]  <- "He decided"

data_fig5$x        <- factor(data_fig5$x, levels = c("She decided", "He decided"), ordered = FALSE)
data_fig5$type     <- factor(data_fig5$type, levels = c("purchase", "activity"), ordered = FALSE)
data_fig5$earnings <- factor(data_fig5$relinc, levels = c("Man higher-earner", "Woman higher-earner", "Equal earners"), ordered = FALSE)

data_fig5$response.level[data_fig5$response.level == "Man Has Final Say"]       <- "Man Has\nFinal Say"
data_fig5$response.level[data_fig5$response.level == "Happy Wife Happy Life"]   <- "Happy Wife\nHappy Life"

data_fig5$response.level <- factor(data_fig5$response.level, 
                                    levels = c("Practical Efficiency", "Assured Acquiescence", 
                                               "Money Matters", "Work Together",
                                               "Taking Turns", "Man Has\nFinal Say", "Happy Wife\nHappy Life"))

fig5 <- data_fig5 %>%
  ggplot(aes(x = predicted, y = earnings, fill = x)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), color="#ADB5BD", width=.4) +
  geom_point(size = 3.5, shape=21, alpha = 0.7) +
  facet_grid(response.level ~ type , scales = "free", space = "free", switch = "y") +
  scale_y_discrete(limits=rev, position = "right") +
  scale_x_continuous(limits = c(0, .5), breaks = c(.0, .25, .5)) +
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
        title    = "Predicted probability of topic for purchase and activity decisions",
        subtitle = "by vignette gender and relative earnings",
        caption  = "Predicted percentages calculated from multinomial regression models\nreported in Appendix Table D. Presented with 95% confidence intervals.")

fig5

ggsave(filename = file.path(figDir, "fig5.png"), fig5, width=6.5, height=8, units="in", dpi=300, bg = 'white')
