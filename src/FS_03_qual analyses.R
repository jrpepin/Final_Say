#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_03_qual analyses.R
# William J. Scarborough & Joanna R. Pepin
#------------------------------------------------------------------------------------

# This file analyzes the qualitative responses.
safe <- qualdata
qualdata <- safe

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


#####################################################################################
# Selected 6 Topic Model
#####################################################################################
set.seed(376)
model <- FitLdaModel(dtm             = mSTEMMED, 
                     k               = 6,    
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

# top words
model$top_terms <- GetTopTerms(phi = model$phi, M = 50)
t(model$top_terms)
topterms        <- data.frame(model$top_terms)
topterms <- tibble::rownames_to_column(topterms, "rank") 
topterms$rank <- as_numeric(topterms$rank)

write_xlsx(topterms,  path = file.path(outDir, "topterms6topicfair.xlsx"))

# Table of Phi, which is where top words come from. Used to plot words in word clouds or dot plots
phi<-model$phi
phi<-data.frame(phi)
write_xlsx(phi,  path = file.path(outDir, "phi6topicfair.xlsx"))

# Assigning docs to topics with probabilities
set.seed(376)
assign <- predict(model, mSTEMMED,
                  iterations = 1000, 
                  burnin     = 100)

str(assign) # assign

# output to xlsx
assignments <-data.frame(assign)
assignments <- tibble::rownames_to_column(assignments, "longid") 
assignments$longid <- as_numeric(assignments$longid)
str(assignments )
head(assignments)

assign <- left_join(qualdata, assignments) %>%
  select(-c(qual, wN, same, topic, fair))


write_xlsx(assign , path = file.path(outDir,"assignments6topicfair.xlsx"))

#####################################################################################
# LDA Vis
#####################################################################################

## Combine topterms and phi values
df1 <- topterms %>%
  pivot_longer(cols = starts_with("t_"), names_to = "topic", values_to = "word")

df2 <- data.frame(t(phi[-1]))
df2 <- tibble::rownames_to_column(df2, "word")

df2 <- df2 %>%
  pivot_longer(!word, names_to = "topic", values_to = "phi")

clouddata <- left_join(df1, df2) %>%
  dplyr::arrange(desc(phi))


## Clean dataset
clouddata <- clouddata %>% # label topics
  mutate(topic = case_when(
     topic == "t_1" ~ "Topic 5: Man has Final Say",
     topic == "t_2" ~ "Topic 3: Give & Take",
     topic == "t_3" ~ "Topic 1: Taking Turns",
     topic == "t_4" ~ "Topic 6: Happy Wife, Happy Life",
     topic == "t_5" ~ "Topic 4: Some's Gotta Choose",
     topic == "t_6" ~ "Topic 2: Money Matters",
  ))

clouddata$topic <- factor(clouddata$topic, 
                          levels = c("Topic 1: Taking Turns", 
                                     "Topic 2: Money Matters", 
                                     "Topic 3: Give & Take",
                                     "Topic 4: Some's Gotta Choose",
                                     "Topic 5: Man has Final Say",
                                     "Topic 6: Happy Wife, Happy Life"), ordered = FALSE)

clouddata <- clouddata %>% # top 5 indicator
  mutate(top5 = case_when(
    rank <= 5 ~ "yes",
    TRUE      ~ "no"))
    
## Create wordcloud

fig_wc <- clouddata %>%
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
        panel.spacing        = unit(1.5, "lines")) +
  labs(title    = "Word clouds with highest-ranking word stems per topic", 
       subtitle = "Word stems weighted by probability of being found in topic")

ggsave(file.path(figDir, "fig_wc.png"), fig_wc, height = 9, width = 6, units="in",dpi = 300)



