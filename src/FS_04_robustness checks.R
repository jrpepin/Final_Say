#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_04_robustness checks.R
# William J. Scarborough & Joanna R. Pepin
#------------------------------------------------------------------------------------

# This file includes the codes for the robustness checks, 
# including those mentioned in notes and the appendix.


# NOTE 3 ############################################################################
# Our findings remain similar when we omit decisions on vacation destinations from our analysis.

robustQD <- quantdata[!(quantdata$item=="where to go on vacation"),]
table(robustQD$item)

logit1robust <- glm(idum ~ perI + relinc + organize + mar + child + dur + item + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age,
                    robustQD , weights = weight, family="binomial")

logit3robust <- glm(idum ~ perI * relinc + organize + mar + child + dur + item + 
                      gender+relate+parent+raceeth+educ+employ+incdum+age,
                    robustQD , weights = weight, family="binomial")

## Panel A ------------------------------------------------------------------------------------
AME_log1robust <- summary(margins(logit1robust, variables = c("perI", "relinc")))

AME_log1robust 

## Panel B ------------------------------------------------------------------------------------
### Item **************************************************************************************
AME3robust <- summary(margins(logit3robust, 
                              variables = "relinc",
                              at = list(perI = 0:1)))
AME3robust 


#----------------------------------------------------------------------------------------------
#Topic model analysis
#Checking if similar topics emerge when vacation is omitted
#Summary: same topics emerge. But a five-topic model now fits the data better here.
#the five topic result essentially splits Equality or Bust into 2 topics (topics 2 & 3 in results below)
#one is rooted in compromise (topic 2), the other in discussion (topic 3).
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
#Topic Modelling Check w/out Vacation Item
# Add item type
itemdata <- data %>% # create long format dataset
  select(CaseID, item) %>%
  pivot_longer(
    cols = c(item),
    values_to = "item") %>%    
  group_by(CaseID) %>%  # create a long format id
  mutate(longid = as_numeric(CaseID)*100+row_number()) %>%
  ungroup()

ls(itemdata)
table(itemdata$item)

qualdata <- left_join(qualdata, itemdata) 
ls(qualdata)
table(qualdata$item)

# This file analyzes the qualitative responses.

robustQualD <- qualdata[!(qualdata$item=="where to go on vacation"),]
table(robustQualD$item)

# generating corpus
Corpusprep <-data.frame(
  doc_id=robustQualD$longid,
  text=robustQualD$qual
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
robustQualD$qual[15] # compared to this for check

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

#------------------------------------------------------------------------------------
# Fitting Model

## Fit a 1 through 20 LDA models and Compare Coherence
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

#------------------------------------------------------------------------------------
# Selected 4 Topic Model

set.seed(376)
model <- FitLdaModel(dtm             = mSTEMMED, 
                     k               = 5,    
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


# NOTE 5 ############################################################################
 # We interacted the gender of the decision maker presented in the vignette with 
 # the order of vignette conditions (e.g. whether the same or different gender decision 
 # maker was shown for decisions on family purchases and shared activities). 

# TABLE C ############################################################################

 # Table C. Results of Fixed Effects Models: 
 # Average Marginal Effects of Decision-Maker Gender by Decision Type on Perceptions of Fairness

   # Lacking confidence in ability to make these estimates in R. 
   # Used Stata results in paper instead. See the .do file to replicate paper results :(

femodels <- femodels %>%
  select(CaseID, idum, adum, perI, perA, weight) %>%
  rename(dum_1 = idum,  dum_2  = adum,
         per_1 = perI,  per_2  = perA) %>%
  pivot_longer(
    !c(CaseID, weight), names_to = c(".value", "decision"), names_sep = "_") %>%
  mutate(per      = case_when(per == 0 ~ "Anthony",
                              per == 1 ~ "Michelle"),
         decision = as.numeric(decision))

# survival package
m <- survival::clogit(dum ~ per + decision + per * decision +strata(as.numeric(CaseID)), 
                      data = femodels, method = "efron", weights = weight)
summary(m)

# fixest package
mod <- fixest::feglm(dum ~ per + decision + per * decision | CaseID,
                     panel.id = ~ CaseID,
                     binomial(link = "logit"),
                     data = femodels,
                     weights = ~ weight)
summary(mod)

# TABLE D ############################################################################

 # this tests only vacation (as purchase) to weekend outing (as activity)
 # to isolate role of planning in decision-making, because these are both sort of
 # leisure activities, but vacays require much more planning.

vwQD <- quantdata[(quantdata$item=="where to go on vacation") & (quantdata$activity=="shared weekend activities"), ]
table(vwQD$item)
table(vwQD$activity)


logit1vw <- glm(idum ~ perI + relinc + organize + mar + child + dur +
                  gender+relate+parent+raceeth+educ+employ+incdum+age,
                vwQD, weights = weight, family="binomial")

logit2vw <- glm(adum ~ perA + relinc + organize + mar + child + dur + order +
                  gender+relate+parent+raceeth+educ+employ+incdum+age,
                vwQD , weights = weight, family="binomial")

logit3vw <- glm(idum ~ perI * relinc + organize + mar + child + dur +
                  gender+relate+parent+raceeth+educ+employ+incdum+age,
                vwQD , weights = weight, family="binomial")

logit4vw <- glm(adum ~ perA * relinc + organize + mar + child + dur + order +
                  gender+relate+parent+raceeth+educ+employ+incdum+age,
                vwQD , weights = weight, family="binomial")


## Panel A ------------------------------------------------------------------------------------
AME_log1vw  <- summary(margins(logit1vw,  variables = c("perI", "relinc")))
AME_log2vw  <- summary(margins(logit2vw,  variables = c("perA", "relinc")))

# test equality of coefficients between Item & Activity
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
z_genderAvw <- (AME_log1vw[[1,2]] - AME_log2vw[[1,2]]) / sqrt(AME_log1vw[[1,3]]^2 + AME_log2vw[[1,3]]^2)
z_equalAvw  <- (AME_log1vw[[2,2]] - AME_log2vw[[2,2]]) / sqrt(AME_log1vw[[2,3]]^2 + AME_log2vw[[2,3]]^2)
z_fmoreAvw  <- (AME_log1vw[[3,2]] - AME_log2vw[[3,2]]) / sqrt(AME_log1vw[[3,3]]^2 + AME_log2vw[[3,3]]^2)

p_genderAvw <- 2*pnorm(-abs(z_genderAvw)) 
p_equalAvw  <- 2*pnorm(-abs(z_equalAvw)) 
p_fmoreAvw  <- 2*pnorm(-abs(z_fmoreAvw)) 

AME_log1vw
AME_log2vw

print(paste("Gender test of equality: p =", round(p_genderAvw, digits = 3)))
print(paste("Woman Higher-Earner test of equality: p =", round(p_fmoreAvw, digits = 3)))
print(paste("Equal test of equality: p =", round(p_equalAvw, digits = 3)))


## Panel B ------------------------------------------------------------------------------------

### Item **************************************************************************************
AME3vw <- summary(margins(logit3vw, 
                          variables = "relinc",
                          at = list(perI = 0:1)))
AME3vw
## test difference between coefficients
### create Z scores
z_equalIvw <- (AME3vw[[1,3]] - AME3vw[[2,3]]) / sqrt(AME3vw[[1,4]]^2 + AME3vw[[2,4]]^2)
z_womanIvw <- (AME3vw[[3,3]] - AME3vw[[4,3]]) / sqrt(AME3vw[[3,4]]^2 + AME3vw[[4,4]]^2)
z_equalIvw
z_womanIvw
### create p values
p_equalIvw  <- 2*pnorm(-abs(z_equalIvw))
p_womanIvw  <- 2*pnorm(-abs(z_womanIvw))

### report p values
print(paste("(ITEM) test of equality-- Equal Earners * gender: p =", round(p_equalIvw, digits = 3)))
print(paste("(ITEM) test of equality-- Woman Higher-Earner * gender: p =", round(p_womanIvw, digits = 3)))


### Activity **********************************************************************************
AME4vw <- summary(margins(logit4, 
                          variables = "relinc",
                          at = list(perA = 0:1)))
AME4vw
## test difference between coefficients
### create Z scores
z_equalAvw  <- (AME4vw[[1,3]] - AME4vw[[2,3]]) / sqrt(AME4vw[[1,4]]^2 + AME4vw[[2,4]]^2)
z_womanAvw  <- (AME4vw[[3,3]] - AME4vw[[4,3]]) / sqrt(AME4vw[[3,4]]^2 + AME4vw[[4,4]]^2)
z_equalAvw
z_womanAvw

### create p values
p_equalAw  <- 2*pnorm(-abs(z_equalAvw))
p_womanAvw  <- 2*pnorm(-abs(z_womanAvw))

### report p values
print(paste("(ACTIVITY) test of equality-- Equal Earners * gender: p =", round(p_equalAvw, digits = 3)))
print(paste("(ACTIVITY) test of equality-- Woman Higher-Earner * gender: p =", round(p_womanAvw, digits = 3)))

### test equality of coefficients between Item & Activity ************************************

## Z scores
z_equal_MAN_ABvw  <- (AME3vw[[1,3]] - AME4vw[[1,3]]) / sqrt(AME3vw[[1,4]]^2 + AME4vw[[1,4]]^2)
z_equal_FEM_ABvw  <- (AME3vw[[2,3]] - AME4vw[[2,3]]) / sqrt(AME3vw[[2,4]]^2 + AME4vw[[2,4]]^2)

z_lower_MAN_ABvw  <- (AME3vw[[3,3]] - AME4vw[[3,3]]) / sqrt(AME3vw[[3,4]]^2 + AME4vw[[3,4]]^2)
z_lower_FEM_ABvw  <- (AME3vw[[4,3]] - AME4vw[[4,3]]) / sqrt(AME3vw[[4,4]]^2 + AME4vw[[4,4]]^2)

z_equal_MAN_ABvw
z_equal_FEM_ABvw
z_lower_MAN_ABvw
z_lower_FEM_ABvw

## p values
p_lower_MAN_ABvw  <- 2*pnorm(-abs(z_lower_MAN_ABvw)) 
p_equal_MAN_ABvw  <- 2*pnorm(-abs(z_equal_MAN_ABvw)) 

p_lower_FEM_ABvw  <- 2*pnorm(-abs(z_lower_FEM_ABvw)) 
p_equal_FEM_ABvw  <- 2*pnorm(-abs(z_equal_FEM_ABvw)) 

print(paste("(ITEM VS ACT) Man * Equal Earners test of equality: p =", round(p_equal_MAN_ABvw, digits = 3)))
print(paste("(ITEM VS ACT) Female * Equal Earners test of equality: p =", round(p_equal_FEM_ABvw, digits = 3)))

print(paste("(ITEM VS ACT) Man * Lower-Earner test of equality: p =", round(p_lower_MAN_ABvw, digits = 3)))
print(paste("(ITEM VS ACT) Female * Lower-Earner test of equality: p =", round(p_lower_FEM_ABvw, digits = 3)))

