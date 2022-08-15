#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_02_quant analyses robustness check
# Joanna R. Pepin & William J. Scarborough
#------------------------------------------------------------------------------------

# This is for note 5 in methods section of paper.
# confirming findings when we omit decision on vacation destination from the analyses.

#------------------------------------------------------------------------------------
# Confirming logistic regression results 

## Specify reference levels
quantdata$relate   <- relevel(quantdata$relate,   ref = "Never married")
quantdata$raceeth  <- relevel(quantdata$raceeth,  ref = "White")
quantdata$educ     <- relevel(quantdata$educ,     ref = "High school")
quantdata$employ   <- relevel(quantdata$employ,   ref = "Employed")
quantdata$incdum   <- relevel(quantdata$incdum,   ref = "< than $50,000")
quantdata$earner   <- relevel(quantdata$earner,   ref = "Higher earner")
quantdata$order    <- relevel(quantdata$order,    ref = "Same")
quantdata$perI     <- as.numeric(quantdata$iperson == "Michelle") # create dummy variables
quantdata$perA     <- as.numeric(quantdata$aperson == "Michelle") # create dummy variables

table(quantdata$item)

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


#------------------------------------------------------------------------------------
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

