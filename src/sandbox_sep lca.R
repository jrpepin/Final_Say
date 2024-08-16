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

names(out_list) <- c("High-stakes", "Low-stakes") # Rename lists 

out_list$`High-stakes`[["stakes"]] <- "High-stakes" # Add list identifier
out_list$`Low-stakes`[["stakes"]]  <- "Low-stakes"

new_df <- as_tibble(rbind(out_list$`High-stakes`,  out_list$`Low-stakes`))


tabS5 <- new_df %>%
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
  
## Note shading cells doesn't work, so must do that manually (sorry)
read_docx() %>% 
  body_add_par(paste("Table S5. Average Probabilistic Coherence for LDA Models with Independent Topic Models by Decision-Type")) %>% 
  body_add_flextable(value = tabS5) %>% 
  print(target = file.path(outDir, "finalsay_tableS5.docx")) # save table

### TOP WORDS ------------------------------------------------------------------

in_list2  <- list(out_list[[1]], out_list[[2]])
out_list3 <- lapply(in_list2, function(mSTEMMED){
  
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
  left_join(., theta_sum) %>%
  dplyr::arrange(desc(phi))

})

names(out_list3) <- c("High", "Low") # Rename lists 
out_list3$`High`[["stakes"]] <- "High-stakes" # Add list identifier
out_list3$`Low`[["stakes"]]  <- "Low-stakes"

tabS6 <- as_tibble(rbind(out_list3$`High`,  out_list3$`Low`))

