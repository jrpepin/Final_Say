#-------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_01_measures & sample.R
# Joanna R. Pepin & William J. Scarborough
#-------------------------------------------------------------------------------

# This file creates the variables for analysis and then the analytic sample.

# Table 01 ---------------------------------------------------------------------
## Experimental Design (No table generated)

################################################################################
# Prep the data for analysis
################################################################################

## Load the data ---------------------------------------------------------------
data <- read_sav(file.path(dataDir, rawdata))   # load the raw data file

## Select the Variables --------------------------------------------------------
data <- select(data, CaseID, weight, PPAGE, PPGENDER, PPEDUCAT, PPWORK, PPMARIT, 
               PPINCIMP, PPETHM, REL2, REL1, PPREG4, PPT01, PPT25, PPT612, 
               PPT1317, DOV_RELSTAT, DOV_PARENTST, DOV_EARNINGS, DOV_RELDUR, 
               DOV_ITEM, DOV_PERSON_B04,B05, DOV_ACTIVITY, DOV_PERSON2_B06, B07, 
               B04, B06, B01, B02_Shared, B03_Shared, B02_Individual, 
               B03_Individual, DOV_B02_MaxValue, DOV_B03_MaxValue)

## Rename variables ------------------------------------------------------------
data <- rename(data, 
               age      = PPAGE,           gender    = PPGENDER,       
               educ     = PPEDUCAT,        work      = PPWORK,          
               relate   = PPMARIT,         income    = PPINCIMP,
               race     = PPETHM,          relfreq   = REL2,           
               religion = REL1,            region    = PPREG4,          
               mar      = DOV_RELSTAT,  	 child     = DOV_PARENTST,
               relinc   = DOV_EARNINGS,    dur       = DOV_RELDUR,     
               item     = DOV_ITEM,        iperson   = DOV_PERSON_B04,  
               qual1    = B05,             activity  = DOV_ACTIVITY,
               aperson  = DOV_PERSON2_B06, qual2     = B07,            
               ifair    = B04,             afair     = B06,             
               organize = B01,             herjoint  = B02_Shared,
               hisjoint = B03_Shared,      herindv   = B02_Individual, 
               hisindv  = B03_Individual)

## Generate a codebook ---------------------------------------------------------
sjPlot::view_df(data) # Load codebook in viewer pane
sjPlot::view_df(data, file = file.path(outDir, "codebook.html"))                # Save codebook as html file

### Example of viewing variable and value labels
data$gender %>% attr('label') ### Viewing variable labels
data$gender %>% attr('labels') ### Viewing value labels

## Change class ----------------------------------------------------------------
### SPSS labels https://martinctc.github.io/blog/working-with-spss-labels-in-r/

fcols <- c("gender", "educ", "work", "relate", "income", "race", "relfreq", 
           "religion", "region", "mar", "child", "relinc", "dur", "item", 
           "iperson", "activity", "aperson", "ifair", "afair", "organize", 
           "DOV_B02_MaxValue", "DOV_B03_MaxValue")

icols <- c("age", "PPT01", "PPT25", "PPT612", "PPT1317", 
           "herjoint", "hisjoint", "herindv", "hisindv")

ccols <- c("CaseID", "qual1", "qual2")

data[fcols] <- lapply(data[fcols], sjlabelled::as_label)
data[icols] <- lapply(data[icols], as.integer)
data[ccols] <- lapply(data[ccols], as.character)

################################################################################
# Prep the demographic variables for analysis
################################################################################

## age -------------------------------------------------------------------------
summary(data$age)

## gender ----------------------------------------------------------------------
table(data$gender)
data$gender <-data$gender %>%
  droplevels()
table(data$gender)

## education -------------------------------------------------------------------
table(data$educ)
data$educ <-data$educ %>%
  droplevels()
table(data$educ)

## work ------------------------------------------------------------------------
table(data$work)
data$work <-data$work %>%
  droplevels()

data <- data %>%
  mutate(
    employ = case_when(
      work == "Working - as a paid employee"                  | 
      work == "Working - self-employed"                       ~ "Employed",
      work == "Not working - on temporary layoff from a job"  |
      work == "Not working - looking for work"                ~ "Unemployed",
      work == "Not working - retired"                         |
      work == "Not working - disabled"                        |
      work == "Not working - other"                           ~ "Not in labor force",
      TRUE                                                    ~  NA_character_ 
    ))

data$employ <- factor(data$employ, 
                      levels  = c("Employed", "Unemployed", 
                                  "Not in labor force"), 
                      ordered = FALSE)
table(data$employ)

## relate -------------------------------------------------------------------------
table(data$relate)
data$relate <-data$relate %>%
  droplevels()

data <- data %>%
  mutate(
    relate = case_when(
      relate == "Married"                       ~ "Married",
      relate == "Living with partner"           ~ "Cohabit",
      relate == "Never married"                 ~ "Never married",
      relate == "Divorced"                        |
      relate == "Separated"                       |
      relate == "Widowed"                       ~ "Divorced/Sep./Widow",
      TRUE                                      ~  NA_character_ 
    ))

data$relate <- factor(data$relate, 
                      levels  = c("Married", "Cohabit", "Never married", 
                                 "Divorced/Sep./Widow"), 
                      ordered = FALSE)
table(data$relate)

## income ----------------------------------------------------------------------
table(data$income)
data$income <-data$income %>%
  droplevels() %>%
  as.integer()

data <- data %>%
  mutate(
    incdum = case_when(
      income <= 11          ~ "< than $50,000",
      income >= 12          ~ "> than $50,000",
      TRUE                  ~  NA_character_ 
    ))

data$incdum <- factor(data$incdum, 
                      levels  = c("< than $50,000", "> than $50,000"), 
                      ordered = FALSE)
table(data$incdum)

## race/eth ----------------------------------------------------------------------
table(data$race)
data$race <-data$race %>%
  droplevels()

data <- data %>%
  mutate(
    raceeth = case_when(
      race == "White, Non-Hispanic"           ~ "White",
      race == "Black, Non-Hispanic"           ~ "Black",
      race == "Hispanic"                      ~ "Hispanic",
      race == "Other, Non-Hispanic"              |
      race == "2+ Races, Non-Hispanic"        ~ "Other",
      TRUE                                    ~  NA_character_ 
    ))

data$raceeth <- factor(data$raceeth, 
                       levels  = c("White", "Black", "Hispanic", "Other"), 
                       ordered = FALSE)
table(data$raceeth)

## religion freq ----------------------------------------------------------------
table(data$relfreq)

data <- data %>%
  mutate(
    relfreq = case_when(
      relfreq == "More than once a week"           ~ "Weekly plus",
      relfreq == "Once a week"                     ~ "Weekly",
      relfreq == "Once or twice a month"           ~ "Monthly",
      relfreq == "A few times a year"                 |
      relfreq == "Once a year or less"             ~ "Yearly",
      relfreq == "Never"                           ~ "Never",
      relfreq == "Refused"                         ~ "Unknown",
      TRUE                                         ~  NA_character_ 
    ))

data$relfreq <- factor(data$relfreq, 
                       levels  = c("Weekly plus", "Weekly", "Monthly", 
                                   "Yearly", "Never", "Unknown"), 
                       ordered = FALSE)
table(data$relfreq)

## religion -------------------------------------------------------------------
table(data$religion)

data$religion <-data$religion %>%
  as.integer()

data <- data %>%
  mutate(
    religion = case_when(
      religion == 1          ~ "Catholic",
      religion == 2          ~ "Evangelical or Portestant Christian",
      religion == 13         ~ "None",
      TRUE                   ~ "Other Religion"
    ))

data$religion <- factor(data$religion, 
                        levels  = c("Evangelical or Portestant Christian", 
                                   "Catholic", "None", "Other Religion"), 
                        ordered = FALSE)
table(data$religion)

## region -------------------------------------------------------------------
table(data$region)
data$region <-data$region %>%
  droplevels()
table(data$region)

## parent -------------------------------------------------------------------
table(data$PPT01)
table(data$PPT25)
table(data$PPT612)
table(data$PPT1317)

data <- data %>%
  mutate(
    parent = case_when(
      PPT01  >= 1 |  PPT25  >= 1 | 
      PPT612 >= 1 | PPT1317 >= 1  ~ "Parent",
      TRUE                        ~ "Not a parent"
    ))

table(data$parent)

data$parent <- factor(data$parent, 
                      levels  = c("Not a parent", "Parent"), 
                      ordered = FALSE)

################################################################################
# Prep the vignette variables for analysis
################################################################################

## relate/parent condition -----------------------------------------------------
table(data$mar, data$child)

data <- data %>%
  mutate(
    marpar = case_when(
      mar  == "are married"   & child == "one child together" ~ "Married/Parent",
      mar  == "are married"   & child == "no children"        ~ "Married/No kids",
      mar  == "live together" & child == "one child together" ~ "Cohabit/Parent",
      mar  == "live together" & child == "no children"        ~ "Cohabit/No kids",
      TRUE                                                    ~  NA_character_ 
    ))

data$marpar <- factor(data$marpar, 
                      levels  = c("Cohabit/No kids", "Cohabit/Parent",
                                  "Married/No kids", "Married/Parent"), 
                      ordered = FALSE)
table(data$marpar)

## relative earnings condition ------------------------------------------------------
table(data$relinc)

data$relinc <-data$relinc %>%
  as.integer()

data <- data %>%
  mutate(
    relinc = case_when(
      relinc == 1          ~ "Man higher-earner",
      relinc == 2          ~ "Woman higher-earner",
      relinc == 3          ~ "Equal earners",
      TRUE                 ~ NA_character_
    ))

data$relinc <- factor(data$relinc, 
                      levels  = c("Man higher-earner", "Woman higher-earner", 
                                  "Equal earners"), 
                      ordered = FALSE)
table(data$relinc)

## a non-gendered earner identifier
data <- data %>%
  mutate(
    earner = case_when(
      (relinc  == "Man higher-earner"   & iperson == "Anthony")  |
      (relinc  == "Woman higher-earner" & iperson == "Michelle") ~ "Higher earner",
      (relinc  == "Woman higher-earner" & iperson == "Anthony")  |
      (relinc  == "Man higher-earner"   & iperson == "Michelle") ~ "Lower earner",
      TRUE                                                       ~ "Equal earners"))

data$earner <- factor(data$earner, 
                      levels  = c("Higher earner", "Lower earner",
                                  "Equal earners"), 
                      ordered = FALSE)

## duration condition ----------------------------------------------------------
table(data$dur)

################################################################################
# Prep the vignette response variables for analysis
################################################################################

## purchase (item) condition ---------------------------------------------------
table(data$item)

## activity condition ----------------------------------------------------------
table(data$activity)

## item fairness ---------------------------------------------------------------
table(data$ifair)

data$ifair[data$ifair == "Refused"]   <- NA

data$ifair <-data$ifair %>%
  droplevels()

table(data$ifair)

### dummy var
data <- data %>%
  mutate(
    idum = case_when(
      ifair  == "Very fair"   | ifair == "Somewhat fair"          ~ 1,
      ifair  == "Very unfair" | ifair == "Somewhat unfair"        ~ 0
    ))
data$idum <-as.integer(data$idum)

## activity fairness -----------------------------------------------------------
table(data$afair)

data$afair[data$afair == "Refused"]   <- NA

data$afair <-data$afair %>%
  droplevels()

table(data$afair)

### dummy var
data <- data %>%
  mutate(
    adum = case_when(
      afair  == "Very fair"   | afair == "Somewhat fair"          ~ 1,
      afair  == "Very unfair" | afair == "Somewhat unfair"        ~ 0
    ))
data$adum <-as.integer(data$adum)


## item person -----------------------------------------------------------------
table(data$iperson)

## item person -----------------------------------------------------------------
table(data$aperson)

## repeat decider --------------------------------------------------------------
data <- data %>%
  mutate(
    order = case_when(
      iperson == "Anthony"   & aperson == "Anthony"  ~"Same",
      iperson == "Michelle"  & aperson == "Michelle" ~"Same",
      iperson == "Michelle"  & aperson == "Anthony"  ~"Mixed",
      iperson == "Anthony"   & aperson == "Michelle" ~"Mixed"
    ))

data$order <- factor(data$order, 
                     levels  = c("Same", "Mixed"), 
                     ordered = FALSE)

## organize --------------------------------------------------------------------
table(data$organize)

data$organize[data$organize == "Refused"]   <- NA

data$organize <-data$organize %>%
  droplevels()


data <- data %>%
  mutate(
    organize = case_when(
      organize == "Have a shared account in which they both deposit all their earned income" ~ "Shared",
      organize == "Keep all their earned income in separate, individual accounts"            ~ "Separate",
      organize == "Have both a shared account and separate, individual accounts"             ~ "Both",
      TRUE                                                                                   ~ NA_character_
    ))

data$organize <- factor(data$organize, 
                        levels = c("Shared", "Separate", "Both"), 
                        ordered = FALSE)
table(data$organize)

### Replace allocation range as missing if less than zero dollars

data$herjoint[data$herjoint == -1]   <- NA
data$hisjoint[data$hisjoint == -1]   <- NA
data$herindv[data$herindv == -1]     <- NA
data$hisindv[data$hisindv == -1]     <- NA

data %>% # identify cases where selected both but allocation is all joint
  filter(is.na(herindv) | is.na(hisindv)) %>%
  filter(organize == "Both") %>%
  select(CaseID, herindv, hisindv, herjoint, hisjoint)

## consider dropping cases 902 & 1962

data <- data %>% ## correct cases 1050 & 3866
  mutate(
    herjoint = replace(
      herjoint,
      is.na(herjoint) & organize == "Both" & CaseID != 902 & CaseID != 1962,
      0),
    hisindv = replace(
      hisindv,
      is.na(hisindv) & organize == "Both" & CaseID != 902 & CaseID != 1962,
      0))

data$herjoint


# identify cases where selected both but allocation is only individual
data$jointtot <- data$herjoint + data$hisjoint

data %>%
  filter(is.na(jointtot)   & 
           is.na(herjoint) & 
           is.na(hisjoint) & organize == "Both") %>%
  select(CaseID, herindv, hisindv, herjoint, hisjoint, jointtot)

## will drop cases 1013

################################################################################
# Create the analytic sample
################################################################################

quantdata <- data

nrow(data)
nrow(quantdata)

## list-wise deletion -- resulting in 42 deleted observations
quantdata <- quantdata %>%
  filter(!is.na(organize) & #### COME BACK AND DELETE THIS RESTRICTION
           !is.na(afair) &
           !is.na(ifair))
nrow(data)
nrow(quantdata)

## Generate a new codebook -----------------------------------------------------

## keep only variables needed for analyses

quantdata <- quantdata %>%
  select(CaseID, weight,
         gender, relate, parent, raceeth, educ, employ, incdum, age, religion, 
         relfreq, mar, child, marpar, relinc, earner, dur, organize, 
         herindv, hisindv, herjoint, hisjoint, jointtot,
         item, activity, aperson, iperson, order, adum, afair, idum, ifair,
         qual1, qual2)

sjPlot::view_df(quantdata) # Load codebook in viewer pane
sjPlot::view_df(quantdata, file = file.path(outDir, "codebook_processed.html")) # Save codebook as html file

nrow(quantdata)

################################################################################
# Qualitative Dataset prep
################################################################################

qualdata <- quantdata %>% # create long format dataset
  select(CaseID, qual1, qual2) %>%
  pivot_longer(
    cols = c(qual1, qual2),
    names_to = "x",
    values_to = "qual") %>%  
  group_by(CaseID) %>%  # create a long format id
  mutate(longid = as_numeric(CaseID)*100+row_number()) %>%
  ungroup()
nrow(qualdata)
qualdata$qual <- tolower(qualdata$qual) # make text all lower case
qualdata$qual <- gsub("[[:punct:]]", "", qualdata$qual) # remove special characters


# addressing one-word answers
qualdata$wN <- str_count(qualdata$qual, "\\w+") # count the number of words

oneword <- qualdata %>% # create data object to view one word responses
  filter(wN == 1 | CaseID == 32 | CaseID == 3860)

qualdata <- qualdata %>% 
  mutate(
    qual = case_when(
      qual   == paste("baecausemaybemichellewouldnotenjoytheplacethatanthonydecided",
                      "onandsincetheycaresomuchabouteachotherithinkitisonlyfairand",
                      "sincetheybothputmoneyontherevacationtheybothshouldhaveasayin",
                      "wheretheygo", sep = "") #longid 3201
      ~ paste("baecause maybe michelle would not enjoy the place that anthony",
              "decided on and since they care so much about each other I think it is only",
              "fair and since they both put money on there vacation they both should have",
              "a say in where they go", sep = ""),
      qual   == paste("itsoundstomelikemichelleisgiveingintoanthonysohewontgetupset",
                      "evenifshedoesntlikethemovieanthonypickedtogoandseesometimesif",
                      "peoplecareabouttherepartertheywilldowhattheywantsothereparter",
                      "wontfeelbadarehurt", sep = "")  # longid 3202
      ~ paste("it sounds to me like michelle is giveing into anthony so he wont get upset even if",
              "she doesnt like the movie anthony picked to go and see sometimes if people care about there",
              "parter they will do what they want so there parter wont feel bad are hurt", sep = ""), 
      qual   == "togetherunderstand"    
      ~ "together understand",
      qual   == "theyhavegoodrelatishp" 
      ~ "they have good relatishp",
      qual   == "theyshouldmakedecisionstogether" 
      ~ "they should make decisions together",
      qual   == "decisionsshouldbeequil" 
      ~ "decisions should be equil",     
      qual   == paste("ithinkitwouldhavebeenbetteriftheywouldhavemadethedesisiontogether",
                      "thiscouldstartanadisagreementlater", sep="")       # longid 386001
      ~ paste("i think it would have been better if they would have made the desision",
              "together this could start an a disagreement later", sep=""), 
      qual   == "thesamereasonbeforeanycoupleshouldbeabletoshareadescion" # longid 386002
      ~ "the same reason before any couple should be able to share a descion",   
      TRUE   ~  as.character(qual)
    ))

# addressing spelling
qual.tm <- unnest_tokens(qualdata, word, qual) # create data w/ CaseId & words
head(qual.tm[, c("CaseID", "word")]) # view new dataset

words <- unique(qual.tm$word) # create list of unique words
head(words) # view list of words
length(words) # number of unique words

## find the misspelled words
bad.words <- hunspell(words)

bad.words <- unique(unlist(bad.words))
length(bad.words)

## Extract the first suggestion for each bad word.
sugg.words <- sapply(bad.words, function(x) hunspell_suggest(x)[[1]][1])

head(sugg.words)

## combine objects
word.list <- as.data.frame(cbind(bad.words, sugg.words))

freq.word <- count(qual.tm, word)
freq.word <- inner_join(freq.word, word.list, by = c(word = "bad.words"))

freq.word <- freq.word %>%
  arrange(desc(n))

freq.word$sugg.words <- tolower(freq.word$sugg.words) # make text all lower case
freq.word$sugg.words <- gsub("[[:punct:]]", "", freq.word$sugg.words) # remove special characters

freq.word

## corrected suggested words
freq.word$sugg.words[freq.word$word == "dont"]            <- "do not"
freq.word$sugg.words[freq.word$word == "anthonys"]        <- "anthony"
freq.word$sugg.words[freq.word$word == "michelles"]       <- "michelle"
freq.word$sugg.words[freq.word$word == "couldnt"]         <- "could not"
freq.word$sugg.words[freq.word$word == "shouldnt"]        <- "should not"
freq.word$sugg.words[freq.word$word == "didnt"]           <- "did not"
freq.word$sugg.words[freq.word$word == "isnt"]            <- "is not"
freq.word$sugg.words[freq.word$word == "thats"]           <- "that is"
freq.word$sugg.words[freq.word$word == "im"]              <- "i am"
freq.word$sugg.words[freq.word$word == "wouldnt"]         <- "would not"
freq.word$sugg.words[freq.word$word == "theyre"]          <- "they are"
freq.word$sugg.words[freq.word$word == "wasnt"]           <- "was not"
freq.word$sugg.words[freq.word$word == "michele"]         <- "michelle"
freq.word$sugg.words[freq.word$word == "michael"]         <- "michelle"
freq.word$sugg.words[freq.word$word == "arent"]           <- "are not"
freq.word$sugg.words[freq.word$word == "theres"]          <- "there is"
freq.word$sugg.words[freq.word$word == "aint"]            <- "are not"
freq.word$sugg.words[freq.word$word == "na"]              <- "NA"
freq.word$sugg.words[freq.word$word == "deside"]          <- "decide"
freq.word$sugg.words[freq.word$word == "youre"]           <- "you are"
freq.word$sugg.words[freq.word$word == "whos"]            <- "who is"
freq.word$sugg.words[freq.word$word == "antony"]          <- "anthony"
freq.word$sugg.words[freq.word$word == "theyll"]          <- "they will"
freq.word$sugg.words[freq.word$word == "ive"]             <- "i have"
freq.word$sugg.words[freq.word$word == "theyve"]          <- "they have"
freq.word$sugg.words[freq.word$word == "idk"]             <- "i do not know"
freq.word$sugg.words[freq.word$word == "dicision"]        <- "decision"
freq.word$sugg.words[freq.word$word == "desicion"]        <- "decision"
freq.word$sugg.words[freq.word$word == "wouldve"]         <- "would have"
freq.word$sugg.words[freq.word$word == "couldve"]         <- "could have"
freq.word$sugg.words[freq.word$word == "decesion"]        <- "decision"
freq.word$sugg.words[freq.word$word == "havent"]          <- "have not"
freq.word$sugg.words[freq.word$word == "opion"]           <- "opinion"
freq.word$sugg.words[freq.word$word == "cuz"]             <- "because"
freq.word$sugg.words[freq.word$word == "desision"]        <- "decision"
freq.word$sugg.words[freq.word$word == "andor"]           <- "and or"
freq.word$sugg.words[freq.word$word == "elses"]           <- "elses"
freq.word$sugg.words[freq.word$word == "doesn"]           <- "does not"
freq.word$sugg.words[freq.word$word == "doesnt"]          <- "does not"
freq.word$sugg.words[freq.word$word == "antony"]          <- "anthony"
freq.word$sugg.words[freq.word$word == "desion"]          <- "decision"
freq.word$sugg.words[freq.word$word == "dosent"]          <- "does not"
freq.word$sugg.words[freq.word$word == "finically"]       <- "financially"
freq.word$sugg.words[freq.word$word == "lol"]             <- "lol"
freq.word$sugg.words[freq.word$word == "mutal"]           <- "mutual"
freq.word$sugg.words[freq.word$word == "opinon"]          <- "opinion"
freq.word$sugg.words[freq.word$word == "sayso"]           <- "say so"
freq.word$sugg.words[freq.word$word == "anthonty"]        <- "anthony"
freq.word$sugg.words[freq.word$word == "decession"]       <- "decision"
freq.word$sugg.words[freq.word$word == "decied"]          <- "decided"
freq.word$sugg.words[freq.word$word == "headofhousehold"] <- "head of household"
freq.word$sugg.words[freq.word$word == "heres"]           <- "hers"
freq.word$sugg.words[freq.word$word == "milkshakes"]      <- "michelle"
freq.word$sugg.words[freq.word$word == "soot"]            <- "so"
freq.word$sugg.words[freq.word$word == "heelles"]         <- "michelle"

freq.word$sugg.words  <- freq.word$sugg.words %>% 
  replace_na("unknown") # replacing missing sugg.words

## replacing bad words in qual responses
bad.whole.words <- paste0("\\b", freq.word$word, "\\b")
sugg.words <-  freq.word$sugg.words

vect.corpus <- sugg.words
names(vect.corpus) <- bad.whole.words

qualdata$qual <- stringr::str_replace_all(qualdata$qual, vect.corpus)

# Removing invalid responses 
##(FILE "exclusion criteria qual data' IDENTIFIES INVALID RESPONSES)

## Don't know responses 

dk <- qualdata$longid %in% 
  c(   1401,   1402,  24202,  28102,  31501,  31502,  52701,  57201, 
      57202,  60302,  64401,  64402,  69202, 102001, 102002, 111901, 
      112301, 112302, 112802, 119301, 159702, 180302, 187501, 188802,
      195402, 201202, 221601, 225902, 268102, 306001, 306002, 317001, 
      317002, 330601, 346001, 346102, 362102, 391201, 413102, 424402)

refused <- qualdata$longid %in%
  c(   1501,	  1502,	  1601,	  1602,	  1802,	 2202,	  4401,	 16501,	
      16502,	 17901,  17902,	 21801,	 21802,	 24802,	 29202,	 31301,	
      31701,	 31702,  32202,	 32301,	 32402,	 39302,	 40901,	 40902,	
      44301,	 44302,  56802,	 59202,	 67902,	 68601,	 68602,  83801,	
      85802,	 90001,  90002,	 91002,	 91301,	 91302,	 95301,	 95302,	
      97202,   98102,	 98401,	105401,	114602,	114901,	117402,	120602,	
      127002,	129302,	135702,	140202,	141501,	141502,	144502,	145202,	
      158702,	165301,	165302,	175301,	175302,	187501,	187801,	187802,	
      195402,	196201,	196202,	203501,	204502,	210701,	224101,	224102,	
      225901,	225902,	237702,	255502,	258302,	262501,	262502,	265602,	
      266802,	275501,	275502,	277902,	278801,	280402,	284301,	284302,	
      286702,	297101,	297102,	310902,	324002,	324101,	324601,	324602,	
      332301,	344901,	347601,	347602,	347902,	348701,	348702,	348901,	
      348902,	349001,	349901,	349902,	350102,	359501,	368402,	377601,	
      377602,	382602,	382802,	383601,	384302,	390602,	391202,	391402,	
      399801,	399802,	408201,	408202,	408301,	408302,	409301,	409302,	
      410201,	410202)

qualdata$sample          <-"sample"
qualdata$sample[dk]      <- "dont know"
qualdata$sample[refused] <- "refused"

## Response says the response is the same as the last one
same <- qualdata$longid %in%
  c(   3502,	  3602,  13102,	 15702,	 21202,	 29602,	 31302,	 38502,
      39502,	 52202,	 63602,	 63702,	 79702,	 85002,	 89202,	 90802,
      92102,	 94902,  98102,	103902,	107002,	111902,	115802,	135002,	
      140802, 167402,	177602,	184402,	185102,	187502,	195702,	200302,	
      201402,	203302,	211802,	213002,	214702,	222302,	233002,	243802,	
      252702,	258202,	260802,	269002,	269702,	273002,	275302,	295102,	
      300802,	316602,	332602,	370302,	393002,	410902,	412002)

qualdata$same        <- 0
qualdata$same[same]  <- 1

qualdata <- qualdata %>% 
  group_by(CaseID) %>% 
  mutate(qual2 = case_when(
    row_number()==1 ~ qual,
    TRUE ~  NA_character_)) %>%
  fill(qual2) %>%
  ungroup() %>%
  mutate(qual = ifelse(same == 1, qual2, qual)) %>%
  select(-qual2)

# Ensuring consistent coding for "50/50" which came up sometimes
qualdata$qual <- str_replace(qualdata$qual, "50 50", "5050")
qualdata$qual <- str_replace(qualdata$qual, "50  50", "5050")
qualdata$qual <- str_replace(qualdata$qual, "fifty fifty", "5050")

# Add item/activity fairness perception
fairdata <- data %>% # create long format dataset
  select(CaseID, idum, adum) %>%
  pivot_longer(
    cols = c(idum, adum),
    names_to = "topic",
    values_to = "fair") %>%  
  group_by(CaseID) %>%  # create a long format id
  mutate(longid = as_numeric(CaseID)*100+row_number()) %>%
  ungroup()

qualdata <- left_join(qualdata, fairdata) 

# -- clean up global environment
remove(fairdata)
remove(oneword)
remove(qual.tm)
remove(word.list)
remove(freq.word)

################################################################################
# Create the analytic sample
################################################################################

# generating data frame with all cases (decision was fair and unfair)
qualdataFULL <- qualdata     %>%
  filter(sample == "sample") %>% 
  filter(wN > 0) 

## Sample sizes
length(unique(qualdataFULL[["CaseID"]])) #3853
length(qualdataFULL[["CaseID"]]) #7570 person-responses

# -- if only want to look at fair responses
# qualdata <- qualdata %>%
#   filter(fair == 1) %>% # n = 4,798
#   filter(sample == "sample") %>% # n = 4,689
#   filter(wN > 0) # 4539 (number of words greater than zero)
# 
## Sample sizes
# length(unique(quantdata[["CaseID"]])) #3978
# length(unique(qualdata[["CaseID"]]))  #2891

message("End of FS_01_measures and sample") # Marks end of R Script
