#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_01_measures & sample.R
# Joanna R. Pepin
#------------------------------------------------------------------------------------

# This file creates the variables for analysis.

#####################################################################################
# Prep the data for analysis
#####################################################################################

## Load the data --------------------------------------------------------------------
data <- read_sav(file.path(dataDir, rawdata))   # load the raw data file
qual <- read_dta(file.path(qualDir, qualdata))  # load the qualitative data file

data <- merge(x = data, y = qual, by = "CaseID", all.x = TRUE) # Join the two data files
remove(qual) # clean up the global enivronment

## Select the Variables -------------------------------------------------------------
data <- select(data, CaseID, PPAGE, PPGENDER, PPEDUCAT, PPWORK, PPMARIT, PPINCIMP, PPETHM, 
               REL2, REL1, PPREG4, PPT01, PPT25, PPT612, PPT1317,
               DOV_RELSTAT, DOV_PARENTST, DOV_EARNINGS, DOV_RELDUR, DOV_ITEM, DOV_PERSON_B04,
               B05, DOV_ACTIVITY, DOV_PERSON_B06, B07, B04, B06, B01, B02_Shared,
               B03_Shared, B02_Individual, B03_Individual, DOV_B02_MaxValue, DOV_B03_MaxValue)

## Rename variables -----------------------------------------------------------------
data <- rename(data, 
               age      = PPAGE,          gender    = PPGENDER,       educ       = PPEDUCAT,
               work     = PPWORK,         relate    =	PPMARIT,        income     = PPINCIMP,
               race     = PPETHM,         relfreq   = REL2,           religion   = REL1,
               region   = PPREG4,         mar       = DOV_RELSTAT,  	child      = DOV_PARENTST,
               relinc   = DOV_EARNINGS,   dur       = DOV_RELDUR,     item       = DOV_ITEM,
               iperson  = DOV_PERSON_B04, iqual     = B05,            activity   = DOV_ACTIVITY,
               aperson  = DOV_PERSON_B06, aqual     = B07,            ifair      = B04,
               afair    = B06,            organize  = B01,            herjoint   = B02_Shared,
               hisjoint = B03_Shared,     herindv   = B02_Individual, hisindv    = B03_Individual)

## Generate a codebook -------------------------------------------------------------
sjPlot::view_df(data) # Load codebook in viewer pane
sjPlot::view_df(data, file = file.path(outDir, "codebook.html")) # Save codebook as html file

### Example of viewing variable and value labels
data$gender %>% attr('label') ### Viewing variable labels
data$gender %>% attr('labels') ### Viewing value labels

## Change class ---------------------------------------------------------------------
### Working with SPSS labels https://martinctc.github.io/blog/working-with-spss-labels-in-r/

fcols <- c("gender", "educ", "work", "relate", "income", "race", "relfreq", "religion",
          "region", "mar", "child", "relinc", "dur", "item", "iperson", "activity", 
           "aperson", "ifair", "afair", "organize", "DOV_B02_MaxValue", "DOV_B03_MaxValue")

icols <- c("age", "PPT01", "PPT25", "PPT612", "PPT1317", 
           "herjoint", "hisjoint", "herindv", "hisindv")

ccols <- c("CaseID", "iqual", "aqual")

data[fcols] <- lapply(data[fcols], as_label)
data[icols] <- lapply(data[icols], as.integer)
data[ccols] <- lapply(data[ccols], as.character)


#####################################################################################
# Prep the demographic variables for analysis
#####################################################################################

## age -----------------------------------------------------------------------------
summary(data$age)

## gender --------------------------------------------------------------------------
table(data$gender)
  data$gender <-data$gender %>%
    droplevels()
  levels(atus$sex) <- c('Male', 'Female')
table(data$gender)

## education -----------------------------------------------------------------------
table(data$educ)
  data$educ <-data$educ %>%
    droplevels()
table(data$educ)

## work ----------------------------------------------------------------------------
table(data$work)
  data$work <-data$work %>%
    droplevels()

  data <- data %>%
    mutate(
      employ = case_when(
        work   == "Working - as a paid employee"                  | 
        work == "Working - self-employed"                       ~ "Employed",
        work   == "Not working - on temporary layoff from a job"  |
        work == "Not working - looking for work"                ~ "Unemployed",
        work   == "Not working - retired"                         |
        work == "Not working - disabled"                          |
        work == "Not working - other"                           ~ "Not in labor force",
        TRUE                                                    ~  NA_character_ 
      ))
  
  data$employ <- factor(data$employ, levels = c("Employed", "Unemployed", "Not in labor force"), ordered = FALSE)
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

  data$relate <- factor(data$relate, levels = c("Married", "Cohabit", "Never married", "Divorced/Sep./Widow"), ordered = FALSE)
table(data$relate)

## income -------------------------------------------------------------------------
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
  
  data$incdum <- factor(data$incdum, levels = c("< than $50,000", "> than $50,000"), ordered = FALSE)
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
  
  data$raceeth <- factor(data$raceeth, levels = c("White", "Black", "Hispanic", "Other"), ordered = FALSE)
table(data$raceeth)

## religion freq ----------------------------------------------------------------
table(data$relfreq)

  data <- data %>%
    mutate(
      relfreq = case_when(
        relfreq == "More than once a week"           ~ "Weekly plus",
        relfreq == "Once a week"                     ~ "Weekly",
        relfreq == "Once or twice a month"           ~ "Monthly",
        relfreq == "A few times a year"              |
        relfreq == "Once a year or less"             ~ "Yearly",
        relfreq == "Never"                           ~ "Never",
        relfreq == "Refused"                         ~ "Unknown",
        TRUE                                         ~  NA_character_ 
      ))

  data$relfreq <- factor(data$relfreq, levels = c("Weekly plus", "Weekly", "Monthly", "Yearly", "Never", "Unknown"), ordered = FALSE)
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

  data$religion <- factor(data$religion, levels = c("Evangelical or Portestant Christian", "Catholic", 
                                                    "None", "Other Religion"), ordered = FALSE)
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


#####################################################################################
# Prep the vignette variables for analysis
#####################################################################################

## relate/parent condition ----------------------------------------------------------
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

  data$marpar <- factor(data$marpar, levels = c("Cohabit/No kids", "Cohabit/Parent", 
                                                "Married/No kids", "Married/Parent"), ordered = FALSE)
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

data$relinc <- factor(data$relinc, levels = c("Man higher-earner", "Woman higher-earner", 
                                              "Equal earners"), ordered = FALSE)
table(data$relinc)

#####################################################################################
# Prep the power response variables for analysis
#####################################################################################

### Come back to these -- will need to restructe data and combine columns after merge

## item condition -------------------------------------------------------------------
table(data$item)

## activity condition ---------------------------------------------------------------
table(data$activity)

## item fairness --------------------------------------------------------------------
table(data$ifair)

  data$ifair[data$ifair == "Refused"]   <- NA
  
  data$ifair <-data$ifair %>%
    droplevels()
  
table(data$ifair)

## activity fairness ----------------------------------------------------------------
table(data$afair)

data$afair[data$afair == "Refused"]   <- NA

data$afair <-data$afair %>%
  droplevels()

table(data$afair)

## item person ----------------------------------------------------------------------
table(data$iperson)

## item person ----------------------------------------------------------------------
table(data$aperson)

#####################################################################################
# Prep the qualitative coding variables for analysis
#####################################################################################

## reason ---------------------------------------------------------------------------
data <- data %>%
  mutate(
    reason = case_when(
      money    ==1              ~ "$ Talks",
      equality ==1              ~ "Equality or Bust",
      hoh      ==1 | wife==1    ~ "Gender Trumps All",
      giving   ==1              ~ "Giving In",
      hastobe  ==1              ~ "Has to be Made",
      itemsp   ==1              ~ "Item Specific",
      trades   ==1 | forfam==1  ~ "Other",
      unclear  ==1 | noansw==1  ~ "Unclear/No Answer",
      TRUE                      ~ NA_character_
    ))

table(data$reason)

## sexism type ----------------------------------------------------------------------
table(data$hoh)
table(data$wife)

  data <- data %>%
    mutate(
      sexism = case_when(
        hoh  == 1         ~ "Head of household",
        wife == 1         ~ "Happy wife",
        TRUE              ~ NA_character_
      ))

table(data$sexism)
