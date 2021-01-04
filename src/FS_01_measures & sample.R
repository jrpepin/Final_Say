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
data <- read_sav(file.path(dataDir, rawdata))

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
               region   = PPREG4,         mar       = DOV_RELSTAT,  	parent     = DOV_PARENTST,
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
          "region", "mar", "parent", "relinc", "dur", "item", "iperson", "activity", 
           "aperson", "ifair", "afair", "organize", "PPT01", "PPT25", "PPT612", "PPT1317",
           "DOV_B02_MaxValue", "DOV_B03_MaxValue" )

icols <- c("age", "herjoint", "hisjoint", "herindv", "hisindv" )

ccols <- c("CaseID", "iqual", "aqual")

data[fcols] <- lapply(data[fcols], as_label)
data[icols] <- lapply(data[icols], as.integer)
data[ccols] <- lapply(data[ccols], as.character)


#####################################################################################
# Prep the variables for analysis
#####################################################################################

## gender -------------------------------------------------------------------------

table(data$gender)
