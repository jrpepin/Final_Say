#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_00_setup and packages.R
# Joanna R. Pepin & William J. Scarborough (https://github.com/WilliamScarborough)
#------------------------------------------------------------------------------------

#####################################################################################
## Install and load required packages
#####################################################################################
# install.packages("pacman")         # Install pacman package if not installed
library("pacman")                  # Load pacman package

# Install packages not yet installed & load them
pacman::p_load(
       here,       # relative file paths
       dplyr, 
       tidyr, 
       haven,      # import spss data file with labels 
       expss,      # add variable labels 
       sjlabelled, # convert labels to factors
       sjPlot,     # generate a codebook
       survey,     # analyze survey data
       srvyr,      # analyze survey data
       nnet,       # multinomial models
       gtsummary,  # make tables
       kableExtra, # make tables
       margins,    # average marginal effects
       cowplot,    # graphing
       ggplot2,    # graphing
       ggeffects,  # predicted probabilities
       sjmisc,
       colorspace, 
       ggrepel,    # graphing
       ggpubr,
       ggtext,     # Color labels in ggtitle
       scales,     # percentages for ggplots axes
       officer,    # producing word output
       flextable,  # producing word output
       tidytext,   # addressing spelling
       hunspell,
       stringi,
       stringr,
       # LDA Analysis
       magrittr,
       tm,
       wordcloud,
       RColorBrewer,
       topicmodels,
       plyr,
       e1071,
       foreign,
       readxl,
       writexl,
       ldatuning,
       textmineR,
       stopwords,
       foreach,
       LDAvis,
       gistr,
       extrafont,
       tagcloud, 
       RJSONIO,
       ggwordcloud,
       weights, # drop leading 0 in geom_text labels
       colorspace #color palettes of figures
       )

if(!require(conflicted)){
  devtools::install_github("r-lib/conflicted")
  library(conflicted)
}

# Address any conflicts in the packages
conflict_scout() # Identify the conflicts
conflict_prefer("remove", "base")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("ggsave", "cowplot")
conflict_prefer("replace_na", "tidyr")
conflict_prefer("here", "here")
conflict_prefer("read_stata", "haven")
conflict_prefer("vars", "ggplot2")

#####################################################################################
# Download the data
#####################################################################################
## Public Perception of Money in Families
## Data can be accessed here: http://tessexperiments.org/study/pepin791
## Data import code assumes the researcher downloaded the Stata data files.

rawdata <- "TESS3_217_Pepin_Client.sav"           # Name of the data file downloaded

#####################################################################################
# Set-up the Directories
#####################################################################################

projDir <- here()                                 # File path to this project's directory
dataDir <- here("../../Data/TESS")                # Name of folder where the TESS data was downloaded
srcDir  <- "src"                                  # Name of the sub-folder where we will save our source code (R scripts)
funDir  <- "src/functions"                        # File path where we will save our functions
qualDir <- "src/qual"                             # File path where saved qualitative coding results
outDir  <- "output"                               # Name of the sub-folder where we will save results
figDir  <- "figs"                                 # Name of the sub-folder where we will save generated figures

## This will create sub-directory folders in the projDir if they don't exist
if (!dir.exists(here::here(srcDir))){
  dir.create(srcDir)
} else {
  print("SRC directory already exists!")
}

if (!dir.exists(here::here(outDir))){
  dir.create(outDir)
} else {
  print("Output directory already exists!")
}

if (!dir.exists(here::here(figDir))){
  dir.create(figDir)
} else {
  print("Figure directory already exists!")
}

if (!dir.exists(here::here(qualDir))){
  dir.create(qualDir)
} else {
  print("Qualitative data directory already exists!")
}

message("End of FS_00_setup and packages") # Marks end of R Script
