#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_00_setup and packages.R
# Joanna R. Pepin
#------------------------------------------------------------------------------------

#####################################################################################
## Install and load required packages
#####################################################################################
if(!require(renv)){           # https://rstudio.github.io/renv/articles/renv.html
  install.packages("renv")
  library(renv)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

# to import spss data file with labels 
if(!require(haven)){
  install.packages("haven")
  library(haven)
}

# To convert labels to factors
if(!require(sjlabelled)){
  install.packages("sjlabelled")
  library(sjlabelled)
}

# To generate a codebook
if(!require(sjPlot)){
  install.packages("sjPlot")
  library(sjPlot)
}

if(!require(tableone)){
  install.packages("tableone")
  library(tableone)
}

if(!require(cowplot)){
  install.packages("cowplot")
  library(cowplot)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(colorspace)){
  install.packages("colorspace")
  library(colorspace)
}

if(!require(ggrepel)){
  install.packages("ggrepel")
  library(ggrepel)
}

if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}

if(!require(here)){
  install.packages("here")
  library(here)
}

if(!require(conflicted)){
  devtools::install_github("r-lib/conflicted")
  library(conflicted)
}

# renv::init() # initialize a new project-local environment with a private R library
renv::snapshot() # Save the state of the project library to the lockfile (called renv.lock)

# Address any conflicts in the packages
conflict_scout() # Identify the conflicts
conflict_prefer("remove", "base")
conflict_prefer("filter", "dplyr")
conflict_prefer("ggsave", "cowplot")


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
dataDir <- "C:/Users/Joanna/Dropbox/Data/TESS"    # Name of folder where the TESS data was downloaded
srcDir  <- "src"                                  # Name of the sub-folder where we will save our source code (R scripts)
funDir  <- "src/functions"                        # File path where we will save our functions
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

if (!dir.exists(here::here(funDir))){
  dir.create(funDir)
} else {
  print("Functions directory already exists!")
}

## Set house color palette
source(file.path(funDir, "jrp_colors.R"))

message("End of FS_00_setup and packages") # Marks end of R Script
