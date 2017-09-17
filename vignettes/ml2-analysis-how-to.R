## ----echo=FALSE----------------------------------------------------------
require(manylabRs)
library(reshape2)
library(plyr)
library(broom)

## ---- eval=FALSE---------------------------------------------------------
#  library(devtools)
#  install_github("ManyLabsOpenScience/manylabRs")

## ------------------------------------------------------------------------
library(manylabRs)
library(tidyverse)

df <- get.analyses(studies = 1, analysis.type = 1)

## ------------------------------------------------------------------------
head(tbl_df(df$raw.case$Huang.1))

## ------------------------------------------------------------------------
glimpse(tbl_df(df$aggregated$Huang.1))

## ------------------------------------------------------------------------
cat(paste0(df$aggregated$Huang.1$test.ConsoleOutput))

## ----step1, echo=TRUE, tidy=FALSE----------------------------------------
# NOTE: This example follows some (but not all!!) steps of the main function: get.analyses()

# Select analysis 1 [Huang.1] and source 'brasilia'
runningAnalysis <- 1
runningGroup    <- 'brasilia'

# Get information about the analysis to run
masteRkeyInfo  <- get.GoogleSheet(data='ML2masteRkey')$df[runningAnalysis,]

# Get the appropriate 'raw' dataset [Slate1 or Slate2]
ifelse(masteRkeyInfo$study.slate == 1, data(ML2_S1),data(ML2_S2))

# Organise the information into a list object
analysisInfo   <- get.info(masteRkeyInfo, colnames(ML2.S1), subset="all")

# Use analysisInfo to generate a cahin of filter instructions to select valid variables and cases
filterChain <- get.chain(analysisInfo)

## ------------------------------------------------------------------------
filterChain

## ----step 2--------------------------------------------------------------
# Apply the filterChain to select aprropriate variables from ML2.S1
df.raw <- eval(parse(text=paste("ML2.S1", filterChain$df)))

# Apply the filterChain to generate a list object that represents the design cells
df.split <- get.sourceData(filterChain, df.raw[df.raw$source%in%runningGroup,], analysisInfo)

# Create a list object with data vectors and appropriate labels, that can be passed to the analysis function
vars   <- eval(parse(text=paste0(masteRkeyInfo$stat.vars,'(df.split)',collapse="")))

## ----step 3--------------------------------------------------------------
# Get the paramers the parameters to use for the statistical analysis
stat.params <<- analysisInfo$stat.params

# Run the analysis listed in masteRkey column 'stat.test' usinf the data vectors in 'vars'
stat.test   <- with(vars, eval(parse(text = masteRkeyInfo$stat.test)))

## ----step 4--------------------------------------------------------------
# Return descriptives and summaries
describe <- get.descriptives(stat.test = stat.test, vars = vars, keytable  = masteRkeyInfo)

# Generate output
ESCI <- generateOutput(describe = describe, runningGroup = runningGroup, runningAnalysis = runningAnalysis)

