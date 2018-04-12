## ----setup, echo=FALSE, message=FALSE, warning=FALSE, include=FALSE------
knitr::opts_chunk$set(tidy=FALSE)
require(manylabRs)

## ---- eval=FALSE---------------------------------------------------------
#  library(devtools)
#  install_github("ManyLabsOpenScience/manylabRs")
#  library(manylabRs)

## ----echo=TRUE, message=FALSE, warning=FALSE, include=FALSE--------------
rootdir = "YOUR PATHNAME TO DATAFILES ROOTDIR"

indir = list(RAW.DATA   = "DIRECTORY IN YOUR rootdir CONTAINING DATAFILES",
             MASTERKEY  = "", 
             SOURCEINFO = "")

## ---- echo=FALSE---------------------------------------------------------
MyRootDir    <- "~/Documents/GitHub/manylabRs/"
MyRawDataDir <- "random3rd"
data.names   <- list(Slate1 = "ML2.Slate1.Random3rdDE.rds",
                     Slate2 = "ML2.Slate2.Random3rdDE.rds")

