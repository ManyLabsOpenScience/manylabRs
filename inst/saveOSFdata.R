# DhKkZo83SedKmY5V0fGD365umeL7qPzwxaOnjV4qSxZIPoF3wj1vtR9ax3ECugT3hxWlpi
# osfr::login(pat = "DhKkZo83SedKmY5V0fGD365umeL7qPzwxaOnjV4qSxZIPoF3wj1vtR9ax3ECugT3hxWlpi")


# Code to bulk generate analyses, saves consoe output to a log file.
#library(manylabRs)
source('~/Documents/GitHub/manylabRs/manylabRs/R/manylabRs_SOURCE.R')
init()


# 2. Structure & Goal Pursuit (Kay et al., 2014) \
# Kay.1

# Setup variables ---------------------------------------------------------------------------------------------------
tp     <- 1
subset <- "all"

ML2.key <- ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
ML2.key <- ML2.key[!is.na(ML2.key$unique.id),]

SourceInfoTable    <- get.GoogleSheet(url = "https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/pub?gid=1435507167&single=true&output=csv")$df

WeirdInfoTable <-  get.GoogleSheet(url = "https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/pub?gid=1176562150&single=true&output=csv")$df

root    <- normalizePath(file.path("~","OSFdata"))
studies <- unique(ML2.key$study.description)
level0  <- ML2.key$study.analysis
level1  <- c("Global","By Site","By Order", "By WEIRD")
level2  <- c("Data","Results")

dirs <- list()
cnt = 0
for(s in seq_along(studies)){
  level0s <- level0[ML2.key$study.description%in%studies[s]]
  for(l0 in seq_along(level0s)){
    ID <- which(ML2.key$study.analysis%in%level0s[l0])
    level1s  <- level1[c((ML2.key$study.global.include[ID]==1), TRUE,
                         (ML2.key$study.figure2.include[ID]==1),
                         (ML2.key$study.figure2.include[ID]==1))]
    for(l1 in seq_along(level1s)){
      for(l2 in seq_along(level2)){
        cnt <- cnt + 1
        dirs[[cnt]] <- file.path(root,studies[s],level0s[l0],level1s[l1],level2[l2])
      }
    }
  }
  rm(level0s, level1s)
}

cnt <- cnt+1
dirs[[cnt]] <- file.path(root,"!!RawData")
cnt <- cnt+1
dirs[[cnt]] <- file.path(root,"!!KeyTables")

# Write the directories
lapply(dirs, FUN = dir.create, recursive = TRUE)

export(ML2.key,file.path(dirs[[cnt]],"ML2_KeyTable.csv"))
export(SourceInfoTable,file.path(dirs[[cnt]],"ML2_SourceInfoTable.csv"))
export(WeirdInfoTable,file.path(dirs[[cnt]],"ML2_SourceInfoTable_Weird.csv"))

# load("~/Dropbox/Manylabs2/Raw Data after Cleaning/RAW_DATA/ML2_Rawdata_S1.RData")
# load("~/Dropbox/Manylabs2/Raw Data after Cleaning/RAW_DATA/ML2_Rawdata_S2.RData")

ML2_S1 <- rio::import("~/Dropbox/Manylabs2/Raw Data after Cleaning/Package_data/ML2_S1.rda")
ML2_S2 <- rio::import("~/Dropbox/Manylabs2/Raw Data after Cleaning/Package_data/ML2_S2.rda")


export(ML2_S1,file.path(dirs[[cnt-1]],"ML2_Slate1.csv"))
export(ML2_S2,file.path(dirs[[cnt-1]],"ML2_Slate2.csv"))




#
# for(s in studs){
#
#   # Get the correct slate according to info in ML2.key['study.slate']
#   if(ML2.key[s,'study.slate'] == 1){
#     ML2.df <- load("~/Dropbox/Manylabs2/Raw Data after Cleaning/RAW_DATA/ML2_Rawdata_S1.RData")
#   } else {
#     ML2.df <- load("~/Dropbox/Manylabs2/Raw Data after Cleaning/RAW_DATA/ML2_Rawdata_S2.RData")
#   }
#
#   # Add a unique ID
#   ML2.df$uID = seq(1, nrow(ML2.df))
#
#   # Get info to create a dataset for the current study
#   # keytable <- ML2.key[s,]
#   ML2.in <- get.info(ML2.key[s, ], colnames(ML2.df), subset)
#
#
#   # Generate chain to select variables for the data frame and create a filter chain for the variables to use for analysis
#   # Info based on KeyTable information in study.vars, cases.include, site.include, params.NA
#   ML2.id <- get.chain(ML2.in)
#
#   # Apply the df chain to select relevant subset of variables
#   ML2.df <- eval(parse(text=paste("ML2.df", ML2.id$df)))
#
# }
#
#
#
# ML2.key$stat.test[[s]]
#
#
#
#
#
# if(saveRDSfile){
#   fname <- c(file.path(normalizePath(dir.out),"RESULTS_RDS",subset,paste0("ML2_results_global_",subset,".rds")),
#              file.path(normalizePath(dir.out),"RESULTS_RDS",subset,paste0("ML2_results_primary_",subset,".rds")),
#              file.path(normalizePath(dir.out),"RESULTS_RDS",subset,paste0("ML2_results_secondary_",subset,".rds")),
#              file.path(normalizePath(dir.out),"RESULTS_RDS","by_order",paste0("Data_Figure_StudyOrder_",subset,".rds")))[tp]
#   cat(paste0("\nSaving list object with analyses...\n"))
#   # only save non-empty objects
#
#   saveRDS(dfout,file=fname)
# }
#
#
#
# analysis  <- c('study.global.include', 'study.primary.include', 'study.secondary.include','study.global.include')
# if(!is.null(tp)){
#     ID.global <- which(ML2.key[ , analysis[tp]]==1)
# }
#
# skip         <- NULL
# studies      <- ML2.key$unique.id[ID.global][!ML2.key$unique.id[ID.global]%in%skip]
#
# saveRDSfile  <- TRUE
# saveCSVfile  <- TRUE
#
# # Set console output to file ---------------------------------------------------------------------------------------
# startLog <- function(tp){
#   if(length(studies)==length(ML2.key$unique.id[ID.global][!ML2.key$unique.id[ID.global]%in%skip])){
#     con <- file(c(paste0("ML2log_Global_",subset,".txt"),
#                   paste0("ML2log_Primary_",subset,".txt"),
#                   paste0("ML2log_Secondary_",subset,".txt"),
#                   paste0("ML2log_StudyOrder_",subset,".txt"))[tp])
#   } else {
#     con <- file(c(paste0("ML2log_Global_",paste0(studies,collapse = "_"),"_",subset,".txt"),
#                   paste0("ML2log_Primary_",paste0(studies,collapse = "_"),"_",subset,".txt"),
#                   paste0("ML2log_Secondary_",paste0(studies,collapse = "_"),"_",subset,".txt"),
#                   paste0("ML2log_StudyOrder_",paste0(studies,collapse = "_"),"_",subset,".txt"))[tp])
#   }
#   # Set the sink to 'con'
#   sink(con, append=TRUE)
#   sink(con, append=TRUE, type="message")
#   return(con)
# }
#
# restore <- function(con){
#     # Restore output to console
#     sink()
#     sink(type="message")
#
#     close(con)
#     closeAllConnections()
# }
#
# con <- startLog(tp)
#
# # This will echo all input and not truncate 150+ character lines...
# tryCatch(testScript(studies = studies,
#                     tp = tp,
#                     subset = subset,
#                     saveCSVfile = saveCSVfile,
#                     saveRDSfile = saveRDSfile),
#          finally = restore(con))
#
# # Ori effects ----
#
#
# # Test ---
# tp <- 4
# studies <- 83
# dfout <- get.analyses(studies = studies, analysis.type = tp, subset = subset)
#
#
#
#
#
#
#
