# Code to bulk generate analyses, saves consoe output to a log file.
#
# devtools::source_url('https://raw.githubusercontent.com/FredHasselman/manylabRs/master/pkg/R/C-3PR_ASCII.R')
# init()

source('~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/ManyLabRs/manylabRs/pkg/R/C-3PR_ASCII.R')
init()


# Setup variables ---------------------------------------------------------------------------------------------------
tp <- 4
subset = "all"

ML2.key <- ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
ML2.key <- ML2.key[!is.na(ML2.key$unique.id),]

analysis  <- c('study.global.include', 'study.primary.include', 'study.secondary.include','study.global.include')
if(!is.null(tp)){
    ID.global <- which(ML2.key[ , analysis[tp]]==1)
}

skip         <- NULL #c(14,15,38)
studies      <- ML2.key$unique.id[ID.global][!ML2.key$unique.id[ID.global]%in%skip]

saveRDSfile  <- TRUE
saveCSVfile  <- TRUE

# Set console output to file ---------------------------------------------------------------------------------------
startLog <- function(tp){
  if(length(studies)==length(ML2.key$unique.id[ID.global][!ML2.key$unique.id[ID.global]%in%skip])){
    con <- file(c(paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Global_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Primary_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Secondary_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_StudyOrder_",subset,".txt"))[tp])
  } else {
    con <- file(c(paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Global_",paste0(studies,collapse = "_"),"_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Primary_",paste0(studies,collapse = "_"),"_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Secondary_",paste0(studies,collapse = "_"),"_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_StudyOrder_",paste0(studies,collapse = "_"),"_",subset,".txt"))[tp])
  }
  # Set the sink to 'con'
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  return(con)
}

restore <- function(con){
    # Restore output to console
    sink()
    sink(type="message")

    close(con)
    closeAllConnections()
}

con <- startLog(tp)

# This will echo all input and not truncate 150+ character lines...
tryCatch(testScript(studies = studies,
                    tp = tp,
                    subset = subset,
                    saveCSVfile = saveCSVfile,
                    saveRDSfile = saveRDSfile),
         finally = restore(con))

