#' get.analyses
#'
#' @param studies    Numeric vector with unique study IDs listed in the `masteRkey` table (default = all IDs).
#' @param tp     An optional number indicating Global (1), Primary (2, default) or Secondary (3) analyses.
#' @param Nmin.raw     Minimum raw sample size allowed to be included in the analyses.
#' @param Nmin.cond     Minimum sample size per condition allowed to be included in the analyses.
#'
#' @return A list object with analysis results.
#' @export
#'
#' @family "get." functions
#'
#' @details Run analyses for (selected) ML2 studies.
#'
#' @examples
get.analyses <- function(studies       = NA,
                         analysis.type = NA,
                         Nmin.raw  = 30,
                         Nmin.cond = 15,
                         subset    = c("all","WEIRD","NON-WEIRD")[1],
                         rootdir   = "~/Dropbox/Manylabs2/TestOutput",
                         indir     = list(RAW.DATA = "RAW.DATA.PRIVATE",MASTERKEY = "",SOURCEINFO = ""),
                         outdir    = list(ROBJECTS    = "ROBJECTS",RESULTS.RDS = "RESULTS.RDS")){

  tp  <- analysis.type
  wop <- options(warn=-1, expressions=10000)

  # Load Key Table
  if(indir$MASTERKEY==""){
    ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
    disp(paste("Downloaded keytable Googlesheet: ML2_masteRkey [https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/]"), header = "get.analyses", footer = FALSE)
  } else {
    ML2.key <- read.xlsx(file.path(rootdir,indir$MASTERKEY,"ML2_masteRkey.xlsx"),"ML2masteRkey")
    disp(paste0("Loaded keytable from disk: ML2_masteRkey.xlsx [",file.path(rootdir,indir$MASTERKEY),"]"), header = "get.analyses", footer = FALSE)
  }

  ML2.key <- ML2.key[!is.na(ML2.key$unique.id),]

  # Load data
  if(indir$RAW.DATA==""){
    ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
    disp(paste("Downloaded data from OSF: 'ML2_RawData_S1.rds' and 'ML2_RawData_S1.rds'"), header = FALSE, footer = FALSE)
  } else {
    ML2.S1 <- readRDS(file.path(rootdir,indir$RAW.DATA,"ML2_RawData_S1.rds"))
    ML2.S2 <- readRDS(file.path(rootdir,indir$RAW.DATA,"ML2_RawData_S2.rds"))
    disp(paste0("Loaded data from disk: 'ML2_RawData_S1.rds' and 'ML2_RawData_S1.rds'[",file.path(rootdir,indir$RAW.DATA),"]"), header = FALSE, footer = FALSE)
  }

  # Load information about sources
  if(indir$SOURCEINFO==""){
    SourceInfoTable    <- get.GoogleSheet(url = "https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/pub?gid=1435507167&single=true&output=csv")$df
    disp(paste("Downloaded information about the data sources from Googlesheet: 'ML2_SourceInfo.xlsx' [https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/]"), header = FALSE, footer = FALSE)
  } else {
    SourceInfoTable    <- readRDS(file.path(rootdir,indir$SOURCEINFO,"ML2_SourceInfo.xlsx"))
    disp(paste0("Loaded information about the data sources from disk: 'MML2_SourceInfo.xlsx' [",file.path(rootdir,indir$SOURCEINFO),"]"), header = FALSE, footer = FALSE)
  }

  # Decide which analyses to run on which groups
  toRun  <- decide.analysis(ML2.key, studies, tp)

  # Prepare list objects
  ML2.data           <- vector("list", length = length(ML2.key$study.analysis))
  names(ML2.data)    <- ML2.key$study.analysis
  ML2.output         <- vector("list", length = length(ML2.key$study.analysis))
  names(ML2.output)  <- ML2.key$study.analysis
  ML2.rawdata        <- vector("list", length = length(ML2.key$study.analysis))
  names(ML2.rawdata) <- ML2.key$study.analysis
  #ML2.descriptivesDump <- list()

  studs <- toRun$studiess
  cnt   <- 0

  # START STUDIES ----------------------------------

  for(s in studs){

    # Get the correct slate according to info in ML2.key['study.slate']
    if(ML2.key[s,'study.slate'] == 1){ML2.df <- ML2.S1
    } else {
      ML2.df <- ML2.S2
    }

    # WEIRD <- ML2.df$Country%in%c("Australia","Austria","Canada","France","Germany","Hungary","Italy","New Zealand","Poland","Portugal","Serbia","Spain","Sweden", "The Netherlands","UK","USA")
    #

    # Add a unique ID
    ML2.df$uID = seq(1, nrow(ML2.df))

    # Get info to create a dataset for the current study
    # keytable <- ML2.key[s,]
    ML2.in <- get.info(ML2.key[s, ], colnames(ML2.df), subset)


    # Generate chain to select variables for the data frame and create a filter chain for the variables to use for analysis
    # Info based on KeyTable information in study.vars, cases.include, site.include, params.NA
    ML2.id <- get.chain(ML2.in)

    # Apply the df chain to select relevant subset of variables
    ML2.df <- eval(parse(text=paste("ML2.df", ML2.id$df)))


    # if(!subset%in%"all"){
    #   if(subset%in%"WEIRD"){
    #     ML2.df <- ML2.df[ML2.df$Weird==1,]
    #   } else {
    #     ML2.df <- ML2.df[ML2.df$Weird==0,]
    #   }
    # }


  if(NROW(ML2.df)>0){

    ML2.df$study.order <- NA
    stmp <- strsplit(ML2.df$StudyOrderN,"[|]")

    Stud <- ML2.key$study.name[[s]]
    if(Stud%in%"Tversky"){Stud <- "Tversky.Gati"}
    if(Stud%in%"Rottenstreich"){Stud <- "Rottenstrich"}
    if(Stud%in%"Ross"&(ML2.key[s,'study.slate'] == 1)){Stud <- "Ross.Slate1"}
    if(Stud%in%"Ross"&(ML2.key[s,'study.slate'] == 2)){Stud <- "Ross.Slate2"}
    if(Stud%in%"vanLange"){Stud <- "VanLange"}
    if(Stud%in%"Giessner"){Stud <- "Geissner"}

    ML2.df$study.order <- laply(seq_along(stmp), function(o){which(grepl(Stud,stmp[[o]]))%0!0%NA})

    # Loop over groups within study
    ugroup       <- sort(na.exclude(unique(eval(parse(text=toRun$ugroup)))))
    tp           <- toRun$tp
    ML2.sr       <- list()
    ML2.var      <- list()
    outputSource <- list()
    dataSource   <- list()
    raw.df       <- list()
    clean.df     <- list()
    testVarEqual <- ML2.in$stat.params$var.equal


    # runGroups <- sort(na.exclude(unique(ML2.df$study.order)))

    cnt          <- cnt + 1

    ifelse(tp[cnt]==1,
           runGroups <- "all",
           runGroups <- ugroup
    )

    disp(paste(s, ML2.key$study.analysis[[s]],"- START"), header = toupper(ML2.key$study.analysis[[s]]), footer = FALSE)
    cat("\n")

    # START GROUPS ----------------------------------------------


    for(g in seq_along(runGroups)){

      listIT     <- FALSE
      nMin1      <- FALSE
      nMin2      <- FALSE
      compN <- compN1 <- compN2 <- 0

      if(tp[cnt]<4){
        if(runGroups[g]=="all"){gID <- rep(TRUE, nrow(ML2.df))} else {gID <- ML2.df$source%in%runGroups[g]}
      } else {
        gID <-  ML2.df$study.order%in%runGroups[g]
      }

      # Check nMin
      if(sum(gID, na.rm=TRUE) >= Nmin.raw){
        nMin1 <- TRUE
        # Get a list containing the data frames to be used in the analysis
        ML2.sr[[g]] <- get.sourceData(ML2.id, ML2.df[gID, ], ML2.in)
      }

      # Double-check nMin
      if(nMin1){
        compN  <- ML2.sr[[g]]$N
        compN1 <- sum(ML2.sr[[g]]$RawDataFilter[[1]]$Included, na.rm = TRUE)
        compN2 <- sum(ML2.sr[[g]]$RawDataFilter[[2]]$Included, na.rm = TRUE)
        if(any(compN >= Nmin.raw)&(all(compN1>=Nmin.cond, compN2>=Nmin.cond))){nMin2 <- TRUE}
      }

      # START ANALYSIS ----------------------------------------

      if(all(nMin1,nMin2)){

        # Organize and Calculate variables for the analysis using function according to ML2.info: 'stat.vars'
        ML2.var[[g]] <- eval(parse(text=paste0(ML2.key[s,'stat.vars'],'(ML2.sr[[g]])',collapse="")))

        # Check equal variance assumption
        if(!is.na(testVarEqual)){
          if(testVarEqual){
            logtxt <- paste(s,ML2.key$study.analysis[[s]],'-', runGroups[g])
            ML2.in$stat.params$var.equal <- decide.EqualVar(ML2.var[[g]],ML2.in$study.vars.labels, ML2.key[s, ], group = logtxt)
          }}

        # Run the analysis according to ML2.key: 'stat.test'
        stat.params <<- ML2.in$stat.params
        stat.test   <- try.CATCH(with(ML2.var[[g]],eval(parse(text = ML2.key$stat.test[[s]]))))

        # if(grepl("approximation", stat.test$warning)){stat.test$warning <- NULL}

        if(all(is.null(stat.test$warning), grepl("simpleWarning",stat.test$warning),
               !grepl("Error", stat.test$value[[1]]),
               !grepl("message", names(unlist(stat.test))[1]))){
          stat.test  <- stat.test$value
          ConsoleOut <- paste(capture.output(print(stat.test)),collapse="\n")
          listIT     <- TRUE
        }

        # START RECORD DATA -------------------------------------

        if(listIT){

          describe <- get.desriptives(stat.test = stat.test,
                                      vars      = ML2.var[[g]],
                                      keytable  = ML2.key[s,])

          var.lor <- ifelse(grepl("OR",describe$test$estype),
                            sum(1/(table(ML2.var[[g]]$Condition,ML2.var[[g]]$Response)), na.rm = TRUE),
                            NA)

          ESCI  <-   generateOutput(describe        = describe,
                                    var.lor         = var.lor,
                                    runningGroup    = runGroups[g],
                                    runningAnalysis = paste(s,ML2.key$study.analysis[[s]]))

          ESCI$cohensQ   <- NA
          ESCI$cohensQ.l <- NA
          ESCI$cohensQ.u <- NA
          ESCI$bootR1    <- NA
          ESCI$bootR2    <- NA
          ESCI$bootCI.l  <- NA
          ESCI$bootCI.u  <- NA

          if(describe$test$estype=="Z.f"){

            if(stat.test$method=="Fisher r-to-Z transformed test for difference between 2 independent correlations"){
              ESCI[,which(colnames(ESCI)=="d"):NCOL(ESCI)] <- NA

              Wilcox.out<- twopcor(x1=ML2.var[[g]]$r1[[1]],
                                   x2=ML2.var[[g]]$r1[[2]],
                                   y1=ML2.var[[g]]$r2[[1]],
                                   y2=ML2.var[[g]]$r2[[2]])

              ESCI$ncp       <- stat.test$statistic
              ESCI$ncp.lo    <- "fisherZ - 2 corr"
              ESCI$ncp.hi    <- "fisherZ - 2 corr"
              ESCI$cohensQ   <- stat.test$effect.size
              ESCI$cohensQ.l <- stat.test$effect.size.ci[1]
              ESCI$cohensQ.u <- stat.test$effect.size.ci[2]
              ESCI$bootR1    <- Wilcox.out$r1
              ESCI$bootR2    <- Wilcox.out$r2
              ESCI$bootCI.l  <- Wilcox.out$ci[1]
              ESCI$bootCI.u  <- Wilcox.out$ci[2]
              ESCI$r         <- Wilcox.out$r1-Wilcox.out$r2
              ESCI$l.r       <- Wilcox.out$ci[1]
              ESCI$u.r       <- Wilcox.out$ci[2]

            } else {
              #ESCI[,c(7:21,31:38)] <- NA
              ESCI$ncp       <- stat.test$statistic
              ESCI$ncp.lo    <- "fisherZ - 1 corr"
              ESCI$ncp.hi    <- "fisherZ - 1 corr"
              ESCI$cohensQ   <- stat.test$effect.size
              ESCI$cohensQ.l <- stat.test$effect.size.ci[1]
              ESCI$cohensQ.u <- stat.test$effect.size.ci[2]
            }
          }

          # Raw and clean datasets

          if(length(ML2.sr[[g]]$RawDataFilter)>1){
            case.include <- ML2.sr[[g]]$RawDataFilter[[1]]$Included|ML2.sr[[g]]$RawDataFilter[[2]]$Included
            df1 <- ML2.sr[[g]]$RawDataFilter[[1]][ ,-which(colnames( ML2.sr[[g]]$RawDataFilter[[1]])=="Included")]
            raw.df[[g]] <-  cbind.data.frame(df1, analysis.type = c("Global","Primary","Secondary","Order")[tp[cnt]],subset=subset,case.include = case.include)
          } else {
            case.include <- ML2.sr[[g]]$RawDataFilter[[1]]$Included
            df1 <- ML2.sr[[g]]$RawDataFilter[[1]][ ,-which(colnames( ML2.sr[[g]]$RawDataFilter[[1]])=="Included")]
            raw.df[[g]] <-  cbind.data.frame(df1, analysis.type = c("Global","Primary","Secondary","Order")[tp[cnt]],subset=subset,cases.include = case.include)
          }


          if(tp<4){
          if(runGroups[g]!="all"){
            fID <- unique(ML2.df$.id[ML2.df$source==runGroups[g]])
            sID <- SourceInfoTable$Source%in%runGroups[g]&SourceInfoTable$Filename%in%fID
            if(sum(sID)==1){
              SourceInfo1 <- SourceInfoTable[sID, ]
              SourceInfo2 <- raw.df[[g]] %>% filter(case.include) %>% group_by(source) %>%
               summarise(
                  N.sources.global    = length(unique(Source.Global)),
                  N.sources.primary   = length(unique(Source.Primary)),
                  N.sources.secondary = length(unique(Source.Secondary)),
                  N.countries         = length(unique(Country)),
                  N.locations         = length(unique(Location)),
                  N.languages         = length(unique(Language)),
                  Pct.WEIRD           = mean(Weird, na.rm=TRUE)*100,
                  Tbl.Execution       = paste0(capture.output(table(Execution)),collapse="\n"),
                  Tbl.subjectpool     = paste0(capture.output(table(SubjectPool)),collapse="\n"),
                  Tbl.setting       = paste0(capture.output(table(Setting)),collapse="\n"),
                  Tbl.Tablet        = paste0(capture.output(table(Tablet)),collapse="\n"),
                  Tbl.Pencil        = paste0(capture.output(table(Pencil)),collapse="\n"),
                  N.studyorders1    = length(unique(StudyOrderN)),
                  N.IDiffOrderN     = length(unique(IDiffOrderN)),
                  N.uIDs            = length(unique(uID)),
                  N.studyorders2    = length(unique(study.order)),
                  Tbl.analysistype  = paste0(capture.output(table(analysis.type)),collapse="\n"),
                  Tbl.subset        = paste0(capture.output(table(subset)),collapse="\n"),
                  N.cases.included  = sum(case.include, na.rm=TRUE),
                  N.cases.excluded  = sum(raw.df[[g]]$case.include==FALSE,na.rm=TRUE)
                  )
              SourceInfo<-cbind(SourceInfo1,SourceInfo2)
              # colnames(SourceInfo) <- c("name","name.Global",colnames(SourceInfoTable)[3:NCOL(SourceInfoTable)])
              }
            } else {
              # SourceInfo   <- t(ldply(1:NCOL(SourceInfoTable), function(c){
              #   tbl_df(data.frame(src = paste(unlist(unique(SourceInfoTable[sID,c]),use.names = FALSE),collapse ="\n")))
              # }))
              #
            # colnames(SourceInfo) <- c("name","name.GLobal",colnames(SourceInfoTable)[3:NCOL(SourceInfoTable)])
            # rownames(SourceInfo) <- NULL
              #}))

            SourceInfo <- raw.df[[g]] %>% filter(case.include) %>%
              summarise(
                N.sources.global    = length(unique(Source.Global)),
                N.sources.primary   = length(unique(Source.Primary)),
                N.sources.secondary = length(unique(Source.Secondary)),
                N.countries         = length(unique(Country)),
                N.locations         = length(unique(Location)),
                N.languages         = length(unique(Language)),
                Pct.WEIRD           = mean(Weird, na.rm=TRUE)*100,
                Tbl.Execution       = paste0(capture.output(table(Execution)),collapse="\n"),
                Tbl.subjectpool     = paste0(capture.output(table(SubjectPool)),collapse="\n"),
                Tbl.setting       = paste0(capture.output(table(Setting)),collapse="\n"),
                Tbl.Tablet        = paste0(capture.output(table(Tablet)),collapse="\n"),
                Tbl.Pencil        = paste0(capture.output(table(Pencil)),collapse="\n"),
                N.studyorders1    = length(unique(StudyOrderN)),
                N.IDiffOrderN     = length(unique(IDiffOrderN)),
                N.uIDs            = length(unique(uID)),
                N.studyorders2    = length(unique(study.order)),
                Tbl.analysistype  = paste0(capture.output(table(analysis.type)),collapse="\n"),
                Tbl.subset        = paste0(capture.output(table(subset)),collapse="\n"),
                N.cases.included  = n(),
                N.cases.excluded  = sum(raw.df[[g]]$case.include==FALSE,na.rm=TRUE)
                )
            }
            } else {
              SourceInfo <- raw.df[[g]] %>% filter(case.include) %>%
                 summarise(
                  N.sources.global    = length(unique(Source.Global)),
                  N.sources.primary   = length(unique(Source.Primary)),
                  N.sources.secondary = length(unique(Source.Secondary)),
                  N.countries         = length(unique(Country)),
                  N.locations         = length(unique(Location)),
                  N.languages         = length(unique(Language)),
                  Pct.WEIRD           = mean(Weird, na.rm=TRUE)*100,
                  Tbl.Execution       = paste0(capture.output(table(Execution)),collapse="\n"),
                  Tbl.subjectpool     = paste0(capture.output(table(SubjectPool)),collapse="\n"),
                  Tbl.setting       = paste0(capture.output(table(Setting)),collapse="\n"),
                  Tbl.Tablet        = paste0(capture.output(table(Tablet)),collapse="\n"),
                  Tbl.Pencil        = paste0(capture.output(table(Pencil)),collapse="\n"),
                  N.studyorders1    = length(unique(StudyOrderN)),
                  N.IDiffOrderN     = length(unique(IDiffOrderN)),
                  N.uIDs            = length(unique(uID)),
                  N.studyorders2    = length(unique(study.order)),
                  Tbl.analysistype  = paste0(capture.output(table(analysis.type)),collapse="\n"),
                  Tbl.subset        = paste0(capture.output(table(subset)),collapse="\n"),
                  N.cases.included  = n(),
                  N.cases.excluded  = sum(raw.df[[g]]$case.include==FALSE,na.rm=TRUE)
                )
            }

          rownames(SourceInfo) <- NULL

          test  <- describe$test
          descr <- describe$descr.raw
          outputSource[[g]] <- get.output(key      = ML2.key[s,],
                                          vars     = ML2.var[[g]],
                                          descr    = descr,
                                          group    = runGroups[g],
                                          analysis = c("Global","Primary","Secondary","Order")[tp[cnt]],
                                          varEqual = stat.params$var.equal,
                                          test     = test,
                                          ESCI     = ESCI,
                                          test.ConsoleOutput = ConsoleOut,
                                          SourceInfo = SourceInfo,
                                          stat.test = stat.test)

          # Data list for output to spreadsheet
          dataSource[[g]] <- list(
            study.id      = ML2.key$study.id[[s]],
            study.slate   = ML2.key$study.slate[[s]],
            study.name    = ML2.key$study.name[[s]],
            study.source  = runGroups[g],
            analysis.type = c("Global","Primary","Secondary","Order")[tp[cnt]],
            analysis.name = ML2.key$study.analysis[[s]],
            subset        = subset,
            stat.info     = ML2.in,
            stat.data.cleanchain = ML2.id,
            stat.data.raw       = raw.df[[g]],
            stat.data.cleaned   = ML2.sr[[g]][1:length(ML2.sr[[g]])-1],
            stat.data.analysed  = ML2.var[[g]][1:length(ML2.var[[g]])-1],
            stat.test = stat.test)

          suppressMessages(clean.df[[g]] <- ldply(dataSource[[g]]$stat.data.analysed,melt))
          colnames(clean.df[[g]])[colnames(clean.df[[g]])==".id"] <- "Condition"

          rm(stat.params)
        } else {# LISTIT
          cat("\nListIT = FALSE\n")
          if(grepl("observations",as.character(stat.test$value))){
            disp(paste(s, ML2.key$study.analysis[[s]],'-',
                       runGroups[g],'>> Not enough observations'),
                 header = FALSE, footer = FALSE)
          } else {
            disp(paste(s,ML2.key$study.analysis[[s]],'-', runGroups[g],'>> stat.test failed:'),
                 header = FALSE, footer = FALSE)
            # disp(paste('value: ',stat.test$value),
            #      header = FALSE, footer = FALSE)
            disp(paste('warning:',stat.test$warning),
                 header = FALSE, footer = FALSE)
          }
          ConsoleOut <- paste(gsub("[[:punct:]]", "", stat.test$warning, perl = TRUE), collapse="\n")
          NN <- lengths(ML2.var[[g]])
          NN <- NN[!names(NN)=="N"]
          N  <- rep(ML2.var[[g]]$N,length.out = length(NN))
          ML2.rnd <- llply(seq_along(NN), function(nn) rnorm(N[nn]))#eval(parse(text=paste(names(NN[nn])," = rnorm(101)"))))
          names(ML2.rnd) <- names(NN)
          stat.test  <- try.CATCH(with(ML2.var[[g]],eval(parse(text = ML2.key$stat.test[[s]]))))
          stat.test  <- stat.test$value
          #RANDOMdata <- TRUE
          # Analysis error, there may be descriptives, but set ESCI to NA
          ESCI[1:length(ESCI)] <- rep(NA,length(ESCI))
        }#Listit = FALSE
      } # all nMin 1,2

      # Report on errors

      if(!nMin1){
        disp(paste0(s,' ',ML2.key$study.analysis[[s]],' - ',
                    runGroups[g],' not included in results >> Cases in source file (',
                    sum(gID, na.rm = TRUE),') < Nmin.raw (',Nmin.raw,')'),
             header = FALSE, footer = FALSE)
      } # Check nMin 1}
      if(!nMin2){
        disp(paste0(s,' ',ML2.key$study.analysis[[s]],' - ',
                    runGroups[g],' not included in results >> Valid cases after varfun (n',
                    c(1,2)[compN < Nmin.cond],"=", compN[compN < Nmin.cond],') < Nmin.cond (',Nmin.cond,')'),
             header = FALSE, footer = FALSE)
      } # Double-check nMin

    } # iterate groups

    disp(paste(s, ML2.key$study.analysis[[s]],"- COMPLETED"), header = FALSE)

    ML2.output[[s]]  <- ldply(outputSource)
    ML2.rawdata[[s]] <- ldply(raw.df)

    if(outdir$ROBJECTS!=""){
      save(dataSource, file = file.path(rootdir,outdir$ROBJECTS,paste0(ML2.key$study.analysis[[s]],"_",c("Global","Primary","Secondary","Order")[tp[cnt]],".RData")))
    }

    rm(ML2.in, ML2.var, ML2.id, ML2.df, ML2.sr, outputSource, dataSource, raw.df, clean.df, descr, SourceInfo, nMin1, nMin2, listIT)

  } else { # if nrow > 0

    disp(paste(s, ML2.key$study.analysis[[s]],"- SKIPPED"), header = FALSE)

    ML2.output[[s]]  <- NULL
    ML2.rawdata[[s]] <- NULL

    rm(ML2.in, ML2.var, ML2.id, ML2.df, ML2.sr)
  }

    } # for s i studies

  options(wop)
  return(list(raw.case   = ML2.rawdata,
              aggregated = ML2.output)
  ) #), descriptivesDump = ML2.descriptivesDump))
}


#' get.GoogleSheet
#'
#' @param url    Hyperlink to the GoogleSheet, ending in command \code{".../export?format=csv"}.
#' @param data    If no URL is provided, which dataset? (default = "ML2masteRkey").
#' @param dfCln    Should the variable names be cleaned (replace spaces and punctuation by a period "."). Default is \code{FALSE}.
#' @param Sep     Symbol to use when changing column names (default: ".").
#'
#' @family "get." functions
#'
#' @return A list object with fields:
#'
#' \itemize{
#' \item Returned if \code{dataSet = TRUE}  (default):
#' \itemize{
#' \item \code{df}:  A data table generated by \code{\link{tbl_df}} from package \code{dplyr}.
#' \item \code{info}: Information about the downloaded file including a time stamp, the URL and original row and column names.
#' }
#' \item Returned if \code{dataSet = FALSE}:
#' \itemize{
#' \item \code{FilePath}: The local path to the downloaded file.
#' }
#' }
#' @export
#'
#' @examples
get.GoogleSheet <- function(url=NULL,data=c('ML1data','ML2masteRkey','ML2data')[2],dfCln=FALSE,Sep = "."){
  if(is.null(url)){
    switch(data,
           ML1data        = url <- 'https://docs.google.com/spreadsheets/d/19ay71M8jiqIZhSj3HR0vqaJUwAae1QHzBybjBu5yBg8/export?format=csv',
           ML2masteRkey   = url <- 'https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/export?format=csv',
           ML2data        = url <- ''
    )}
  # GET(url) will only get 100 rows, thanks to Sacha Epskamp for this "complete scrape" code.
  tmp  <- tempfile()
  info <- httr::GET(url, httr::write_disk(tmp, overwrite = TRUE))
  df   <- dplyr::tbl_df(read.csv(tmp,  stringsAsFactors = FALSE, header = TRUE)) #import(tmp, format = "csv", stringsAsFactors = FALSE, header = TRUE))
  if(dfCln==TRUE){
    df   <- df.Clean(df)
  } else {
    df$df  <- df
    df$nms <- NA
    df$rws <- NA
  }

  return(list(df = df$df,
              info = list(Info=info,
                          GoogleSheet.colnames=dplyr::tbl_df(data.frame(ori.colnames=df$nms)),
                          GoogleSheet.rownames=dplyr::tbl_df(data.frame(ori.rownames=df$rws))))
  )
}

#' get.OSFfile
#'
#' @param code    Either a full url ("https://osf.io/XXXXX/"), or just the OSF code.
#' @param dir   Output location (default is \code{tempdir()}).
#' @param scanMethod     Either \code{readLines} or \code{RCurl}. Leave missing to choose automatically.
#' @param downloadMethod    One of \code{httr} (default), \code{downloader} or \code{curl}.
#' @param dataSet     Is the file data set which can be imported using \code{\link{import}} from package \code{rio}?
#' @param dfCln    Should the variable names be cleaned (replace spaces and punctuation by a period "."). Default is \code{FALSE}.
#'
#' @author Fred Hasselman, based on code by Sasha Epskamp
#' @family "get." functions
#'
#' @details Function to download a file hosted on OSF. Modified from code originally written by Sacha Epskamp.
#' @return A list object with fields:
#' \itemize{
#' \item Returned if \code{dataSet = TRUE}  (default):
#' \itemize{
#' \item \code{df}:  A data table generated by \code{\link{tbl_df}} from package \code{dplyr}.
#' \item \code{info}: Information about the downloaded file including a time stamp, the URL and original row and column names.
#' }
#' \item Returned if \code{dataSet = FALSE}:
#' \itemize{
#' \item \code{FilePath}: The local path to the downloaded file.
#' }
#' }
#' @export
#'
#' @examples
#' #Get the RP:P data hosted on OSF.
#' dfRPP <- get.OSFfile(code='https://osf.io/fgjvw/', dfCln=TRUE)$df
get.OSFfile <- function(code, dir = tempdir(), scanMethod, downloadMethod = c("httr","downloader","curl"), dataSet = TRUE, dfCln = FALSE){
  # require(bitops)
  # require(RCurl)
  # require(downloader)
  # require(httr)
  # require(rio)
  # require(dplyr)

  # Check if input is code:
  if (!grepl("osf\\.io",code)){
    URL <- sprintf("https://osf.io/%s/",code)
  } else URL <- code

  # Scan page:
  if (grepl("Windows",Sys.info()[['sysname']],ignore.case=TRUE)){
    try(setInternet2(TRUE))
  }

  if (missing(scanMethod)){
    scanMethod <- ifelse(grepl("Windows",Sys.info()[['sysname']],ignore.case=TRUE), "readLines", "RCurl")
  }
  if (scanMethod == "readLines"){
    Page <- paste(readLines(URL),collapse="\n")
  } else if (scanMethod == "RCurl"){
    Page <- RCurl::getURL(URL)
  } else if (scanMethod == "httr"){
    Page <- httr::GET(URL)
    Page <- paste(Page,collapse="\n")
  } else stop("Invalid scanMethod")

  # Create download link:
  URL <- gsub("/$","",URL)
  #   Link <- paste0(URL,"/?action=download&version=1")
  Link <- paste0(URL,"/?action=download")

  # Extract file name:
  FileName <- regmatches(Page,gregexpr("(?<=\\<title\\>OSF \\| ).*?(?=\\</title\\>)", Page, perl=TRUE))[[1]]
  FullPath <- paste0(dir,"/",FileName)

  info <- NULL
  # Download file:
  if (downloadMethod[[1]]=="httr"){
    info <- httr::GET(Link, httr::write_disk(FullPath, overwrite = TRUE))
  } else if (downloadMethod[[1]]=="downloader"){
    downloader::download(Link, destfile = FullPath, quiet=TRUE)
  } else if (downloadMethod[[1]]=="curl"){
    system(sprintf("curl -J -L %s > %s", Link, FullPath), ignore.stderr = TRUE)
  }  else stop("invalid downloadMethod")

  df <- NULL
  if(dataSet==TRUE){
    suppressWarnings(df <- rio::import(FullPath,setclass = "tbl_df"))
    if(dfCln==TRUE){df <- df.Clean(df)} else {df$df <- df}

    return(list(df   = df$df,
                info = list(FilePath=FullPath,
                            Info=info,
                            ori.Colnames= dplyr::tbl_df(data.frame(ori.colnames=df$nms)),
                            ori.Rownames= dplyr::tbl_df(data.frame(ori.rownames=df$rws))
                ))
    )
  } else {
    # Return location of file:
    return(FilePath=FullPath)
  }
}


#' get.CSVdata
#'
#' @param path    Path to the data.
#' @param files    A list of \code{.csv} / \code{.xlsx} files containing raw ML2 data.
#' @param finishedOnly    Only import cases with value of variable \code{Finished = 1} (default).
#'
#' @return
#' @export
#'
#' @examples
get.CSVdata <- function(path, fID, finishedOnly=TRUE){
  files <- paste0(path,"/",fID)

  if(grepl(".xlsx",files)){
    temp <- rio::import(files, sheet=1, format = "xlsx", setclass = "tbl_df")
    colnames(temp)[1:10] <- temp[1,1:10]
    df <- dplyr::slice(temp,-1)
  } else {
    t1 <- scan(files,sep=",",nlines = 1,what="character", quiet = T)
    t2 <- scan(files,sep=",",skip = 1, nlines = 1,what="character", quiet = T)
    t1[1:10] <- t2[1:10]
    df <- try.CATCH(tbl_df(read.csv(files,skip=2,header=F,stringsAsFactors=F,col.names=t1)))
    ifelse(is.data.frame(df$value), {df <- df$value}, {df <- tbl_df(read.csv(files,skip = 2,header = F,stringsAsFactors = F,col.names = t1, fileEncoding="latin1"))})
  }

  if(finishedOnly){
    # Remove cases that did not complete the study
    df <- df %>% filter(Finished == 1)
  }
  #df$.id <- fID
  return(df)
}

#' get.Order
#'
#' @param df    A ManyLabs2 data frame.
#' @param S1    Are the data from slate1 (default) or slate2?
#'
#' @author Fred Hasselman
#' @family "get." functions
#'
#' @return A list object with fields:
#' \itemize{
#' \item \code{df}: ManyLabs2 data frame in which the Qualtrics study order has been added to each case.
#' \item \code{Problems}: Cases for which the study order information could not be retrieved.
#' }
#'
#' @examples
get.Order <- function(df, S1=TRUE){
  #   require(plyr)
  ifelse(S1,{
    url <-  "https://docs.google.com/spreadsheets/d/1al8b5nv9AoNdOOlI-RfXf5bCTJY2ophdUIiYhCFGegI/pub?gid=269287357&single=true&output=csv"
    cols <- 3:15
  },{
    url <- "https://docs.google.com/spreadsheets/d/1AvJguRhN8i7MsbcjnGtEnNZ9b-LNTNKINSoHm8Z2f28/pub?gid=1370595041&single=true&output=csv"
    cols <- 3:17
  }
  )
  df.Order <- get.GoogleSheet(url = url)$df
  ProblemID <- list()
  cnt = 0

  for(i in 1:nrow(df)){
    if(!nzchar(df$StudyOrder[i]%0!0%0)){
      cnt = cnt + 1
      #             if(df$IDiffOrder[i]==""){
      #                 df$StudyOrderN[i] <- NA
      #                 df$IDiffOrderN[i] <- NA
      #                 Problem = paste("No study order strings in",df$StudyOrder[i],"and",df$IDiffOrder[i])
      #             } else {
      #                 if(!nzchar(df$StudyOrder[i])){df$StudyOrderN[i] <- NA}
      #                 if(df$IDiffOrder[i]==""){
      #                     df$IDiffOrderN[i] <- NA
      #                     Problem = paste("Wrong study order var? Ind.Diff var",df$IDiffOrder[i],"is empty")
      #                 }
      #             }
      #             ProblemID[[cnt]] <- cbind(rowNum     = i,
      #                                       fileName   = df$.id[i],
      #                                       ResponseID = df$ResponseID[i],
      #                                       Problem    = Problem,
      #                                       StudyOrder = df$StudyOrder[i],
      #                                       IDiffOrder = df$IDiffOrder[i])
    } else {
      OrderString <- unlist(strsplit(x = df$StudyOrder[i], split = "[|]"))
      ls   <- list()
      Problem.s <- list()
      cnt2 <- 0
      for(s in 1:length(OrderString)){
        ls[[s]] <- colnames(df.Order)[cols][df.Order[df.Order$Filename%in%df$.id[i], cols] %in% OrderString[[s]]]
        if(length(ls[[s]])==0){
          cnt2 = cnt2 + 1
          if(OrderString[[s]]%in%df.Order[df.Order$Filename%in%df$.id[i], cols]){
            Problem.s[[cnt2]] <- paste(OrderString[[s]],"(", colnames(df.Order)[cols][df.Order[df.Order$Filename%in%df$.id[i], cols] == OrderString[[s]]], ")")
            ls[[s]] <- NA
          } else {
            Problem.s[[cnt2]] <- paste(OrderString[[s]], "( mismatch )")
          }
        }
      }

      #             if(cnt2!=0){
      #                 cnt = cnt + 1
      #                 ProblemID[[cnt]] <- cbind(rowNum     = i,
      #                                           fileName   = df$.id[i],
      #                                           ResponseID = df$ResponseID[i],
      #                                           Problem    = paste(unlist(Problem.s), collapse="|"),
      #                                           StudyOrder = paste(unlist(ls) ,collapse="|"),
      #                                           IDiffOrder = df$IDiffOrder[i])
      #             }

      df$StudyOrderN[i] <- paste(unlist(ls) ,collapse="|")

    }

    df$IDiffOrderN[i] <- df$IDiffOrder[i]

  }

  return(list(df = df,
              Problems = ldply(ProblemID))
  )

}


#' get.zavCode
#'
#' @param df    Dataset: ML2.S2
#' @param lookup     The lookup table
#'
#' @return The code for each sentence.
#' @export
#'
#' @family "get." functions
#'
#' @examples
get.zavCode <- function(df = NULL, lookup = NULL){
  # Generate variable names.
  varsCOLD <- paste0("zav1.",1:13)
  varsHOT  <- paste0("zav2.",1:13)
  # Decide wether case took HOT or COLD priming condition.
  if(all(df[,varsCOLD] == "")){prime <- varsHOT} else {prime <- varsCOLD}
  # For each sentence, check whether it was correctly unscrambled
  code <- numeric()
  for(v in seq_along(prime)){
    # Search only options corresponding to the Country and Language for this case.
    options <- lookup$value[(lookup$variable == prime[v]) & (lookup$Country == df$Country) & (lookup$Language == df$Language)] %in% df[prime[v]]
    # Return the code (0,1,2) for this sentence.
    rtrn <- na.exclude(lookup$Correct[(lookup$variable == prime[v]) & (lookup$Country == df$Country) & (lookup$Language == df$Language)][options])
    if(length(rtrn) == 0){code[v] <- NA} else {code[v] <- rtrn[1]}
  }
  # Add variables for this case.
  df[paste0("zav.code",1:13)] <- code
  df$zav.include.strict <- all(code == 1)
  df$zav.include.primed <- all(code > 0)
  if(all(df[varsCOLD] == "")){df$zav.condition <- "Hot"} else {df$zav.condition <- "Cold"}
  return(df)
}

get.desriptives <- function(stat.test, vars, keytable){

  if(class(stat.test)=="htest.fisherz"){
    tmp <- unclass(stat.test)
    stat.test <- tmp[1:8]
    stat.test$estimate <- tanh(tmp$effect.size[[1]])
    stat.test$conf.int <- c(tanh(tmp$effect.size.ci[[1]]),tanh(tmp$effect.size.ci[[2]]))
    #attr(stat.test$conf.int,"conf.level")
    class(stat.test) <- "htest"
  }

  # if(grepl("lm",esType)){
  #
  # }

  esType <- gsub("lm[.]","",keytable$stat.type)


  # Descriptive structures ------------------------------------------------------------------------------------------
  N <- vars$N
  vars$N <- NULL

  if(any(names(vars) == "df")){vars$df <- NULL}

  if(any(names(stat.test) == "method")){
    method = stat.test$method
  } else {
    method <- class(stat.test)[1]
  }

  if(any(laply(vars,is.factor))){

    if((esType!="X2")&(!grepl("OR",esType))){
      descr.sum    <- ldply(unique(vars$Condition), function(c){
        Response <- unlist(vars[1])
        cbind(name = c,
              tidy(summary(na.exclude(Response[vars$Condition==c])))
        )}
      )

      id        <- unlist(colwise(is.factor)(as.data.frame(vars)))
      tmp       <- as.data.frame(vars)
      Cid <- which(grepl("(ID)",colnames(tmp)))
      if(length(Cid)!=0){tmp  <- select(tmp, -Cid)}
      descr.raw <- ddply(tmp, names(tmp)[id], tidy)
      descr.raw <- descr.raw[!grepl("[*]",descr.raw$column), ]
    }

    if((esType=="X2")|(grepl("OR",esType))){
      N <- c(N[1],N[1],N[2],N[2])
      descr.sum <- ldply(unique(vars$Condition), function(c){
        dfOut <- tidy(summary(na.exclude(factor(vars$Response[vars$Condition==c]))))
        cbind.data.frame(name      = paste0(c,".",rownames(dfOut)),
                         count     = dfOut$x,
                         n         = N[c],
                         prop.cond = dfOut$x/sum(dfOut$x))
      })
      descr.sum$prop.tot <- descr.sum$count/sum(descr.sum$count)
      descr.raw <- descr.sum
    }

  } else {

    descr.sum  <- ldply(vars, function(gd){
      if(is.list(gd)){gd<-unlist(gd)}
      cbind(tidy(summary(na.exclude(gd)))
      )})

    colnames(descr.sum)[colnames(descr.sum)==".id"] <- "name"

    descr.raw  <- ldply(vars, function(ignore){
      #if(is.list(gd)){gd<-unlist(gd)}
      if(!is.data.frame(ignore)){ignore <- data.frame(ignore)}
      return(tidy(ignore))})

    colnames(descr.raw)[colnames(descr.raw)==".id"] <- "name"
  }

  # Test structure --------------------------------------------------------------------------------------------------

  if(esType=="f"|grepl("lm",keytable$stat.type)){

    test <- cbind.data.frame(statistic = (eval(parse(text=keytable$stat.ncp))),
                             parameter = (rbind(eval(parse(text=keytable$stat.df)))),
                             p.value   = (eval(parse(text=keytable$stat.p.value)))
    )

    if(esType=="f"){
      colnames(test) <- c('statistic','parameter1','parameter2','p.value')
    } else {
      colnames(test) <- c('statistic','parameter1','p.value')
    }

  } else {

    suppressMessages(test <- tidy(stat.test))

    if(esType=="OR"){
      test$parameter <- NA
    }

  }

  if(any(colnames(test)%in%c("n1","n2"))){
    colnames(test)[colnames(test)%in%c("n1","n2")] <- gsub("n","parameter",colnames(test)[colnames(test)%in%c("n1","n2")])
  }

  test$estype <- keytable$stat.type

  return(list(descr.raw = descr.raw,
              descr.sum = descr.sum,
              test      = test) #cbind.data.frame(method = method, test))
  )
}

get.packageData <- function(){
  # Internal use only
  # Load Key Table
  ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df

  # Load data
  setwd("~/Dropbox/Manylabs2/TestOutput/RAW.DATA.PRIVATE")
  ML2.Slate1 <- readRDS("ML2_RawData_S1.rds")
  ML2.Slate2 <- readRDS("ML2_RawData_S2.rds")

  # Prepare list objects
  ML2.data <- list()

  for(s in 1:2){
    vnames <- list()
    for(r in 1:sum(ML2.key$study.slate==s, na.rm = TRUE)){
      vnames[[r]] <- unlist(eval(parse(text=ML2.key$study.vars[ML2.key$study.slate==s][[r]])), use.names = FALSE)
    }
    ML2.data[[s]] <- unique(unlist(vnames))
  }

  ML2.S1 <- dplyr::select(ML2.Slate1, one_of(c(".id", "Source.Global", "Source.Primary", "Source.Secondary", "Country" , "Language", "Execution", "SubjectPool", "Setting", "Tablet", "Pencil", "StudyOrder", "IDiffOrder","StudyOrderN", "IDiffOrderN", ML2.data[[1]]))
  )

  ML2.S2 <- dplyr::select(ML2.Slate2, one_of(c(".id", "Source.Global", "Source.Primary", "Source.Secondary", "Country", "Language", "Execution", "SubjectPool", "Setting", "Tablet", "Pencil", "StudyOrder", "IDiffOrder", "StudyOrderN", "IDiffOrderN", ML2.data[[2]]))
  )

  setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/ManyLabRs/manylabRs")
  devtools::use_data(ML2.S1, ML2.S2, overwrite = TRUE)
}


#' testScript
#'
#' FOR TESTING PURPOSES
#'
#' @param studies     Unique analysis number(s) from the matsterkey sheet.
#' @param tp      Analysis type (1 = 'study.global.include', 2 = 'study.primary.include', 3 = 'study.secondary.include').
#' @param saveRDSfile     Save an RDS file of the output.
#'
#' @return
#' @export
#'
#' @examples
testScript <- function(studies,
                       tp,
                       saveCSVfile=NA,
                       saveRDSfile=NA,
                       subset = c("all","WEIRD","NONWEIRD")[1],
                       dir.out = "~/Dropbox/Manylabs2/TestOutput"){

  analysis  <- c('study.global.include', 'study.primary.include', 'study.secondary.include', 'study.by.order')

  if(between(tp,2,3)){subset="all"}
  dfout <- get.analyses(studies = studies, analysis.type = tp, subset = subset)

  ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df

  if(saveRDSfile){
    fname <- c(paste0(dir.out,"/RESULTS.RDS/ML2_results_global_",subset,".rds"),
               paste0(dir.out,"/RESULTS.RDS/ML2_results_primary_",subset,".rds"),
               paste0(dir.out,"/RESULTS.RDS/ML2_results_secondary_",subset,".rds"),
               paste0(dir.out,"/RESULTS.RDS/Data_Figure_StudyOrder_",subset,".rds"))[tp]
    cat(paste0("\nSaving list object with analyses...\n"))
    # only save non-empty objects

    saveRDS(dfout,file=fname)
  }

  #setwd(dir.out)

  if(saveCSVfile){
    cat(paste0("\nSaving raw cases...\n"))
    l_ply(seq_along(dfout$raw.case),
          function(d){
            if(!is.null(dfout$raw.case[[d]])&NCOL(dfout$raw.case[[d]])>0){
              rio::export(dfout$raw.case[[d]],
                          file = paste0(dir.out,"/RAW.CASE/", names(dfout$raw.case)[d],".",analysis[[tp]],".","RAW.CASE_",subset,".csv"))
              }
            })

    if(tp==1){
      cat(paste0("\nSaving global results...\n"))
      l_ply(seq_along(dfout$aggregated),
            function(d){
              if(!is.null(dfout$aggregated[[d]])&NCOL(dfout$aggregated[[d]])>0){
                rio::export(dfout$aggregated[[d]],
                            file = paste0(dir.out,"/GLOBAL/",
                                          names(dfout$aggregated)[d],".", analysis[[tp]],"_",subset,".csv"))}})}

    if(between(tp,2,3)){
      cat(paste0("\nSaving by_group results...\n"))
      l_ply(seq_along(dfout$aggregated),
            function(d){
              if(!is.null(dfout$aggregated[[d]])&NCOL(dfout$aggregated[[d]])>0){
                rio::export(dfout$aggregated[[d]],
                            paste0(dir.out,"/AGGREGATE/",
                                   names(dfout$aggregated)[d],".", analysis[[tp]],".csv"))}})}
    if(tp==4){
      cat(paste0("\nSaving by_order results...\n"))
      l_ply(seq_along(dfout$aggregated),
            function(d){
              if(!is.null(dfout$aggregated[[d]])&NCOL(dfout$aggregated[[d]])>0){
                rio::export(dfout$aggregated[[d]],
                            paste0(dir.out,"/ORDER/",
                                   names(dfout$aggregated)[d],".", analysis[[tp]],"_",subset,".csv"))}})}
  }

  if(saveRDSfile){
    all <- ldply(dfout$aggregated)

    if(!is.null(all)&NCOL(all)>0){
    rio::export(all, paste0(dir.out,"/GLOBAL/ALL_",analysis[[tp]],"_",subset,".csv"))
    rio::export(all, paste0(dir.out,"/GLOBAL/ALL_",analysis[[tp]],"_",subset,".xlsx"))
    }
  }

  # } else {
  #     l_ply(seq_along(dfout$aggregated),
  #           function(d){
  #               if(!is.null(dfout$raw.case[d])){
  #                   export(dfout$raw.case[d],
  #                          paste0(getwd(),"/AGGREGATE/", names(dfout$aggregated)[d],".", analysis[[tp]],".csv"))}})}
  #               write.csv(x = dfout$aggregated[[d]][dfout$aggregated[[d]]$analysis.type%in%analysis[[tp]],],
  #                                 file = paste0(getwd(),"/AGGREGATE/", names(dfout$aggregated)[d],".", analysis[[tp]],".csv"),
  #                                 row.names = FALSE))
  # }
  #}
}


decide.EqualVar <- function(vars, labels, key, alpha=.05, criterion = 2, group, verbose = FALSE){

  vars$N  <- NULL
  longDat <- NULL

  if(length(vars)==2){

    if(any(laply(vars,is.factor))){

      longDat <- cbind.data.frame(vars)
      colnames(longDat)[laply(vars,is.factor)]    <- "group"
      colnames(longDat)[!(laply(vars,is.factor))] <- "xy"

    } else {

      if(all(lengths(vars)>1)){
        longDat <- ldply(unlist(vars))
        longDat <- data.frame(xy = longDat$V1, group = factor(c(rep(1, length(vars[[1]])),
                                                                rep(2, length(vars[[2]])))))
      }
    }

    if(!is.null(longDat)){

      t1 <- var.test(xy ~ group, data = longDat)
      t2 <- car::leveneTest(xy ~ group, data = longDat)
      t3 <- bartlett.test(xy ~ group, data = longDat)
      t4 <- fligner.test(xy ~ group, data = longDat)

      IDeq <- c(t1$p.value, t2$`Pr(>F)`[[1]], t3$p.value, t4$p.value) > alpha

      varEqual <- sum(IDeq, na.rm = TRUE) >= criterion

      if(verbose){
        cat(paste0("\n>> decide.EqualVar <<\n\n",sum(IDeq)," out of ",length(IDeq)," tests for equality of variances indicate equal population variances (p > ",alpha,"):\n\n", paste0("1. ", t1$method," (p = ",round(t1$p.value,digits = 2),")\n2. ",attributes(t2)$heading,"(p = ",round(t2$`Pr(>F)`[[1]],digits = 2),")\n3. ",t3$method," (p = ",round(t3$p.value, digits = 2),")\n4. ",t4$method," (p = ",round(t4$p.value,digits = 2),")\n")))}

    } else {
      varEqual <- NA
    }

  } else {
    varEqual <- NA
  }

  #   form <-  gsub("[():]|(summary|lm|lmer|lme4|lmerTest|t.test|anova|lmer|cor.test|chiq.test)","",key$stat.test)

  disp(paste0(group,": var.equal set to: ",varEqual),header = FALSE, footer = FALSE)
  return(varEqual)
}

tidyDF <- function(df){
  for(l in seq_along(df$labels)){
    ldply(df$labels[[l]], function(d) tidy(data.frame(eval(parse(text = paste0('df$',d))))))
  }
}

get.output <- function(key, vars, descr, group, analysis, varEqual, test, ESCI, test.ConsoleOutput, SourceInfo, stat.test){

  if(key$stat.type == "OR"){
    # Data list for calculating Effect Sizes CI based on NCP
    output <- data.frame(
      study.id      = key$study.id,
      study.slate   = key$study.slate,
      study.name    = key$study.name,
      study.source  = group,
      analysis.type = analysis,
      analysis.name = key$study.analysis,
      stat.N        = sum(vars$N, na.rm = T),
      stat.n1       = ifelse(length(vars$N)==2,vars$N[[1]],vars$N),
      stat.n2       = ifelse(length(vars$N)==2,vars$N[[2]],NA),
      stat.cond1    = descr[1, ],
      stat.cond2    = descr[2, ],
      stat.cond3    = descr[3, ],
      stat.cond4    = descr[4, ],
      test.type     = key$stat.type,
      test          = test,
      test.varequal = varEqual,
      test.table    = paste(capture.output(print(knitr::kable(table(vars$Condition,vars$Response),format = 'rst'))),collapse="\n"),  #paste0(knitr::kable(table(vars$Condition,vars$Response),format = 'rst'),collapse="\n"),
      ESCI          = ESCI,
      test.ConsoleOutput = test.ConsoleOutput,
      source        = SourceInfo
    )
  }

  if(key$stat.type!="OR"){
    # Data list for calculating Effect Sizes CI based on NCP
    # output <- data.frame(
    #     study.id      = key$study.id,
    #     study.slate   = key$study.slate,
    #     study.name    = key$study.name,
    #     study.source  = group,
    #     analysis.type = analysis,
    #     analysis.name = key$study.analysis,
    #     stat.N        = sum(vars$N, na.rm = T),
    #     stat.n1       = ifelse(length(vars$N)==2,vars$N[[1]],vars$N),
    #     stat.n2       = ifelse(length(vars$N)==2,vars$N[[2]],NA),
    #     ,
    #     test.type     = key$stat.type,
    #     test          = test,
    #     test.varequal = varEqual,
    #     ESCI          = ESCI
    # )

    for(r in 1:nrow(descr)){
      eval(parse(text = paste0("stat.cond",r," =  descr[r, ]",colapse="")))
    }

    eval(parse(text=paste0("output <- data.frame(study.id = key$study.id, study.slate = key$study.slate, study.name = key$study.name, study.source  = group, analysis.type = analysis, analysis.name = key$study.analysis, stat.N = sum(vars$N, na.rm = T), stat.n1 = ifelse(length(vars$N)==2,vars$N[[1]],vars$N), stat.n2 = ifelse(length(vars$N)==2,vars$N[[2]],NA), ", paste0("stat.cond",1:nrow(descr)," = ","stat.cond",1:nrow(descr), collapse = ", "),", test.type = key$stat.type, test = test, test.varequal = varEqual, ESCI = ESCI, test.ConsoleOutput = test.ConsoleOutput, source = SourceInfo)", collapse = ", ")))
  }

  return(output)
}


decide.analysis <- function(ML2.key, studies=NA, tp = NA){

  analysis <- c('study.global.include', 'study.primary.include', 'study.secondary.include','study.global.include')
  groups   <- c('ML2.df$Source.Global','ML2.df$Source.Primary','ML2.df$Source.Secondary','ML2.df$study.order')

  if(is.null(tp)){tp <- NA}
  if(any(is.na(studies))){studies <- na.exclude(ML2.key$unique.id)}
  if(tp==4){
    studies <- studies[studies%in%ML2.key$unique.id[ML2.key$study.figure2.include==1]]
  }

  if(is.na(tp[1])){
    disp(paste0("Analyzing global, primary and secondary studies"), header = FALSE, footer = TRUE)
    studiess <- c(ML2.key$unique.id[ML2.key$study.global.include[studies]==1],
                  ML2.key$unique.id[ML2.key$study.primary.include[studies]==1],
                  ML2.key$unique.id[ML2.key$study.secondary.include[studies]==1])
    tps = c(rep(1,length(ML2.key$unique.id[ML2.key$study.global.include[studies]==1])),
            rep(2,length(ML2.key$unique.id[ML2.key$study.primary.include[studies]==1])),
            rep(3,length(ML2.key$unique.id[ML2.key$study.secondary.include[studies]==1]))
    )
  } else {
    disp(paste0("Analyzing studies in ", analysis[tp]), header = FALSE, footer = TRUE)
    studiess <- lapply(analysis[tp], function(c) ML2.key$unique.id[ML2.key[,c]==1])
    tps      <- unlist(sapply(seq_along(studiess), function(s) rep(tp[s], length(studiess[[s]]))))
    studiess <- unlist(studiess)
  }


  tp           <- tps[!is.na(studiess)&studiess%in%studies]

  if(any(is.na(studiess[!is.na(studiess)&studiess%in%studies]))){stop("Analysis ID and analysis type do not agree [e.g. analysis type is 'primary', but analysis ID refers to 'secondary']")}

  return(list(studiess = studiess[!is.na(studiess)&studiess%in%studies],
              ugroup   = groups[tp],
              tp       = tp)
  )
}

#' get.plotly
#'
#' @param data Dataframe with ML2 testresutls and ESCI output.
#'
#' @return
#' @export
#'
#' @examples
get.plotly <- function(data,analysis_url){

  p <- plot_ly(data,
               x    = ESCI.r,
               y    = test.p.value,
               mode = "markers",
               hoverinfo = "text",
               size = ESCI.N.total,
               text = paste("<b>r</b> =",ESCI.r,"<b>p</b> <",signif(test.p.value,digits = 4),"<br>",
                            #"<i>Analysis:</i><a href=",analysis_url,">",.id,"</a><br>",
                            "<i>Sample:</i>",study.source),
               name = analysis_url) %>%
    add_trace(x=seq(-1,1),y=rep(0.05,3),  line = list(color="green"), name="p<.05") %>%
    add_trace(x=seq(-1,1),y=rep(0.000001,3), line = list(color="orange"), name="p<.000001") %>%
    add_trace(x=seq(-1,1),y=rep(0.000000000001,3), line = list(color="red"), name="p<.000000000001") %>%
    layout(yaxis = list(title = "p-value"),
           xaxis = list(title = "Effect Size r"))
  l <- plotly_build(p)
  l$data[[1]]$text <- gsub("<br>ESCI.N.total (size):"," - <b>N</b> = ",l$data[[1]]$text,fixed=T)
  l$data[[1]]$marker$size <- l$data[[1]]$marker$size/10
  return(plotly_build(l))
}



#' renderHTMLresults
#'
#' @param pageID
#'
#' @return
#' @export
#'
#' @examples
renderHTMLresults <- function(pageID) {
  rmarkdown::render(input  = "ML2_interactive_results.Rmd",
                    params = list(
                      set_title = paste("ManyLabs2 -",pageID),
                      pageID    = pageID,
                      fName     = paste0(paste("ML2",pageID,sep='_'),".html")),
                    output_format = "html_document",
                    output_file   = paste0(paste("ML2",pageID,sep='_'),".html"),
                    output_dir    = "interActive/"
  )
}



generateOutput <-  function(describe = describe,
                            var.lor  = NA,
                            runningGroup = "None",
                            runningAnalysis = "None"){

  ESCI <- list(value = NULL,
               warning = "init")

  test  = describe$test
  descr = describe$descr.raw

  if(grepl("OR",test$estype, fixed = TRUE)){
    Nv <- c(descr$n[1],descr$n[3])
  } else {
    Nv <- c(descr$n[1],descr$n[2])
  }

  if(!is.na(test[1,1])){

    ESCI <- try.CATCH(any2any(testInfo  = test,
                              df1       = test[, which(grepl("parameter",colnames(test)))[1]],
                              df2       = test[, which(grepl("parameter2",colnames(test)))],
                              N         = sum(Nv, na.rm = TRUE),
                              n1        = Nv[1],
                              n2        = Nv[2],
                              esType    = test$estype,
                              var.lor   = var.lor,
                              CL          = stat.params$conf.level,
                              keepSign    = TRUE,
                              alternative = ifelse(is.null(test$alternative),
                                                   stat.params[[4]],
                                                   as.character(test$alternative))
    ))
  }

  if(all(is.null(ESCI$warning),
         grepl("simpleWarning",ESCI$warning),
         !grepl("Error", ESCI$value[[1]]),
         !grepl("message", names(unlist(ESCI))[1]),
         !is.na(test[1,1]))){
    ESCI  <- ESCI$value
    es.id <- which(colnames(ESCI)%in%"r")
  } else {
    ESCI  <- test
    es.id <- 1
  }

  ifelse(is.na(ESCI[es.id]),
         disp(paste(runningAnalysis,'-',
                    runningGroup,'>> ES r is NA'), header = FALSE, footer = FALSE),

         ifelse(abs(ESCI[es.id][1])>=.95,
                disp(paste(runningAnalysis,'-',
                           runningGroup,'>> ES r is extreme hi/lo: ',
                           round(ESCI[es.id][1],digits=2)), header = FALSE, footer = FALSE),

                ifelse(abs(ESCI[es.id][1])<=0.05,
                       disp(paste(runningAnalysis,'-',
                                  runningGroup,'>> ES r is close to 0: ',
                                  round(ESCI[es.id][1],digits=2)), header = FALSE, footer = FALSE),

                       ifelse(any(is.infinite(ESCI[es.id][[1]])),
                              disp(paste(runningAnalysis,'-',
                                         runningGroup,'>> ES r (CI) is infinite'),
                                   header = FALSE, footer = FALSE),
                              NA)
                )
         )
  )

  return(ESCI)
}



# write.csv(x = dfout$raw.case[[d]],
#                   file = paste0(getwd(),"/RAW.CASE/",
#                                 names(dfout$raw.case)[d],".",analysis[[tp]],".","RAW.CASE",".csv"),
#                   row.names = FALSE))

# l_ply(seq_along(dfout$dumpDescriptives),
#       function(d) write.csv(x    = dfout$dumpDescriptives[[d]],
#                             file = paste0(getwd(),"/RAW.DESCRIPTIVES/", names(dfout$dumpDescri# ptives)[d],".",analysis[[tp]],".","RAW.DESCRIPTIVES",".csv"), row.names = FALSE))
# write.csv(x = dfout$aggregated[[d]], file = paste0(getwd(),"/GLOBAL/", names(dfout$aggregated)[d],".", analysis[[tp]],".csv"), row.names = FALSE))

