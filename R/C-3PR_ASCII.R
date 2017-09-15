init <- function(){

  srcDir <- "~/Documents/GitHub/manylabRs/manylabRs/R/"
  #source(paste0(srcDir,"C-3PR_ASCII.R"))
  source(paste0(srcDir,'getData.R'))
  source(paste0(srcDir,'inIT.R'))
  source(paste0(srcDir,'ML2_variable_functions.R'))
  source(paste0(srcDir,'fRedsRutils.R'))

  # require(devtools)
  # srcDir <- "https://raw.githubusercontent.com/FredHasselman/manylabRs/master/pkg/R/"
  #
  # devtools::source_url(paste0(srcDir,"C-3PR_ASCII.R"))
  # devtools::source_url(paste0(srcDir,'getData.R'))
  # devtools::source_url(paste0(srcDir,'inIT.R'))
  # devtools::source_url(paste0(srcDir,'ML2_variable_functions.R'))
  # devtools::source_url(paste0(srcDir,'fRedsRutils.R'))

  # Function inIT will load and -if necessary- install packages passed in a list (unIT will do the reverse operation).
  in.IT(c("MBESS","reshape2","plyr","tidyverse","metafor","RCurl","xlsx","broom","httr","compute.es","downloader","car", "lme4", "lmerTest","exact2x2","ggplot2","gplots","gridExtra","lattice","latticeExtra","rio","scales","lubridate"))

  ## ML2.key <<- get.GoogleSheet(data='ML2masteRkey')$df
}

#' disp
#'
#' @param message     A message to be displayed in the Console.
#' @param header     Print a header of '~' symbols (=\code{TRUE}), or '~' symbols with few words of text (=\code{character vector})
#' @param footer     Print a footer '~' symbols.
#'
#' @description Displays easy-to-spot text in the Console.
#'
#' @export
disp <- function(message='Hello world!', header = "disp", footer = TRUE){

  ps <- "# "

  msg <- textConnection(message)
  mWidth <- max(laply(readLines(msg),nchar))
  if(!grepl(ps,message)) mWidth <- mWidth+2

  if(is.character(header)){
    hWidth <- max(laply(header,nchar))
    mWidth <- max(hWidth,mWidth)
  }

  dmessage <- list()
  for(m in 1:length(message)){
    # b <- floor((mWidth-nchar(message[m]))/2)
    e <- mWidth-nchar(message[m])
    dmessage[[m]] <- paste0(ps,message[m])
  }

  banner <- paste0(rep('~', mWidth), collapse = "")
  if(is.character(header)){
    b <- floor((nchar(banner)-nchar(header))/2)
    e <- ceiling((nchar(banner)-nchar(header))/2)
    leader <- paste0('\n\t',paste0(rep('~',b),collapse=""),header,paste0(rep('~',e),collapse=""))
  }
  if(header == TRUE){
    leader <- banner
  }
  if(header == FALSE){
    leader <- paste0(ps)
  }

  if(footer){
    cat(paste0('\n\t',leader,'\n\t',dmessage,'\n\t',banner,'\n'))
  } else {
    cat(paste0('\n\t',leader,'\n\t',dmessage))
  }
  close(msg)
  return(invisible(message))
}

#' fill_viol
#'
#' @details  Function to create geom_ploygon calls for \code{\link{vioQtile}}
#'
#' @param gr.df Internal.
#' @param gr Internal.
#' @param qtile Internal.
#' @param probs Internal.
#'
#' @return A list for \code{\link{vioQtile}}
#'
#' @export
#'
#' @description  This is adapted from: \url{http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot}
#'
#' @seealso vioQtile
#'
fill_viol<-function(gr.df,gr,qtile,probs){
  #     require(ggplot2)
  #     require(dplyr)

  ifelse(is.null(qtile),{
    cuts <- cut(gr.df$y, breaks = quantile(gr.df$y, probs, na.rm=T, type=3, include.lowest = T, right = T), na.rm=T)},{
      cuts <- cut(gr.df$y, breaks = qtile, na.rm=T)
    }
  )

  quants <- dplyr::mutate(gr.df,
                          x.l=x-violinwidth/2,
                          x.r=x+violinwidth/2,
                          cuts=cuts)

  plotquants <- data.frame(x=c(quants$x.l,rev(quants$x.r)),
                           y=c(quants$y,rev(quants$y)),
                           id=c(quants$cuts,rev(quants$cuts)))

  #cut by quantile to create polygon id
  geom <- geom_polygon(aes(x=x,y=y,fill=factor(id)),data=plotquants,alpha=1)

  return(list(quants=quants,plotquants=plotquants,geom=geom))
}

#' vioQtile
#'
#' @param gg     A ggplot.
#' @param qtiles    Quantiles.
#' @param probs     Probabilities.
#' @param labels    Labels.
#' @param withData    Return Data.
#'
#' @details
#'  This is adapted from: \url{http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot}
#'
#'  \strong{Changed:}
#'  Deal with 'empty' quantile groups
#'  Deal with original data
#'  More input, more output
#'
#' @export
#'
vioQtile <- function(gg=NULL,qtiles=NULL,probs=seq(0,1,.25),labels=paste(probs[-1]*100),withData=FALSE){
  #  require(ggplot2)

  g.df <- ggplot2::ggplot_build(gg)$data[[1]]    # use ggbuild to get the outline co-ords

  ifelse(is.null(qtiles),{
    gg <- gg + lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x, ],x,NULL,probs)$geom)},{
      gg <- gg + lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x, ],x,qtiles[x, ],probs)$geom)}
  )

  gg <- gg + ggplot2::geom_hline(aes(yintercept=0)) +
    ggplot2::scale_fill_grey(name="Quantile\n",labels=labels,guide=guide_legend(reverse=T,label.position="right")) +
    ggplot2::stat_summary(fun.y=median, geom="point", size=8, color="grey80",shape=21,fill="white")

  if(withData){
    ifelse(is.null(qtiles),{
      ggData <- lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x,],x,NULL,probs))},{
        ggData <- lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x,],x,qtiles[x,],probs))
      }
    )
    return(list(ggGraph=gg,ggData=ggData))
  } else {
    return(gg)
  }
}

# varfuns - Functions used to prepare data for analysis ------------------------------

clean.Source <- function(source.raw, SourceTable){
  source.clean <- gsub("([[:blank:]]|[[:punct:]]|[[:cntrl:]])+","",source.raw)
  for(s in seq_along(SourceTable$Source.Field.Raw)){
    ID <- which(source.clean%in%SourceTable$Source.Field.Raw[[s]])
    source.clean[ID] <- SourceTable$Source.Field.Correct[[s]]
  }
  return(as.data.frame(source.clean))
}

# func: Get additional changes, variables, etc.
get.fieldAdd <- function(data,stable){
  data$Source.Global    <- NA
  data$Source.Primary   <- NA
  data$Source.Secondary <- NA
  data$Country      <- NA
  data$Location     <- NA
  data$Language     <- NA
  data$Weird        <- NA
  data$Execution    <- NA
  data$SubjectPool  <- NA
  data$Setting      <- NA
  data$Tablet       <- NA
  data$Pencil       <- NA
  data$StudyOrder   <- NA
  data$IDiffOrder   <- NA

  for(s in seq_along(stable$Source)){
    ID <- which((stable$Source[[s]]==data$source)&(stable$Filename[[s]]==data$.id))
    if(length(ID)>0){
      data$Source.Global[ID]    <- stable$Source.Global[[s]]
      data$Source.Primary[ID]   <- stable$Source.Global[[s]]
      data$Source.Secondary[ID] <- stable$Source[[s]]
      data$Country[ID]      <- stable$Country[[s]]
      data$Location[ID]     <- stable$Location[[s]]
      data$Language[ID]     <- stable$Language[[s]]
      data$Weird[ID]        <- stable$Weird[[s]]
      data$Execution[ID]    <- stable$Execution[[s]]
      data$SubjectPool[ID]  <- stable$SubjectPool[[s]]
      data$Setting[ID]      <- stable$Setting[[s]]
      data$Tablet[ID]       <- stable$Tablet[[s]]
      data$Pencil[ID]       <- stable$Pencil[[s]]
      data[ID, "StudyOrder"]   <- data[ID, stable$StudyOrder[[s]]]
      data[ID, "IDiffOrder"]   <- data[ID, stable$IDiffOrder[[s]]]
    }
  }
  return(as.data.frame(data))
}

scanColumns <- function(pattern, data){
  #    require(plyr)
  # Function to scan columns of a dataframe for a regex pattern and return a variable (in rows) by case (in columns) logical matrix
  # Includes some error catching, correcting and reporting
  idS    <- list()
  cNames <- colnames(data)
  for(c in seq(1,ncol(data))){
    tmp      <- try.CATCH(c(grepl(pattern,data[[c]],useBytes = T,ignore.case=T)))
    if(is.null(tmp$warning)){
      idS[[c]]<-tmp$value
    } else {
      cat('Error in column: ', colnames(data)[c],'->\t',paste0(tmp$warning),'\n')
    }
  }
  idD   <- ldply(idS)
  cs    <- rowSums(idD)
  colID <- which(cs>0)
  idS   <- t(idD[colID, ])

  return(list(idMatrix = idS,
              idColnames = cNames[colID])
  )
}

# func: Clean data fields to NA according to pattern
# Default: Set test trials and -99 to NA
clean.ML2fieldsNA <- function(source.raw, pattern="(test|-99)", S1 = TRUE, ps = "# "){

  # First mark list of known test runs for removal
  disp(message = 'Marking known test sessions for removal', header = 'Clean ML2 Test Data - Step 1', footer = F)

  source.raw$Finished[which(c("R_blS3XHmD4p14kf3",
                              "R_8Cjo1Lguez90bqt",
                              "R_bC85NEijsCjOqep")%in%source.raw$ResponseID)] <- 0

  textcolumns <- c('age','sex','hometown','education','comments','raceother','cogref.1','cogref.2','cogref.3')

  if(!S1){
    textcolumns <- c(textcolumns, 'ross.s2.1_1_TEXT', 'ross.s2.1_2_TEXT', 'sava1.2', 'sava1.3', 'sava1.7', 'sava1.8',
                     'sava1.12', 'sava1.13', 'sava1.18', 'sava1.19', 'sava1.24', 'sava1.25', 'sava1.30', 'sava1.31',
                     'sava1.36', 'sava1.37', 'sava1.41', 'sava1.42', 'sava2.2', 'sava2.3', 'sava2.7', 'sava2.8',
                     'sava2.12', 'sava2.13', 'sava2.18', 'sava2.19', 'sava2.24', 'sava2.25', 'sava2.30', 'sava2.31',
                     'sava2.36', 'sava2.37', 'sava2.41', 'sava2.42', 'zhon1.1', 'zhon2.1', 'zav.int.1', 'zav1.1',
                     'zav1.2', 'zav1.3', 'zav1.4', 'zav1.5', 'zav1.6', 'zav1.7', 'zav1.8', 'zav1.9', 'zav1.10',
                     'zav1.11', 'zav1.12', 'zav1.13', 'zav2.1', 'zav2.2', 'zav2.3', 'zav2.4', 'zav2.5', 'zav2.6',
                     'zav2.7', 'zav2.8', 'zav2.9', 'zav2.10', 'zav2.11', 'zav2.12', 'zav2.13')
  }

  if(S1){
    textcolumns <- c(textcolumns, 'crit1.1_1_TEXT', 'crit1.1_2_TEXT', 'crit1.1_3_TEXT',
                     'ross.s1.1_1_TEXT', 'ross.s1.1_2_TEXT', 'kay1.1', 'kay1.2_7_TEXT',
                     'kay1.2_8_TEXT', 'kay1.2_9_TEXT', 'kay1.3',  'kay2.1', 'kay2.2_7_TEXT', 'kay2.2_8_TEXT',
                     'kay2.2_9_TEXT', 'kay2.3', 'and1.1', 'and2.1'
    )
  }

  # Now search for 'test' and apply Finished == 1 filter
  disp(message =  paste(c("Checking columns:\n\t\t ",paste(textcolumns, collapse="\n\t",ps,"\t "),
                          "\n\t",ps," for a variant of pattern: 'test'"),
                        collapse=""), header = 'Clean ML2 Test Data - Step 2', footer = F)

  idS <- scanColumns(pattern='test',data = source.raw[, textcolumns])
  if(sum(colSums(idS$idMatrix) > 0)){
    source.raw$Finished[(rowSums(idS$idMatrix)>0)] <- 0
  } else {
    source.clean <- source.raw
  }

  # Remove the test sessions
  source.clean <- source.raw %>% filter(Finished == 1)

  # Now look for '-99'
  disp(message = 'Checking all columns except "LocationLongitude" for pattern: "-99"',
       header = 'Clean Test ML2 Data - Step 3', footer = F)
  idS <- scanColumns(pattern='-99', data = source.clean[ ,which(!colnames(source.clean)%in%c("LocationLongitude"))])
  # Set -99 to NA
  for(c in seq(1,ncol(idS$idMatrix))){
    source.clean[idS$idMatrix[ ,c], as.numeric(colnames(idS$idMatrix)[c])] <- NA
  }
  disp(message = 'Done!')
  return(source.clean)
}

#' get.chain
#'
#' @param inf Internal
#'
#' @export
#'
get.chain <- function(inf){
  # Build a filter chain

  filt.vars <- unlist(inf$study.cases.include,recursive=F)

  # include sites
  filt.site <- paste0(" %>% dplyr::filter(",paste0(inf$study.sites.include),")")
  #filt.site <- paste0(" %>% filter(is.character(.id))")

  # Data frame filter
  filt.df <- paste0(" %>% dplyr::select(", paste0(inf$id.vars,collapse=","),")", filt.site)

  #Variables filter
  return(list(df=filt.df,vars=filt.vars))
}

#' get.cases
#'
#' @param rule Internal
#' @param study.vars Internal
#' @param study.vars.labels Internal
#' @param stat.params Internal
#'
#' @export
#'
get.cases <- function(rule,study.vars,study.vars.labels,stat.params){
  #rule <- cases.include
  type  <-names(rule)
  if(!is.matrix(rule[[1]])){rule  <- rbind(rule[[1]])} else {rule <- rule[[1]]}
  Nrule <- nrow(rule)

  isna <- ifelse(stat.params$censorNA,{" & !is.na(X)"},{""})
  do <- list()

  # Rule 1 should always be about the variables in 'study.vars'
  # Rule 2..N can concern a subset of the variables in 'study.vars'.
  # Assumption for type="each": Rule 2 number of variables is a multiple of number of conditions, with variables grouped to fit to each condition.
  # Assumption for type="sep": Rule 2 contains a separate rule independent of variables listed in the first rule.

  r = 1
  filtvars <- study.vars.labels[names(study.vars.labels)%in%rule[r,1]]

  # Start with 'pre'
  pre <-llply(unlist(filtvars), function(v){paste0(v,' %>% filter(')})

  if(type == "each"){

    if(all(filtvars[[1]]%in%names(study.vars))){
      filtvars <- study.vars
      do <- laply(seq_along(filtvars),
                  function(v) laply(seq_along(filtvars[[v]]),
                                    function(vv) paste0(gsub("X", filtvars[[v]][vv],rule[r,2]),
                                                        gsub("X",filtvars[[v]][vv], isna)))
      )
    }

    if(!is.matrix(do)){do <- as.matrix(do)}

    if(Nrule > 1){
      s <- ncol(do)
      for(r in 2:Nrule){
        filtvars <- unlist(study.vars.labels[names(study.vars.labels)%in%rule[r,1]])
        do  <- cbind(do, laply(seq_along(filtvars),
                               function(vv) paste0(gsub("X",filtvars[[vv]],rule[r,2]))))
      }
    }
    case <- llply(seq_along(pre), function(fr) paste0(pre[[fr]], paste0(do[fr, ],collapse = ' & '),")"))
    names(case) <- names(study.vars)
  }

  if(type == "sep"){
    if(all(filtvars[[1]]%in%names(study.vars))){
      filtvars <- study.vars
      do <- llply(seq_along(filtvars),
                  function(v) laply(seq_along(filtvars[[v]]),
                                    function(vv) paste0(gsub("X",filtvars[[v]][vv],rule[r,2]),
                                                        gsub("X",filtvars[[v]][vv],isna)))
      )
    }

    names(do) <- names(filtvars)

    if(Nrule > 1){
      for(r in 2:Nrule){
        if(rule[r,1]%in%names(do)){
          filtvars <- unlist(study.vars.labels[names(study.vars.labels)%in%rule[r,1]])
          do[[r]] <- c(do[[r]], laply(seq_along(filtvars),
                                      function(vv) paste0(gsub("X",filtvars[[vv]],rule[r,2])))
          )
        }
      }
    }

    case <- llply(seq_along(pre), function(fr) paste0(pre[[fr]], paste0(do[[fr]],collapse = ' & '),")"))
    names(case) <- names(study.vars)
  }

  return(eval(parse(text=paste0('list(',rule[1],'=case)'))))
}

#' get.info
#'
#' @param keytable Internal
#' @param cols Internal
#'

#' @export
#'

get.info <- function(keytable,cols, subset){
  # Read Variables and Parameters from:
  #keytable <- ML2.key[s,]
  study.vars         <- eval(parse(text=keytable[,'study.vars']))
  study.vars.labels  <- eval(parse(text=keytable[,'study.vars.labels']))
  cases.include      <- eval(parse(text=keytable[,'study.cases.include']))
  stat.params        <- eval(parse(text=keytable[,'stat.params']))
  #cases <- llply(seq_along(cases.include),function(i) get.cases(cases.include[i],study.vars,study.vars.labels,stat.params))
  cases              <- get.cases(cases.include, study.vars, study.vars.labels, stat.params)
  sites.include      <- eval(parse(text=keytable[,'study.sites.include']))
  if(sites.include[[1]][1]=="all"){sites.include[[1]]<-'is.character(source)'}
  if(subset!="all"){
    W <- ifelse(subset=="WEIRD",1,0)
    sites.include[[1]] <- paste0(sites.include[[1]][1],' & (Weird == ',W,')')
    }

  # Find correct columns in this dataset according to ML2.key: 'ML2.in$study.vars'
  id.vars  <- which(cols%in%c(unlist(study.vars),'uID','.id','age','sex','source','Source.Global','Source.Primary','Source.Secondary','Country','Location','Language','Weird','SubjectPool','Setting','Tablet','Pencil','Execution', 'StudyOrderN','IDiffOrderN'))
  return(list(study.vars          = study.vars,
              study.vars.labels   = study.vars.labels,
              stat.params         = stat.params,
              study.cases.include = cases,
              study.sites.include = sites.include,
              id.vars             = id.vars))
}

#' get.sourceData
#'
#' @param ML2.id  Internal
#' @param ML2.df  Internal
#' @param ML2.in  Inernal
#'
#' @return A list with fields \code{study.vars} (data organised according to the \code{masteRkey} spreadsheet), \code{study.vars/labels}, \code{N}, and \code{RawDataFilter}(raw data, unfiltered).
#' @export
#'

get.sourceData <- function(ML2.id,ML2.df,ML2.in){
  N       <- numeric(length(ML2.in$study.vars))
  #study.vars[1]    <- unlist(ML2.in$study.vars)
  vars <- list()
  dfname <- list()
  RawData <- list()
  #id <- factor(ML2.df$.id)
  for(i in seq_along(ML2.in$study.vars)){
    dfname[i] <- names(ML2.in$study.vars)[[i]]
    eval(parse(text=paste0(names(ML2.in$study.vars)[[i]],
                           " <- tbl_df(dplyr::select(ML2.df,",
                           paste0(c(ML2.in$study.vars[[i]],'uID'), collapse=","),"))"))
    )

    # Change text to numbers
    suppressWarnings(if(
      any(eval(parse(text = paste0('apply(',
                                   names(ML2.in$study.vars)[i],',2,is.numeric)==FALSE'))))){
      eval(parse(text = paste0(names(ML2.in$study.vars)[i],' <- data.frame(sapply(which(apply(',
                               names(ML2.in$study.vars)[i],', 2, is.numeric)==FALSE), function(si) as.numeric(',
                               names(ML2.in$study.vars)[i],'[[si]])))')))
    })
  }

  for(i in seq_along(dfname)){
    eval(parse(text=paste0(dfname[i],' <- ', unlist(ML2.id$vars)[i])))
    RawData[[i]] <- dplyr::mutate(ML2.df,
                                  Included = eval(parse(text=paste0('ML2.df$uID %in% ',dfname[i],'$uID')))
    )
    #    N[i]         <- eval(parse(text=paste0("nrow(",dfname[i],")"))
  }

  if(ML2.in$stat.params$within){sameID <- ML2.df$uID[RawData[[1]]$Included&RawData[[2]]$Included]}

  #  sameID <- eval(parse(text=paste0(dfname[which.max(N)],"$uID %in% ", dfname[which.min(N)],"$uID") ))
  for(i in seq_along(dfname)){
    # Check if datasets are equal length for within subject analyses
    if(ML2.in$stat.params$within){
      eval(parse(text=paste0(dfname[i]," <- ",dfname[i],"[",dfname[i],"$uID %in% sameID, ]")))
      RawData[[i]] <- mutate(ML2.df,
                             Included = eval(parse(text=paste0('ML2.df$uID %in% ', dfname[i],'$uID',collapse="&")))
      )
    }
    N[i] <- eval(parse(text=paste0("sum(!is.na(",dfname[i],"), na.rm = TRUE)")))
    eval(parse(text=paste0(dfname[i],"<-", dfname[i]," %>% dplyr::select(which(colnames(",dfname[i],")!='uID'))")))
    eval(parse(text=paste0(dfname[i],' -> vars[[i]]')))
  }

  vars[[length(ML2.in$study.vars)+1]] <- N
  #if(length(ML2.in$study.vars.labels)==0){ML2.in$study.vars.labels <- list(NoLabels="None")}
  vars[[length(ML2.in$study.vars)+2]] <- ML2.in$study.vars.labels
  vars[[length(ML2.in$study.vars)+3]] <- RawData
  names(vars) <- c(names(ML2.in$study.vars),"N","labels","RawDataFilter")
  return(vars)
}

#' any2any
#'
#' Converts most common test statistics into most common (signed) effect sizes.
#'
#' @param st     Value(s) of a test statistic.
#' @param df1     Degrees of freedom
#' @param df2     NULL or degrees of freedom of the denominator for the f-distribution.
#' @param N     Number of data points used in calculation of test-statistic.
#' @param n1    Number of data points in sample 1.
#' @param n2    Number of data points in sample 2.
#' @param esType     Type of test statistic. One of: "t", "lm.t", "f", "lm.f", "r", "X2", "Z", "lm.Z"
#' @param CIcalc     If \code{TRUE} (default) the Confidence Interval for the test statistic in \code{x} will be calculated using the "Confidence limits for noncentral parameters" functions in package (e.g., for type - "t": \link[MBESS]{conf.limits.nct}).
#' @param CL    Confidence Limit (default: .95).
#' @param rID    Correlation among predictor values in a linear model.
#' @param q      Number of predictors in the model.
#' @param alternative     Alternative hypothesis (defult = "two").
#' @param keepSign     Return effect size with sign of test statistic? (default = TRUE).
#' @param keepSignNames     Which effect sizes should keep the sign if \code{keepSign = TRUE}? Default is to keep the sign for: "r","l.r","u.r","fisher.z","l.z","u.z".
#'
#' @details The procedure to calculate a variety of effect sizes is as follows:
#'
#' \itemize{
#' \item If \code{CIcalc == FALSE}, \code{package::compute.es} will be used to convert the test statistic to a large number of effect size estimates. The confidence intervals around the effect size estimates will be based meta-analytic estimates of effect size variance (e.g., for type - "t": \link[compute.es]{tes}).
#' \item If \code{CIcalc == TRUE}, \code{package::MBESS} will be used to calculate the confidence interval for the test statistic based on its noncentral distribution (e.g., for type - "t": \link[MBESS]{conf.limits.nct}). Subsequently the test statistic, as well as its lower and upper confidence limit will each be passed to \code{compute.es} seperately.
#' \item If \code{keepSign == TRUE} the sign of the test statistic will be copied to all the effect sizes in \code{keepSignNames}.
#' }
#'
#' @note The prefix "lm" is currently disregarded, but will be implemented in future versions to indicate the test statistic is in fact a fixed factor in a linear model.
#'
#' @author
#' F Hasselman (inspired by RP:P function \code{any2r} by CHJ Hartgerink)
#'
#' @return The effect sizes calculated by \code{compute.es} corresponding to the test statistic(s), with either meta-analytic, or, exact CI.
#' @export
#'

any2any <- function(testInfo,
                    df1 = NULL,
                    df2 = NULL,
                    N   = NULL, n1 = NULL, n2 = NULL,
                    esType  = NA,
                    var.lor = NA,
                    CIcalc  = TRUE, CL = .95, rID = 0, q = 1,
                    alternative   = "two", keepDirection = TRUE,
                    keepSign      = TRUE,
                    keepSignNames = c("r","l.r","u.r","fisher.z","l.z","u.z")){
  # require(MBESS)
  # require(compute.es)
  esType.cl <- NA

  ifelse(grepl("two",alternative),{alternative <<- "two"},{alternative <<- "one"})

  #  ifelse(any(grepl("estimate",colnames(testInfo))), {st <- testInfo$estimate},{st <- testInfo$statistic})
  if(any(grepl("(.r)+", esType),(esType%in%c("OR")))){
    st <- testInfo$estimate
  } else {
    st <- testInfo$statistic
  }

  if(grepl("Z.f", esType, fixed = TRUE)){
    if(length(testInfo$estimate)>1){
      esType <- esType.cl <- "Z"
      st <- testInfo$statistic
    }
  }

  if(grepl("Z", esType, fixed = TRUE)){
      if(alternative=="one"){
        n1 <- n2 <- N/2
        }
  }
  #which(colnames(ESCI) == "d"):NCOL(ESCI)
  if(is.null(st)){stop("No test statistic to caclulate ES-CI.")}

  # Use Cohen's dz for paired t-test
  if(esType%in%"t.p"){
    n1 <- n2 <- N/2
    st <- testInfo$statistic / sqrt(N)
  }


  # Check for model-based t-statistics
  if(grepl("(lm.t)+",esType)){
    # Make df of the predictor
    n1 <- n2 <- (df1+1)/2
  }


  # Treat model based stats as 'regular'
  esType.cl <- gsub("(lm.)","",esType)


  # Settings for model-based OR
  if(grepl("(lm.OR)+",esType)){
    st        <- exp(st)
    esType.cl <- "Asym"
    CIcalc    <- FALSE
  }


  if(is.na(esType.cl)){esType.cl<-esType}

  if(is.na(st)|is.null(st)|is.nan(st)){

    ES <- compute.es::tes(t=2, level= 95, n.1 = 100, n.2 = 100, verbose = FALSE, dig = 5)
    ES[seq_along(ES)] <- NA
    ES <- c(st,st,st,ES)

    CIcalc <- FALSE
  }


  if(CIcalc){

    getCI <- TRUE

    if(esType=="OR"){
      # Fisher exact test gives exact noncentral hypergeometric CI
      sCI <- cbind(ncp    = testInfo$estimate,
                   ncp.lo = testInfo$conf.low,
                   ncp.hi = testInfo$conf.high)
      getCI <- FALSE
    }

    if((grepl("Z.f", esType, fixed = TRUE))&(length(testInfo$estimate)==1)){
      sCI <- cbind(ncp    = testInfo$estimate,
                   ncp.lo = testInfo$conf.low,
                   ncp.hi = testInfo$conf.high)
      esType <- esType.cl <- "r"
      getCI <- FALSE
    }

    if(getCI){
      sCI <- get.ncpCI(st, df1, df2, N, esType.cl, CL, keepSign, alternative)
      if(esType=="f"){sCI[1,is.na(sCI)]<-1}
      if(esType%in%c("t","t.p","t.r","Z")){sCI[1,is.na(sCI)]<-0}
    }
    # no CI
  } else {
    sCI <- cbind(ncp  = st)
  }

    esComp <- list()
    cnt    <- 0

    for(cnt in seq_along(sCI)){

      x <- sCI[cnt]
      if(x==0|is.na(x)|is.nan(x)){
        x <- rnorm(1)*1e-12
        disp("ES converison: A test statistic of 0 (or NA, or NaN) was changed to rnorm(1) * 1e-12 in order to enable ES conversion.", header= FALSE, footer = FALSE)}

      # This effectively ignores model based stats
      esType <- gsub("lm.","",esType,fixed=TRUE)
      #esType <- gsub("Z.f","r",esType,fixed=TRUE)

      ncCI <- list()
      switch(esType,
             t.p  = esComp[[cnt]] <- compute.es::des(d   = x, n.1 = (N/2), n.2 = (N/2), level=CL*100, verbose = FALSE, dig = 5),
             t    = esComp[[cnt]] <- compute.es::tes(t   = x, level=CL*100,
                                                     n.1 = n1, n.2 = n2, verbose = FALSE, dig = 5),
             lm.t = esComp[[cnt]] <- compute.es::a.tes(t=x, level=CL*100,
                                                       n.1 = n1, n.2 = n2, R = rID, q = q,
                                                       verbose = FALSE, dig = 5),
             t.r  = esComp[[cnt]] <- compute.es::res(r = x, level=CL*100, var.r = ((1-x^2)^2)/(N-1),
                                                     n = N, verbose = FALSE, dig = 5),
             r  = esComp[[cnt]] <- compute.es::res(r = x, level=CL*100, var.r = NULL,
                                                   n = N, verbose = FALSE, dig = 5),
             #compute.es::res(r=x, level=CL, n=N, verbose = FALSE, dig = 5),
             f    = esComp[[cnt]] <- compute.es::fes(f=x, level=CL*100,
                                                     n.1 = n1, n.2 = n2, verbose = FALSE, dig = 5),
             lm.f = esComp[[cnt]] <- compute.es::a.fes(f=x, level=CL*100,
                                                       n.1 = n1, n.2 = n2, R = rID, q = q,
                                                       verbose = FALSE, dig = 5),
             X2   = esComp[[cnt]] <- compute.es::chies(chi.sq = x, level = CL*100,
                                                       n = N, verbose = FALSE, dig = 5),
             Z    = esComp[[cnt]] <- compute.es::pes(  p = pnorm(abs(x), lower.tail= FALSE)*2, level = CL*100,
                                                       n.1 = n1, n.2 = n2,
                                                       tail = "one", verbose = FALSE, dig = 5),
             lm.Z  = esComp[[cnt]] <- compute.es::a.pes(p = pnorm(abs(x), lower.tail= FALSE)*2, level = CL*100,
                                                        n.1 = n1, n.2 = n2, R = rID, q = q,
                                                        tail = "one", verbose = FALSE, dig = 5),
             OR    = esComp[[cnt]] <- compute.es::lores(lor=log(x), n.1 = n1, n.2 = n2,
                                                        var.lor = var.lor, verbose = FALSE, dig = 5, level = CL*100)
      )
    }

    # This section re-calculates CI based on the exact CI for the test statistic obtained from MBESS in function get.ncpCI
    if(cnt>1){

      if(esType%in%c("r","t.r")){

        ncp <- compute.es::tes(t=2, level= 95, n.1 = 100, n.2 = 100, verbose = FALSE, dig = 5)
        ncp[seq_along(ncp)] <- NA
        id.l <- c("l.d","l.r", "l.z", "l.or", "l.lor")
        id.u <- c("u.d","u.r", "u.z", "u.or", "u.lor")
        id.e <- c("d", "r", "fisher.z", "OR", "lOR")
        rNames <- names(res(r=1,var.r=.5, n=100, level=95,dig=5,verbose = FALSE))
        ncp[,rNames] <- esComp[[1]][,rNames]
        ncp$N.total <- N
        ncp$n.1 <- n1
        ncp$n.2 <- n2

        if(esType=="t.r"){
          sCI <- get.ncpCI(testInfo$statistic, df1, df2, N, "t", CL, keepSign)
        }
      } else {

        ncp  <- esComp[[1]]
        id.l <- c("l.d", "l.g", "l.r", "l.z", "l.or", "l.lor")
        id.u <- c("u.d", "u.g", "u.r", "u.z", "u.or", "u.lor")
        id.e <- c("d", "g", "r", "fisher.z", "OR", "lOR")
      }

      ncp[,id.l] <- esComp[[2]][,id.e]
      ncp[,id.u] <- esComp[[3]][,id.e]

      ES <- cbind(sCI, ncp[,colnames(ncp)!="NNT"])

    } else {

      if(esType.cl%in%"Asym"){
        sCI <- cbind(ncp    = esComp[[1]]$lOR,
                     ncp.lo = esComp[[1]]$l.lor,
                     ncp.hi = esComp[[1]]$u.lor)
      }
      ncp  <- esComp[[1]]
      ES   <- cbind(sCI, ncp[,colnames(ncp)!="NNT"])

    }

    colnames(ES)[1:cnt] <- c("ncp","ncp.lo","ncp.hi")[1:cnt]

    # compute.es keeps the sign for d and related ES, but not for r if tes and des are used,.
    # If keepSign = TRUE the sign from d will be copied to r and related es.

    # unique(ML2.key$stat.type)
    #  "t"    "t.r"  "OR"   "lm.t" "Z"    "f"    "lm.Z"
    if(!all((sign(ES$ncp)==sign(ES[ ,c("d","r")])),(sign(ES$ncp.lo)==sign(ES[ ,c("l.d","l.r")])),(sign(ES$ncp.hi)==sign(ES[ ,c("u.d","u.r")])), na.rm = TRUE) & !esType%in%c("OR","t.r","r")){
      if(keepSign){
        if(esType%in%c("X2","f")){
          id.l <- which(colnames(ES) %in% c("l.d", "l.g", "l.r", "l.z"))
          id.u <- which(colnames(ES) %in% c("u.d", "u.g", "u.r", "u.z"))
          id.e <- which(colnames(ES) %in% c("d", "cliffs.d", "g", "r", "fisher.z"))
          col.id <- c(id.e,id.l,id.u)
        }
        if(any(esType%in%c("lm.Z","Z"))){ # esType=="Z"|esType=="lm.Z"
          col.id <-which(colnames(ES)%in%c("d","l.d","u.d",keepSignNames))
        }
        if(esType%in%c("t","lm.t")){
          col.id <-which(colnames(ES)%in%keepSignNames)
        }
        col.id <- sort(col.id)
        ES[ ,col.id] <- ES[ ,col.id] * sign(sCI)[1:cnt]
      }
    }

  return(ES)
}

#' get.ncpCI
#'
#' @param x  A noncentrality parameter.
#' @param df1  Degrees of freeddom.
#' @param df2  NULL or degrees of freedom of the denominator for the f-distribution.
#' @param N  Sample size
#' @param esType     Type of test statistic. One of: "t", "t.r", lm.t", "f", "lm.f", "r", "X2", "Z", "lm.Z"
#' @param CL    Confidence Limit (default: .95).
#' @param keepSign     Return effect size with sign of test statistic? (default = TRUE).
#' @param keepDirection  Use the information in \code{alternative} to decide on one-sided vs. two-sided confidence intervals. Default is \code{TRUE}. If \code{FALSE}, two-sided CIs will be calclulated irrespective of the direction of the \code{alternative}.
#' @param alternative    Alternative hypothesis (defult = "two").
#'
#'

#' @export
#'

get.ncpCI <- function(x, df1, df2, N, esType, CL=.95, keepSign = TRUE, keepDirection = TRUE, alternative = "two.sided"){
  #require(MBESS)
  esType <- gsub("lm.","",esType)
  ncCI   <- list()

  Talpha  <- 1-CL
  if(grepl("two",alternative)|keepDirection==FALSE){
    CLimsZ  <- c(Talpha/2, CL + (Talpha/2))
    CLims   <- c(NULL,NULL)
    Tsides <- 2
  }
  if(!grepl("two",alternative)&keepDirection==TRUE){
    ifelse(alternative == "greater",
           CLims <- c(-Inf,  1-Talpha),
           CLims <- c(Talpha, +Inf))
    CLimsZ <- CLims
    Tsides <- 1
    CL <- NULL
  }



  switch(esType,
         t.p = ncCI <- list(Lower.Limit = MBESS::ci.sm(sm = x, alpha.lower = CLims[1], alpha.upper = CLims[2],
                                                conf.level=CL,N=N)$Lower.Conf.Limit.Standardized.Mean,
                            Upper.Limit = MBESS::ci.sm(sm = x, alpha.lower = CLims[1], alpha.upper = CLims[2],
                                                conf.level=CL,N=N)$Upper.Conf.Limit.Standardized.Mean),
         t   = ncCI <- MBESS::conf.limits.nct(t.value=x, alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                              df=df1),
         f   = ncCI <- MBESS::conf.limits.ncf(F.value=x, alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                              df.1=df1, df.2=df2),
         #t.r = ncCI <- MBESS::conf.limits.nct(t.value=x, conf.level=CL, df=df1),
         t.r = ncCI <- list(Lower.Limit = MBESS::ci.R(R=abs(x), alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                               N=N, K=1)$Lower.Conf.Limit.R,
                            Upper.Limit = MBESS::ci.R(R=abs(x), alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                               N=N, K=1)$Upper.Conf.Limit.R),
         X2  = ncCI <- MBESS::conf.limits.nc.chisq(Chi.Square=x, alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                                   df=df1),
         Z   = ncCI <- list(Lower.Limit = (x + qnorm(CLimsZ[2], lower.tail = FALSE)),
                            Upper.Limit = (x + qnorm(CLimsZ[1], lower.tail = FALSE)))
  )

  #!all(sign(ES$ncp)==sign(ES[ ,c("d","r")]),na.rm = TRUE)&esType!="OR"
  if((sign(x)==-1)&keepSign&grepl("t.r",esType)){
    out <- cbind(ncp    = x,
                 ncp.lo = sign(x) * ncCI$Upper.Limit,
                 ncp.hi = sign(x) * ncCI$Lower.Limit)
  } else {
    out <- cbind(ncp    = x,
                 ncp.lo = ncCI$Lower.Limit,
                 ncp.hi = ncCI$Upper.Limit)
  }
  return(out)
}

#' cor.test.fisherZ
#'
#' @param r1 First correlation
#' @param r2 Second correlation
#' @param n1 First sample size
#' @param n2 Second sample size
#' @param p Compute p-value? (default = TRUE)
#' @param Cohens.q Compute effect size Cohen's q (default = TRUE)
#' @param alpha Alpha evel for significance test
#' @param alternative One of "greater", "less", "two.sided" (default)
#'
#'
#'

#' @export
#'

cor.test.fisherZ <- function(r1 = NULL,
                             r2 = NULL,
                             n1 = NULL,
                             n2 = NULL,
                             p  = TRUE,
                             Cohens.q    = TRUE,
                             conf.level  = .95,
                             alternative = "two.sided",
                             null.value  = 0,
                             cor.type = "pearson"){

  effect.size    <- NULL
  effect.size.ci <- NULL
  interp <- ""

  if(grepl("two.sided",alternative)){
    sides <- 2
  } else {
    sides <- 1
  }

  alpha <- 1-conf.level

  oneCor = FALSE
  if(all(is.null(r2),is.null(n2))){
    oneCor = TRUE
  } else {
    if(all(is.na(r2),is.na(n2))){
      oneCor = TRUE
    }
  }

  if(oneCor){
    if((dim(as.matrix(r1))[2]==2)){
      r1 <- cor(r1[,1],r1[,2],use="pairwise.complete.obs", method = cor.type)
    } else{
      if(all((dim(as.matrix(r1))!=1))){
        disp(message = "r1 needs to be:", header = "cor.test.fisherZ", footer = FALSE)
        disp(message = "- Either a single numerical value representing a correlation,",
             header = FALSE, footer = FALSE)
        disp(message = "- Or a 2 column matrix from which a correlation r1 can be calculated",
             header = FALSE)
        stop
      }
    }

    z <- atanh(r1)/sqrt(1/(n1-3))
    conf.low   <- tanh(atanh(r1) - (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3)))))
    conf.high  <- tanh(atanh(r1) + (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3)))))
    if(p){p<-2*(1-pnorm(abs(z)))} else {p=NULL}
    if(Cohens.q){
      effect.size <- (atanh(r1)- 0)
      names(effect.size) <- "Cohen's q (Zr1-Zρ0)"
      conf.low.q   <- effect.size - (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))))
      conf.high.q  <- effect.size + (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))))
      effect.size.ci <- c(conf.low.q, conf.high.q)
      names(effect.size.ci) <- paste(conf.level*100,"percent effect-size confidence interval:")
      #<.1: no effect; .1 to .3: small effect; .3 to .5: intermediate effect; >.5: large effect.
      interp <- ifelse(abs(effect.size)<.1,"No effect",
                       ifelse(between(abs(effect.size),.1,.3),"Small effect",
                              ifelse(between(abs(effect.size),.3,.5),"Intermediate effect","Large effect"
                              )
                       )
      )
      names(interp) <- "Interpretation of magnitude:"
    }

    names(z)         <- "Fisher.z"
    estimate         <- r1
    names(estimate)  <- c("cor")
    parameter        <- n1
    names(parameter) <- "n1"

    method    <- "Fisher r-to-Z transformed test for difference between 1 observed correlation and a hypothesized value."
    data.name <- paste("r1 (",cor.type,")")

  } else {
    if((dim(as.matrix(r1))[2]==2)&(dim(as.matrix(r2))[2]==2)){
      r1 <- cor(r1[,1],r1[,2], use = "pairwise.complete.obs", method = cor.type)
      r2 <- cor(r2[,1],r2[,2], use = "pairwise.complete.obs", method = cor.type)
    } else{
      if(all((dim(as.matrix(r1))!=1))&all(dim(as.matrix(r2))!=1)){
        disp(message = "r1 and r2 each need to be:", header = "cor.test.fisherZ", footer = FALSE)
        disp(message = "- Either a single numerical value representing a correlation,", header = FALSE, footer = FALSE)
        disp(message = "- Or a 2 column matrix from which a correlation r1 and r2 can be calculated", header = FALSE)
        stop
      }
    }

    z  <- ((atanh(r1)-atanh(r2))/sqrt((1/(n1-3))+(1/(n2-3))))
    fm <- qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))+(1/(n2-3)))
    # conf.low.r1   <- tanh(atanh(r1)-fm)
    # conf.high.r1  <- tanh(atanh(r1)+fm)
    # conf.low.r2   <- tanh(atanh(r2)-fm)
    # conf.high.r2  <- tanh(atanh(r2)+fm)

    conf.low   <- paste0("cor1 [",
                         round(tanh(atanh(r1)-fm), digits = 3),",",
                         round(tanh(atanh(r1)+fm), digits = 3),"]")
    conf.high  <- paste0("cor2 [",
                         round(tanh(atanh(r2)-fm), digits = 3),",",
                         round(tanh(atanh(r2)+fm), digits = 3),"]")

    if(p){p<-2*(1-pnorm(abs(z)))} else {p=NULL}
    if(Cohens.q){
      effect.size <- (atanh(r1) - atanh(r2))
      names(effect.size) <- "Cohen's q (Zr1-Zr2-Zρ0)"
      conf.low.q   <- effect.size - (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))+(1/(n2-3))))
      conf.high.q  <- effect.size + (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))+(1/(n2-3))))
      effect.size.ci <- c(conf.low.q, conf.high.q)
      names(effect.size.ci) <- paste(conf.level*100,"percent effect-size confidence interval:")
      #<.1: no effect; .1 to .3: small effect; .3 to .5: intermediate effect; >.5: large effect.
      interp <- ifelse(abs(effect.size)<.1,"No effect",
                       ifelse(between(abs(effect.size),.1,.3),"Small effect",
                              ifelse(between(abs(effect.size),.3,.5),"Intermediate effect","Large effect"
                              )
                       )
      )
      names(interp) <- "Interpretation of magnitude:"
    }

    names(z) <- "Fisher.z"
    estimate <- c(r1, r2)
    names(estimate)<- c("cor1","cor2")
    parameter <- c(n1,n2)
    names(parameter) <- c("n1", "n2")
    method <- "Fisher r-to-Z transformed test for difference between 2 independent correlations"
    data.name <- paste("r1; r2 (",cor.type,")")
  }

  conf.int <- c(conf.low, conf.high)
  attr(conf.int,"conf.level") <- conf.level

  names(null.value) <- "correlation"

  stat.test <- structure(list(statistic = z,
                              parameter = parameter,
                              p.value   = p,
                              estimate  = estimate,
                              null.value = null.value,
                              alternative = alternative,
                              method    = method,
                              data.name = data.name,
                              conf.int  = conf.int,
                              effect.size = effect.size,
                              effect.size.ci = effect.size.ci,
                              effect.size.int = interp)
  )
  class(stat.test) <- "htest.fisherz"
  return(stat.test)
}

print.htest.fisherz <- function(obj) {
  class(obj) <- "htest"
  print(obj)
  cat(names(obj$effect.size), "\n")
  cat(obj$effect.size, "\n")
  cat(names(obj$effect.size.int))
  cat(obj$effect.size.int, "\n")
  cat(names(obj$effect.size.ci)[1], "\n")
  cat(obj$effect.size.ci, "\n")
}


# Wilcox functions for bootstrapped CI on difference between correlations
# https://raw.githubusercontent.com/nicebread/WRS/master/pkg/R/Rallfun-v31.R

twopcor<-function(x1,y1,x2,y2,SEED=TRUE){
  #
  #   Compute a .95 confidence interval for
  #   the difference between two Pearson
  #   correlations corresponding to two independent
  #   goups.
  #
  #   This function uses an adjusted percentile bootstrap method that
  #   gives good results when the error term is heteroscedastic.
  #
  #   WARNING: If the number of boostrap samples is altered, it is
  #   unknown how to adjust the confidence interval when n1+n2 < 250.
  #
  nboot<-599  #Number of bootstrap samples
  if(SEED)set.seed(2) # set seed of random number generator so that
  #             results can be duplicated.
  X<-elimna(cbind(x1,y1))
  x1<-X[,1]
  y1<-X[,2]
  X<-elimna(cbind(x2,y2))
  x2<-X[,1]
  y2<-X[,2]
  print("Taking bootstrap samples; please wait")
  data1<-matrix(sample(length(y1),size=length(y1)*nboot,replace=TRUE),nrow=nboot)
  bvec1<-apply(data1,1,pcorbsub,x1,y1) # A 1 by nboot matrix.
  data2<-matrix(sample(length(y2),size=length(y2)*nboot,replace=TRUE),nrow=nboot)
  bvec2<-apply(data2,1,pcorbsub,x2,y2) # A 1 by nboot matrix.
  bvec<-bvec1-bvec2
  ilow<-15
  ihi<-584
  if(length(y1)+length(y2) < 250){
    ilow<-14
    ihi<-585
  }
  if(length(y1)+length(y2) < 180){
    ilow<-11
    ihi<-588
  }
  if(length(y1)+length(y2) < 80){
    ilow<-8
    ihi<-592
  }
  if(length(y1)+length(y2) < 40){
    ilow<-7
    ihi<-593
  }
  bsort<-sort(bvec)
  r1<-cor(x1,y1)
  r2<-cor(x2,y2)
  ci<-c(bsort[ilow],bsort[ihi])
  list(r1=r1,r2=r2,ci=ci)
}


elimna<-function(m){
  #
  # remove any rows of data having missing values
  #
  if(is.list(m)){
    for(j in 1:length(m))m[[j]]=na.omit(m[[j]])
    elimna=m
  }
  if(!is.list(m)){
    if(is.null(dim(m)))m<-as.matrix(m)
    ikeep<-c(1:nrow(m))
    for(i in 1:nrow(m))if(sum(is.na(m[i,])>=1))ikeep[i]<-0
    elimna<-m[ikeep[ikeep>=1],]
  }
  elimna
}


pcorbsub<-function(isub, x, y)
{
  #
  #  Compute Pearson's correlation using x[isub] and y[isub]
  #  isub is a vector of length n,
  #  a bootstrap sample from the sequence of integers
  #  1, 2, 3, ..., n
  #
  pcorbsub<-cor(x[isub],y[isub])
  pcorbsub
}





z.test <- function(x = 0, mu = 0, pi = NULL, N = 0, sigma = 1, proportion=FALSE, alternative = "two.sided"){
  require(dplyr)
  if(proportion){
    if(!is.null(pi)){
      if(all(dplyr::between(x,0,1),dplyr::between(pi,0,1))){
        SE <- sqrt((pi * (1-pi))/N)
        z <- (x-pi)/SE
      } else {
        stop("Argument x and/or pi not in [0,1]")
      }
    } else {
      stop("Argument pi (expected population proportion) is NULL.")
    }
  } else {
    SE <- sigma / sqrt(N)
    z <- (x-mu)/SE
  }

  if(alternative=="two.sided"){
    p <- 1-pnorm(abs(z))
  } else {
    p <- 2*(1-pnorm(abs(z)))
  }
  names(z) <- "Z"

  if(proportion){
    null.value <- pi
    names(null.value) <- "π"
  } else {
    names(null.value) <- "mu"
    null.value <- mu
  }

  if(proportion){
    estimate <- x-pi
    names(estimate) <- "(p-π)"
  } else {
    estimate <- x-mu
    names(estimate) <- "(x-mu)"
  }

  statistic <- z
  ifelse(proportion,
         names(statistic) <- "(p-π)/sqrt(π*(1-π)/N)",
         names(statistic) <- "(x-mu)/(sigma/sqrt(N))"
  )
  parameter <- N
  names(parameter) <- "N"
  stat.test <- structure(list(statistic = z,
                              null.value = null.value,
                              estimate = estimate,
                              statistic = statistic,
                              method="Z test",
                              parameter=parameter,
                              p.value=p)
  )
  class(stat.test) <- "htest"
  return(stat.test)
}

#
# #' get.SwarmPlot
# #'
# #' @param df  A data frame.
# #'
#
# #' @export
# #'
#
# get.SwarmPlot <- function(df){
#
# df<-outlist1
#     sourceInfo <- get.GoogleSheet(url="https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/export?format=csv")$df
#
#
#     nameChange <- list(
#         Huang.1 = "Direction & SES (Huang et al., 2014)",
#         Kay.1 = "Structure & Goal Pursuit (Kay et al., 2014)",
#         Alter.1 = "Incidental Disfluency (Alter et al., 2007)",
#         Graham.1 = "Moral Foundations (Graham et al., 2009)",
#         Rottenstreich.1 = "Affect & Risk (Rottenstreich & Hsee, 2001)",
#         Bauer.1 = "Priming Consumerism (Bauer et al., 2012)",
#         Miyamoto.1 = "Correspondence Bias (Miyamoto & Kitayama, 2002)",
#         Inbar.1 = "Disgust & Homophobia (Inbar et al., 2009)",
#         Critcher.1 = "Incidental Anchors (Critcher & Gilovich, 2008)",
#         vanLange.1 = "Social Value Orientation (Van Lange et al., 1997)",
#         Hauser.1 = "Trolley Dilemma 1 (Hauser et al., 2007)",
#         Anderson.1 = "SMS & Well-Being (Anderson et al., 2012)",
#         Ross.1 = "False Consensus 1 (Ross et al., 1977)",
#         Ross.2 = "False Consensus 2 (Ross et al., 1977)",
#         Giessner.1 = "Position & Power (Giessner & Schubert, 2007)",
#         Tversky.1 = "Framing (Tversky & Kahneman, 1981)",
#         Hauser.2 = "Trolley Dilemma 2 (Hauser et al., 2007)",
#         Risen.1 = "Tempting Fate (Risen & Gilovich, 2008)",
#         Savani.1 = "Actions are Choices (Savani et al., 2010)",
#         Norenzayan.1 = "Intuitive Reasoning (Norenzayan et al., 2002)",
#         Hsee.1 = "Less is Better (Hsee, 1998)",
#         Gray.1 = "Moral Typecasting (Gray & Wegner, 2009)",
#         Zhong.1 = "Moral Cleansing (Zhong & Liljenquist, 2006)",
#         Schwarz.1 = "Assimilation & Contrast (Schwarz et al., 1991)",
#         Shafir.1 = "Choosing or Rejecting (Shafir, 1993)",
#         Zaval.3 = "Priming Warmth (Zaval et al., 2014)",
#         Knobe.1 = "Intentional Side-Effects (Knobe, 2003)",
#         Gati.1a = "Direction & Similarity (Tversky & Gati, 1978)",
#         Risen.3 = "Label for Risen.3"
#     )
#
#     df$slabel <- "No label"
#     l_ply(seq_along(nameChange), function(l) df$slabel[as.character(df$.id)==names(nameChange)[[l]]] <<- nameChange[[l]])
#     df$slabel <- factor(df$slabel)
#
#     df$Country <- "No Country"
#     l_ply(seq_along(df$.id), function(l) df$Country[l] <<- sourceInfo$Country[sourceInfo$Source.Global==df$study.source[l]])
#
#     df$USA                    <- "Non-USA"
#     df$USA[df$Country=="USA"] <- "USA"
#
#     df$.id    <- factor(df$.id)
#     btype     <- "swarm"
#     pdf(tempfile())
#     bs    <- beeswarm(ESCI.r ~ slabel, data = df,
#                       horizontal = FALSE, corral = "none",
#                       corralWidth = 5,
#                       pch = 21, spacing = 2, cex = .5,
#                       priority = "ascending", method = btype, do.plot = TRUE)[, c(1, 2, 4, 6)]
#     dev.off()
#
#     colnames(bs)[4] <- "labels"
#     #laply(unique(df$analysis), function(r) strsplit(x = r, split = "[.]")[[1]][1])
#
#     df <- data.frame(df,bs)
#     se <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}
#
#     df$meanES <- ldply(unique(df$labels),
#                        function(r) cbind(rep(mean(df$y[df$labels==r], na.rm = TRUE),
#                                              sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]
#     df$seES <- ldply(unique(df$labels),
#                      function(r) cbind(rep(se(df$y[df$labels==r]),
#                                            sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]
#
#     #df    <- df[order(df$meanES, decreasing = F), ]
#     df <- arrange(df, meanES)
#
#     df$xx <- ldply(unique(df$labels),
#                    function(r) cbind(scale(df$x[df$labels==r], scale = F)))[,1]
#     df$xf <- ldply(seq_along(unique(df$labels)),
#                    function(r) cbind(df$xx[df$labels==unique(df$labels)[r]] +
#                                          seq(10,10*length(unique(df$labels)), by=10)[r]))[,1]
#     df$xn <- ldply(seq_along(unique(df$labels)),
#                    function(r) cbind(rep(seq(10,10*length(unique(df$labels)),by=10)[r],
#                                          sum(df$labels==unique(df$labels)[r]))))[,1]
#
#     keylabs <- c("Mean of Sample Effect Sizes","Effect Size of Global Mean")
#
#     mxN       <- max(df$ESCI.N.total)
#     mypalette <- c("red3","steelblue")
#
#     df$sigf <- NA
#     df$sigf[df$test.p.value> .05] <- "Not Significant"
#     df$sigf[df$test.p.value<=.05] <- "Significant"
#     df$sigf <- factor(df$sigf)
#
#     df$USA <- factor(df$USA)
#
#     df <- df[df$ESCI.N.total>=30,]
#
#     dfG <- summarise(group_by(df,.id),
#                      y= median(ESCI.r,na.rm = T),
#                      ymin=median(ESCI.l.r, na.rm = T),
#                      ymax=median(ESCI.u.r, na.rm = T))
#
#      dfG  <- arrange(dfG, y)
#
#      dfG$x <- seq(10,10*nrow(dfG),by=10)
#
#
#     #gnsize <-
#   g <- ggplot(df, aes(x=xf, y=y)) +
#         geom_vline(xintercept = unique(df$xn), colour = "grey80",alpha = 1) +
#         geom_hline(yintercept = 0, colour = "ivory4") +
#       # geom_errorbar(data=dfG,aes(x=x,y=y,ymin=ymin,ymax=ymax),
#       #               color="black",alpha=1,size=1.2) +
#         geom_point(aes(fill = USA), size = 2, #ESCI.N.total/mxN),
#                    col="snow2", pch=21) +
#        geom_point(data=dfG,aes(x=x,y=y),
#                     color="black",fill="grey80",alpha=1,size=3,pch=23) +
#       scale_y_continuous("Effect Size r", limits = c(-1,1)) +
#         scale_x_continuous("", breaks = unique(df$xn),
#                                labels = unique(paste(1:nrow(dfG))),
#                                expand = c(0, 10)) +
#         scale_fill_manual("",values = mypalette,
#                           guide   = guide_legend(override.aes = list(size = 4),
#                                                  byrow = TRUE)
#                           ) +
#         # scale_size_continuous("Sample Size", breaks = c(0.01, 0.1, 0.3, 0.5,0.8,1),
#         #                       labels = round(c(0.01, 0.1, 0.3, 0.5,0.8, 1) * mxN),
#         #                       guide = guide_legend(override.aes=list(colour="grey30",fill="grey70"), byrow = TRUE)
#         # ) +
#         scale_shape_manual(labels=keylabs, values=c(24,25),guide=guide_legend("")) +
#         gg.theme() + coord_flip()  +
#         theme(legend.position = "top", legend.background=element_rect())
#
#     return(g)
#
# }


get.SwarmPlot <- function(df, anonymous=FALSE, addSize=FALSE, addMedian=TRUE, addGlobalES = TRUE, addOriES=TRUE, addLabel=FALSE, oriES=NULL, fillvar=c("USA","sigf")[1]){

  #sourceInfo <- get.GoogleSheet(url="https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/export?format=csv")$df

  # if(is.null(oriES)&addOriES){
  #  oriES <- import("/Users/Fred/Dropbox/Manylabs2/TestOutput/ORI.EFFECTS/ML2_ori_effects_MasterKey.xlsx")
  # }

  if(!is.null(oriES)){addOriES=FALSE}
  if(addOriES){
    # Load Key Table
    oriES   <- get.GoogleSheet(data='ML2masteRkey')$df
    ID.ori  <- which(nchar(oriES$orig.ES.r)>0)
    ID.add  <- which(oriES$study.figure2.include==1)
    Analyses.ori <- sort(oriES$study.analysis[ID.ori])
    Analyses.add <- sort(oriES$study.analysis[ID.add])
    # dft<- data.frame(includeFig2=Analyses.add)
    # dft$oriEffect <-NA
    # dft$oriEffect[Analyses.add%in%Analyses.ori] <- Analyses.ori[Analyses.ori%in%Analyses.add]
    # rest<-Analyses.ori[!(Analyses.ori%in%Analyses.add)]
    # dft[(nrow(dft)+1):(nrow(dft)+length(rest)),1:2] <- cbind(rep(NA,length(rest)),rest)
  }


  df$slabel <- df$study.id

  # if(!anonymous){
  #     l_ply(seq_along(oriES$study.analysis), function(l) df$slabel[tolower(as.character(df$.id))==oriES$study.analysis[l]] <<- oriES$description[l])
  #     df$slabel <- factor(df$slabel)
  # } else {
  #     l_ply(seq_along(oriES$study.analysis), function(l) df$slabel[tolower(as.character(df$.id))==oriES$study.analysis[l]] <<- l)
  #     df$slabel <- factor(df$slabel)
  # }

  df$.id    <- factor(df$.id)
  btype     <- "swarm"
  pdf(tempfile())
  bs    <- beeswarm(ESCI.r ~ slabel, data = df,
                    horizontal = FALSE, corral = "none",
                    corralWidth = 5,
                    pch = 21, spacing = 2, cex = .5,
                    priority = "ascending", method = btype, do.plot = TRUE)[, c(1, 2, 4, 6)]
  dev.off()

  colnames(bs)[4] <- "labels"

  df <- data.frame(df,bs)
  se <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}

  df$meanES <- ldply(unique(df$labels),
                     function(r) cbind(rep(mean(df$y[df$labels==r], na.rm = TRUE),
                                           sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]
  df$seES <- ldply(unique(df$labels),
                   function(r) cbind(rep(se(df$y[df$labels==r]),
                                         sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]

  df <- arrange(df, meanES)

  df$xx <- ldply(unique(df$labels),
                 function(r) cbind(scale(df$x[df$labels==r], scale = F)))[,1]
  df$xf <- ldply(seq_along(unique(df$labels)),
                 function(r) cbind(df$xx[df$labels==unique(df$labels)[r]] +
                                     seq(10,10*length(unique(df$labels)), by=10)[r]))[,1]
  df$xn <- ldply(seq_along(unique(df$labels)),
                 function(r) cbind(rep(seq(10,10*length(unique(df$labels)),by=10)[r],
                                       sum(df$labels==unique(df$labels)[r]))))[,1]

  keylabs <- c("Mean of sample ES","ES of grand mean","Original ES")[c(addMedian,addGlobalES,addOriES)]
  mrkrs  <- c(22,21,23)[c(addMedian,addGlobalES,addOriES)]

  mxN       <- max(df$ESCI.N.total)

  #Colorblindsafe colors
  cwhite = "#f7f7f7"
  ccream = "#2166ac"
  cblank = "#d1e5f0"
  corange = "#f4a582"
  cblue  = "#2166ac"
  cred   = "#b2182b"
  cpurp  = "#b2abd2"

  mypalette <- c(cred,cblue)

  df$sigf <- NA
  df$sigf[df$test.p.value> .05] <- "Not Significant"
  df$sigf[df$test.p.value<=.05] <- "Significant"
  df$sigf <- factor(df$sigf)

  # df$Country <- "No Country"
  # l_ply(seq_along(df$.id), function(l) df$Country[l] <<- sourceInfo$Country[sourceInfo$Source.Global==df$study.source[l]])

  df$USA                    <- "Non-USA"
  df$USA[df$source.Country=="USA"] <- "USA"

  df$USA <- factor(df$USA)

  df <- df[df$ESCI.N.total>=30,]

  dfG <- summarise(group_by(df,.id),
                   y= mean(ESCI.r,na.rm = T),
                   ymin=mean(ESCI.l.r, na.rm = T),
                   ymax=mean(ESCI.u.r, na.rm = T))

  dfG   <- arrange(dfG, y)
  dfG$x <- seq(10,10*nrow(dfG),by=10)

  dfGlobal <- summarise(group_by(df,.id),
                        y= max(GlobalES,na.rm = TRUE),
                        ymin=max(GlobalESlo, na.rm = TRUE),
                        ymax=max(GlobalEShi, na.rm = TRUE))
  dfGlobal   <- arrange(dfGlobal, y)
  dfGlobal$x <- seq(10,10*nrow(dfGlobal),by=10)

  if(addOriES){
    oriES      <- oriES[ID.ori,]
    oriES$mES <- laply(na.exclude(oriES$study.analysis), function(s) dfG$y[tolower(dfG$.id)%in%tolower(s)])
    oriES     <- dplyr::arrange(oriES, mES)
    oriES$x   <- seq(10,10*nrow(oriES),by=10)
  }
 df$fillvar<- df[,fillvar]

  g <-ggplot(df, aes(x=xf, y=y)) +
    geom_vline(xintercept = unique(df$xn), colour = "grey80",alpha = 1) +
    geom_hline(yintercept = 0, colour = "ivory4")

  if(addSize){
    g <-  g +
      geom_point(aes(fill = fillvar, size = ESCI.N), col=cwhite, pch=21)
    # scale_size_continuous("Sample Size", breaks = c(0.01, 0.1, 0.3, 0.5,0.8,1),
    #                       labels = round(c(0.01, 0.1, 0.3, 0.5,0.8, 1) * mxN),
    #                       guide = guide_legend(override.aes=list(colour="grey30",fill="grey70"), byrow = TRUE)
    # )
  } else {
    g <-  g +
      geom_point(aes(fill = fillvar), size = 2, col=cwhite, pch=21)
  }

  if(addMedian){
    g <- g + geom_point(data=dfG,aes(x=x,y=y),
                        color="black",fill=cpurp,alpha=1,size=3,pch=22)
  }

  if(addGlobalES){
    g <- g + geom_point(data=dfGlobal,aes(x=x,y=y),
                        color="black",fill=cblank,alpha=1,size=3,pch=21)
  }

  if(addOriES){
    g <- g +
      geom_point(data=oriES,aes(x=x,y=ESCI.r),
                 color="black",fill=corange,alpha=1,size=3,pch=23)
  }

  if(addLabel){
    g <- g +
      geom_text(aes(label=study.source,hjust=0,color=fillvar),
                size= 1,
                angle = 45,
                position=position_dodge(.9))
  }

  g <- g +
    scale_y_continuous("Effect Size r", limits = c(-1,1)) +
    scale_x_continuous("", breaks = unique(df$xn),
                       labels = unique(paste(df$labels)),
                       expand = c(0, 10)) +
    scale_fill_manual("Sample",values = mypalette,  guide   = guide_legend(override.aes = list(size = 4), byrow = TRUE)) +
    gg.theme() + coord_flip()  +
    theme(legend.position = "top", legend.background=element_rect())

  return(g)

}

get.oriESCI <- function(CL=.95){

  shafir.ori   <-  as.table(matrix(c(31,54,38,47),nrow = 2, ncol = 2, dimnames = list(c("parent A", "parent B"), c("Award","Deny"))))

  tverski.ori  <- as.table(matrix(c(69,24,25,63),nrow = 2, ncol = 2, dimnames = list(c("Go","Don't go"),c("Cheap", "Costly"))))

  # savani.ori   <- as.table(matrix(c(45,60,45,68),nrow = 2, ncol = 2, dimnames = list(c("Americans", "Indians"), c("Personal","Interpersonal"))))

  savani.ori   <- as.table(matrix(c(60,45,68,45),nrow = 2, ncol = 2, dimnames = list(c("Indians", "Americans"), c("Personal","Interpersonal"))))


  # Load Key Table
  ML2.ori <- get.GoogleSheet(data='ML2masteRkey')$df
  ID.ori  <- which(nchar(ML2.ori$orig.stat.type)>0)
  out     <- list()

  cnt <- 0

  #skip <- c(4,16)
  skip = NULL
  for(s in ID.ori){

    test <- data.frame(statistic = NA,
                       estimate  = NA,
                       estimate1 = NA,
                       estimate2 = NA, alternative=NA,
                       parameter=NA, parameter1=NA, parameter2=NA,
                       conf.low=NA, conf.high=NA)

    cnt<-cnt+1

    study.id         <- ML2.ori$study.id[[s]]
    study.slate      <- ML2.ori$study.slate[[s]]
    study.name       <- ML2.ori$study.name[[s]]
    study.analysis   <-  ML2.ori$study.analysis[[s]]
    test$statistic   <- ML2.ori$orig.stat.ncp[[s]]

    cat(paste(study.analysis,"\n"))
    # for OR and t.r and Z.f
    test$estimate    <- ML2.ori$orig.stat.estimate[[s]]
    test$estimate1   <- ML2.ori$orig.stat.estimate1[[s]]
    test$estimate2   <- ML2.ori$orig.stat.estimate2[[s]]
    test$alternative <- ML2.ori$orig.test.alternative[[s]]
    df1 <- ML2.ori$orig.stat.df1[[s]]
    df2 <- ML2.ori$orig.stat.df1[[s]]
    N   <- ML2.ori$orig.stat.N[[s]]
    n1  <- ML2.ori$orig.stat.n1[[s]]
    n2  <- ML2.ori$orig.stat.n2[[s]]
    esType <- ML2.ori$orig.stat.type[[s]]
    var.lor <- NA

    if(study.name=="Tversky"){
      var.lor    <- sum(1/(tverski.ori))
      stat.test  <- fisher.exact(tverski.ori)
      test <- tidy(stat.test)
      test$parameter <- NA
    }

    if(study.analysis=="Savani.1a"){
      var.lor    <- sum(1/(savani.ori))
      stat.test  <- fisher.exact(savani.ori)
      test <- tidy(stat.test)
      colnames(test)[1] <- "statistic"
      test$parameter <- NA
    }

    # if(study.name=="Shafir"){
    #   var.lor    <- sum(1/(shafir.ori))
    #   stat.test  <- fisher.exact(shafir.ori)
    #   test <- tidy(stat.test)
    #   test$parameter <- NA
    # }

    if(esType=="Z.f"){
      stat.test <- cor.test.fisherZ(r1=ML2.ori$orig.stat.estimate1[[s]],
                                    r2= ML2.ori$orig.stat.estimate2[[s]],
                                    n1=n1,
                                    n2=n2)
      tmp <- unclass(stat.test)
      stat.test <- tmp[1:8]
      test$estimate <- test$statistic <- tanh(tmp$effect.size[[1]])
      test$conf.low <-  tanh(tmp$effect.size.ci[[1]])
      test$conf.high <- tanh(tmp$effect.size.ci[[2]])
      class(stat.test) <- "htest"
    }

    if(esType=="t.r"){
      stat.test <- cor.test.fisherZ(r1 = test$estimate,
                                    r2 = NULL,
                                    n1=n1,
                                    n2=NULL)
      tmp <- unclass(stat.test)
      stat.test <- tmp[1:8]
      test$estimate <- tanh(tmp$effect.size[[1]])
      test$statistic <- test$estimate * sqrt((N-2)/(1-test$estimate^2))
      df1 <- N-2
      # test$conf.low <-  tanh(tmp$effect.size.ci[[1]])
      # test$conf.high <- tanh(tmp$effect.size.ci[[2]])
      class(stat.test) <- "htest"

    }

    if(esType=="f"){
      test$parameter1 <- df1
      test$parameter2 <- df2
    } else {
      test$parameter1 <- df1
    }



    if(!(cnt%in%skip)){

      #if(cnt==27){var.lor <- sum(1/shafir.ori)}

      if(!is.na(test[1,1])){

        ESCI <- try.CATCH(any2any(testInfo  = test,
                                  df1       = df1,
                                  df2       = df2,
                                  N         = N,
                                  n1        = n1,
                                  n2        = n1,
                                  esType    = esType,
                                  var.lor   = var.lor,
                                  CL        = CL,
                                  keepSign    = TRUE,
                                  alternative = ifelse(is.null(test$alternative),
                                                       ML2.ori$stat.params[[4]],
                                                       as.character(test$alternative)))
                          )
      }

    } else {
      ESCI <- NA
    }

    if(is.null(ESCI$warning)|all(grepl("simpleWarning",ESCI$warning),
           !grepl("Error", ESCI$value[[1]]),
           !grepl("message", names(unlist(ESCI))[1]),
           !is.na(test[1,1]))){
      ESCI  <- ESCI$value
      es.id <- which(colnames(ESCI)%in%"(ESCI.r|r)+")
    } else {
      ESCI  <- test
      es.id <- 1
    }

    ML2.ori$orig.stat.ncp[s]     <- ESCI$ncp
    ML2.ori$orig.stat.ncp.ciL[s] <- ESCI$ncp.lo
    ML2.ori$orig.stat.ncp.ciU[s] <- ESCI$ncp.hi
    #ML2.ori$orig.stat.p.value
    ML2.ori$orig.ES.d[s]         <- ESCI$d
    ML2.ori$orig.ES.d.ciL[s]     <- ESCI$l.d
    ML2.ori$orig.ES.d.ciU[s]     <- ESCI$u.d
    ML2.ori$orig.ES.r[s]         <- ESCI$r
    ML2.ori$orig.ES.r.ciL[s]     <- ESCI$l.r
    ML2.ori$orig.ES.r.ciU[s]     <- ESCI$u.r

    out[[cnt]] <- data.frame(
      study.id      = study.id,
      study.slate   = study.slate,
      study.name    = study.name,
      study.analysis=study.analysis,
      testInfo  = test[1:2],
      df1 = df1,
      df2 = df2,
      N   = N,
      n1  = n1,
      n2  = n2,
      esType = esType,
      test$alternative,
      ESCI = ESCI)
  }

  df<-ldply(out)

  return(list(oriFULL   = df,
              masterKey = ML2.ori)
  )
}




splitViolin <- function(...){

  dir.in   <- "~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/"
  outlist1 <- rio::import(paste0(dir.in,"Data_Figures_Primary.rds"))
  outlist2 <- rio::import(paste0(dir.in,"Data_Figures_Secondary.rds"))
  outlistG <- rio::import(paste0(dir.in,"Data_Figures_Global.rds"))

  oriEffects <- get.GoogleSheet(data='ML2masteRkey')$df
  oriEffects$source.WEIRD <- NA

  WEIRDori <-oriEffects$orig.sample.nation%in%c("Australia","Austria","Canada","France","Germany","Hungary","Italy","New Zealand","Poland","Portugal","Serbia","Spain","Sweden", "The Netherlands","Netherlands","UK","USA","US","US & Canada","Various (mainly US, GB and Canada).")

  MIX <-oriEffects$orig.sample.nation%in%c("US & India","US & Japan","MTurk", "US & Hong Kong")

  nonWEIRDori <- !WEIRDori&!MIX

  oriEffects$source.WEIRD[MIX]          <- 3
  oriEffects$source.WEIRD[nonWEIRDori]  <- 2
  oriEffects$source.WEIRD[WEIRDori]     <- 1

  # df$source.WEIRD.f <- factor(df$source.WEIRD,
  #                             levels = c(0,1,2),
  #                             labels = c("USA","WEIRD","non-WEIRD"))

  oriEffects$source.WEIRD.f <- factor(oriEffects$source.WEIRD,
                                      levels = c(1,2,3),
                                      labels = c("WEIRD","non-WEIRD","Mixed"))

  df<-outlist1

  df$labels <- df$.id
  df$meanES <- ldply(unique(df$.id), function(r) rep(mean(df$ESCI.r[df$.id==r], na.rm = TRUE)))
  df        <- dplyr::arrange(df, meanES)

  dfG <- summarise(group_by(df,.id),
                   y= median(ESCI.r,na.rm = T),
                   ymin=mean(ESCI.l.r, na.rm = T),
                   ymax=mean(ESCI.u.r, na.rm = T))

  dfG   <- arrange(dfG, y)

  dfAg <- aggregate(ESCI.r ~ .id, df, mean)

  df$labels   <- factor(df$.id, levels = dfAg[order(dfAg$ESCI.r), ".id"])
  df$sigf <- "p > .05"
  df$sigf[df$test.p.value<.05] <- "p < .05"
  df$sigf.f <- factor(df$sigf)

  df$sigf.f <- relevel(df$sigf.f, ref = "p > .05")
  df$USA <- "non-USA"
  df$USA[df$source.Country=="USA"] <- "USA"

  myCols <- brewer.pal(11,"RdYlBu")[c(1,6,11)]
  myCols <- c("#d73027","#4575b4","#5aae61")

  WEIRD <- df$source.Country%in%c("Australia","Austria","Canada","France","Germany","Hungary","Italy","New Zealand","Poland","Portugal","Serbia","Spain","Sweden", "The Netherlands","UK","USA")

  nonWEIRD <- !WEIRD

  df$source.WEIRD                           <- 2
  df$source.WEIRD[WEIRD]                    <- 1
  #df$source.WEIRD[df$source.Country=="USA"] <- 0

  # df$source.WEIRD.f <- factor(df$source.WEIRD,
  #                             levels = c(0,1,2),
  #                             labels = c("USA","WEIRD","non-WEIRD"))

  df$source.WEIRD.f <- factor(df$source.WEIRD,
                              levels = c(1,2),
                              labels = c("WEIRD","non-WEIRD"))

  df$meanES <- ldply(unique(df$labels),
                     function(r) cbind(rep(mean(df$ESCI.r[df$labels==r], na.rm = TRUE),
                                           sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]

  df <- arrange(df, meanES)

  df$splitv <- df$source.WEIRD.f

  #Get the group levels for the split variable

  splits = unique(df[['splitv']])

  # Calculate and scale group densities
  #
  # I'm rescaling the density curves so that they all have the same peak height and don't overlap. Edit the mutate() line if you want to have the density polygons be different sizes (e.g., scaled so that they show the relative amount of data in each group).


  tblF <- summarise(group_by(df, labels, splitv),
                    N = n())
  nnames <- tblF$labels[tblF$N==1]
  ssplitv  <- tblF$splitv[tblF$N==1]

  for(c in seq_along(nnames)){
    df[nrow(df)+1,] <- df[df$labels%in%nnames[c]&df$splitv%in%ssplitv[c],]
  }

  pdat = df %>%
    group_by(labels, splitv) %>%
    do(tidy(density(.[['ESCI.r']]))) %>%
    rename(loc = x, dens = y) %>%
    mutate(dens = 0.5 * dens / max(dens)) %>%
    ungroup()


  # Calculate summary statistics in a separate dataframe
  #
  # If you need more summary statistics, add new variables to the summarise() call here, and add the additional variable names to the gather() call.
  #
  sums = df %>%
    group_by(labels, splitv, source.name) %>%
    summarise(sample_loc = first(ESCI.r)) %>%
    ungroup() %>%
    gather(segment, loc_sum, sample_loc)

  means = df %>%
    group_by(labels, splitv) %>%
    summarise(mean_loc = median(ESCI.r, na.rm = T)) %>%
    ungroup() %>%
    gather(segment, loc_sum, mean_loc)

  # Calculate the corresponding points on each group's density curve
  #
  # To do this, I'm taking the scaled density curves stored in pdat, then feeding them into the approx() function, plus the y-axis locations of the summary statistics (loc_sum), to get the corresponding x-axis values for the summary statistics.


  sums = left_join(pdat, sums, by=c('labels', 'splitv')) %>%
    group_by(labels, splitv, source.name) %>%
    do(data.frame(loc     = unique(.$loc_sum),
                  dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']])) %>%
    ungroup()

  means = left_join(pdat, means, by=c('labels', 'splitv')) %>%
    group_by(labels, splitv) %>%
    do(data.frame(loc     = unique(.$loc_sum),
                  dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']])) %>%
    ungroup()

  pdat <- arrange(pdat, sort)

  #Create a vector of offsets with each x-axis group's coordinate

  offsets = unique(df[['labels']]) %>% {setNames(0:(length(.) - 1), .)}

  # Offset the densities and summary statistics
  #
  # Modify pdat and sums to add offset_dens columns, which are the density curve values offset by the x-axis group. Also, for the groups that are on the left side of the split violins, invert the densities.

  pdat = pdat %>%
    mutate(offset_dens = offsets[.[['labels']]] + ifelse(.[['splitv']] == splits[1], -dens, dens))

  means = means %>%
    mutate(offset = offsets[.[['labels']]],
           offset_dens = offset + ifelse(.[['splitv']] == splits[1], -dens, dens))



  means$oriES <- NA
  means$oriWEIRD <- NA
  means$oriWEIRD.f <- NA
  for(n in seq_along(means$labels)){
    means$oriES[n]  <-  oriEffects$orig.ES.r[na.exclude(oriEffects$study.analysis)%in%means$labels[n]] %||% NA
    if(means$splitv[n]%in%"WEIRD"&oriEffects$source.WEIRD[na.exclude(oriEffects$study.analysis)%in%means$labels[n]]==1){
      means$oriWEIRD[n]  <- "WEIRD"
    }
    if(means$splitv[n]%in%"non-WEIRD"&oriEffects$source.WEIRD[na.exclude(oriEffects$study.analysis)%in%means$labels[n]]==2){
      means$oriWEIRD[n]  <- "non-WEIRD"
    }
  }


  sums = sums %>%
    mutate(offset = offsets[.[['labels']]],
           offset_dens = offset + ifelse(.[['splitv']] == splits[1], -dens, dens),
           offset_fix  = offset + ifelse(.[['splitv']] == splits[1], -.2, .2))

  sums$offset_densN <- NA
  for(smpl in unique(df$source.name)){
    tblN <- df[df$source.name%in%smpl,c('labels','ESCI.N.total')]
    for(l in unique(tblN$labels)){
      sums$offset_densN[sums$source.name%in%smpl&sums$labels%in%l] <- tblN$ESCI.N.total[tblN$labels==l]
    }
  }

  sums$tsize <- sums$offset + ifelse(sums$splitv == splits[1],
                                     -.2, #(sums$offset_densN/max(sums$offset_densN,na.rm = T)),
                                     .2) #(sums$offset_densN/max(sums$offset_densN,na.rm = T)))
  #sums$tsize[is.infinite(sums$tsize)] <- sums$offset[is.infinite(sums$tsize)]

  lablocs <- (summarise(group_by(sums,labels),
                        labloc = max(offset,na.rm = TRUE)))

  lablocs$oriES  <- NA
  lablocs$globES <- NA
  lablocs$meanES <- NA
  lablocs$oriWEIRD  <- NA

  for(n in seq_along(lablocs$labels)){
    lablocs$oriES[n]  <-  oriEffects$orig.ES.r[na.exclude(oriEffects$study.analysis)%in%lablocs$labels[n]] %||% NA
    lablocs$globES[n] <-  unique(df$GlobalES[df$labels%in%lablocs$labels[n]])
    lablocs$meanES[n] <-  unique(df$meanES[df$labels%in%lablocs$labels[n]])
    lablocs$oriWEIRD[n] <-  as.character(oriEffects$source.WEIRD.f[na.exclude(oriEffects$study.analysis)%in%lablocs$labels[n]]) %||% NA
  }

  lablocs$globES[is.na(lablocs$globES)] <- 0

  # lablocs <- lablocs %>%
  #   mutate(offset = offsets[.[['labels']]],
  #          offset_dens = offset + ifelse(.[['splitv']] == splits[1], -dens, dens),

  #Colorblindsafe colors
  cwhite = "#f7f7f7"
  ccream = "#2166ac"
  cblank = "#d1e5f0"
  corange = "#f4a582"
  cblue  = "#2166ac"
  cblueL = "#d1e5f0"
  cred   = "#d6604d"
  credL  = "#f7f7f7"
  cpurp  = "#b2abd2"

  mypalette <- c(cred,cblue)


  pdat$offset_dens[pdat$labels==unique(df$labels)[n]]

  dfSum <- gather(lablocs, key = EStype, value = ES, oriES, globES, meanES)
  dfSum$EStype.f <- factor(dfSum$EStype, levels = c("oriES","globES","meanES"), labels = c("Original ES","ES of Grand Mean", "Mean ES of Samples"))
  dfSumS <- dfSum[dfSum$EStype=="oriES",]

  outdir <- "/Users/Fred/Dropbox/Manylabs2/Figures"


  wd = 8
  hg = 10

  pdat$splitv <- relevel(pdat$splitv, ref = "non-WEIRD")
  # dfSumS$oriWEIRD <- factor(dfSumS$oriWEIRD, levels = c("non-WEIRD","WEIRD","Mixed"))
  # dfSumS$oriWEIRD <- relevel(dfSumS$oriWEIRD, ref = "non-WEIRD")

  #sums$tsize.s<-rescale(sums$tsize,to=c(-.5,.5)

  oriWEIRD <- means[!is.na(means$oriWEIRD),]

  oriWEIRD$oriWEIRD.f <- "Original"

  cols <- c("non-WEIRD" = cred, "WEIRD" = cblue, "Original"="#5aae61")

  #Plot
  library(scales)
  g <- ggplot(pdat, aes(offset_dens, loc, group = interaction(pdat[['labels']], pdat[['splitv']])), colour = splitv) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_path(size=1, alpha = .5, color = "grey60")+
    geom_path(aes(colour=splitv)) +
    geom_segment(data=sums, aes(x = offset, y = loc, xend = tsize, yend = loc, colour = splitv),
                 inherit.aes=FALSE, alpha=.7, size=.2) +
    geom_segment(data=means, aes(x = offset, y = loc, xend = offset_dens, yend = loc, colour =  splitv),
                 inherit.aes=FALSE, size=.8, alpha=1) +
    #geom_segment(data=oriWEIRD, aes(x = offset, y = oriES, xend = offset_dens, yend = oriES, colour = oriWEIRD.f),inherit.aes=FALSE, size=.8, alpha=.7) +
    geom_point(data=oriWEIRD, aes(x = offset, y = oriES), shape = 19 ,inherit.aes=FALSE, size=2, alpha=.7, colour="#5aae61") +
    scale_x_continuous(name = '', breaks = unname(offsets), labels = names(offsets)) +
    scale_colour_manual('ES Distribution',values = cols) +
    #scale_fill_manual('ES Distribution',values = c("#d1e5f0","#f4a582")) +
    scale_shape_manual('Original ES',values = c(73,19,17)) +
    ylim(-1,1)+
    ylab('Effect Size r') +  theme_minimal() +
    theme(legend.position = "top",
          legend.background  =element_rect(),
          panel.grid.major.y =element_line(colour="grey60"),
          panel.grid.major.x =element_blank(),
          panel.grid.minor.x =element_blank()) +
    coord_flip()
  g
}







#   if(!is.na(test[1,1])){
#
#     var.lor <- ifelse(grepl("OR",ML2.key$stat.type[[s]]),
#                       sum(1/(table(ML2.var[[g]]$Condition,ML2.var[[g]]$Response)), na.rm = TRUE), NA)
#
#     ESCI <- try.CATCH(any2any(testInfo  = test,
#                               df1 = test[, which(grepl("parameter",colnames(test)))[1]],
#                               df2 = test[, which(grepl("parameter2",colnames(test)))],
#                               N  = sum(ML2.var[[g]]$N, na.rm = TRUE),
#                               n1 = ML2.var[[g]]$N[1],
#                               n2 = ML2.var[[g]]$N[2],
#                               esType = ML2.key$stat.type[[s]],
#                               var.lor = var.lor,
#                               CL          = stat.params$conf.level,
#                               keepSign    = TRUE,
#                               alternative = ifelse(is.null(test$alternative),
#                                                    stat.params[[4]],
#                                                    as.character(test$alternative))
#                               ))
#
#     if(all(is.null(ESCI$warning), grepl("simpleWarning",ESCI$warning),
#            !grepl("Error", ESCI$value[[1]]),
#            !grepl("message", names(unlist(ESCI))[1]))){
#       ESCI  <- ESCI$value
#       es.id <- which(colnames(ESCI)%in%"r")
#     } else {
#       ESCI  <- test
#       es.id <- 1
#     }
#
#   } else {
#
#     ESCI  <- test
#     es.id <- 1
#
#   }
# ifelse(is.na(ESCI[es.id]),
#        disp(paste(s,ML2.key$study.analysis[[s]],'-',
#                   runGroups[g],'>> ES r is NA'), header = FALSE, footer = FALSE),
#
#        ifelse(abs(ESCI[es.id][1])>=.95,
#               disp(paste(s,ML2.key$study.analysis[[s]],'-',
#                          runGroups[g],'>> ES r is extreme hi/lo: ',
#                          round(ESCI[es.id][1],digits=2)), header = FALSE, footer = FALSE),
#
#               ifelse(abs(ESCI[es.id][1])<=0.05,
#                      disp(paste(s,ML2.key$study.analysis[[s]],'-',
#                                 runGroups[g],'>> ES r is close to 0: ',
#                                 round(ESCI[es.id][1],digits=2)), header = FALSE, footer = FALSE),
#
#                      ifelse(any(is.infinite(ESCI[es.id][[1]])),
#                             disp(paste(s,ML2.key$study.analysis[[s]],'-',
#                                        runGroups[g],'>> ES r (CI) is infinite'),
#                                  header = FALSE, footer = FALSE),
#                             NA)
#               )
#        )
# )

#
# generateAnalysisReport <- function(listIT,Nmin1,Nmin2){
#   if(!listIT){
#     if(grepl("observations",as.character(stat.test$value))){
#       disp(paste(s, ML2.key$study.analysis[[s]],'-',
#                  runGroups[g],'>> Not enough observations'),
#            header = FALSE, footer = FALSE)
#     } else {
#       disp(paste(s,ML2.key$study.analysis[[s]],'-', runGroups[g],'>> stat.test failed:'),
#            header = FALSE, footer = FALSE)
#       # disp(paste('value: ',stat.test$value),
#       #      header = FALSE, footer = FALSE)
#       disp(paste('warning:',stat.test$warning),
#            header = FALSE, footer = FALSE)
#     }
#     ConsoleOut <- paste(gsub("[[:punct:]]", "", stat.test$warning, perl = TRUE), collapse="\n")
#     NN <- lengths(ML2.var[[g]])
#     NN <- NN[!names(NN)=="N"]
#     N  <- rep(ML2.var[[g]]$N,length.out = length(NN))
#     ML2.rnd <- llply(seq_along(NN), function(nn) rnorm(N[nn]))#eval(parse(text=paste(names(NN[nn])," = rnorm(101)"))))
#     names(ML2.rnd) <- names(NN)
#     stat.test  <- try.CATCH(with(ML2.var[[g]],eval(parse(text = ML2.key$stat.test[[s]]))))
#     stat.test  <- stat.test$value
#     RANDOMdata <<- TRUE
#     #listIT     <- TRUE
#     if(nMin1){
#       disp(paste0(s,' ',ML2.key$study.analysis[[s]],' - ',
#                   runGroups[g],' not included in results >> Cases in source file (',
#                   sum(gID, na.rm = TRUE),') < Nmin.raw (',Nmin.raw,')'),
#            header = FALSE, footer = FALSE)
#     } # Check nMin 1}
#     if(Nmin2){
#       disp(paste0(s,' ',ML2.key$study.analysis[[s]],' - ',
#                   runGroups[g],' not included in results >> Valid cases after varfun (n',
#                   c(1,2)[ML2.sr[[g]]$N < Nmin.cond],"=", ML2.sr[[g]]$N[ML2.sr[[g]]$N < Nmin.cond],') < Nmin.cond (',Nmin.cond,')'),
#            header = FALSE, footer = FALSE)
#     } # Double-check nMin
#   }
# }
