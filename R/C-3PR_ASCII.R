# Data Cleaning functions  ------------------

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

# 'get' functions ----------------

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
  if(is.null(ML2.df$uID)){ML2.df$uID <- seq_along(ML2.df$source)}
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
#' # Get the data from analysis 1 [Huang.1] listed on the \href{masteRkey spreadsheet}{https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/}
#'
#' df <- get.analyses(studies = 1, analysis.type = 1)
#'
#' # 'raw' pre-filter dataset
#' head(tbl_df(df$raw.case$Huang.1))
#'
#' # analysis results
#' glimpse(tbl_df(df$merged.results$Huang.1))
#'
get.analyses <- function(studies       = NA,
                         analysis.type = NA,
                         Nmin.raw  = 30,
                         Nmin.cond = 15,
                         subset    = c("all","WEIRD","NON-WEIRD")[1],
                         rootdir   =normalizePath(paste0(find.package("manylabRs"),"/data")),
                         indir     = list(RAW.DATA = "RAW.DATA.PRIVATE",MASTERKEY = "MASTERKEY", SOURCEINFO = "SOURCEINFO"),
                         outdir    = list(ROBJECTS = "ROBJECTS",RESULTS.RDS = "RESULTS.RDS"),
                         onlineTables = TRUE){


  paths <- unique(c(paste0(rootdir,"/",indir), paste0(rootdir,"/",outdir)))
  for(d in paths[!dir.exists(paths)]){
    dir.create(d, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }

  tp  <- analysis.type
  wop <- options(warn=-1, expressions=10000)

  # Load Key Table

  if(indir$MASTERKEY==""|onlineTables){
    ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
    disp(paste("Downloaded keytable Googlesheet: ML2_masteRkey [https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/]"), header = "get.analyses", footer = FALSE)
  } else {
    ML2.key <- read.xlsx(file.path(rootdir,indir$MASTERKEY,"ML2_masteRkey.xlsx"),"ML2masteRkey")
    disp(paste0("Loaded keytable from disk: ML2_masteRkey.xlsx [",file.path(rootdir,indir$MASTERKEY),"]"), header = "get.analyses", footer = FALSE)
  }

  ML2.key <- ML2.key[!is.na(ML2.key$unique.id),]

  # Load data
  if(indir$RAW.DATA==""|onlineTables){
    ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
    disp(paste("Downloaded data from OSF: 'ML2_RawData_S1.rds' and 'ML2_RawData_S2.rds'"), header = FALSE, footer = FALSE)
  } else {
    ML2.S1 <- readRDS(file.path(rootdir,indir$RAW.DATA,"ML2_RawData_S1.rds"))
    ML2.S2 <- readRDS(file.path(rootdir,indir$RAW.DATA,"ML2_RawData_S2.rds"))
    disp(paste0("Loaded data from disk: 'ML2_RawData_S1.rds' and 'ML2_RawData_S1.rds'[",file.path(rootdir,indir$RAW.DATA),"]"), header = FALSE, footer = FALSE)
  }

  # Load information about sources
  if(indir$SOURCEINFO==""|onlineTables){
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

      ML2.df$study.order <- laply(seq_along(stmp), function(o){which(grepl(Stud,stmp[[o]]))%00%NA})

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

            describe <- get.descriptives(stat.test = stat.test,
                                        vars      = ML2.var[[g]],
                                        keytable  = ML2.key[s,])

            var.lor <- ifelse(grepl("OR",describe$test$estype),
                              sum(1/(table(ML2.var[[g]]$Condition,ML2.var[[g]]$Response)), na.rm = TRUE),
                              NA)

            ESCI  <-   generateOutput(describe        = describe,
                                      var.lor         = var.lor,
                                      runningGroup    = runGroups[g],
                                      runningAnalysis = paste(s,ML2.key$study.analysis[[s]]))

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
#'
#' @export
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
    if(!nzchar(df$StudyOrder[i]%00%0)){
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

#' get.descriptives
#'
#' @param stat.test Output of the analysis
#' @param vars Data used
#' @param keytable masteRkey
#'
#'
#' @export
#'
get.descriptives <- function(stat.test, vars, keytable){

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

  test$estype    <- keytable$stat.type
  test$method    <- stat.test$method

  if(test$estype=="Z.f"){
    if(test$method=="Fisher r-to-Z transformed test for difference between 2 independent correlations"){

      Wilcox.out<- twopcor(x1=vars$r1[[1]],
                           x2=vars$r1[[2]],
                           y1=vars$r2[[1]],
                           y2=vars$r2[[2]])

      test$fZ.ncp       <- stat.test$statistic
      test$fZ.ncp.lo    <- "fisherZ - 2 corr"
      test$fZ.ncp.hi    <- "fisherZ - 2 corr"
      test$fZ.cohensQ   <- stat.test$effect.size
      test$fZ.cohensQ.l <- stat.test$effect.size.ci[1]
      test$fZ.cohensQ.u <- stat.test$effect.size.ci[2]
      test$fZ.bootR1    <- Wilcox.out$r1
      test$fZ.bootR2    <- Wilcox.out$r2
      test$fZ.bootCI.l  <- Wilcox.out$ci[1]
      test$fZ.bootCI.u  <- Wilcox.out$ci[2]
      test$fZ.r         <- Wilcox.out$r1-Wilcox.out$r2
      test$fZ.l.r       <- Wilcox.out$ci[1]
      test$fZ.u.r       <- Wilcox.out$ci[2]
    } else {
      test$fZ.ncp       <- stat.test$statistic
      test$fZ.ncp.lo    <- "fisherZ - 1 corr"
      test$fZ.ncp.hi    <- "fisherZ - 1 corr"
      test$fZ.cohensQ   <- stat.test$effect.size
      test$fZ.cohensQ.l <- stat.test$effect.size.ci[1]
      test$fZ.cohensQ.u <- stat.test$effect.size.ci[2]
    }
  }

  return(list(descr.raw = descr.raw,
              descr.sum = descr.sum,
              test      = test)
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
