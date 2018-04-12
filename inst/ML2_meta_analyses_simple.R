# META ANALYSES - MANYLABS 2 -

# SETUP -----------------------------------------------------------------------------------------------------------
source('/Users/Fred/Documents/GitHub/manylabRs/manylabRs/R/C-3PR_ASCII.R')
init()

dir.out <- "/Users/Fred/Dropbox/Manylabs2/Figures"
dir.in  <- "~/Dropbox/Manylabs2/TestOutput/RESULTS_RDS/"
outlist1 <- rio::import(paste0(dir.in,"Data_Figure.rds"))
outlist2 <- rio::import(paste0(dir.in,"Data_Table.rds"))
outlistG <- rio::import(paste0(dir.in,"Data_Global.rds"))


outlist1$online <- NA
outlist1$online[outlist1$source.Setting%in%c("In a classroom","In a lab")] <- "lab"
outlist1$online[outlist1$source.Setting%in%c("Online (at home)")] <- "online"
outlist1$online.f <- factor(outlist1$online)
outlist1$source.country.f <- factor(outlist1$source.Country)


ML2.key     <- get.GoogleSheet(data='ML2masteRkey')$df

outlist1[grepl("Gati",unique(as.character(outlist1$analysis.name))),]


rmaR0 <- rmaR1 <- rmaR2 <- dfol <- list()

cnt=0
for(an in unique(as.character(outlist1$analysis.name))){

  ID <- as.character(outlist1$analysis.name)%in%an
  d  <- outlist1[ID,]

  if((nrow(d)>0) & !grepl("Inbar|Schwarz",an)){
    cnt<-cnt+1

    r   <- sum(ID)
    tmp <- data.frame(
      analysis.name = c(rep(an,r)),
      study.source  = as.character(d$study.source),
      study.slate   = c(d$study.slate),
      source.WEIRD.f  = factor(d$source.Weird),
      source.online.f = d$online.f,
      source.country.f = d$source.country.f,
      stat.N        = c(d$stat.N),
      test.p.value  = c(d$ESCI.pval.r),
      test.l.r      = c(d$ESCI.l.r),
      test.r        = c(d$ESCI.r),
      test.h.r      = c(d$ESCI.u.r),
      test.vi.r    = c(d$ESCI.var.r),
      test.l.d      = c(d$ESCI.l.d),
      test.d        = c(d$ESCI.d),
      test.h.d      = c(d$ESCI.u.d),
      test.vi.d    = c(d$ESCI.var.d)
    )

    id0 <- tmp$test.vi.r <= 1e-8
    if(sum(id0,na.rm = T)>0){
      tmp$test.vi.r[id0] <- tmp$test.vi.r[id0] + 1e-8
    }

    tmp <- tmp[!is.na(tmp$test.vi.r),]

    dfol[[cnt]] <- tmp

    metR0 <- rma(yi=test.r,
                 vi=test.vi.r,
                 slab = analysis.name,
                 data=tmp)
    confint(metR0)

    rmaR0[[cnt]] <- list(model = metR0, CI= confint(metR0))

    tmp2 <- tmp[!is.na(tmp$source.online),]

    metR1 <- rma(yi=test.r,
                 vi=test.vi.r,
                 mods = ~ source.online.f,
                 slab = analysis.name,
                 data=tmp2)

    rmaR1[[cnt]] <- list(model = metR1, CI= confint(metR1))

    metR2 <- rma(yi=test.r,
                 vi=test.vi.r,
                 mods = ~ source.WEIRD.f,
                 slab = analysis.name,
                 data=tmp)

    rmaR2[[cnt]] <- list(model = metR2, CI= confint(metR2))
  }
}



get.MetaResults <- function(metaObj){
  out <- ldply(metaObj,function(d){
    data.frame(
      analysis.name = d$model$slab[1],
      QE = d$model$QE%00%NA,
      pval.QE = d$model$QEp%00%NA,
      QM = d$model$QM%00%NA,
      pval.QM = d$model$QMp%00%NA,
      df   = (d$model$k-d$model$m)%00%NA,
      sig2 = sum(d$model$sigma2, na.rm = TRUE)%00%NA,
      tau2 = d$model$tau2%00%NA,
      tau2.l = d$CI$random[1,2]%00%NA,
      tau2.u = d$CI$random[1,3]%00%NA,
      tau   = d$CI$random[2,1]%00%NA,
      tau.l = d$CI$random[2,2]%00%NA,
      tau.u = d$CI$random[2,3]%00%NA,
      I2     = d$model$I2%00%NA,
      I2pct   = d$CI$random[3,1]%00%NA,
      I2pct.l = d$CI$random[3,2]%00%NA,
      I2pct.u = d$CI$random[3,3]%00%NA,
      H2   = d$CI$random[4,1]%00%NA,
      H2.l = d$CI$random[4,2]%00%NA,
      H2.u = d$CI$random[4,3]%00%NA,
      # rho  = d$model$rho%00%NA,
      # ICC  = (d$model$sigma2[1]/sum(d$model$sigma2, na.rm = TRUE))%00%0,
      console_out = paste(capture.output(print(summary(d$model))),collapse="\n"))
  })
  return(out)
}


mods1 <-  list(nomod_uni = rmaR0,
               online_uni = rmaR1,
               weird_uni = rmaR2)

outMeta_uni <- ldply(mods1,get.MetaResults)

dir.out <- "/Users/Fred/Dropbox/Manylabs2/TestOutput/RESULTS_META"
export(outMeta_uni,paste0(dir.out,"/meta_analysis_wide_",as.Date(now()),".xlsx"))

pdf(paste0(dir.out,"/FunnelPerAnalysis_mod2_",as.Date(now()),"2.pdf"), paper="a4r")


for(m in seq_along(dfol)){

  if(!is.null(dfol[[m]])){

    gplots::textplot(capture.output(summary(rmaR0[[m]]$model),rmaR0[[m]]$CI))
  title(paste("no moderator:",dfol[[m]]$analysis.name[1], "\nI2:",round(rmaR0[[m]]$model$I2,10)))


    funnel(rmaR0[[m]]$model, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), pch = (rmaR0[[m]]$model$X[,1]), refline=0, main = paste(dfol[[m]]$analysis.name[1]))


    gplots::textplot(capture.output(summary(rmaR1[[m]]$model),rmaR1[[m]]$CI))
    title(paste("online moderator:",dfol[[m]]$analysis.name[1], "\nI2:",round(rmaR1[[m]]$model$I2,10)))


    funnel(rmaR1[[m]]$model, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), pch = (rmaR1[[m]]$model$X[,1]), refline=0, main = paste(dfol[[m]]$analysis.name[1]))

    gplots::textplot(capture.output(summary(rmaR2[[m]]$model),rmaR2[[m]]$CI))
    title(paste("weird moderator:",dfol[[m]]$analysis.name[1], "\nI2:",round(rmaR1[[m]]$model$I2,10)))

    funnel(rmaR2[[m]]$model,level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),pch = (rmaR2[[m]]$model$X[,1]), refline=0, main = paste(dfol[[m]]$analysis.name[1]))
}

}
dev.off()



########################################################
##### MANY LABS 2: META-ANALYSES INBAR AND SCHWARZ #####
########################################################

#rm(list=ls())

################
### PACKAGES ###
################

#install.packages(c("stringr", "metafor"))
library(stringr)
library(metafor)


outdir <- "~/Dropbox/Manylabs2/TestOutput/RESULTS_META" # Set working directory to location where data is stored

rmaR0 <- rmaR1 <- rmaR2  <- list()

cnt=1

##### Inbar #####
dat_Inbar <- read.csv(file = "~/Dropbox/Manylabs2/TestOutput/RESULTS_ANALYSIS/all/Inbar.1a_study_primary_include_all.csv") # Load data

### Extract Cohen's q
#q <- as.numeric(str_extract_all(as.character(dat_Inbar$test.ConsoleOutput), "(?<=0\\) \n\\s{0,1})[-0-9.]+"))
#q <- dat_Inbar$ESCI.cohensQ

### Compute sampling variance Cohen's q
vi <- 1/(dat_Inbar$stat.n1-3)+1/(dat_Inbar$stat.n2-3)

### Recode source.Setting variable (Merge "In a lab" and "In a classroom" and discard
# "Other (please indicate)")
levels(dat_Inbar$source.Setting) <- list(online = c("Online (at home)"), lab = c("In a lab", "In a classroom"))

### RE meta-analysis without moderators
metR0 <- rma(yi = ESCI.cohensQ, vi = vi, slab = analysis.name, data = dat_Inbar)

### RE meta-analysis with source.Weird as moderator
metR1 <- rma(yi = ESCI.cohensQ, vi = vi, mods = ~source.Weird, slab = analysis.name, data = dat_Inbar)

### RE meta-analysis with recoded source.Setting as moderator
metR2 <- rma(yi = ESCI.cohensQ, vi = vi, mods = ~ source.Setting, slab = analysis.name, data = dat_Inbar)

rmaR0[[cnt]] <- list(model=metR0, CI = confint(metR0))
rmaR1[[cnt]] <- list(model=metR1, CI = confint(metR1))
rmaR2[[cnt]] <- list(model=metR2, CI = confint(metR2))

#
# mods2 <-  list(nomod_uni = rmaR0,
#                online_uni = rmaR1,
#                weird_uni = rmaR2)
#
# outMeta_uni <- ldply(mods2,get.MetaResults)
#
# export(outMeta_uni,paste0(outdir,"/meta_analysisInbar_",as.Date(now()),".xlsx"))

################################################################################

##### Schwarz #####
dat_Schwarz <- read.csv(file = "~/Dropbox/Manylabs2/TestOutput/RESULTS_ANALYSIS/all/Schwarz.1a_study_primary_include_all.csv") # Load data
#rmaR0 <- rmaR1 <- rmaR2 <- dfol <- list()

cnt <-2

### Extract Cohen's q
q <- as.numeric(str_extract_all(as.character(dat_Schwarz$test.ConsoleOutput), "(?<=0\\) \n\\s{0,1})[-0-9.]+"))

### Compute sampling variance Cohen's q
vi <- 1/(dat_Schwarz$stat.n1-3)+1/(dat_Schwarz$stat.n2-3)

### Recode source.Setting variable (Merge "In a lab" and "In a classroom" and discard
# "Other (please indicate)")
levels(dat_Schwarz$source.Setting) <- list(online = c("Online (at home)"), lab = c("In a lab", "In a classroom"))

### RE meta-analysis without moderators
metR0 <- rma(yi = ESCI.cohensQ, vi = vi, slab = analysis.name, data = dat_Schwarz)

### RE meta-analysis with source.Weird as moderator
metR1 <- rma(yi = ESCI.cohensQ, vi = vi, mods = ~ source.Weird, slab = analysis.name, data = dat_Schwarz)

### RE meta-analysis with recoded source.Setting as moderator
metR2 <- rma(yi = ESCI.cohensQ, vi = vi, mods = ~ source.Setting, slab = analysis.name, data = dat_Schwarz)


rmaR0[[cnt]] <- list(model=metR0, CI = confint(metR0))
rmaR1[[cnt]] <- list(model=metR1, CI = confint(metR1))
rmaR2[[cnt]] <- list(model=metR2, CI = confint(metR2))

mods2<-  list(nomod_uni = rmaR0,
               online_uni = rmaR1,
               weird_uni = rmaR2)

outMeta_uni <- ldply(mods2,get.MetaResults)

export(outMeta_uni,paste0(outdir,"/meta_analysis_Q_",as.Date(now()),".xlsx"))
