#' gg.theme
#'
#' A gg theme
#'
#' @param type One of \code{clean}, or \code{noax}
#' @param useArial Use the Arial font (requires \code{.afm} font files in the \code{afmPath})
#' @param afmPATH Path to Arial \code{.afm} font files.
#'
#'
#' @details Will generate a \code{clean} ggplot theme, or a theme without any axes (\code{noax}).
#'
#' Some scientific journals explicitly request the Arial font should be used in figures.
#' This can be achieved by using \code{.afm} font format (see, e.g. \url{http://www.pure-mac.com/font.html}).
#'
#' @return A theme for \code{ggplot2}.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data.frame(x = rnorm(n = 100), y = rnorm(n = 100)), aes(x = x, y = y)) + geom_point()
#' g + gg.theme()
#' g + gg.theme("noax")
#'
gg.theme <- function(type=c("clean","noax"),useArial = FALSE, afmPATH="~/Dropbox"){

  if(length(type)>1){type <- type[1]}

  if(useArial){
    set.Arial(afmPATH)
    bf_font="Arial"
  } else {bf_font="Helvetica"}

  switch(type,
         clean = ggplot2::theme_bw(base_size = 12, base_family=bf_font) +
           theme(axis.text.x     = element_text(size = 10),
                 axis.title.y    = element_text(vjust = +1.5),
                 panel.grid.major  = element_blank(),
                 panel.grid.minor  = element_blank(),
                 legend.background = element_blank(),
                 legend.key = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.line  = element_line(colour = "black")),
         noax = ggplot2::theme(line = element_blank(),
                      text  = element_blank(),
                      title = element_blank(),
                      plot.background = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
  )
}

#' gg.plotHolder
#'
#' @param useArial    Use the Arial font (requires \code{.afm} font files in the \code{afmPath})
#' @param afmPATH    Path to Arial \code{.afm} font files.
#'
#' @return A blank \code{ggplot2} object that can be used in concordance with \code{grid.arrange}.
#' @export
#'
#' @examples
#' # Create a plot with marginal distributions.
#' library(ggplot2)
#' library(scales)
#'
#' df <- data.frame(x = rnorm(n = 100), y = rnorm(n = 100),
#'                  group = factor(sample(x=c(0,1),
#'                  size = 100, replace = TRUE))
#'                  )
#'
#' scatterP <- ggplot(df, aes(x = x, y =y, colour = group)) +
#'             geom_point() +
#'             gg.theme()
#'
#' xDense <- ggplot(df, aes(x = x, fill = group)) +
#'           geom_density(aes(y= ..count..),trim=FALSE, alpha=.5) +
#'           gg.theme("noax") +
#'           theme(legend.position = "none")
#'
#' yDense <- ggplot(df, aes(x = y, fill = group)) +
#'           geom_density(aes(y= ..count..),trim=FALSE, alpha=.5) +
#'           coord_flip() +
#'           gg.theme("noax") +
#'           theme(legend.position = "none")
#'
#' library(gridExtra)
#'
#' grid.arrange(xDense,
#'              gg.plotHolder(),
#'              scatterP,
#'              yDense,
#'              ncol=2, nrow=2,
#'              widths=c(4, 1.4), heights=c(1.4, 4)
#'              )
#'
gg.plotHolder <- function(useArial = F,afmPATH="~/Dropbox"){
 # require(ggplot2)
    ggplot2::ggplot() +
    geom_blank(aes(1,1)) +
    theme(line = element_blank(),
          text  = element_blank(),
          title = element_blank(),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
    )
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
#' @description  This is adapted from: \url{http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot}
#'
#' @seealso vioQtile
#'
fill_viol<-function(gr.df,gr,qtile,probs){

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


swarmPlot <- function(df, anonymous=FALSE, addSize=FALSE, addMedian=TRUE, addGlobalES = TRUE, addOriES=TRUE, addLabel=FALSE, oriES=NULL, fillvar=c("USA","sigf")[1]){

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

  df$meanES <- plyr::ldply(unique(df$labels),
                     function(r) cbind(rep(mean(df$y[df$labels==r], na.rm = TRUE),
                                           sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]
  df$seES <- plyr::ldply(unique(df$labels),
                   function(r) cbind(rep(se(df$y[df$labels==r]),
                                         sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]

  df <- arrange(df, meanES)

  df$xx <- plyr::ldply(unique(df$labels),
                 function(r) cbind(scale(df$x[df$labels==r], scale = F)))[,1]
  df$xf <- plyr::ldply(seq_along(unique(df$labels)),
                 function(r) cbind(df$xx[df$labels==unique(df$labels)[r]] +
                                     seq(10,10*length(unique(df$labels)), by=10)[r]))[,1]
  df$xn <- plyr::ldply(seq_along(unique(df$labels)),
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

  dfG <- dplyr::summarise(group_by(df,.id),
                   y= mean(ESCI.r,na.rm = T),
                   ymin=mean(ESCI.l.r, na.rm = T),
                   ymax=mean(ESCI.u.r, na.rm = T))

  dfG   <- arrange(dfG, y)
  dfG$x <- seq(10,10*nrow(dfG),by=10)

  dfGlobal <- dplyr::summarise(group_by(df,.id),
                        y= max(GlobalES,na.rm = TRUE),
                        ymin=max(GlobalESlo, na.rm = TRUE),
                        ymax=max(GlobalEShi, na.rm = TRUE))
  dfGlobal   <- arrange(dfGlobal, y)
  dfGlobal$x <- seq(10,10*nrow(dfGlobal),by=10)

  if(addOriES){
    oriES      <- oriES[ID.ori,]
    oriES$mES <- plyr::laply(na.exclude(oriES$study.analysis), function(s) dfG$y[tolower(dfG$.id)%in%tolower(s)])
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

#' get.plotly
#'
#' Get a plotly plot.
#'
#' @param data Dataframe with ML2 testresutls and ESCI output.
#' @param analysis_url url
#'
#' @export
#'
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


# Rmd2htmlWP <- function(infile, outfile, sup = T) {
#   require(markdown)
#   require(knitr)
#   mdOpt <- markdownHTMLOptions(default = T)
#   mdOpt <- mdOpt[mdOpt != "mathjax"]
#   mdExt <- markdownExtensions()
#   mdExt <- mdExt[mdExt != "latex_math"]
#   if (sup == T) {
#     mdExt <- mdExt[mdExt != "superscript"]
#   }
#   knit2html(input = infile, output = outfile, options = c(mdOpt), extensions = c(mdExt))
# }

#
# [copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ]
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multi.PLOT <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ANALYSIS functions ----------------


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
#' Fred Hasselman (inspired by RP:P function \code{any2r} by CHJ Hartgerink)
#'
#' @return The effect sizes calculated by \code{compute.es} corresponding to the test statistic(s), with either meta-analytic, or, exact CI.
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
      n1<-N/2
      n2<-N/2
      alternative<-"two"
    }
  }


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
           Z    = esComp[[cnt]] <- compute.es::pes(p = pnorm(abs(x), lower.tail= FALSE)*2, level = CL*100,
                                                   n.1 = n1, n.2 = n2, tail = "two", verbose = TRUE, dig = 5),
           lm.Z  = esComp[[cnt]] <- compute.es::a.pes(p = pnorm(abs(x), lower.tail= FALSE)*2, level = CL*100,
                                                      n.1 = n1, n.2 = n2, R = rID, q = q,
                                                      tail = alternative, verbose = FALSE, dig = 5),
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
      r1 <- stats::cor(r1[,1],r1[,2],use="pairwise.complete.obs", method = cor.type)
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
      r1 <- stats::cor(r1[,1],r1[,2], use = "pairwise.complete.obs", method = cor.type)
      r2 <- stats::cor(r2[,1],r2[,2], use = "pairwise.complete.obs", method = cor.type)
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


#' z.test
#'
#' An `htest` implentation of the z test for an abserved mean or proportion.
#'
#' @param x Observed value
#' @param mu Population mean under H_{0}
#' @param pi Population proportion under H_{0}
#' @param N Sample size
#' @param sigma Population standard deviation
#' @param proportion Observed proportion
#' @param alternative
#'
#' @export
#'
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
  r1<-stats::cor(x1,y1)
  r2<-stats::cor(x2,y2)
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
  pcorbsub<-stats::cor(x[isub],y[isub])
  pcorbsub
}


#' decide.Equalvar
#'
#' @param vars int
#' @param labels int
#' @param key int
#' @param alpha int
#' @param criterion int
#' @param group int
#' @param verbose  int
#'
#'
#' @export
#'
decide.EqualVar <- function(vars, labels, key, alpha=.05, criterion = 2, group, verbose = FALSE){

  vars$N  <- NULL
  longDat <- NULL

  if(length(vars)==2){

    if(any(plyr::laply(vars,is.factor))){

      longDat <- cbind.data.frame(vars)
      colnames(longDat)[plyr::laply(vars,is.factor)]    <- "group"
      colnames(longDat)[!(plyr::laply(vars,is.factor))] <- "xy"

    } else {

      if(all(lengths(vars)>1)){
        longDat <- plyr::ldply(unlist(vars))
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
    plyr::ldply(df$labels[[l]], function(d) tidy(data.frame(eval(parse(text = paste0('df$',d))))))
  }
}

#' @title try.CATCH both warnings (with value) and errors
#'
#' @description
#'  In longer simulations, aka computer experiments,
#'  you may want to
#'  1) catch all errors and warnings (and continue)
#'  2) store the error or warning messages
#'
#'  Here's a solution  (see R-help mailing list, Dec 9, 2010):
#'
#' Catch *and* save both errors and warnings, and in the case of
#' a warning, also keep the computed result.
#'
#' @export
#' @param expr an \R expression to evaluate
#' @return a list with 'value' and 'warning', where value' may be an error caught.
#' @author Martin Maechler;
#' Copyright (C) 2010-2012  The R Core Team
try.CATCH <- function(expr){
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}


#' Rose tinted infix
#'
#'
#' @param x If \code{x} is any of \code{Inf,-Inf,NA,NaN,NULL,length(x)==0}, it will return \code{y}; otherwise it returns \code{x}.
#' @param y The value to return in case of catastrophy \code{>00<}
#'
#' @export
#' @author Fred Hasselman
#' @description When your functions wear these rose tinted glasses, the world will appear to be a nicer, fluffier place.
#' @seealso purrr::%||%
#' @examples
#' Inf %00% NA
#'
#' numeric(0) %00% ''
#'
#' NA %00% 0
#'
#' NaN %00% NA
#'
#' NULL %00% NA
`%00%` <- function(x,y){
  l0<-isna<-isnan<-isinf<-isnll<-FALSE
  if(length(x)==0){
    l0=TRUE
  } else {
    if(all(is.na(x)))       isna =TRUE
    if(all(is.nan(x)))      isnan=TRUE
    if(all(is.infinite(x))) isinf=TRUE
    if(all(is.null(x)))     isnll=TRUE
  }
  if(any(l0,isna,isnan,isinf,isnll)){x<-y}
  return(x)
}

init <- function(){
  # for testing purposes
  srcDir <- "~/Documents/GitHub/manylabRs/manylabRs/R/"
  #source(paste0(srcDir,"C-3PR_ASCII.R"))
  #source(paste0(srcDir,'getData.R'))
  source(paste0(srcDir,'inIT.R'))
  #source(paste0(srcDir,'ML2_variable_functions.R'))
  #source(paste0(srcDir,'fRedsRutils.R'))

  # Function inIT will load and -if necessary- install packages passed in a list (unIT will do the reverse operation).
  in.IT(c("MBESS","reshape2","plyr","tidyverse","metafor","RCurl","xlsx","broom","httr","compute.es","downloader","car", "lme4", "lmerTest","exact2x2","ggplot2","gplots","gridExtra","lattice","latticeExtra","rio","scales","lubridate"))

}

#' disp
#'
#' @param message     A message to be displayed in the Console.
#' @param header     Print a header of '~' symbols (=\code{TRUE}), or '~' symbols with few words of text (=\code{character vector})
#' @param footer     Print a footer '~' symbols.
#'
#' @description Displays easy-to-spot text in the Console.
#'
#' @author Fred Hasselman
#'
#' @export
#'
disp <- function(message='Hello world!', header = "disp", footer = TRUE){

  ps <- "# "

  msg <- textConnection(message)
  mWidth <- max(plyr::laply(readLines(msg),nchar))
  if(!grepl(ps,message)) mWidth <- mWidth+2

  if(is.character(header)){
    hWidth <- max(plyr::laply(header,nchar))
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



#' scaleR
#'
#' @description Rescale a vector to a user defined range defined by user.
#'
#' @param x     Input vector or data frame.
#' @param mn     Minimum value of original, defaults to \code{min(x, na.rm = TRUE)}.
#' @param mx     Maximum value of original, defaults to \code{max(x, na.rm = TRUE)}.
#' @param hi     Minimum value to rescale to, defaults to \code{0}.
#' @param lo     Maximum value to rescale to, defaults to \code{1}.
#'
#'
#' @details Three uses:
#' \enumerate{
#' \item scaleR(x)             - Scale x to data range: min(x.out)==0;      max(x.out)==1
#' \item scaleR(x,mn,mx)       - Scale x to arg. range: min(x.out)==mn==0;  max(x.out)==mx==1
#' \item scaleR(x,mn,mx,lo,hi) - Scale x to arg. range: min(x.out)==mn==lo; max(x.out)==mx==hi
#' }
#'
#' @author Fred Hasselman
#'
#' @examples
#' # Works on numeric objects
#' somenumbers <- cbind(c(-5,100,sqrt(2)),c(exp(1),0,-pi))
#'
#' scaleR(somenumbers)
#' scaleR(somenumbers,mn=-100)
#
#' # Values < mn will return < lo (default=0)
#' # Values > mx will return > hi (default=1)
#' scaleR(somenumbers,mn=-1,mx=99)
#'
#' scaleR(somenumbers,lo=-1,hi=1)
#' scaleR(somenumbers,mn=-10,mx=101,lo=-1,hi=4)
scaleR <- function(x,mn=min(x,na.rm=T),mx=max(x,na.rm=T),lo=0,hi=1){
  x <- as.data.frame(x)
  u <- x
  for(i in 1:dim(x)[2]){
    mn=min(x[,i],na.rm=T)
    mx=max(x[,i],na.rm=T)
    if(mn>=mx){warning("Minimum (mn) >= maximum (mx).")}
    if(lo>=hi){warning("Lowest scale value (lo) >= highest scale value (hi).")}
    ifelse(mn==mx,{u[,i]<-rep(mx,length(x[,i]))},{
      u[,i]<-(((x[i]-mn)*(hi-lo))/(mx-mn))+lo
      id<-complete.cases(u[,i])
      u[!id,i]<-0
    })
  }
  return(u)
}

center <- function(numvec, na.rm=TRUE, type = c("mean","median")[1]){
  switch(type,
         mean   = centroid <- mean(as.numeric(numvec), na.rm=na.rm),
         median = centroid <- median(as.numeric(numvec), na.rm=na.rm),
         )
  return((as.numeric(numvec) - centroid))
}

normalise <- function(numvec, na.rm=TRUE){
  (as.numeric(numvec) - mean(as.numeric(numvec),na.rm=na.rm)) / sd(as.numeric(numvec),na.rm=na.rm)
}

# Help lme4 get a better convergence
nlopt <- function(par, fn, lower, upper, control) {
  # Add to call: control = lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE
  .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper,
                            opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
                                        maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
       fval = res$objective,
       conv = if (res$status > 0) 0 else res$status,
       message = res$message
  )
}

# Convert decimal point
c2p <- function(text,N=1){
  if(!is.character(text)){text<-as.character(text)}
  if(sum(grepl("[,]",text))>=N){text <- gsub(",",".",text)}
  return(text)
}

# Count missing values in x
nmissing <- function(x){
  sum(is.na(x))
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data = NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval = .95, .drop=TRUE) {
  library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=na.rm) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  #datac <- rename(datac, c("mean" = measurevar))


  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}
