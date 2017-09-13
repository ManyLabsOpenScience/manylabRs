#' gg.theme
#'
#' @param type      One of \code{"clean"}, or \code{"noax"}
#' @param useArial    Use the Arial font (requires \code{.afm} font files in the \code{afmPath})
#' @param afmPATH    Path to Arial \code{.afm} font files.
#'
#' @details Will generate a \code{"clean"} ggplot theme, or a theme without any axes (\code{"noax"}).
#'
#' Some scientific journals explicitly request the Arial font should be used in figures. This can be achieved by using \code{.afm} font format (see, e.g. http://www.pure-mac.com/font.html).
#'
#' @return A theme for \code{ggplot2}.
#' @export
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data.frame(x = rnorm(n = 100), y = rnorm(n = 100)), aes(x = x, y = y)) + geom_point()
#' g + gg.theme()
#' g + gg.theme("noax")
gg.theme <- function(type=c("clean","noax"),useArial = F, afmPATH="~/Dropbox"){
 # require(ggplot2)

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

set.Arial <- function(afmPATH="~/Dropbox"){
  # Set up PDF device on MAC OSX to use Arial as a font in Graphs
  if(nchar(afmPATH>0)){
    if(file.exists(paste0(afmPATH,"/Arial.afm"))){
      Arial <- Type1Font("Arial",
                         c(paste(afmPATH,"/Arial.afm",sep=""),
                           paste(afmPATH,"/Arial Bold.afm",sep=""),
                           paste(afmPATH,"/Arial Italic.afm",sep=""),
                           paste(afmPATH,"/Arial Bold Italic.afm",sep="")))
      if(!"Arial" %in% names(pdfFonts())){pdfFonts(Arial=Arial)}
      if(!"Arial" %in% names(postscriptFonts())){postscriptFonts(Arial=Arial)}
      return()
    } else {disp(header='useArial=TRUE',message='The directory did not contain the *.afm version of the Arial font family')}
  } else {disp(header='useArial=TRUE',message='Please provide the path to the *.afm version of the Arial font family')}
}

#
# plot.loglog <- function(fd.OUT){
#     require(ggplot2)
#     require(scales)
#   g <- ggplot2::ggplot(fd.OUT$PLAW, aes(x=size,y=bulk), na.rm=T) +
#     scale_x_log10(breaks = log_breaks(n=abs(diff(range(round(log10(fd.OUT$PLAW$size)))+c(-1,1))),base=10),
#                   labels = trans_format("log10", math_format(10^.x)),
#                   limits = range(round(log10(fd.OUT$PLAW$size)))+c(-1,1)) +
#     scale_y_log10(breaks = log_breaks(n=abs(diff(range(round(log10(fd.OUT$PLAW$bulk)))+c(-1,1))),base=10),
#                   labels = trans_format("log10", math_format(10^.x)),
#                   limits = range(round(log10(fd.OUT$PLAW$bulk)))+c(-1,1)) +
#     geom_point() +
#     geom_abline(intercept = fd.OUT[[2]]$fitlm1$coefficients[[1]], slope = fd.OUT[[2]]$fitlm1$coefficients[[2]], colour = "red", size = 2) +
#     ggtitle(paste("Regression over ",length(fd.OUT[[2]]$fitlm1$fitted.values)," frequencies/bins",sep=""))+
#     xlab("Frequency (log10)")+ylab("Power (log10)") +
#     annotation_logticks() +
#     annotate("text",x=10^-2,y=10^5,label=paste("Slope = ",round(fd.OUT[[2]]$alpha,digits=2),sep="")) +
#       gg.theme("clean")
#   return(g)
# }

# #' PSDslope
# #'
# #' @param y    A time series object, or a vector that can be converted to a time series object.
# #' @param fs    Sample frequency (defults to 1).
# #' @param nfft    Number of frequencies to estimate (defaults to next power of 2)
# #' @param fitRange    Vector of length 2 with range of frequencies to perform log-log fit.
# #' @param plot    Plot the log-log spectrum and slope.
# #'
#
# #' @export
# #'
# #' @examples
# #'
# PSDslope <- function(y  = ts(rnorm(n = 1024), frequency = 1),
#                      fs = frequency(y),
#                      nfft = 2^(nextpow2(length(y)/2)),
#                      fitRange = c(1,round(.1*nfft)),
#                      plot = FALSE){
#   require(oce)
#   require(signal)
#   if(!is.ts(y)){ts(y, frequency = fs)}
#
#   win <- signal::hamming(n=nfft)
#
#   perioGram <- oce::pwelch(x = y, window = win, fs = frequency(y), nfft = nfft, plot = FALSE)
#   spec <- data.frame(Frequency = perioGram$freq, Power = perioGram$spec)
#   spec[1,1:2] <- NA
#   fit <- lm(log10(spec$Power[fitRange[1]:fitRange[2]])~log10(spec$Power[fitRange[1]:fitRange[2]]))
#   return(list(spec = spec,
#               slope = fit)
#   )
# }

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
#' @export
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

# MULTIPLOT FUNCTION ------------------------------------------------------------------------------------------------------------------
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
#  require(grid)

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
#' @param y The value to return in case of catastrophy \code{>!<}
#'

#' @export
#' @author Fred Hasselman
#' @description When your functions wear these rose tinted glasses, the world will appear to be a nicer, fluffier place.
#' @seealso purrr::%||%
#' @examples
#' Inf %0!0% NA
#'
#' numeric(0) %0!0% ''
#'
#' NA %0!0% 0
#'
#' NaN %0!0% NA
#'
#' NULL %0!0% NA
`%0!0%` <- function(x,y){
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


# OLD NETWORK FUNCTIONS -------------------------------------------------------------------------------------------

#
# brainButter <- function(TSmat, fs=500, band=c(lfHIp=4,hfLOp=40), Np=9){
#   # Extract frequency bands from columns of TSmat that are commonly used in Neuroscience
#   # Low Freq. High-pass (1st) and High Freq. Low-pass (2nd) FIR1 filter is applied at frequencies specified as band=c(lfHIp=...,hfLOp=...)
#   require("signal")
#
#   fHI <- butter(Np,band[[1]]*2/fs,"high")
#   fLO <- butter(Np,band[[2]]*2/fs,"low")
#
#   TSflt <- apply(TSmat,2,function(TS) filtfilt(f=fHI,x=TS))
#   TSflt <- apply(TSflt,2,function(TS) filtfilt(f=fLO,x=TS))
#
#   #TSflt <- apply(TSflt,2,fltrIT,f=fLO)
#
#   return(TSflt)
# }
#
# brainFir1 <- function(TSmat, fs=500, band=c(lfHIp=4,hfLOp=40), Np=2/band[1]){
#   # Extract frequency bands from columns of TSmat that are commonly used in Neuroscience
#   # Low Freq. High-pass (1st) and High Freq. Low-pass (2nd) FIR1 filter is applied at frequencies specified as band=c(lfHIp=...,hfLOp=...)
#   require("signal")
#
#   if(2/band[1]>Np){print(paste("Incorrect filter order Np... using 2/",band[1]," = ",(2/band[1]),sep=""))}
#
#   fBP <- fir1(floor(Np*fs),band/(fs/2),type="pass");
#
#   TSflt <- apply(TSmat,2,function(TS) filtfilt(f=fBP,1,x=TS))
#
#
#   #   fHI <- fir1(floor(Np*fs),band[1]/(fs/2),type="high");
#   #   fLO <- fir1(floor(Np*fs),band[2]/(fs/2),type="low");
#   #
#   #   TSflt <- apply(TSmat,2,function(TS) filtfilt(f=fHI,1,x=TS))
#   #   TSflt <- apply(TSflt,2,function(TS) filtfilt(f=fLO,1,x=TS))
#
#
#   return(TSflt)
# }
#
#
# ssi2sbi <- function(SImat,threshold){
#   # Signed Similarity matrix to "signed binary" matrix
#
#   idS   <- which(SImat<0)
#   BImat <- abs(as.matrix(SImat))
#   diag(BImat) <- 0
#   BImat[BImat <= threshold] <- 0
#   BImat[BImat >  threshold] <- 1
#   BImat[idS] <- BImat[idS]*-1
#
#   return(BImat)
# }
#
# si2bi <- function(SImat,threshold){
#   # Unsigned Similarity matrix to unsigned binary matrix
#
#   ifelse(any(SImat<0),{
#     print("Signed matrix, use: ssi2sbi()")
#     break},{
#       BImat <- as.matrix(SImat)
#       diag(BImat) <- 0
#       BImat[BImat <= threshold] <- 0
#       BImat[BImat >  threshold] <- 1})
#
#   return(BImat)
# }
#
# ssi2sth <- function(SImat,threshold){
#   # Signed Similarity matrix to "signed thresholded" matrix
#
#   idS   <- which(SImat<0)
#   THmat <- abs(as.matrix(SImat))
#   diag(THmat) <- 0
#   THmat[THmat <= threshold] <- 0
#   THmat[idS] <- THmat[idS]*-1
#
#   return(THmat)
# }
#
# si2th <- function(SImat,threshold){
#   # Similarity matrix to thresholded matrix
#
#   ifelse(any(SImat<0),{
#     print("Signed matrix, use: ssi2sth()")
#     break},{
#       THmat <- as.matrix(SImat)
#       THmat[THmat <= threshold] <- 0})
#
#   return(THmat)
# }
#
#
# plotBIN <- function(BImat){
#
#   g <- graph.adjacency(BImat, weighted=T, mode = "undirected",diag=F)
#   g <- simplify(g)
#
#   # set colors and sizes for vertices
#   V(g)$degree <- degree(g)
#
#   rev<-scaleRange(log1p(V(g)$degree))
#   rev[rev<=0.3]<-0.3
#
#   V(g)$color       <- rgb(scaleRange(V(g)$degree), 1-scaleRange(V(g)$degree),  0, rev)
#   V(g)$size        <- 25*rev
#   V(g)$frame.color <- NA
#
#   # set vertex labels and their colors and sizes
#   V(g)$label       <- V(g)$name
#   V(g)$label.color <- rgb(0, 0, 0, rev)
#   V(g)$label.cex   <- rev
#
#   # set edge width and color
#
#   E(g)$width <- 4
#   E(g)$color <- rgb(.5, .5, 0, .6)
#   set.seed(958)
#
#   #   layout1=layout.spring(g)
#   #    layout2=layout.fruchterman.reingold(g)
#   #    layout3=layout.kamada.kawai(g)
#   #   layout5 = layout.spring(g,mass=0.3,repulse=T)
#
#   #   CairoFontMatch(fontpattern="Arial")
#   #   CairoFonts(regular="Arial:style=Normal")
#
#   #   CairoPDF(pname,10,10)
#   #   plot(g, layout=layout.sphere)
#   #   dev.off()
#   #
#
#   plot(g, layout=layout.sphere)
#
#   return(g)
# }
#
# plotMAT <- function(BImat,l=NULL){
#
#   g <- graph.adjacency(BImat, weighted=T, mode = "undirected",diag=F)
#   #g <- simplify(g)
#
#   # set colors and sizes for vertices
#   V(g)$degree <- degree(g)
#
#   rev<-scaleRange(V(g)$degree)
#   rev[rev<=0.4]<-0.4
#
#   V(g)$color       <- rgb(scaleRange(V(g)$degree), 1-scaleRange(V(g)$degree),  0, rev)
#   V(g)$size        <- 20*rev
#   V(g)$frame.color <- NA
#
#   # set vertex labels and their colors and sizes
#   V(g)$label       <- V(g)$name
#   V(g)$label.color <- rgb(0, 0, 0, .8)
#   V(g)$label.cex   <- 1.1
#
#   # set edge width and color
#   #  rew<-E(g)$weight
#   #  rew[rew<=0.3]<-0.3
#   #
#   edge.central=edge.betweenness(g)
#   #
#   for (i in 1:ecount(g)) {E(g)$width[i]=0.3+sqrt((edge.central[i]))}
#
#   # E(g)$width <- 2*E(g)$weight
#   E(g)$color <- rgb(.5, .5, 0, .6)
#   set.seed(958)
#
#   if(is.null(l)){l<-layout.fruchterman.reingold(g,niter=500,area=vcount(g)^2.3,repulserad=vcount(g)^2.8)}
#
#   plot(g,layout=l)
#   return(g)
# }
#
#
# plotSIGNth <- function(sSImat){
#
#   g <- graph.adjacency(sSImat, weighted=TRUE)
#   E(g)$sign <- E(g)$weight
#   E(g)$curved <- is.mutual(g)
#   E(g)$lty <- ifelse( E(g)$sign > 0, 1, 1)
#   E(g)$arrow.size <- .2
#   E(g)$width <- 3
#   #E(g)$color <- rgb(scaleRange(abs(E(g)$weight)), 1-scaleRange(abs(E(g)$weight)), 0, 1)
#   #layout1=layout.fruchterman.reingold(g)
#
#   V(g)$label.color <- rgb(0, 0, 0, 1)
#   V(g)$label.cex <- 1.4
#   V(g)$vs   <- graph.strength(g, mode="in")
#   V(g)$vs.u <- scaleRange(graph.strength(g))
#   #V(g)$color<- ifelse( V(g)$vs > 0, rgb(V(g)$vs.u, 1-V(g)$vs.u, 0, 1), rgb(1-V(g)$vs.u, V(g)$vs.u, 0, 1))
#
#   E(g)$es.u  <- scaleRange(E(g)$weight)
#   E(g)$color <- ifelse( E(g)$sign > 0, rgb(0, 1, 0, .2), rgb(1, 0, 0, .2))
#   return(g)
# }
#
# plotSW <- function(n,k,p){
#
#   g <- watts.strogatz.game(1, n, k, p)
#
#   V(g)$degree <- degree(g)
#
#   # set colors and sizes for vertices
#   rev<-scaleRange(log1p(V(g)$degree))
#   rev[rev<=0.2]<-0.2
#   rev[rev>=0.9]<-0.9
#   V(g)$rev <- rev
#
#   V(g)$color       <- rgb(V(g)$rev, 1-V(g)$rev,  0, 1)
#   V(g)$size        <- 25*V(g)$rev
#
#   # set vertex labels and their colors and sizes
#   V(g)$label       <- ""
#
#   E(g)$width <- 1
#   E(g)$color <- rgb(0.5, 0.5, 0.5, 1)
#
#   return(g)
# }
#
# plotBA <- function(n,pwr,out.dist){
#   #require("Cairo")
#
#   g <- barabasi.game(n,pwr,out.dist=out.dist,directed=F)
#   V(g)$degree <- degree(g)
#
#   # set colors and sizes for vertices
#   rev<-scaleRange(log1p(V(g)$degree))
#   rev[rev<=0.2] <- 0.2
#   rev[rev>=0.9] <- 0.9
#   V(g)$rev <- rev
#
#   V(g)$color    <- rgb(V(g)$rev, 1-V(g)$rev,  0, 1)
#   V(g)$size     <- 25*V(g)$rev
#   # V(g)$frame.color <- rgb(.5, .5,  0, .4)
#
#   # set vertex labels and their colors and sizes
#   V(g)$label <- ""
#
#   E(g)$width <- 1
#   E(g)$color <- rgb(0.5, 0.5, 0.5, 1)
#
#   return(g)
# }
