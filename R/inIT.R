#' @title Initialise It
#' @description Load and/or install R packages
#'
#' @param need    A vector of package names to be loaded. The wrapper functions have a predefinded \code{need} list and can be used as shortcuts (see details).
#' @param inT    Logical. If \code{TRUE} (default), packages in \code{need} wil be installed if they are not available on the system.
#'
#' @details \code{in.IT} will check if the Packages in the list argument \code{need} are installed on the system and load them. If \code{inT=TRUE} (default), it will first install the packages if they are not present and then proceed to load them.
#'
#' @export
#'
#' @author Fred Hasselman
#'
#' @family initialise packages
#'
#' @examples
#' in.IT(c("reshape2", "plyr", "dplyr"))
in.IT <- function(need=NULL,inT=TRUE){
    ip <- .packages(all.available=TRUE)
    if(any((need %in% ip)==FALSE)){
        if(inT==TRUE){
            install.packages(need[!(need %in% ip)])
        } else {
            cat('Package(s):\n',paste(need[(need %in% ip)==FALSE],sep='\n'),'\nnot installed.\nUse in.IT(c("packagename1","packagename2",...),inT=TRUE)')
            need <- need[(need %in% ip)==TRUE]
        }
    }
    ok <- sapply(1:length(need),function(p) require(need[[p]],character.only=TRUE))
}

#
# \itemize{
#     \item \strong{in.IO}: Load I/O and data handling tools, \code{need = c("plyr","reshape2","RCurl","httr","dplyr","rio")}
#     \item \strong{in.PLOT}: Load tools for plotting \code{need = c("lattice","latticeExtra","gplots","ggplot2","grid","gridExtra","scales","effects","RColorBrewer")}
#     \item \strong{in.NLTS}: Load tools for Nonlinear Time Series Analysis, \code{need = c("fractaldim","fractalrock","RTisean","tsDyn","tseries","tseriesChaos")}
#     \item \strong{in.SN}: Load tools for signal analysis (Matlab style) \code{need = c("pracma","signal","EMD","hht","matlab","oce")}
# }
# #' @title Wrapper for in.IT: Load I/O and data handling tools
# #'
# #' @rdname in.IT
# #'
# #' @export
# #'
# #' @examples
# #' in.IO()
# in.IO <- function(need = c("xlsx","plyr","doBy","reshape2","RCurl","XML","httr","dplyr")){
#     in.IT(need)
# }
# #' @title Wrapper for in.IT: Parallel computing tools
# #'
# #' @rdname in.IT
# #'
# #' @export
# #'
# #' @examples
# #' in.PAR()
# in.PAR <- function(need = c("parallel","doParallel","foreach")){
#     in.IT(need)
# }
# #' @title Wrapper for in.IT: Tools for plotting
# #'
# #' @rdname in.IT
# #'
# #' @export
# #'
# #' @examples
# #' in.PLOT()
# in.PLOT <- function(need = c("lattice","latticeExtra","gplots","ggplot2","grid","gridExtra","scales","effects","RColorBrewer")){
#     in.IT(need)
# }
# #' @title Wrapper for in.IT: Nonlinear Time Series packages
# #'
# #' @rdname in.IT
# #'
# #' @export
# #'
# #' @examples
# #' in.NLTS()
# in.NLTS <- function(need = c("fractaldim","fractalrock","RTisean","tsDyn","tseries","tseriesChaos")){
#     in.IT(need)
# }
# #' @title Wrapper for in.IT: Signal analysis packages
# #'
# #' @rdname in.IT
# #'
# #' @export
# #'
# #' @examples
# #' in.SN()
# in.SIGN <- function(need=c("pracma","signal","EMD","hht","matlab","oce")){
#     in.IT(need)
# }

#' @title Un-initialise It
#' @description Unload and/or uninstall R packages.
#' @param loose    A vector of package names to be unloaded.
#' @param unT    Logical. If \code{TRUE}, packages in \code{loose} wil be un-installed if they are available on the system.
#'
#' @details \code{un.IT}will check if the Packages in the list argument \code{loose} are installed on the system and unload them. If \code{unT=TRUE} it will first unload the packages if they are loaded, and then proceed to uninstall them.
#'
#' @export
#'
#' @author Fred Hasselman
#'
#' @family initialise packages
#'
#' @examples
#' \dontrun{un.IT(loose = c("reshape2", "plyr", "dplyr"), unT = FALSE)}
un.IT <- function(loose,unT=FALSE){
  dp <- .packages()
  if(any(loose %in% dp)){
    for(looseLib in loose[(loose %in% dp)]){detach(paste0("package:",looseLib), unload=TRUE,character.only=TRUE)}
  }
  rm(dp)
  if(unT==TRUE){
    dp <- .packages(all.available=TRUE)
    if(any(loose %in% dp)){remove.packages(loose[(loose %in% dp)])}
  }
}

