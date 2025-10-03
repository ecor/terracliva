NULL
#' 
#' p-Distance (p-norm of distance) between L-Moments of two different time series or samples derving from different datasets (e.g. preciptation datasets).  
#' 
#' @param la,x  first set of L-moments  or L-moment ratios
#' @param lb  second set of L-moments  or L-moment ratios
#' @param check_lmoms logical. If it is \code{TRUE}, L-moments are checked through \code{\link{are.lmoms.valid}}
#' @param p exponent. Default is 2 (euclidean distance)
#' @param nnmom names of l-moment ratios used to define the Lp space.
#' @param add_t logical. If \code{TRUE} \code{t} is calculated. See default 
#' @param condt3t4,condtt3 logical conditions passed to \link[lmomPi]{are.lmoms.valid}  
#' @param prefix character string used as index (e.g. L-momoment per each month: \code{M01_l_1,M01_l_2,..,M02_l_1,M02_l_2,...}). See default and function code.  
#' @param prefix0 character string vector used as a first level in case \code{la} and \code{b} are passed through a unique argument.
#' @param simplify logical condition. If \code{TRUE} result is simplified to a vector or an array if possible. 
#' 
#' 
#' 
#' @param ... further arguments

#' @param ... further arguments
#' @importFrom stringr str_detect str_sub str_replace str_length
#' 
#' 
#' @export
#'
#' @references  \url{https://en.wikipedia.org/wiki/Lp_space}
#' 
#' 
#' 
#' @examples
#' 
#' set.seed(1234)
#' 
#' library(lmomPi) 
#' 
#' para <- c(0,90.0,11)
#' distrib <- "pe3"
#' 
#' yA <- runif(1000)
#' valA <- qua(para=para,f=yA,distrib=distrib)
#' yB <- runif(1000)
#' valB <- qua(para=para,f=yB,distrib=distrib)
#' 
#' 
#' lmomA <- samlmu(yA)
#' lmomB <- samlmu(yB)
#' 
#' out <- lmpdistcliva(lmomA,lmomB)
#' 
#' lmomAl <- samlmu(yA,ratio=FALSE)
#' lmomBl <- samlmu(yB,ratio=FALSE)
#' 
#' outl <- lmpdistcliva(lmomAl,lmomBl) 
#' 
#' #####
#' 
#' library(terra)
#' lmom_chirps <- system.file("ext_data/lm_chirps_2000_2024.tif",package="terracliva")  |> rast()
#' lmom_mswep  <- system.file("ext_data/lm_mswep_2000_2024.tif",package="terracliva")  |> rast()
#' ### Bujumbura (BJM)
#' lat <- -3.323889
#' lon <- 29.318611
#' ichs <- cellFromXY(lmom_chirps, xy=data.frame(lon,lat))
#' imps <- cellFromXY(lmom_mswep, xy=data.frame(lon,lat))
#' 
#' a_chirps <- lmom_chirps[ichs] |> unlist()
#' b_mswep  <- lmom_mswep[imps] |> unlist()
#' 
#' otm <- lmpdistcliva(a_chirps,b_mswep,prefix="monthly")
#' prefix <- "M%02d_" |> sprintf(1:12)
#' otm2 <- lmpdistcliva(a_chirps,b_mswep,prefix="monthly")
#' #################
#' names(a_chirps) <- paste0("la_",names(a_chirps))
#' names(b_mswep) <- paste0("lb_",names(b_mswep))
#' otm3 <- lmpdistcliva(c(a_chirps,b_mswep),prefix="monthly")
#' 
#' 


lmpdistcliva <- function(x,lb=NULL,...,la=x,nnmom=c("t","t_3","t_4"),p=2,check_lmoms=TRUE,add_t=TRUE,condt3t4=FALSE,condtt3=FALSE,prefix="",prefix0=c("la","lb"),simplify=TRUE) {
  
   if (is.null(lb)) {
     
     lb <- la[str_detect(names(la),prefix0[2])]
     names(lb) <- str_sub(names(lb),str_length(prefix0[2])+2)
     ###
     la <- la[str_detect(names(la),prefix0[1])]
     names(la) <- str_sub(names(la),str_length(prefix0[1])+2) ## there is a separator, e.g. "_"
   } 
  
  
   if (is.null(prefix)) prefix <- ""
   if (all(is.na(prefix))) prefix <- ""
   
   if (prefix=="monthly") prefix <- "M%02d_" |> sprintf(1:12)
   #print(prefix)
   out <- list()
   
   for (itprefix in prefix) {
    ## print(itprefix)
     if (itprefix=="") {
       a <- la 
       b <- lb
     } else{
      a <- la[str_detect(names(la),itprefix)]
      b <- lb[str_detect(names(lb),itprefix)]
      names(a) <- names(a) |> str_replace(itprefix,"")
      names(b) <- names(b) |> str_replace(itprefix,"")
     }
     ####
    alcv <- (names(a)=="l_cv")
    #print(alcv)
    if (any(alcv))  names(a)[alcv] <- "t"
    ####
    blcv <- (names(b)=="l_cv")
    #print(blcv)
    if (any(blcv))  names(b)[blcv] <- "t"
    ####
   
   
   
   
  
  #  print(a)
  #  print(b)
  
    if (check_lmoms) {
       
      a <- a |> are.lmoms.valid(return_numeric=TRUE,condt3t4=condt3t4,condtt3=condtt3)
      b <- b |> are.lmoms.valid(return_numeric=TRUE,condt3t4=condt3t4,condtt3=condtt3)
    
    }
    if (all(c("l_1","l_2") %in% names(a)) & (!("t" %in% names(a))) & add_t)  a[["t"]] <- a[["l_2"]]/a[["l_1"]] 
    if (all(c("l_1","l_2") %in% names(b)) & (!("t" %in% names(b))) & add_t)  b[["t"]] <- b[["l_2"]]/b[["l_1"]]   
  #nmom <- length(a)
  # if (nmom>2) {
  #   areta <- all(sprintf("l_%d",3:nmom)) %in% c(names(a)) & all(c("l_1","l_2") %in% names(a))
  #   if (areta) {
  #     a[sprintf("t_%d",3:nmom)] <- a[sprintf("l_%d",3:nmom)]/a["l_2"]
  #     
  #   }
  #   aretb <- all(sprintf("l_%d",3:nmom)) %in% c(names(b)) & all(c("l_1","l_2") %in% names(b))
  #   if (aretb) {
  #     b[sprintf("t_%d",3:nmom)] <- b[sprintf("l_%d",3:nmom)]/b["l_2"]
  #     
  #   }
  #   
  # }
   
   #print(a[nnmom])
   #print(b[nnmom])

     out[[itprefix]] <- (sum(abs(a[nnmom]-b[nnmom])^p))^(1/p)
  }
  
  if (simplify) {
    out <- unlist(out)
    ## clean 
    iqq <- which(str_sub(names(out),-1)=="_")
    if (length(iqq)>0) {
      
      names(out)[iqq] <- str_sub(names(out)[iqq],end=-2)
      
    }
    
  }
  return(out)
  
  
}