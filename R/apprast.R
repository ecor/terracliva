NULL
#' 
#' Computes a function on multi-layer raster, specially thought for Climate Variability Analysis in Spatial Gridded Coverage
#' 
#'
#' @param x a \code{SpatRast-Class} object
#' @param fun function. Default is \code{\link{samlmu}}. See \code{\link{app}},\code{\link{tapp}}
#' @param index see \code{\link{app}}. It can be set equal to \code{"monthly"}
#' @param mm values of months selects for analyis with \code{fun}. Default is \code{1:12} , it is used in case \code{index=="monthly"} 
#' @param npart,npartx,nparty number of partitions along sides.
#' @param filename,overwrite,na.rm,... further arguments for \code{fun}, \code{\link{app}} and \code{\link{tapp}}. See also \code{\link{writeRaster}}.
#' 
#' 
#' @importFrom terra ext crop mosaic sprc
#' @importFrom magrittr  %>% 
#' @importFrom terra app nlyr tapp time
#' @importFrom lmom samlmu
#' @importFrom lubridate month 
#' 
#' @export
#'
#' 
#' 
#' 
#' @examples
#' 
#' library(magrittr)
#' library(terra)
#' library(lmomPi)
#' 
#' years <- 1982:2023
#' dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
#' dataset_path <- system.file("ext_data/precipitation",package="terracliva")
#' dataset_yearly <- "%s/yearly/chirps_yearly_goma_%04d.grd" %>% 
#' sprintf(dataset_path,years) %>% rast()
#' 
#' 
#' 
#' out_yearly <- apprast(dataset_yearly)
#' ## Function must contain a na.rm or ... argument. 
#' funpel <- function(x,distrib="pe3",...) {
#'   o1 <- samlmu(x) 
#'   o <- o1 %>% pel(distrib=distrib)
#'   nn <- names(o)
#'   o <- as.numeric(o)
#'   names(o) <- paste(distrib,nn,sep="_")
#'   o <- c(o1,o)
#'   return(o)
#' }
#' 
#' out_yearly_pel <- apprast(dataset_yearly,fun=funpel)
#' 
#' library(lubridate)
#' dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% 
#' sprintf(dataset_path,years) %>% rast()
#' time(dataset_monthly) <-  names(dataset_monthly) %>% paste0("_01") %>% as.Date(format="X%Y_%m_%d")
#' index_monthly  <- month(time(dataset_monthly)) %>% sprintf(fmt="M%02d")
#' 
#' out_monthly <- apprast(dataset_monthly,index=index_monthly)
#' out_monthly_pel <- apprast(dataset_monthly,fun=funpel,index=index_monthly)
#' out_monthly_pel_ <- apprast(dataset_monthly,fun=funpel,index="monthly")
#' 
#' out_monthly_pel_ <- apprast(dataset_monthly,fun=funpel,index="monthly",mm=3:5)
#' 
#' out_monthly_pel2 <- apprast(dataset_monthly,fun=funpel,index="monthly",mm=3:5,npart=2)
#'
#'
#' (out_monthly_pel_-out_monthly_pel2) |> abs() |> max() 
apprast <- function(x,index=1,fun=samlmu,mm=1:12,na.rm=TRUE,npart=1,npartx=npart,nparty=npart,filename="",overwrite=FALSE,...){
  
  # warning \code{x} must have the proper time aggregation for the analysis before the execution of this function.
  ##out <- tapp(x,indexx,fun=samlmu)
  ##  
  cond_part <- (npartx>1 | nparty>1)
  
  ## EC 20250130
  
  if (cond_part) {
     
     eem <- ext(x)
     out0 <- list()
     for (ix in 1:npartx){
       out0[[ix]] <- list()
       for (iy in 1:nparty) {
         
        ee <- eem
        ee$xmin <- (ix-1)*(eem$xmax-eem$xmin)/npartx+eem$xmin
        ee$xmax <- (ix)*(eem$xmax-eem$xmin)/npartx+eem$xmin
        ee$ymin <- (iy-1)*(eem$ymax-eem$ymin)/nparty+eem$ymin
        ee$ymax <- (iy)*(eem$ymax-eem$ymin)/nparty+eem$ymin
        
        x_crop <- crop(x,y=ee)
        
        
        out0[[ix]][[iy]] <- apprast(x_crop,index=index,fun=fun,
                                    mm=mm,na.rm=na.rm,npart=1,...)
        
        
         
       }
       
       
      
       
     }     
     ## out0 TO BE MERGED 
     
   
     out1 <- out0 |> unlist() |> sprc() |> mosaic(filename=filename,overwrite=overwrite,fun="mean")
     
    
     
     
    # for (ix in 1:npartx){
    #   ####out0[[ix]] <- list()
    #   for (iy in 1:nparty) {
    #   }
    # }
     return(out1)
     
  } 
  
  ## EC 20250130
  # 
  #   ee <- ext(x)
  #   out <- i 
  #   filename <- list(...)$filename
  #   if (is.null(filename)) {}
  #     filename <- tempfile()
  #     extension(filename) <- ".grd"
  #   }
  # 
  # 
  #   out0 <- list()
  #   for (ix in 1:npartx) for (iy in 1:nparty) {
  #   
  #   
  #   
  #   }
  # }

  
  ## TO CONTINUE ON... 
  ##
  if (length(index)<1) index <- 1 
  if (("na.rm" %in% names(formals(fun)))) {
    na.rm.exists=TRUE
  } else {
    na.rm.exists=FALSE
  } 
  

  
  if (!is.na(index[1])) if (index[1]=="monthly") {
    index <- lubridate::month(terra::time(x)) 
    mm <- mm[mm %in% 1:12]
    if (length(mm)==0) mm=1:12
    x <- x[[which(index %in% mm)]]
    index <- lubridate::month(terra::time(x)) %>% sprintf(fmt="M%02d")
  }
  if (length(index)==nlyr(x) & nlyr(x)>1) {
    
    if (na.rm.exists) {
      out <- tapp(x,index=index,fun=fun,na.rm=na.rm,filename=filename,overwrite=overwrite,...)
    } else {
      out <- tapp(x,index=index,fun=fun,filename=filename,overwrite=overwrite,...)
    }
  } else  if (na.rm.exists) {
    
    out <- app(x,fun=fun,na.rm=na.rm,filename=filename,overwrite=overwrite,...)
    
  } else {
    
    out <- app(x,fun=fun,filename=filename,overwrite=overwrite,...)
  }
  
  return(out)
  
}

