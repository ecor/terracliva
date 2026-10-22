NULL
#' 
#' Computes a function on multi-layer raster, specially thought for Climate Variability Analysis in Spatial Gridded Coverage
#' 
#'
#' @param x a \code{SpatRast-Class} object
#' @param fun function. Default is \code{\link[lmom]{samlmu}}. See \code{\link[terra]{app}},\code{\link[terra]{tapp}}
#' @param index see \code{\link[terra]{app}}. It can be set equal to \code{"monthly"}
#' @param mm values of months selects for analyis with \code{fun}. Default is \code{1:12} , it is used in case \code{index=="monthly"} 
#' @param npart,npartx,nparty number of partitions along sides.
#' @param tempdir working directory path for temporary files (e.g. partition tiles)
#' @param tile_formatter formatter used for temporary files (e.g. partition tiles)  
#' @param filename,overwrite,na.rm,... further arguments for \code{fun}, \code{\link[terra]{app}} and \code{\link[terra]{tapp}}. See also \code{\link[terra]{writeRaster}}.
#' 
#' 
#' @importFrom terra ext crop mosaic sprc rast sds
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
#' 
#' \donttest{
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
#' out_monthly <- apprast_v2(dataset_monthly,index=index_monthly)
#' out_monthly_pel <- apprast_v2(dataset_monthly,fun=funpel,index=index_monthly)
#' out_monthly_pel_ <- apprast_v2(dataset_monthly,fun=funpel,index="monthly")
#' 
#' out_monthly_pel_ <- apprast_v2(dataset_monthly,fun=funpel,index="monthly",mm=3:5)
#' 
#' out_monthly_pel2 <- apprast_v2(dataset_monthly,fun=funpel,index="monthly",mm=3:5,npart=2)
#'
#'
#' (out_monthly_pel_-out_monthly_pel2) |> abs() |> max() 
#' 
#' 
#' }
apprast_v2 <- function(x,index=1,fun=samlmu,mm=1:12,na.rm=TRUE,npart=1,npartx=npart,nparty=npart,filename="",tempdir=NULL,tile_formatter="%s.tif",overwrite=FALSE,...){
  
  
  
  if (is.list(x)) x <- rast(sds(x))
    
  #   {
  #   
  #   if (!is.null(names(x))) for (it in names(x)) {
  #     print(names(x[[it]]))
  #     names(x[[it]]) <- paste0(it,names(x[[it]]))
  #   }
  #   
  #   x00 <<- x
  #   
  #   x <- rast(x)
  #   x01 <<- x
  #   
  # }  
  # warning \code{x} must have the proper time aggregation for the analysis before the execution of this function.
  ##out <- tapp(x,indexx,fun=samlmu)
  ##  
  cond_part <- (npartx>1 | nparty>1)
  
  ## EC 20250130
  
  if (cond_part) {
    
    ## directory dei tile: quella indicata dall'utente oppure una sottocartella
    ## temporanea dedicata (makeTiles scrive sempre su disco)
    if (is.null(tempdir)) tempdir <- ""
    if (is.na(tempdir) || tempdir == "") {
      tdir <- file.path(base::tempdir(), paste0("apprast_tiles_", Sys.getpid()))
      tiles_in_memory <- TRUE
    } else {
      tdir <- tempdir
      tiles_in_memory <- FALSE
    }
    if (dir.exists(tdir)) {
      fftemp <- list.files(tdir, full.names = TRUE, recursive = TRUE)
      offtemp <- fftemp |> lapply(file.remove)
    } else {
      dir.create(tdir, recursive = TRUE)
    }
    
    ## template: npartx colonne x nparty righe sull'estensione di x
    tmpl <- rast(ext(x), ncols = npartx, nrows = nparty, crs = crs(x))
    print(file.path(tdir, sprintf(tile_formatter, "input")))
    ## tile di input (makeTiles aggiunge il numero del tile prima dell'estensione)
    ff_in <- makeTiles(x, y = tmpl,
                       filename = file.path(tdir, sprintf(tile_formatter, "input")),
                       overwrite = TRUE, na.rm = FALSE)
    print(ff_in)
    ## nomi dei tile di output
    out_base <- tools::file_path_sans_ext(sprintf(tile_formatter, "output"))
    out_ext  <- tools::file_ext(tile_formatter)
    ff_out <- file.path(tdir, paste0(out_base, seq_along(ff_in), ".", out_ext))
    if (tiles_in_memory) ff_out <- rep("", length(ff_in))
    
    out0 <- lapply(seq_along(ff_in), function(i) {
      x_tile <- rast(ff_in[i])
      time(x_tile) <- time(x)   ## per sicurezza (index="monthly" usa time())
      apprast(x_tile, index = index, fun = fun, mm = mm, na.rm = na.rm,
              npart = 1, filename = ff_out[i], overwrite = overwrite, ...)
    })
    
    out1 <- out0 |> sprc() |> mosaic(filename = filename, overwrite = overwrite, fun = "mean")
    
    if (tiles_in_memory) unlink(tdir, recursive = TRUE)
    
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

