# -*- coding: UTF-8 -*-
NULL
#'  Heat and Cold waves analysis
#'
#'  Heat and Cold waves analysis
#' 
#' @param x time series (e.g. daily maximum teperature)
#' @param timex corresponding vector of dates for \code{x}
#' @param timex_sim corresponding vector of dates in which heat/cold wave magnitude index is calculated
#' @param cold logical cold wave option 
#' @param start_month starting month of the year. Default is 1. (TO TEST) 
#' @param return_vector logical. If \code{TRUE} function returns a vector.
#' @param ... further arguments
#'
#' @importFrom extRemes hwmid
#' @importFrom lubridate year

#' @export
#'
#' @seealso \code{\link{hwmid}}
#' @examples
#' 
#' library(magrittr)
#' library(terra)
#' library(lmomPi)
#' library(extRemes)
#' years <- 1983:2016
#' tmax_dataset_path <- system.file("ext_data/tmax",package="terracliva")
#' tmax_dataset_daily <- "%s/daily/chirts_daily_goma_tmax_%04d.grd" %>% sprintf(tmax_dataset_path,years) %>% rast()
#'
#' tmax <- as.numeric(tmax_dataset_daily[100])
#' timex <- time(tmax_dataset_daily)
#' 
#' 
#'
#' o_hw <- hwmidcliva(x=tmax,timex=timex)
#' o_hw6 <- hwmidcliva(x=tmax,timex=timex,start_month=6)
#'
#' ## COLD WAVE 
#' tmin_dataset_path <- system.file("ext_data/tmin",package="terracliva")
#' tmin_dataset_daily <- "%s/daily/chirts_daily_goma_tmin_%04d.grd" %>% sprintf(tmin_dataset_path,years) %>% rast()
#'
#' tmin <- as.numeric(tmin_dataset_daily[100])
#' 
#' o_cw <- hwmidcliva(x=tmin,timex=timex,cold=TRUE)
#' o_cw2 <- cwmidcliva(x=tmin,timex=timex)
#' 
#' @references 
#' 
#' Ceccherini, G., Russo, S., Ameztoy, I., Marchese, A. F., and Carmona-Moreno, C.
#' Heat waves in Africa 1981-2015, observations and reanalysis, 
#' Nat. Hazards Earth Syst. Sci., 17, 115--125, 2017  
#' \url{https://doi.org/10.5194/nhess-17-115-2017}
#' 
#' Ceccherini, G., Russo, S., Ameztoy, I., Romero, C. P., and Carmona-Moreno, C. 
#' Magnitude and frequency of heat and cold waves in recent decades: the case of South America, 
#' Nat. Hazards Earth Syst. Sci., 16, 821-831,2016  
#' \url{https://doi.org/10.5194/nhess-16-821-2016}
#' 
#' Forzieri, G., Feyen, L., Russo, S. et al. 
#' Multi-hazard assessment in Europe under climate change. 
#' Climatic Change 137, 105-119 (2016). 
#' \url{https://doi.org/10.1007/s10584-016-1661-x}
#' 
#' M. Smid, S. Russo, A.C. Costa, C. Granell, E. Pebesma,
#' Ranking European capitals by exposure to heat waves and cold waves,
#' Urban Climate,Volume 27,2019,Pages 388-402, ISSN 2212-0955,
#' \url{https://doi.org/10.1016/j.uclim.2018.12.010}.
#' (\url{https://www.sciencedirect.com/science/article/pii/S2212095518302700})
#' 
#' 




hwmidcliva <- function(x,timex,timex_sim=timex,return_vector=TRUE,cold=FALSE,start_month=1,...) {
  
  o <- NULL
  ###
  ## see hwmid 
  if (length(x)!=length(timex)) {
  
      msg <- sprintf("Mismatch between x (%d) and timex (%d)!",length(x),length(timex))
      stop(msg)
  }
  
  yTref <- year(timex[1])
  
  #### year can start from the month different from 1 ## EC 20240311
  start_x <- which(month(timex)==start_month)[1]
  end_x <- length(x)
  # print(start_x)
  # print(end_x)
  
  x <- x[start_x:end_x]
  timex <- timex[start_x:end_x]
  
  # print(head(x))
  # print(head(timex))
  Tref <- x
  
  ###
  Temp <- x[which(timex %in% timex_sim)]
  yTemp <- year(timex_sim[1])
  if (cold) { ## EC 20240312
    Tref <- -Tref
    Temp <- -Temp
    
    }
  o <- hwmid(yTref=yTref,Tref=Tref,yTemp=yTemp,Temp)
  ### PROCESS HWMID 
  if (return_vector) {
    o <- o$hwmid[,1] 
    names(o) <- yTemp+1:length(o)-1
  }
  ###
  return(o)
}


NULL
#'
#' @name hwmidcliva
#' @export
#' 
#' @rdname hwmidcliva
#' @aliases cwmidcliva
#' 
#' 
cwmidcliva <- function(...) {
  
  out <- hwmidcliva(...,cold=TRUE)
  return(out)
}


