

set.seed(10)

library(lmomPi) 
library(lmom)
library(data.table)
library(dplyr)
library(terracliva)

para <- c(0,90.0,11)
distrib <- "pe3"
n=30
nn=100
lmom <- list()
for (i in (1:nn)) {
  y <- runif(n)
  if (i==2) y2 <- y 
  v <- qua(para=para,f=y,distrib=distrib)
  if (i==1) v1 <- v 
  if (i==2) v2 <- v 
  lmom[[i]] <- samlmu(v)

}

 as.data.table(t(as.data.frame(lmom))) |> lmrd()





#####
##source("~/local/rpackages/jrc/terracliva/R/lmpdistcliva.R")
df <- expand.grid(a=1:nn,b=1:nn) |> dplyr::filter(b>a)
df$dist <- mapply(a=lmom[df$a],b=lmom[df$b],FUN=lmpdistcliva)

##lmompdistcliva




# 
# 
# yA <- runif(1000)
# valA <- qua(para=para,f=yA,distrib=distrib)
# yB <- runif(1000)
# valB <- qua(para=para,f=yB,distrib=distrib)
# 
# 
# lmomA <- samlmu(yA)
# lmomB <- samlmu(yB)
# 
# out <- expand.grid(a=1:nn,b=1:nn)(lmomA,lmomB)
# 
# lmomAl <- samlmu(yA,ratio=FALSE)
# lmomBl <- samlmu(yB,ratio=FALSE)
# 
# out <- lmpdistcliva(lmomAl,lmomBl) 
