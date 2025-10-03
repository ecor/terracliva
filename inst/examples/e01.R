

set.seed(20)

library(lmomPi) 
library(lmom)
library(data.table)
library(dplyr)
library(terracliva)

########
#> lmom[[1]]
#l_1         l_2         t_3         t_4 
#-16.1488460   0.2037098   0.8972081   0.7573139 
ll <- c(100,0.2037098,0.8972081,0.7573139) 
names(ll) <- c("l_1","l_2","t_3","t_4") 

########
########
########

#para <- c(0,90.0,11)
distrib <- "pe3"
para <- pel(distrib=distrib,lmom=ll)
n=40
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
### remove lmom ()
lmom <- lmom[which(sapply(lmom,are.lmoms.valid))]
####

 as.data.table(t(as.data.frame(lmom))) |> lmrd()





#####
source("~/local/rpackages/jrc/terracliva/R/lmpdistcliva.R")
df <- expand.grid(a=1:nn,b=1:nn) |> dplyr::filter(b>a)
df$dist <- mapply(a=lmom[df$a],b=lmom[df$b],FUN=lmpdistcliva)
df <- df |> arrange(dist)
pint=0.9
qint <- quantile(df$dist,prob=0.9)
df$fill <- "red"
df$fill[df$dist<=qint] <- "skyblue"

library(ggplot2)
#quardare qui:
#https://stackoverflow.com/questions/63289154/colour-segments-of-density-plot-by-bin


gg <- ggplot(df, aes(x = dist)) +
  geom_density(fill="skyblue",alpha = 0.6) +
  labs(title = "Density Plot del Campione",
       x = "Valori",
       y = "Densità")+geom_vline(xintercept=qint,color="red")+
  theme_minimal()

print(gg)












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
