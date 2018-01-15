#### Packages ####
if (!require("poppr")){
  install.packages("poppr", dep=TRUE)
  library(poppr)
}

if (!require("adegenet")){
  install.packages("adegenet", dep=TRUE)
  library(adegenet)
}

if (!require("ape")){
  install.packages("ape", dep=TRUE)
  library(ape)
}

if (!require("hierfstat")){
  install.packages("hierfstat", dep=TRUE)
  library(hierfstat)
}

if(!require("pegas")){
  install.packages("pegas", dep=TRUE)
  library(pegas)
}

if(!require("mmod")){
  install.packages("mmod", dep=TRUE)
  library(mmod)
}

if(!require("shiny")){
  install.packages("shiny", dep=TRUE)
  library(shiny)
}

if(!require("ggplot2")){
  install.packages("ggplot2", dep=TRUE)
  library(ggplot2)
}

if(!require("dplyr")){
  install.packages("dplyr", dep=TRUE)
  library(dplyr)
}

if(!require("ggmap")){
  install.packages("ggmap", dep=TRUE)
  library(ggmap)
}

if(!require("maps")){
  install.packages("maps", dep=TRUE)
  library(maps)
}

if(!require("mapproj")){
  install.packages("mapproj", dep=TRUE)
  library(mapproj)
}

if(!require("mapdata")){
  install.packages("mapdata", dep=TRUE)
  library(mapdata)
}

if(!require("maptools")){
  install.packages("maptools", dep=TRUE)
  library(maptools)
}

if(!require("sp")){
  install.packages("sp", dep=TRUE)
  library(sp)
}

if(!require("raster")){
  install.packages("raster", dep=TRUE)
  library(raster)
}

if(!require("dismo")){
  install.packages("dismo", dep=TRUE)
  library(dismo)
}

if(!require("ggtree")){
  install.packages("ggtree", dep=TRUE)
  library(ggtree)
}

if(!require("ggrepel")){
  install.packages("ggrepel", dep=TRUE)
  library(ggtree)
}

if(!require("reshape")){
  install.packages("reshape", dep=TRUE)
  library(reshape)
}

# run this line fooool

##### OK #####

str <- read.csv(file = "~/Documents/Imperial/R_folder/Shiny/STRAF/straf_db_shiny_only_2.csv")
#str <- read.csv(file = "~/Documents/Imperial/STRAf dataset/random_pop_test.csv")
#str <- str[order(str$Population),] 
head(str)

# Edit to allow conversion toi Genind
#for(i in 11:19) str[,i] <- as.character(floor(as.numeric(as.character(str[,i]))))

# genind
str.gen <- df2genind(str[,4:12], ploidy = 1)
str.gen
# Continent 
other(str.gen)$Continent  <- pop(str.gen) <- str$Continent
con <- pop(str.gen)

# Structure populations 
other(str.gen)$Population  <- pop(str.gen) <- str$Population

x <- scaleGen(str.gen, NA.method="mean")
x <- autoplot(prcomp(x), x = 3, y = 1, data = str, shape = "Source")
x
# snap clust! 
str.gen

# dapc
da <- dapc(str.gen, n.pca = 3, n.da = 1)
scatter(da)

## try function using k-means initialization
#grp.ini <- find.clusters(str.gen)
#grp.ini

## run EM algo
ah <- snapclust.choose.k(str.gen, max = 10, IC = "BIC")
plot(ah)

# snapclust
res <- snapclust(str.gen, k = 2, hybrids = TRUE)
class(res$group)

# combine snapclust group information into genind
other(str.gen)$snapclust  <- pop(str.gen) <- res$group

str.gen$other$snapclust

str <- cbind(as.data.frame(res$group), str)
test


# redo PCA
x <- scaleGen(str.gen, NA.method="mean")
x <- autoplot(prcomp(x), x = 3, y = 1, data = test, colour = "res$group", shape = "Source")
x


plot(res$proba)
names(res)
res$converged
res$n.iter

x <- as.data.frame(res$group)
colnames(x) <- "Population"
head(x)

t <- merge(x = x,  y = str, by = )



