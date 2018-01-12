str <- read.csv(file = "~/Documents/Imperial/STRAf dataset/STRAF_7817_clone_structure.csv")
#str <- read.csv(file = "~/Documents/Imperial/STRAf dataset/random_pop_test.csv")
str <- str[order(str$Population),] 
head(str)

# Edit to allow conversion toi Genind
for(i in 11:19) str[,i] <- as.character(floor(as.numeric(as.character(str[,i]))))

# genind
str.gen <- df2genind(str[,11:19], ploidy = 1)

# Mutation populations 
other(str.gen)$Mutation  <- pop(str.gen) <- str$Mutation
mu <- pop(str.gen)
mu <- addNA(mu)
mu

# Continent 
other(str.gen)$Continent  <- pop(str.gen) <- str$Continent
con <- pop(str.gen)

# Structure populations 
other(str.gen)$Population  <- pop(str.gen) <- str$Population



x <- scaleGen(str.gen, NA.method="mean")
x
class(x)

library(ggfortify)
df <- iris[c(1, 2, 3, 4)]
class(df)
autoplot(prcomp(df), , data = iris, colour = 'Species')

autoplot(prcomp(x), x = 1, y = 3, data = str, colour = 'Population')


dim(x)
pca1 <- dudi.pca(x, cent = FALSE, scale = FALSE, scannf = FALSE, nf = 3)
s.class(pca1$li, pop(str.gen), xax = 3, yax = 1)