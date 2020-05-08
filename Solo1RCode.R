
for(package in c('cluster', 'factoextra', 'fpc', 'NbClust',
                 'reshape', 'plyr',
                 'ggplot2', 'scales', 'grid', 'gridExtra', 'clValid')) {
  if(!require(package, character.only=TRUE)) {
    install.packages(package)
    library(package, character.only=TRUE)
  }
}

rm(package)

setwd("C:/Users/syurasek/OneDrive - Constellation Brands/Documents/Northwestern/PREDICT 450")

load("apphappyData.RData")
ls()
## [1] "apphappy.2.labs.frame" "apphappy.2.num.frame" ##

#Library will load the existing loaded package. 
#Require will install or update when the package is not in our repository

require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
library(GGally)
require(corrplot)
library(dplyr)


numdata <- apphappy.3.num.frame
numlab <- apphappy.3.labs.frame

###missing data###

backup <- numdata

random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}
q12.imp <- random.imp(numdata$q12)
q57.imp <- random.imp(numdata$q57)
numdata[c("q5r1")][is.na(numdata[c("q5r1")])] <- 2

numdata <- data.frame(numdata, q12.imp, q57.imp)

# Subset the data for attitudinal variables
colstart <- which(colnames(numdata) == 'q24r1')
colend <- which(colnames(numdata) == 'q26r17')
numdata.att = numdata[,c(colstart:colend)]
numlab.att = numlab [,c(colstart:colend)]

# Subset the data for nonattitudinal variables
numdata.nonatt = numdata[,c(-colstart:-colend)]
numlab.nonatt = numlab[,c(-colstart:-colend)]


# Count NA rows for attitudinal variables
inclna <- nrow(numdata.att) 
exclna <- nrow(na.omit(numdata.att))
print(paste0("Basis observations: ", inclna - exclna))

# Count NA rows for non-attitudinal variables
inclna1 <- nrow(numdata.nonatt) 
exclna1 <- nrow(na.omit(numdata.nonatt))
print(paste0("Non-basis observations: ", inclna1 - exclna1))

# Count NA rows for original dataset
inclna2 <- nrow(numdata) 
exclna2 <- nrow(na.omit(numdata))
print(paste0("Dataset observations: ", inclna2 - exclna2))

rm(apphappy.3.num.frame, apphappy.3.labs.frame, colstart, colend)

### EDA data
str(numdata.att)
summary(numdata.att)

numdatacorrelation <- cor(numdata.att)
corrplot(numdatacorrelation)

mcor <- cor(numdata.att)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black")

####Cluster tendency EDA###
#OPTION 1
library(factoextra)
set.seed(123)
km <- kmeans(numdata.att, 3)
fviz_cluster(list(data = numdata.att, cluster = km$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# Hierarchical clustering 
fviz_dend(hclust(dist(numdata.att)), k = 3, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = FALSE)

# Compute Hopkins statistic 
res <- get_clust_tendency(numdata.att, n = nrow(numdata.att)-1, graph = FALSE)
res$hopkins_stat

#OPTION 2 much quicker to run
res <- get_clust_tendency(newnumdata, 40, graph = TRUE)
# Hopskin statistic
res$hopkins_stat

# Visualize the dissimilarity matrix
print(res$plot)


###Variable selection###
#Q24 correlation heatmap
numq24<- subset(numdata.att, select=c("q24r1","q24r2","q24r3","q24r4","q24r5","q24r6","q24r7","q24r8","q24r9","q24r10","q24r11","q24r12"))

numq24correlation <- cor(numq24)
corrplot(numq24correlation)

mcor <- cor(numq24)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black")

corrplot(numq24correlation, method="shade", addCoef.col="black", 
         addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
         tl.srt=45, addcolorlabel="no", order="AOE",insig = "p-value")

#Q25 correlation heatmap
numq25<- subset(numdata.att, select=c("q25r1","q25r2","q25r3","q25r4","q25r5","q25r6","q25r7","q25r8","q25r9","q25r10","q25r11","q25r12"))

numq25correlation <- cor(numq25)
corrplot(numq25correlation)

mcor <- cor(numq25)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black")

corrplot(numq25correlation, method="shade", addCoef.col="black", 
         addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
         tl.srt=45, addcolorlabel="no", order="AOE",insig = "p-value")


#Q26 correlation heatmap
numq26<- subset(numdata.att, select=c("q26r3","q26r4","q26r5","q26r6","q26r7","q26r8","q26r9","q26r10","q26r11","q26r12","q26r13","q26r14","q26r15","q26r16","q26r17"))

numq26correlation <- cor(numq26)
corrplot(numq26correlation)

mcor <- cor(numq26)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black")

corrplot(numq26correlation, method="shade", addCoef.col="black", 
         addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
         tl.srt=45, addcolorlabel="no", order="AOE",insig = "p-value")

#new data set exclusing >50% correlation for clustering analysis
newnumdata <- select(numdata.att, -"q24r9", -"q24r11", -"q24r12", -"q25r2", -"q25r4", -"q26r18", -"q26r10",-"q26r14")
newnumdatacorrelation <- cor(newnumdata)
corrplot(newnumdatacorrelation)

mcor4 <- cor(newnumdata)
corrplot(mcor4, method="shade", shade.col=NA, tl.col="black")

corrplot(newnumdatacorrelation, method="shade", addCoef.col="black", 
         addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
         tl.srt=45, addcolorlabel="no", order="AOE",insig = "p-value")


str(numdata)
summary(numdata)
a=table(numdata$q1)
a
barplot(a, main="Age Groups", col=blues9)


b=table(numdata$q1,numdata$q24r1)
b
barplot(b, main="I try to keep up with technological developments by Age Group",col=blues9)

c=table(numdata$q1,numdata$q25r1)
c
barplot(c, main="I consider myself an opinion leader by Age Group",col=blues9)


#Cluster Basis Variables

plots <- as.data.frame(numsub[,-c(1)]) %>%
  gather() %>%                             # make key-value pairs
  ggplot(aes(value)) +                     # values for each variable on x-axis
  facet_wrap(~ key, scales = "free") +  
  geom_density() +                       # plot each as density
  theme(strip.text = element_text(size=5)) # shrink text size
plots                                      # print plots
corr_plots <- ggpairs(as.data.frame(numsub[,-c(1)]),                        # GGally::ggpairs to make correlation plots
                      lower = list(continuous = wrap("points", 
                                                     alpha = 0.3, size=0.1), # default point size too big-shrink & change alpha
                                   combo = wrap("dot", alpha = 0.4,size=0.2)
                      )
)
corr_plots                                 # print corr_plots 



####Clustering analysis method non-hierarchica vs hierarchical clustering
library(NbClust)
set.seed(1)
nc <- NbClust(newnumdata, min.nc=2, max.nc=10, method="kmeans")
table(nc$Best.n[1,])

nc$All.index # estimates for each number of clusters on 32 different metrics of model fit

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by Criteria")


#### try another one SAME AS ABOVE 
nbc.eu <- NbClust(newnumdata, 
                  min.nc=2, max.nc=10, distance='euclidean', 
                  method='kmeans', index='all')
table(nc$Best.n[1,])

plot1 <- fviz_nbclust(nbc.eu) + 
  ggtitle('NbClust Criterion\nK-means + Euclidean Distance')


#####three methods comparision#####
#Reference https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/#elbow-method

# Elbow method
fviz_nbclust(newnumdata, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(newnumdata, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(newnumdata, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#NbClust
NbClust(data = newnumdata, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")
##########################

#### clusteirng analysis validation 
#APN
#AD
#ADM
#FOM

# Stability measures #very memory intensive

library(clValid)

clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(newnumdata, nClust = 2:9, 
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(newnumdata, nClust = 2:9, clMethods = clmethods, 
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)


######################################
### Create a Kmeans clusters
#########################################
#OPTION1
# Compute k-means=2 
set.seed(123)
km.res <- kmeans(newnumdata, 2, nstart = 25)
head(km.res$cluster, 20)

# Visualize clusters using factoextra
fviz_cluster(km.res, newnumdata)

#silhouette analysis
sil <- silhouette(km.res$cluster, dist(newnumdata))
rownames(sil) <- rownames(newnumdata)
head(sil[, 1:2])

fviz_silhouette(sil)

neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]


#OPTION1.1
# Compute k-means=9 
set.seed(123)
km9.res <- kmeans(newnumdata, 9, nstart = 25)
head(km9.res$cluster, 20)

# Visualize clusters using factoextra
fviz_cluster(km9.res, newnumdata)

#silhouette analysis
sil9 <- silhouette(km9.res$cluster, dist(newnumdata))
rownames(sil9) <- rownames(newnumdata)
head(sil9[, 1:2])

fviz_silhouette(sil9)

neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]

fviz_dend(hclust(dist(newnumdata)), k = 9, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = FALSE)

#OPTION2
##Cluster 2
clusterresults <- kmeans(newnumdata,2)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare


newdf <- as.data.frame(clusterresults$cluster)

write.csv(newdf, file = "clusterresults.csv")

write.csv(newnumdata, file = "newnumdata.csv")


############################
### Create a dataset with the original data with the cluster info
### This will be useful for creating profiles for the clusters
#####################################33

newdf <- read.csv("clusterresults.csv")
combdata <- cbind(newnumdata,newdf,numdata$q1)
head(combdata)

require(reshape)
combdata <- rename(combdata, c(clusterresults.cluster="cluster"))
head(combdata)

aggregate(combdata,by=list(byvar=combdata$cluster), mean)

### Combining with original set
combdatalab <- cbind(numlab,newdf,numdata$q1)
head(combdatalab)
write.csv(combdatalab, file = "combdatalab.csv")

require(reshape)
combdatalab <- rename(combdatalab, c(clusterresults.cluster="cluster"))
head(combdatalab)

### visulization
df_means <- ddply(numlab, 
                  .(clusterresults$cluster), colwise(mean))

#q1, q11, q12, q48, q49, q56, q57
x<-with(numlab, table(clusterresults$cluster,q1))
barplot(x, main="Q1.Age Group by Segmentation",
          xlab="Age", col=c("darkblue","lightblue"),
          legend = rownames(x), beside=TRUE)
x

q12<-with(numlab, table(clusterresults$cluster,q12))
barplot(q12, main="Q12.Number of Free Apps Download by Segmentation",
        xlab="Percentage", col=c("darkblue","lightblue"),
        legend = rownames(q12), beside=TRUE)

q11<-with(numlab, table(clusterresults$cluster,q11))
barplot(q11, main="Q11.Number of Apps by Segmentation",
        xlab="Apps", col=c("darkblue","lightblue"),
        legend = rownames(q11), beside=TRUE)
q11

q48<-with(numlab, table(clusterresults$cluster,q48))
barplot(q48, main="Q48. Education by Segmentation",
        xlab="Education level", col=c("darkblue","lightblue"),
        legend = rownames(q48), beside=TRUE)

q49<-with(numlab, table(clusterresults$cluster,q49))
barplot(q49, main="Q49. Maritial Status by Segmentation",
        xlab="Marital Status", col=c("darkblue","lightblue"),
        legend = rownames(q49), beside=TRUE)

q54<-with(numlab, table(clusterresults$cluster,q54))
barplot(q54, main="Q54. Race by Segmentation",
        xlab="Race", col=c("darkblue","lightblue"),
        legend = rownames(q54), beside=TRUE)

q54

q56<-with(numlab, table(clusterresults$cluster,q56))
barplot(q56, main="Q56. Household Income ",
        xlab="Income", col=c("darkblue","lightblue"),
        legend = rownames(q56), beside=TRUE)
q56

q57<-with(numlab, table(clusterresults$cluster,q57))
barplot(q57, main="Q57. Gender by Segmentation",
        xlab="Gender", col=c("darkblue","lightblue"),
        legend = rownames(q57), beside=TRUE)
q57




###Clustering 9 analysis playing around

clusterresults9 <- kmeans(newnumdata,9)
names(clusterresults9)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare


newdf9 <- as.data.frame(clusterresults9$cluster)

combdata9 <- cbind(newnumdata,newdf9,numdata$q1, numlab)
head(combdata9)

require(reshape)
combdata9 <- rename(combdata9, c(clusterresults9.cluster="cluster"))
head(combdata9)

combdata9<-aggregate(combdata9,by=list(byvar=combdata9$cluster), mean)
head(combdata9)

### Combining with original set
combdatalab9 <- cbind(numlab,newdf9,numdata$q1)
head(combdatalab9)

require(reshape)
combdatalab9 <- rename(combdatalab9, c(clusterresults.cluster="cluster"))
head(combdatalab9)

q56_9<-with(numlab, table(clusterresults9$cluster,q56))
barplot(q56_9, main="Q56. Household Income ",
        xlab="Income", col=c("darkblue","lightblue", "green", "red","orange","purple","yellow","light green","dark green"),
        legend = rownames(q56), beside=TRUE)
