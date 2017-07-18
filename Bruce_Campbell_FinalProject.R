
## use read.csv2 since data fields are delimited by semicolons (;)

cls=c(rep("numeric",11),"integer")

wine.data <- read.csv2("FinalProject/winequality-red.csv")

#Specifying the data type via colClasses did not work - so we sapply as.numeric to convert the factor data to numeric.  We may want to convert some of the variables to factors later - but for visualization of the raw features we should use numeric type. 
wine.data[, c(1:12)] <- sapply(wine.data[, c(1:12)], as.numeric)

DF <- wine.data

for ( i in 1:(ncol(wine.data)-1))
{
  feature = wine.data[,i]
  featureName = names(wine.data)[i]
  boxplot(feature~DF$quality,xlab = "Quality", ylab = featureName, main = featureName)
}

#Heatmap
DFHeatMap <- DF[order(DF$quality),]
plot(DFHeatMap$quality)
DFHeatMap$quality <- NULL
dist.mat <- as.matrix( dist(scale(DFHeatMap,center = TRUE,scale = TRUE),method = "euclidian") )
heatmap(dist.mat)


### K-means clustering
DFOrdered <- DF[order(DF$quality),] 

# Determine number of clusters. Here we calculate the 
wss <- (nrow(DFOrdered)-1)*sum(apply(DFOrdered,2,var))
for (i in 2:12)
{
  wss[i] <- sum(kmeans(DFOrdered, centers=i)$withinss)
}

plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title("Sum of square distance to cluster centrod by cluster number \n We seek a kink in this plot to identify the correct number of clusters \n k in [3,8] seems like a good choice.")

# Run k-means with k=6
km.out =kmeans (DF,6, nstart =20)

cluster_kmean.label <- km.out$cluster

boxplot(cluster_kmean.label~DF$quality,xlab = "cluster", ylab = "quality", main = "Quality by Cluster number for k=6 in K-means")

TB <- table(cluster_kmean.label, DF$quality)
library(pander)
pander(TB, caption="Cluster matchup by quality for kmeans k=6.  This is NOT a confusion matrix.  The cluster numbers are not indentified with quality.")

# Run k-means with k=3
km.out =kmeans (DF,3, nstart =20)

cluster_kmean.label <- km.out$cluster

quality_cat <- (ifelse(wine.data$quality<5,1,ifelse(wine.data$quality>6,3,2)))
hist(cluster_kmean.label)
boxplot(cluster_kmean.label~quality_cat,xlab = "cluster", ylab = "coase quality", main = "Quality by Cluster number for k=3 in K-means")

boxplot(cluster_kmean.label~quality_cat)

TB <- table(cluster_kmean.label, DF$quality)
library(pander)
pander(TB, caption="Cluster matchup by quality for kmeans k=3.  This is NOT a confusion matrix.  The cluster numbers are not indentified with quality.")

# Run k-means with k=4
km.out =kmeans (DF,4, nstart =20)

cluster_kmean.label <- km.out$cluster

TB <- table(cluster_kmean.label, DF$quality)
library(pander)
pander(TB, caption="Cluster matchup by quality for kmeans k=4.  This is NOT a confusion matrix.  The cluster numbers are not indentified with quality.")


##---------------------SVM
We split into training and test sets for now.  Notice that there are very few points in the extremes of quality.  There are arguments for and against splitting the extremes.  The features for those sample points could help or hurt a classifyer in the middle ranges of quality.  It's standard to split the data into training and test sets, but we'd like to reserve the right to revisit how we handle the classes with low counts at the end of the analysis.    

#This is only run once - we shared with the group so we're all using the same data for training and test
#train <- sample(nrow(DF), floor(nrow(DF)* 2/3))

#write.csv(train,file = "train.sample.csv",row.names = FALSE ,quote = FALSE)

#Check that it works 
train <- as.integer(unlist(read.csv("train.sample.csv",header = TRUE)))

DFTrain <-DF[train,]

DFTest <-DF[-train,]

TDTrain <- table(DFTrain$quality)

TDTest <- table(DFTest$quality)

ratio.class <- TDTrain / TDTest

library(pander)
pander(TDTrain, caption = "Distribution of quality for training sample")

pander(TDTest, caption = "Distribution of quality for test sample")

### LDA 
library (MASS)
lda.fit=lda(quality~. ,data=DFTrain)
lda.fit
lda.pred=predict (lda.fit , DFTest)
lda.class =lda.pred$class
TD <- table(lda.class ,DFTest$quality)
pander(TD)
ACC_lda = (sum(diag(as.array(TD))))/ length(DFTest$quality)

DFCoarse <-DF
DFCoarse$quality_cat <- ifelse(wine.data$quality<5,'LOW',ifelse(wine.data$quality>6,'HIGH','MEDIUM'))
DFCoarse$quality <-NULL
DFCoarseTrain <-DFCoarse[train,]
DFCoarseTest <-DFCoarse[-train,] 

lda.fit=lda(quality_cat~. ,data=DFCoarseTrain)
lda.fit
lda.pred=predict (lda.fit , DFCoarseTest)
lda.class =lda.pred$class
TD <- table(lda.class ,DFCoarseTest$quality)
pander(TD)
ACC_lda_coarse = (sum(diag(as.array(TD))))/ length(DFTest$quality)

#We do a logistic classification for comparison
DFCoarse <-DF
DFCoarse$quality_cat <- ifelse(wine.data$quality<=5,0,1)
DFCoarse$quality <-NULL
DFCoarseTrain <-DFCoarse[train,]
DFCoarseTest <-DFCoarse[-train,] 

glm.fit <- glm(quality_cat~.,data =DFCoarseTrain,family=binomial )
summary(glm.fit)
glm.probs =predict (glm.fit ,DFCoarseTest,type ="response")

glm.pred=rep (0 ,nrow(DFCoarseTest))
glm.pred[glm.probs >.5]=1
TB <- table(glm.pred ,DFCoarseTest$quality_cat)
pander(TB,caption="Logistic Regression")
ACC_glm_binomial = (TB[1]+TB[4])/ length(DFCoarseTest$quality_cat)
Specificity = TB[1]/sum(DFCoarseTest$quality_cat ==0)
Sensitivity = TB[4]/sum(DFCoarseTest$quality_cat ==1)

#### We require a categorical response for the e1071 package so the data frame for the 2 class SVM is regenerated here

DFCoarse <-DF
DFCoarse$quality_cat <- as.factor(ifelse(wine.data$quality<=5,'LOW','HIGH'))
DFCoarse$quality <-NULL
DFCoarseTrain <-DFCoarse[train,]
DFCoarseTest <-DFCoarse[-train,] 

library (e1071)

#Linear SVM
tune.svm=tune(svm,quality_cat~., data=DFCoarseTrain , kernel ="linear",ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
svm.fit <-tune.svm$best.model
svm.pred <- predict(svm.fit, DFCoarseTest)
TB <- table(svm.pred ,DFCoarseTest$quality_cat )
pander(TB)
ACC_Linear_SVM = (TB[1]+TB[4])/ length(DFCoarseTest$quality_cat)

plot(svm.fit,DFCoarseTrain,fixed.acidity~sulphates)
plot(svm.fit,DFCoarseTrain,density~volatile.acidity)

gamma_default <- 1/(ncol(DFCoarseTrain)-1)
gamma_list <-((1:10)/5)*gamma_default

#RBF SVM
tune.svm=tune(svm,quality_cat~., data=DFCoarseTrain , kernel ="radial",ranges =list(gamma=gamma_list,cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
svm.fit <-tune.svm$best.model
svm.pred <- predict(svm.fit, DFCoarseTest)
TB <- table(svm.pred ,DFCoarseTest$quality_cat )
pander(TB)
ACC_Radial_SVM = (TB[1]+TB[4])/ length(DFCoarseTest$quality_cat)
#Beware - these plots may have a bug for the non linear svm!
plot(svm.fit,DFCoarseTrain,fixed.acidity~sulphates)
plot(svm.fit,DFCoarseTrain,density~volatile.acidity)

#Polynomial SVM
tune.svm=tune(svm,quality_cat~., data=DFCoarseTrain , kernel ="polynomial",ranges =list(gamma=gamma_list,cost=c(0.001 , 0.01, 0.1, 1,5,10,100),degree=c(2,3,4) ))
svm.fit <-tune.svm$best.model
svm.pred <- predict(svm.fit, DFCoarseTest)
TB <- table(svm.pred ,DFCoarseTest$quality_cat )
pander(TB)
ACC_Polynomial_SVM = (TB[1]+TB[4])/ length(DFCoarseTest$quality_cat)
#Beware - these plots may have a bug for the non linear svm!
plot(svm.fit,DFCoarseTrain,fixed.acidity~sulphates)
plot(svm.fit,DFCoarseTrain,density~volatile.acidity)

# nu-svm
tune.svm=tune(svm,quality_cat~., data=DFCoarseTrain , kernel ="radial",type="nu-classification",ranges =list(gamma=gamma_list,cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
svm.fit <-tune.svm$best.model
svm.pred <- predict(svm.fit, DFCoarseTest)
TB <- table(svm.pred ,DFCoarseTest$quality_cat )
pander(TB)
ACC_Radial_nuSVM = (TB[1]+TB[4])/ length(DFCoarseTest$quality_cat)
#Beware - these plots may have a bug for the non linear svm!
plot(svm.fit,DFCoarseTrain,fixed.acidity~sulphates)
plot(svm.fit,DFCoarseTrain,density~volatile.acidity)

#One Versus All for three level classification.  We learned that e1071 now provides direct multiclass classification.
#We leave the code here for now.
# DFCoarse <-DF
# DFCoarse$quality_cat_low_vs_med_high <- as.factor(ifelse(wine.data$quality<5,'LOW','MED_HIGH'))
# DFCoarse$quality_cat_high_vs_low_med <- as.factor(ifelse(wine.data$quality>6,'HIGH','LOW_MED'))
# DFCoarse$quality_cat_med_vs_low_high <- as.factor(ifelse(( wine.data$quality==6 | wine.data$quality==5),'MED','LOW_HIGH'))
# 
# DFCoarse$quality <-NULL
# DFCoarseTrain <-DFCoarse[train,]
# DFCoarseTest <-DFCoarse[-train,] 
# 
# gamma_default <- 1/(ncol(DFCoarseTrain)-1)
# 
# gamma_list <-((1:10)/5)*gamma_default
# cost_list=c(0.001 , 0.01, 0.1, 1,5,10,100)
# 
# #Overrides for fast development change back!
# gamma_list<-c(.17)
# cost_list<-c(.1)
# 
# #LOW VS REST
# tune.svm.quality_cat_low_vs_med_high=tune(svm,quality_cat_low_vs_med_high~., data=DFCoarseTrain , kernel ="radial",ranges =list(gamma=gamma_list,cost=cost_list ),type="nu-classification")
# svm.fit.quality_cat_low_vs_med_high <-tune.svm.quality_cat_low_vs_med_high$best.model
# 
# #MED VS REST
# tune.svm.quality_cat_med_vs_low_high=tune(svm,quality_cat_med_vs_low_high~., data=DFCoarseTrain , kernel ="radial",ranges =list(gamma=gamma_list,cost=cost_list ),type="nu-classification")
# svm.fit.quality_cat_med_vs_low_high<-tune.svm.quality_cat_med_vs_low_high$best.model
# 
# #HIGH VS REST
# tune.svm.quality_cat_high_vs_low_med=tune(svm,quality_cat_high_vs_low_med~., data=DFCoarseTrain , kernel ="radial",ranges =list(gamma=gamma_list,cost=cost_list ),type="nu-classification")
# svm.fit.quality_cat_high_vs_low_med<-tune.svm.quality_cat_high_vs_low_med$best.model

#svm.pred.quality_cat_low_vs_med_high <- predict(svm.fit.quality_cat_low_vs_med_high, DFCoarseTest)
#svm.pred.quality_cat_med_vs_low_high <- predict(svm.fit.quality_cat_med_vs_low_high, DFCoarseTest)
#svm.pred.quality_cat_high_vs_low_med <- predict(svm.fit.quality_cat_high_vs_low_med, DFCoarseTest)


DFCoarse <-DF
DFCoarse$quality_cat <- factor(ifelse(wine.data$quality<5,'LOW',ifelse(wine.data$quality>6,'HIGH','MED')))
DFCoarse$quality <-NULL
DFCoarseTrain <-DFCoarse[train,]
DFCoarseTest <-DFCoarse[-train,]

gamma_default <- 1/(ncol(DFCoarseTrain)-1)

gamma_list <-((1:10)/5)*gamma_default
cost_list=c(0.001 , 0.01, 0.1, 1,5,10,100)

tune.svm.quality_cat=tune(svm,quality_cat~., data=DFCoarseTrain , kernel ="polynomial",ranges =list(gamma=gamma_list,cost=cost_list ))
svm.fit.quality_cat <-tune.svm.quality_cat$best.model

svm.pred.quality_cat <- predict(svm.fit.quality_cat, DFCoarseTest)
TB <- table(svm.pred.quality_cat ,DFCoarseTest$quality_cat )
pander(TB)

ACC_Multiclass = (sum(diag(TB)))/ length(DFCoarseTest$quality_cat)


#Display SVM Results
method.accuracy <- data.frame(method = c("Multiclass SVM 3 class","svm rbf nu 2 class","lda 3 class","lda 6 class","glm 2 class","linear svm 2 class","rbf svm 2 class","polynomial svm 2 class"), accuracy=c(ACC_Multiclass, ACC_Radial_nuSVM,ACC_lda_coarse,ACC_lda,ACC_glm_binomial,ACC_Linear_SVM,ACC_Radial_SVM,ACC_Polynomial_SVM))
method.accuracy <- method.accuracy[order(method.accuracy$accuracy,decreasing = TRUE),]
pander(method.accuracy, caption = "Accuracy by Method")

#Multiclass Accuracy By Class
ACC_Class_HIGH = diag(TB)[1] / sum(DFCoarseTest$quality_cat=='HIGH')
ACC_Class_HIGH

ACC_Class_LOW = diag(TB)[2] / sum(DFCoarseTest$quality_cat=='LOW')
ACC_Class_LOW

ACC_Class_MED = diag(TB)[3] / sum(DFCoarseTest$quality_cat=='MED')
ACC_Class_MED

pander(data.frame(class=c("LOW","MED","HIGH"),accuracy= c(ACC_Class_LOW,ACC_Class_MED,ACC_Class_HIGH)),caption = "Multiclass SVM accuracy by class")



### Now we train a reduced model choosing the significant variables from the glm
DFCoarse <-DF[,c("fixed.acidity","volatile.acidity","chlorides","total.sulfur.dioxide","density","sulphates","alcohol")]
DFCoarse$quality_cat <- factor(ifelse(wine.data$quality<5,'LOW',ifelse(wine.data$quality>6,'HIGH','MED')))
DFCoarse$quality <-NULL
DFCoarseTrain <-DFCoarse[train,]
DFCoarseTest <-DFCoarse[-train,]

tune.svm.quality_cat=tune(svm,quality_cat~., data=DFCoarseTrain , kernel ="polynomial",ranges =list(gamma=gamma_list,cost=cost_list ))
svm.fit.quality_cat <-tune.svm.quality_cat$best.model

svm.pred.quality_cat <- predict(svm.fit.quality_cat, DFCoarseTest)
TB <- table(svm.pred.quality_cat ,DFCoarseTest$quality_cat )
pander(TB)

ACC_Multiclass_reduced = (sum(diag(TB)))/ length(DFCoarseTest$quality_cat)

ACC_Class_HIGH = diag(TB)[1] / sum(DFCoarseTest$quality_cat=='HIGH')
ACC_Class_HIGH

ACC_Class_LOW = diag(TB)[2] / sum(DFCoarseTest$quality_cat=='LOW')
ACC_Class_LOW

ACC_Class_MED = diag(TB)[3] / sum(DFCoarseTest$quality_cat=='MED')
ACC_Class_MED

pander(data.frame(class=c("LOW","MED","HIGH"),accuracy= c(ACC_Class_LOW,ACC_Class_MED,ACC_Class_HIGH)),caption = "Multiclass SVM accuracy by class")


