# This package and the analysis provided below contains the data and analysis performed in the manuscript titled:
# "Shotgun mass spectrometry-based lipid profiling identifies and distinguishes between chronic inflammatory diseases"

# install R packages
install.packages("ggplot2)
install.packages("pROC")
install.packages("remotes")
install.packages("limma")
install.packages("ggpubr")
install.packages("caret")
install.packages("sampling")
library(remotes)

remotes::install_github("ruma1974/lipidProfillingRM@master")


# lib
library(ggplot2)
library(lipidProfillingRM)
library(pROC)
library(limma)
library(ggpubr)
library(caret)
library(sampling)

# start

str(Pheno)
names(Pheno) 
Pheno$Type3


# below code calculates ks_test comparing Age in disease groups versus control group using different age thresholds for control group

# stroke
lipidProfillingRM::plotKSpvalues(Range=25:70, DF=Pheno, colNameControl="Type", conGroup="control", altGroup="Stroke",colNameNumeric="Age", Above=TRUE, main="Stroke: Threshold for age distribution",xlab="Age in years")
# CVD
lipidProfillingRM::plotKSpvalues(Range=25:70, DF=Pheno, colNameControl="Type3", conGroup="control", altGroup="CVD",colNameNumeric="Age", Above=TRUE, main="CVD: Threshold for age distribution",xlab="Age in years")
# SLE
lipidProfillingRM::plotKSpvaluesLowHigh(Range=25:70, DF=Pheno, colNameControl="Type3", conGroup="control", altGroup="LUPUS",colNameNumeric="Age", main="SLE: Threshold for age distribution",xlab="Age in years")


# set thressholds
Th <- 50; 
idxL1 <- which(Pheno$Age<Th & Pheno$Type=="control")
idxL2 <- which(Pheno$Age>=Th & Pheno$Type=="control")

Pheno$Type50 <- Pheno$Type
Pheno$Type50[idxL1] <- "Control_y"
Pheno$Type50[idxL2] <- "Control_o"

Th=55
idxL1 <- which(Pheno$Age<Th & Pheno$Type=="control")
idxL2 <- which(Pheno$Age>=Th & Pheno$Type=="control")

Pheno$Type55 <- Pheno$Type
Pheno$Type55[idxL1] <- "Control_y"
Pheno$Type55[idxL2] <- "Control_o"


# Test statistical associations
DF=Pheno
t.test(Age ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD1",])
t.test(Age ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD2",])
t.test(Age ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD3",])
t.test(Age ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD4",])
t.test(Age ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD5",])

# BMI
t.test(BMI ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD1",])
t.test(BMI ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD2",])
t.test(BMI ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD3",])
t.test(BMI ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD4",])
t.test(BMI ~ Type50, data = DF[DF$Type50=="Control_o" | DF$Type50=="CVD5",])


# PCA Figure 1 in manuscript
# set NAs to half min
Ex[is.na(Ex)] <- min(Ex, na.rm = TRUE)/2
ExN <- preprocessCore::normalize.quantiles(as.matrix(Ex))


# PCA CVD1
ExNlog <- log2(ExN + 1)

ExNlog_c <- ExNlog - rowMeans(ExNlog)

idxL <- which(grepl("Control_o",Pheno$Type50) | grepl("CVD1",Pheno$Type50))

svd1 <- svd(ExNlog_c[,idxL])

Lmain <- paste("CVD1 vs Control","\n n= ",length(idxL))
Pvar <- svd1$d^2/sum(svd1$d^2)*100
Xlab <- paste0("1st PC: ",format(Pvar[1],digits = 2, nsmall = 2),"%")
Ylab <- paste0("2nd PC: ",format(Pvar[2],digits = 2, nsmall = 2),"%")


type <- as.factor(Pheno$Type[idxL])
DF <- data.frame(PC1=svd1$v[,1],PC2=svd1$v[,2],type=type)
p <- ggplot2::ggplot(DF, ggplot2::aes(PC1, PC2)) + ggplot2::geom_point(ggplot2::aes(PC1, PC2, colour = type), size = 2.5)
p <- p + ggplot2::labs(x = Xlab, y = Ylab, title = Lmain, caption = "", subtitle = "")
x11();p


# PCA CVD2
ExNlog <- log2(ExN + 1)

ExNlog_c <- ExNlog - rowMeans(ExNlog)

idxL <- which(grepl("Control_o",Pheno$Type50) | grepl("CVD2",Pheno$Type50))

svd1 <- svd(ExNlog_c[,idxL])

Lmain <- paste("CVD2 vs Control","\n n= ",length(idxL))
Pvar <- svd1$d^2/sum(svd1$d^2)*100
Xlab <- paste0("1st PC: ",format(Pvar[1],digits = 2, nsmall = 2),"%")
Ylab <- paste0("2nd PC: ",format(Pvar[2],digits = 2, nsmall = 2),"%")


type <- as.factor(Pheno$Type[idxL])
DF <- data.frame(PC1=svd1$v[,1],PC2=svd1$v[,2],type=type)
p <- ggplot2::ggplot(DF, ggplot2::aes(PC1, PC2)) + ggplot2::geom_point(ggplot2::aes(PC1, PC2, colour = type), size = 2.5)
p <- p + ggplot2::labs(x = Xlab, y = Ylab, title = Lmain, caption = "", subtitle = "")
x11();p

# PCA CVD3
ExNlog <- log2(ExN + 1)

ExNlog_c <- ExNlog - rowMeans(ExNlog)

idxL <- which(grepl("Control_o",Pheno$Type50) | grepl("CVD3",Pheno$Type50))

svd1 <- svd(ExNlog_c[,idxL])

Lmain <- paste("CVD3 vs Control","\n n= ",length(idxL))
Pvar <- svd1$d^2/sum(svd1$d^2)*100
Xlab <- paste0("1st PC: ",format(Pvar[1],digits = 2, nsmall = 2),"%")
Ylab <- paste0("2nd PC: ",format(Pvar[2],digits = 2, nsmall = 2),"%")


type <- as.factor(Pheno$Type[idxL])
DF <- data.frame(PC1=svd1$v[,1],PC2=svd1$v[,2],type=type)
p <- ggplot2::ggplot(DF, ggplot2::aes(PC1, PC2)) + ggplot2::geom_point(ggplot2::aes(PC1, PC2, colour = type), size = 2.5)
p <- p + ggplot2::labs(x = Xlab, y = Ylab, title = Lmain, caption = "", subtitle = "")
x11();p


# PCA CVD4
ExNlog <- log2(ExN + 1)

ExNlog_c <- ExNlog - rowMeans(ExNlog)

idxL <- which(grepl("Control_o",Pheno$Type50) | grepl("CVD4",Pheno$Type50))

svd1 <- svd(ExNlog_c[,idxL])

Lmain <- paste("CVD4 vs Control","\n n= ",length(idxL))
Pvar <- svd1$d^2/sum(svd1$d^2)*100
Xlab <- paste0("1st PC: ",format(Pvar[1],digits = 2, nsmall = 2),"%")
Ylab <- paste0("2nd PC: ",format(Pvar[2],digits = 2, nsmall = 2),"%")


type <- as.factor(Pheno$Type[idxL])
DF <- data.frame(PC1=svd1$v[,1],PC2=svd1$v[,2],type=type)
p <- ggplot2::ggplot(DF, ggplot2::aes(PC1, PC2)) + ggplot2::geom_point(ggplot2::aes(PC1, PC2, colour = type), size = 2.5)
p <- p + ggplot2::labs(x = Xlab, y = Ylab, title = Lmain, caption = "", subtitle = "")
x11();p

# PCA CVD5
ExNlog <- log2(ExN + 1)

ExNlog_c <- ExNlog - rowMeans(ExNlog)

idxL <- which(grepl("Control_o",Pheno$Type50) | grepl("CVD5",Pheno$Type50))

svd1 <- svd(ExNlog_c[,idxL])

Lmain <- paste("CVD5 vs Control","\n n= ",length(idxL))
Pvar <- svd1$d^2/sum(svd1$d^2)*100
Xlab <- paste0("1st PC: ",format(Pvar[1],digits = 2, nsmall = 2),"%")
Ylab <- paste0("2nd PC: ",format(Pvar[2],digits = 2, nsmall = 2),"%")


type <- as.factor(Pheno$Type[idxL])
DF <- data.frame(PC1=svd1$v[,1],PC2=svd1$v[,2],type=type)
p <- ggplot2::ggplot(DF, ggplot2::aes(PC1, PC2)) + ggplot2::geom_point(ggplot2::aes(PC1, PC2, colour = type), size = 2.5)
p <- p + ggplot2::labs(x = Xlab, y = Ylab, title = Lmain, caption = "", subtitle = "")
x11();p

#- PCA IS
ExNlog <- log2(ExN + 1)

ExNlog_c <- ExNlog - rowMeans(ExNlog)

idxL <- which(grepl("Control_o",Pheno$Type55) | grepl("Stroke",Pheno$Type55))

svd1 <- svd(ExNlog_c[,idxL])

Lmain <- paste("IS vs Control","\n n= ",length(idxL))
Pvar <- svd1$d^2/sum(svd1$d^2)*100
Xlab <- paste0("1st PC: ",format(Pvar[1],digits = 2, nsmall = 2),"%")
Ylab <- paste0("2nd PC: ",format(Pvar[2],digits = 2, nsmall = 2),"%")

type <- as.factor(Pheno$Type[idxL])
DF <- data.frame(PC1=svd1$v[,1],PC2=svd1$v[,2],type=type)
p <- ggplot2::ggplot(DF, ggplot2::aes(PC1, PC2)) + ggplot2::geom_point(ggplot2::aes(PC1, PC2, colour = type), size = 2.5)
p <- p + ggplot2::labs(x = Xlab, y = Ylab, title = Lmain, caption = "", subtitle = "")
x11();p


#- limma MODEL correct for age, sex and statin use. Only code for CVD versus control is listed. The code need to be repeated for each comparison in table S3
# For SLE data the models were made with correction for Sex and Age
Pheno$Type
Pheno$Type2
Pheno$Type3
Pheno$StatinType

names(Fea)[1] <- "Lipid"
names(Fea)

AddModelPheno <- c("Sex","Statin.Use","Age")

ExNlog <- log2(ExN + 1)
P <- Pheno
rownames(ExNlog) <- Fea[[1]]

# subset data
idxL <- which(P[[PhenoName]]=="CVD" | P[[PhenoName]]=="control")
P <- P[idxL,,drop=FALSE]
ExNlog <- ExNlog[,idxL,drop=FALSE]
# Correct for other factors
PhenoL <- c("Type3","Sex","Statin.Use","Age")
P <- P[,PhenoL,drop=FALSE]
# P has a na?
idxL <- complete.cases(P)
P <- P[idxL,,drop=FALSE]
ExNlog <- ExNlog[,idxL]
# build model
mod <- stats::model.matrix(~0+Type3+Sex+Statin.Use+Age,data=P) # 

fit_limma <- limma::lmFit(ExNlog,mod)

cm <- limma::makeContrasts(contrasts=paste("Type3CVD","-","Type3control"),levels=mod)	
tmp <- limma::contrasts.fit(fit_limma, cm)
tmp <- limma::eBayes(tmp)
resLimma <- limma::topTable(tmp,coef=1, sort.by = "P", n = Inf)
resLimma

# LDA plot

# Figure 4a
DF <- Ex
DF <- as.data.frame(t(DF))
DF$fac <- Pheno$cohort
# filter low abundant
idxL <- which(unname(colSums(DF[, -ncol(DF)])) < 30)
DF <- DF[, -idxL]

# create supervised lda model
DF.lda <- MASS::lda(fac ~ ., data = DF)
DF.lda.values <- kernlab::predict(DF.lda)
newdata <- data.frame(type = DF.lda.values$class, lda = DF.lda.values$x)
# plot figure 4a
p <- ggplot2::ggplot(newdata, ggplot2::aes(lda.LD1, lda.LD2)) + ggplot2::geom_point(ggplot2::aes(lda.LD1, lda.LD2, colour = type), size = 2.5)
p <- p+scale_color_manual(values=c("#000000", "#FF0000", "#0000FF","#00FF00", "#C0C0C0", "#800000","#FF00FF", "#00FFFF"))
p <- p + ggplot2::theme_bw()
x11();p

# Figure 4b - control cvd
DF <- Ex
DF <- as.data.frame(t(DF))
DF$fac <- Pheno$cohort
# select groups
DF <- DF[DF$fac=="Control" | DF$fac=="CVD",]
# filter low abundant
idxL <- which(unname(colSums(DF[, -ncol(DF)])) < 30)
DF <- DF[, -idxL]
# create supervised lda model
DF.lda <- MASS::lda(fac ~ ., data = DF)
DF.lda.values <- kernlab::predict(DF.lda)
newdata <- data.frame(type = DF.lda.values$class, lda = DF.lda.values$x)
# plot figure 4b
p <- ggplot2::ggplot(newdata, ggplot2::aes(lda.LD1, lda.LD2)) + ggplot2::geom_point(ggplot2::aes(lda.LD1, lda.LD2, colour = type), size = 2.5)
p <- p+scale_color_manual(values=c("#000000", "#FF0000", "#0000FF","#00FF00", "#C0C0C0", "#800000"))
p <- p + ggplot2::theme_bw()
x11();p

# Figure 4c - control lupus
DF <- Ex
DF <- as.data.frame(t(DF))
DF$fac <- Pheno$cohort
# select groups
DF <- DF[DF$fac=="Control" | DF$fac=="Lupus",]
# filter low abundant
idxL <- which(unname(colSums(DF[, -ncol(DF)])) < 50)
fDF <- DF[, -idxL]

fDFcor <- stats::cor(fDF[, -ncol(fDF)])
highlyCorDescr <- caret::findCorrelation(fDFcor, cutoff = 0.9)
fDFfcor <- fDF[, -highlyCorDescr]
DF.lda <- MASS::lda(fac ~ ., data = fDFfcor)
DF.lda.values <- kernlab::predict(DF.lda)
newdata <- data.frame(type = DF.lda.values$class, lda = DF.lda.values$x)
names(newdata)

p <- ggpubr::ggboxplot(newdata, x = "type", y = "LD1", color = "type", palette = c("red", "green"), add = "jitter")
p <- p + ggpubr::stat_compare_means(method = "t.test", paired = FALSE, size = 8)
x11();p

# Figure 4D - control stroke

DF <- Ex
DF <- as.data.frame(t(DF))
DF$fac <- Pheno$cohort
# select groups
DF <- DF[DF$fac=="Control" | DF$fac=="Stroke",]
# filter low abundant
idxL <- which(unname(colSums(DF[, -ncol(DF)])) < 50)
fDF <- DF[, -idxL]

fDFcor <- stats::cor(fDF[, -ncol(fDF)])
highlyCorDescr <- caret::findCorrelation(fDFcor, cutoff = 0.9)
fDFfcor <- fDF[, -highlyCorDescr]
DF.lda <- MASS::lda(fac ~ ., data = fDFfcor)
DF.lda.values <- kernlab::predict(DF.lda)
newdata <- data.frame(type = DF.lda.values$class, lda = DF.lda.values$x)
names(newdata)

p <- ggpubr::ggboxplot(newdata, x = "type", y = "LD1", color = "type", palette = c("red", "green"), add = "jitter")
p <- p + ggpubr::stat_compare_means(method = "wilcox.test", paired = FALSE, size = 8)
x11();p

## PLS model CVD vs control

set.seed(1)
Pheno$cohort <- Pheno$Type50
Pheno$cohort[grep("CVD",es[["Pheno"]][[c("cohort")]])]="CVD"
DF <- Ex

rownames(DF) <- Fea[, 1]
DF <- as.data.frame(t(DF))
DF$fac <- Pheno$cohort
# filter
DF <- DF["Control_o"==DF$fac | "CVD"==DF$fac, ]
table(DF$fac)

# remove zero variance
nzv <- caret::nearZeroVar(DF, saveMetrics = FALSE)
fDF <- DF[, -nzv]
# remove highly correlated
CorCut <- 0.75
fDFcor <- stats::cor(fDF[, -ncol(fDF)])
highlyCorDescr <- caret::findCorrelation(fDFcor, cutoff = CorCut)

fDFfcor <- fDF[, -highlyCorDescr]
DF <- fDF
DF$fac <- factor(DF$fac,levels=c("Control_o","CVD"))
table(DF$fac)*0.25

# training and test data    
dfL <- split(DF, DF$fac)
selSize <- round(min(unlist(lapply(dfL, nrow))) * 0.75)
x <- sampling::strata(DF, "fac", size = rep(selSize, length(dfL)), method = "srswor")
inTraining <- x$ID_unit
training <- DF[inTraining, ]
testing <- DF[-inTraining, ]

# PLS model
fitControl <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = T)
gpls <- caret::train(fac ~ ., data = training, method = "pls", trControl = fitControl, metric = "Accuracy", tuneLength = 20, preProc = c("zv", "center", "scale"), verbose = TRUE)

# Figure 5b - plot importance
plot(varImp(gpls), 20, main = "PLS")

# Figure 6b confusion matrix 
caret::confusionMatrix(table(predict(gpls, testing), testing$fac))

# Figure 5a - factor
p <- ggplot2::ggplot(gpls)
p <- ggplot2::ggplot(gpls)
p <- p + ggplot2::geom_vline(xintercept = as.numeric(gpls$bestTune), linetype = "dashed", color = "red", size = 2)
p <- p + ggplot2::theme_bw()
x11();p

# Figure 6a - ROC
obj <- roc(testing$fac,predict(gpls, newdata = testing,type="prob")$Control, direction=">", ci=TRUE)
RocWith_ci(obj)

## PLS models stroke 
set.seed(1)

Pheno$Type55

DF <- Ex

rownames(DF) <- Fea[, 1]
DF <- as.data.frame(t(DF))
DF$fac <- Pheno$Type55
# filter

DF <- DF["Stroke"==DF$fac | "Control_o"==DF$fac, ]
table(DF$fac)

# remove zero variance
nzv <- caret::nearZeroVar(DF, saveMetrics = FALSE)
fDF <- DF[, -nzv]
# remove highly correlated
CorCut <- 0.75
fDFcor <- stats::cor(fDF[, -ncol(fDF)])
highlyCorDescr <- caret::findCorrelation(fDFcor, cutoff = CorCut)
fDFfcor <- fDF[, -highlyCorDescr]
# training and test data    
dfL <- split(DF, DF$fac)
selSize <- round(min(unlist(lapply(dfL, nrow))) * 0.75)
x <- sampling::strata(DF, "fac", size = rep(selSize, length(dfL)), method = "srswor")
inTraining <- x$ID_unit
training <- DF[inTraining, ]
testing <- DF[-inTraining, ]

table(training$fac)
table(testing$fac)

# PLS model IS vs control
fitControl <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = T)

gpls <- caret::train(fac ~ ., data = training, method = "pls", trControl = fitControl, metric = "Accuracy", tuneLength = 20, preProc = c("zv", "center", "scale"), verbose = TRUE)
PLS$model <- gpls
#warnings()
# plot
x11()
# Figure 5d
plot(varImp(gpls), 20, main = "PLS",cex.axis=3)


# Figure 5c - factors
p <- ggplot2::ggplot(gpls)
p <- ggplot2::ggplot(gpls)
p <- p + ggplot2::geom_vline(xintercept = as.numeric(gpls$bestTune), linetype = "dashed", color = "red", size = 2)
p <- p + ggplot2::theme_bw()
x11();p

# Figure 6c - ROC IS vs control
obj <- roc(testing$fac,predict(gpls, newdata = testing,type="prob")$Control, direction=">", ci=TRUE)
RocWith_ci(obj)
# confusion matrix
caret::confusionMatrix(table(predict(gpls, testing), testing$fac))

## PLS model SLE vs control
set.seed(1)

DF <- Ex
rownames(DF) <- Fea[, 1]
DF <- as.data.frame(t(DF))
DF$fac <- Pheno$lupus
# filter
DF <- DF["control"==DF$fac | "LUPUS"==DF$fac, ]
table(DF$fac)

# remove zero variance
nzv <- caret::nearZeroVar(DF, saveMetrics = FALSE)
fDF <- DF[, -nzv]
# remove highly correlated
CorCut <- 0.75
fDFcor <- stats::cor(fDF[, -ncol(fDF)])
highlyCorDescr <- caret::findCorrelation(fDFcor, cutoff = CorCut)
fDFfcor <- fDF[, -highlyCorDescr]
DF <- fDF
# training and test data    
dfL <- split(DF, DF$fac)
selSize <- round(min(unlist(lapply(dfL, nrow))) * 0.75)
x <- sampling::strata(DF, "fac", size = rep(selSize, length(dfL)), method = "srswor")
inTraining <- x$ID_unit
training <- DF[inTraining, ]
testing <- DF[-inTraining, ]


# PLS model
fitControl <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = T)

gpls <- caret::train(fac ~ ., data = training, method = "pls", trControl = fitControl, metric = "Accuracy", tuneLength = 20, preProc = c("zv", "center", "scale"), verbose = TRUE)

# Figure 5f - plot importance
plot(varImp(gpls), 20, main = "PLS")

# Figure 6f - confusion
caret::confusionMatrix(table(predict(gpls, testing), testing$fac))

# Figure 5e - factor
p <- ggplot2::ggplot(gpls)
p <- ggplot2::ggplot(gpls)
p <- p + ggplot2::geom_vline(xintercept = as.numeric(gpls$bestTune), linetype = "dashed", color = "red", size = 2)
p <- p + ggplot2::theme_bw()
x11();p

# Figure 6e - ROC
obj <- roc(testing$fac,predict(gpls, newdata = testing,type="prob")$control, direction=">", ci=TRUE)
RocWith_ci(obj)

