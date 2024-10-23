##################### Script Dossier Régressions pénalisées ####################

#chargement packages
library(naniar)
library(missForest)
library(tsoutliers)
library(seastests)
library(TSA)
library(aTSA)
library(tseries)
library(factoextra)
library(FactoMineR)
library(fpc)
library(cluster)
library(corrplot)
library(tidyverse)
library(lgarch)
library(gets)
library(readxl)
library(tidyverse)
library(lgarch)		# Gets modelling
library(gets)
library(glmnet) 	# penalized regressions
library(rbridge)	# bridge regressions
library(foreach)
library(doParallel)	# execute foreach loops in parallel with %dopar% operator
library(ncvreg)
library(SIS)
library(readxl)
library(tidyverse)

#chargement base de données
base_initial<-read.csv2("C:/Users/Admi/Desktop/USbase.csv", sep=";")
str(base_initial)

#Transformation des variables en numerique
base_initial[, -1] <- apply(base_initial[, -1],2, FUN=as.numeric)
str(base_initial)

#Suppression et prélèvement de la dernière ligne pour les transformations
base_nb_transform<-base_initial[645,]
str(base_nb_transform)
base<-base_initial[-645,]

#----------Partie 1 : Analyses exploratoires et descriptives--------------------

##Nettoyage de la base de données pour les valeurs manquantes##

#Valeurs manquantes en fonction des variables 
valeurs_manquantes <- colSums(is.na(base))
valeurs_manquantes
#nombre de variables avec valeurs manquantes
nombre_variables_avec_valeurs_manquantes <- sum(valeurs_manquantes > 0)
nombre_variables_avec_valeurs_manquantes
#nombre de valeurs manquantes par variable 
resultats <- data.frame( Valeurs_manquantes = valeurs_manquantes)
print(resultats)
#nombre de valeurs manquantes au total
nombre_de_valeurs_manquantes <- sum(is.na(base))
nombre_de_valeurs_manquantes
#Classement des variables en fonction du nombre de valeurs manquantes
gg_miss_var(base)

#On supprime la variable ACOGNO et TWEXAFEGSMTHx
#On garde par contre la variable UMCSENTx car valeurs manquantes
#à intervalle régulier
base_modif <- subset(base, select = -c(ACOGNO, TWEXAFEGSMTHx))
base_nb_transformed<-subset(base_nb_transform, select = -c(ACOGNO, TWEXAFEGSMTHx))
str(base_modif)
str(base_nb_transformed)

#imputation de valeurs avec missForest
imputation <- missForest(base_modif[,-1])
imputation
#vérification de valeurs manquantes
nombre_de_valeurs_manquantes <- sum(is.na(imputation))
nombre_de_valeurs_manquantes
#combinaison de la variable date avec les autres variables explicatives
base2<-cbind(base[,1],imputation$ximp)
View(base2)
gg_miss_var(base2)

#détection des outliers
serie<-base2[,-1]
yy <- ts(data = serie, start=c(1970,01),frequency=12)
str(yy)
#outliers sur la série Unemployment_Rate et correction
fit <- tso(yy[,126])
plot(fit)
show(fit)
adj <- fit$yadj
#plot de la variable dépendante
plot(adj, xlab = "Date", ylab = "Civilian Unemployment Rate", col = "blue", main = "Unemployment Rate" )


##test de saisonnalité:

#Détection de la saisonnalité:
# Friedman test
ft <- fried(adj)
show(ft)
#isSeasonal:
is <- isSeasonal(adj, test="wo")
show(is)
# Kruskal-Wallis test
kwt <- kw(adj)
show(kwt)
# QS test
qst <- qs(adj)
show(qst)
# Seasonal dummies
sd <- seasdum(adj)
show(sd)
# Welch test
w <- welch(adj)
show(w)
# Webel-Ollech test:
wo<-combined_test(adj)
show(wo)


# Périodogramme sur Série originale:
periodogram(adj, col = "blue")
#Périodogramme sur Série différencié d'ordre 1
dadj<-diff(adj, differences=1)
periodogram(dadj, col = "blue")

##test de stationnarité

#test de dickey-fuller
adf<-aTSA::adf.test(adj)
aadf<-tseries::adf.test(adj, k=4)
aadf
#stationnaire à partir de 4 lags au seuil de 10%.


#transformation des variables explicatives pour les rendre stationnaires
data<-base2[,-1]
str(data)
nb_transform<-base_nb_transformed[,-1]
str(nb_transform)
#boucle transformation
for (i in 1:125){
  if (nb_transform[1,i]==2){
    data[,i]<-c(NA, diff(data[,i]))
  }else if(nb_transform[1,i]==3){
    data[,i]<-c(NA,NA, diff(diff(data[,i])))
  }else if (nb_transform[1,i]==4){
    data[,i]<-c(log(data[,i]))
  }else if (nb_transform[1,i]==5){
    data[,i]<-c(NA, diff(log(data[,i])))
  }else if (nb_transform[1,i]==6){
    data[,i]<-c(NA,NA,diff(diff(log(data[,i]))))
  }else if (nb_transform[1,i]==7){
    data[,i]<-c(NA, diff(data[,i]/lag(data[,i])-1))
  }else {data[,i]<-data[,i]}
}
str(data)
#Suppression des valeurs manquantes de la base de données 
base3<-na.omit(data)
str(base3)
#Vérification de la stationnarité des variables
# Création d'un dataframe pour stocker les résultats
stationarity_results <- data.frame(Variable = colnames(base3), Is_Stationary = character(126), p_value = numeric(126))
# Boucle pour tester la stationnarité de chaque variable
for (i in 1:126) {
  variable <- base3[[i]]
  result <- adf.test(variable, k=1)
  # Stocker les résultats dans le nouveau dataframe
  stationarity_results[i, "Is_Stationary"] <- ifelse(result$p.value <= 0.10, "Oui", "Non")
  stationarity_results[i, "p_value"] <- result$p.value
}
stationarity_results
#Nombre de variables stationnaires ou non
table(stationarity_results$Is_Stationary)


##Graphique de la variable dépendante

##Graphique de la variable dépendante à niveau
plot(adj, xlab = "Date", ylab = "Civilian Unemployment Rate", col = "blue", main = "Corrected Unemployment Rate" )
#Graphique de la variable dépendante différenciée d'ordre 1
plot(dadj, xlab = "Date", ylab = "Civilian Unemployment Rate", col = "blue", main = "Corrected Unemployment Rate I(1)" )


##statistiques descriptives de la variable dépendante

#stats générales
summary(adj)
which.max(adj)
which.min(adj)
#ecart-type:
round(sd(adj),3)
#kurtosis:
round(kurtosis(adj),3)
#skewness:
round(skewness(adj),3)
#test de normalité:
shapiro.test((adj))
#histogramme 
hist(adj, col = "lightblue", main="Histogramme du taux de chômage")
#boxplot
boxplot(adj, col="blue")

#write.csv2(base3, "base3.csv", row.names = FALSE)

## Classifications

#ACP
base4<-base3
str(base4)

res.pca=(PCA(base4))
res.pca
plot.PCA(res.pca,title="Variables-facteurs",axes=c(1,2),choix="var")
plot.PCA(res.pca,title="Variables-facteurs",axes=c(1,3),choix="var")

#ACP avec degré de contribution par couleur
fviz_pca_var(res.pca,
             axes=c(1,2),
             col.var = "contrib",
             gradient.cols = c("#00AFBB","#FC4E07", "#B9121B"),
             title= "ACP axe 1 et 2")
fviz_pca_var(res.pca,
             axes=c(1,3),
             col.var = "contrib",
             gradient.cols = c("#00AFBB","#FC4E07", "#B9121B"),
             title= "ACP axe 1 et 3")
fviz_pca_var(res.pca,
             axes=c(1,4),
             col.var = "contrib",
             gradient.cols = c("#00AFBB","#FC4E07", "#B9121B"),
             title= "ACP axe 1 et 4")

#valeurs propres
round(res.pca$eig,2)
barplot(res.pca$eig[,1], ylim=c(0,10), main="Valeurs propres",col="#69b3a2",
        border=NA)
fviz_eig(res.pca, choice = c("variance", "eigenvalue"),
         geom = c("bar", "line"), barfill = "#69b3a2",
         barcolor = "#69b3a2", linecolor = "red",
         ncp = 16, addlabels = TRUE,
         main="Histogramme des valeurs propres")

#Contributions
contributions <-round(res.pca$var$contrib,2)
print(contributions)

#Corrélations
correlations <-round(res.pca$var$coord,2)
correlations

#Cosinus carrés
cosinuscarres =round(res.pca$var$cos2,2)
print(cosinuscarres)


#Classification ascendante hiérarchique

#pour déterminer le meilleur nombre de groupes pour la classification:
#Suppression de la variable NONBORRES car considéré comme constante
base5<-subset(base4, select = -c(NONBORRES))
pamk.best<-pamk(base5)
pamk.best
#nombre de classes estimés par silhouette est de 6
plot(pam(base5,pamk.best$nc))

#Création du dendogramme
distance<-dist(base4,method = "euc")
dist.hclust<-hclust(distance, method="ward.D2")
plot(dist.hclust, main="Dendrogramme")
plot(rev(dist.hclust$height),type="h", ylab="variation inertie")
barplot(rev(dist.hclust$height)[1:30],main="Evolution du critére d agrégation")
plot(dist.hclust,hang=-1,cex=0.8)

nbclas<-6
part<-cutree(dist.hclust, k=nbclas)
plot(dist.hclust, main="Coupure en 6 classes")
rect.hclust(dist.hclust, k=nbclas, border = "red" )


##Corrélation:

str(base5)

X11()
#création de la matrice de corrélation
cor_matrice<-cor(base5, method="spearman")
#Corrélogramme sur l'ensemble des données
corrplot(cor_matrice, tl.col = "black", tl.cex = 0.01)

#Corrélation par groupe de variables pour plus de lisibilité
cor_matrice1<-cor(base4[,c(1:20,21:40)], method="spearman")
cor_matrice2<-cor(base5[,c(1:20,41:60)], method="spearman")
cor_matrice3<-cor(base5[,c(1:20,61:80)], method="spearman")
cor_matrice4<-cor(base5[,c(1:20,81:100)], method="spearman")
cor_matrice5<-cor(base5[,c(1:20,101:125)], method="spearman")
cor_matrice6<-cor(base5[,c(41:60,21:40)], method="spearman")
cor_matrice7<-cor(base5[,c(61:80,21:40)], method="spearman")
cor_matrice8<-cor(base5[,c(81:100,21:40)], method="spearman")
cor_matrice9<-cor(base5[,c(101:125,21:40)], method="spearman")
cor_matrice10<-cor(base5[,c(41:60,61:80)], method="spearman")
cor_matrice11<-cor(base5[,c(41:60,81:100)], method="spearman")
cor_matrice12<-cor(base5[,c(41:60,101:125)], method="spearman")
cor_matrice13<-cor(base5[,c(61:80,81:100)], method="spearman")
cor_matrice14<-cor(base5[,c(61:80,101:125)], method="spearman")
cor_matrice15<-cor(base5[,c(81:100,101:125)], method="spearman")

corrplot(cor_matrice1, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice2, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice3, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice4, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice5, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice6, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice7, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice8, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice9, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice10, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice11, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice12, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice13, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice14, tl.col = "black", tl.cex = 0.7)
corrplot(cor_matrice15, tl.col = "black", tl.cex = 0.7)


#-------------------Partie 2 : Sélection de Variables---------------------------


#-------------------------------------------------------------------------------
#Base de données intégrant les retards
library(dplyr)
base_retards<-lag(base5)
View(base5)
View(base_retards)

#retards des variables explicatives
noms_colonnes<-colnames(base5)

for(i in 1:length(noms_colonnes)){
  nouveau_nom<-paste0("Lag1_", noms_colonnes[i])
  colnames(base_retards)[i]<-nouveau_nom
}

#4retards de la variable Unemployement_Rate
Lag2_Unemployment_Rate<-lag(base5[,125],2)
Lag2_Unemployment_Rate<-as.data.frame(Lag2_Unemployment_Rate)

Lag3_Unemployment_Rate<-lag(base5[,125],3)
Lag3_Unemployment_Rate<-as.data.frame(Lag3_Unemployment_Rate)

Lag4_Unemployment_Rate<-lag(base5[,125],4)
Lag4_Unemployment_Rate<-as.data.frame(Lag4_Unemployment_Rate)


#assemblage base de données avec intégration de retards
base_assemblage<-cbind(base5, base_retards, Lag2_Unemployment_Rate, Lag3_Unemployment_Rate, Lag4_Unemployment_Rate)
str(base_assemblage)


base7<-na.omit(base_assemblage)
str(base7)

#write.csv2(base7, "base_avec_retards.csv", row.names = FALSE)
#base7<-read.csv2("C:/Users/Admi/Desktop/M2 ECAP/régression pénalisée/base_avec_retards.csv")



#---------------------------------------------------------------------
# standardized y and x (centered and standardized)
y <- data.frame(base7) %>%
  select(Unemployment_Rate) %>%
  scale(center = T, scale = T) %>%
  as.matrix()

x <- data.frame(base7) %>%
  select(-Unemployment_Rate) %>% # on retire la variable à expliquer y
  scale(center = T, scale = T) %>%
  as.matrix()

##GETS

model <- arx(y, mc = T, ar = NULL, mxreg = x, vcov.type = "white") 
model
getsm <- getsm(model) 
getsm

getsm2 <- getsm(model, arch.LjungB=NULL, ar.LjungB = NULL) 
getsm2
# GETS betas
coef.arx(getsm2)
# Get the name of relevant variables
names_mX <- names(coef.arx(getsm2))
names_mX



#---------------------------------------------------------------------
##Ridge

# Ridge regression
model_cv <- glmnet(x, y, alpha = 0, standardize = T)
plot(model_cv)

# 10-folds CV to choose lambda
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 0, implementation of ridge regression
# choix du meilleur lambda parmi 100
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 

# Figures of lambdas
plot(ridge_cv)

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv <- ridge_cv$lambda.min
lambda_cv

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
summary(model_cv)

# Ridge betas
model_cv$beta


#---------------------------------------------------------------------
## LASSO regression
model_cv_lasso <- glmnet(x, y, alpha = 1, standardize = T)
plot(model_cv_lasso)

# 10-folds CV to choose lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 1, implementation of Lasso regression
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100

# Figures of lambdas
plot(lasso_cv)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- lasso_cv$lambda.1se
lambda_cv

# Evaluation of the final model with the selected lambda
model_cv_lasso <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)

# Lasso betas
model_cv_lasso$beta

# Get the name of relevant variables
which(! coef(model_cv_lasso) == 0, arr.ind = TRUE)


#---------------------------------------------------------------------
## Elastic-Net regression

# Choose alpha sequencially with 0 < alpha < 1
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- glmnet::cv.glmnet(x, y,  alpha = i, lambda = lambdas_to_try, standardize = T, nfolds = 10)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}

# Implementation of EN regression
elasticnet_cv <- search[search$cvm == min(search$cvm), ]
elasticnet_cv

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- elasticnet_cv$lambda.1se
lambda_cv

# Evaluation of the final model with the selected lambda
model_cv_en <- glmnet(x, y, lambda = elasticnet_cv$lambda.1se, alpha = elasticnet_cv$alpha)

# EN betas
model_cv_en$beta
# ou bien : coef(model_cv)

# Get the name of relevant variables
which(! coef(model_cv_en) == 0, arr.ind = TRUE)

# Graphics of lambdas_to_try
lambdas_to_try <- 10^seq(from = -3, to = 5, length.out = 100)
model_graph <- cv.glmnet(x, y,  alpha = elasticnet_cv$alpha, lambda = lambdas_to_try, standardize = T, nfolds = 10)
plot(model_graph)


#---------------------------------------------------------------------
## SCAD
fit_SCAD=ncvreg(x, y, penalty = c("SCAD"))
plot(fit_SCAD)
summary(fit_SCAD, lambda=0.10)

# Validation croisé pour le meilleur lambda 
cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
plot(cvfit_SCAD)

# On attribue le meilleur lambda 
lambda_SCAD <- cvfit_SCAD$lambda.min
lambda_SCAD
#Modele finale 
SCAD_Final=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
SCAD_Final$beta

which(! coef(SCAD_Final) == 0, arr.ind = TRUE)


#--------------------------------------------------------------------------------
# Adaptive Lasso regression using Lasso to compute the weights in the first step
model_cv_al <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
coef_lasso<-predict(model_cv_al,type="coef",s=lambda_cv)
# Weighted with gamma = 0.5
gamma=0.5
w0<-1/(abs(coef_lasso) + (1/length(y)))
poids.lasso<-w0^(gamma)
poids.lasso <- poids.lasso[2:nrow(poids.lasso),]
poids.lasso <- Matrix(poids.lasso)

# Adaptive LASSO
fit_adalasso <- glmnet(x, y, penalty.factor =poids.lasso)
fit_cv_adalasso<-cv.glmnet(x, y,penalty.factor=poids.lasso)

# Figure of lambdas 
plot(fit_cv_adalasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- fit_cv_adalasso$lambda.1se
lambda_cv
# Evaluation of the final model with the selected lambda
model_cv_al <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)

# Lasso betas
model_cv_al$beta

# Get the name of relevant variables
which(! coef(model_cv_al) == 0, arr.ind = TRUE)


################### Réduction de dimensions ########################################################

#Utilisation de la méthode SIS pour la réduction de dimensions

mod01 <- SIS(x, y, family="gaussian", penalty="MCP", tune="cv", nfolds=10, nsis=100)

# indices selected by only SIS
var <- mod01$sis.ix0	
show(var)

base6<-base5[,c(5,12,13,21,22,23,24,25,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
                42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,60,61,62,65,68,71,72,
                73,74,75,76,77,78,79,80,81,82,83,84,85,88,89,90,91,92,94,124,125)]

base6<-base7[,c(28,30,32,33,35,36,37,39,41,43,44,45,47,48,49,50,51,52,53,
                54,55,56,57,60,65,84,89,90,91,92,145,147,152,154,156,157,
                159,160,161,163,165,167,168,169,171,172,173,174,175,176,177,
                178,179,180,181,184,189,213,214,215,216,248,249,250,251,252,125)]

str(base6)

# standardized y and x (centered and standardized)
y <- data.frame(base6) %>%
  select(Unemployment_Rate) %>%
  scale(center = T, scale = T) %>%
  as.matrix()

x <- data.frame(base6) %>%
  select(-Unemployment_Rate) %>% # on retire la variable à expliquer y
  scale(center = T, scale = T) %>%
  as.matrix()

#-------------------------------------------------------------------------------
## Ridge regression with SIS
model_cv <- glmnet(x, y, alpha = 0, standardize = T)
plot(model_cv)

# 10-folds CV to choose lambda
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 0, implementation of ridge regression
# choix du meilleur lambda parmi 100
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 

# Figures of lambdas
plot(ridge_cv)

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv <- ridge_cv$lambda.min
lambda_cv

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
summary(model_cv)

# Ridge betas
model_cv$beta


#---------------------------------------------------------------------
## LASSO regression with SIS

model_cv_lasso <- glmnet(x, y, alpha = 1, standardize = T)
plot(model_cv_lasso)

# 10-folds CV to choose lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 1, implementation of Lasso regression
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100

# Figures of lambdas
plot(lasso_cv)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- lasso_cv$lambda.1se
lambda_cv

# Evaluation of the final model with the selected lambda
model_cv_lasso <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)

# Lasso betas
model_cv_lasso$beta

# Get the name of relevant variables
which(! coef(model_cv_lasso) == 0, arr.ind = TRUE)


#-------------------------------------------------------------------------------

## Elastic-Net regression

# Choose alpha sequencially with 0 < alpha < 1
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- glmnet::cv.glmnet(x, y,  alpha = i, lambda = lambdas_to_try, standardize = T, nfolds = 10)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}

# Implementation of EN regression
elasticnet_cv <- search[search$cvm == min(search$cvm), ]
elasticnet_cv

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- elasticnet_cv$lambda.1se
lambda_cv

# Evaluation of the final model with the selected lambda
model_cv_en <- glmnet(x, y, lambda = elasticnet_cv$lambda.1se, alpha = elasticnet_cv$alpha)

# EN betas
model_cv_en$beta
# ou bien : coef(model_cv)

# Get the name of relevant variables
which(! coef(model_cv_en) == 0, arr.ind = TRUE)

# Graphics of lambdas_to_try
lambdas_to_try <- 10^seq(from = -3, to = 5, length.out = 100)
model_graph <- cv.glmnet(x, y,  alpha = elasticnet_cv$alpha, lambda = lambdas_to_try, standardize = T, nfolds = 10)
plot(model_graph)


#---------------------------------------------------------------------
# SCAD with SIS
library(ncvreg)
fit_SCAD=ncvreg(x, y, penalty = c("SCAD"))
plot(fit_SCAD)
summary(fit_SCAD, lambda=0.10)

# Validation croisé pour le meilleur lambda 
cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
plot(cvfit_SCAD)

# On attribue le meilleur lambda 
lambda_SCAD <- cvfit_SCAD$lambda.min
lambda_SCAD
#Modele finale 
SCAD_Final=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
SCAD_Final$beta

which(! coef(SCAD_Final) == 0, arr.ind = TRUE)


#--------------------------------------------------------------------------------
## Adaptive Lasso with sis

# Adaptive Lasso regression using Lasso to compute the weights in the first step
model_cv_al <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
coef_lasso<-predict(model_cv_al,type="coef",s=lambda_cv)
# Weighted with gamma = 0.5
gamma=0.5
w0<-1/(abs(coef_lasso) + (1/length(y)))
poids.lasso<-w0^(gamma)
poids.lasso <- poids.lasso[2:nrow(poids.lasso),]
poids.lasso <- Matrix(poids.lasso)

# Adaptive LASSO
fit_adalasso <- glmnet(x, y, penalty.factor =poids.lasso)
fit_cv_adalasso<-cv.glmnet(x, y,penalty.factor=poids.lasso)

# Figure of lambdas 
plot(fit_cv_adalasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- fit_cv_adalasso$lambda.1se
lambda_cv
# Evaluation of the final model with the selected lambda
model_cv_al <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)

# Lasso betas
model_cv_al$beta

# Get the name of relevant variables
which(! coef(model_cv_al) == 0, arr.ind = TRUE)

#-------------------------------------------------------------------------------
#GETS with sis

model <- arx(y, mc = T, ar = NULL, mxreg = x, vcov.type = "white") 
model
getsm <- getsm(model) 
getsm

getsm2 <- getsm(model, arch.LjungB=NULL, ar.LjungB = NULL) 
getsm2
# GETS betas
coef.arx(getsm)
# Get the name of relevant variables
names_mX <- names(coef.arx(getsm))
names_mX

#-------------------------------------------------------------------------------
##Random Forest
library(rpart)
library(randomForest)
library(gbm)
library(caret)

? randomForest

# ----------------------------------------------------
# ajustement du modele
# ---------------------------------------------------- 
pX<-ncol(base7)-1
valntree=500
valmtry=floor(pX/3)
valnodesize=5  # par defaut 5 pour un arbre de regression
rdf=randomForest(formula=Unemployment_Rate~.,data=base7, 
                 ntree=valntree, mtry=valmtry, nodesize = valnodesize, 
                 importance=TRUE, proximity=TRUE,nPerm=1)
print(rdf)
plot(rdf)

# avec caret
fit.control <- trainControl(method = "boot",number=20,  search='grid')
tune.mtry <- expand.grid(.mtry = c(1,2,5,10,20)) 
deb=Sys.time()
rf.grid<- train(Unemployment_Rate ~ ., 
                data = base7,
                method = "rf", 
                ntree=300,
                metric='RMSE', 
                tuneGrid =tune.mtry,
                trControl=fit.control)
fin=Sys.time()
print(fin-deb)
print(rf.grid)
plot(rf.grid)

# ----------------------------------------------------
# VI
# ----------------------------------------------------
rdf=randomForest(formula=Unemployment_Rate~.,data=base7, ntree=300, mtry=5, nodesize = 5, 
                 importance=TRUE)
varImpPlot(rdf)
importance(rdf,scale=TRUE)
VI=importance(rdf,scale=TRUE)[,1]
VI[order(VI,decreasing=TRUE)]

# ----------------------------------------------------
# selection de variables
# ----------------------------------------------------
library(VSURF)
resVSURF<-VSURF(formula=Unemployment_Rate~.,data=base7,mtry=5,
                ntree.thres=100, nfor.thres=20,
                ntree.interp=100,nfor.interp=20,
                ntree.pred=100,nfor.pred=10)
plot(resVSURF)
summary(resVSURF)
colnames(base7)[resVSURF$varselect.interp]
colnames(base7)[resVSURF$varselect.pred]

#-------------------------------------------------------------------------------
#Random forest après filtrage
#-------------------------------------------------------------------------------

# ----------------------------------------------------
# ajustement du modele
# ---------------------------------------------------- 
pX<-ncol(base6)-1
valntree=500
valmtry=floor(pX/3)
valnodesize=5  # par defaut 5 pour un arbre de regression
rdf=randomForest(formula=Unemployment_Rate~.,data=base6, 
                 ntree=valntree, mtry=valmtry, nodesize = valnodesize, 
                 importance=TRUE, proximity=TRUE,nPerm=1)
print(rdf)
plot(rdf)

# avec caret
fit.control <- trainControl(method = "boot",number=20,  search='grid')
tune.mtry <- expand.grid(.mtry = c(1,2,5,10,20)) 
deb=Sys.time()
rf.grid<- train(Unemployment_Rate ~ ., 
                data = base6,
                method = "rf", 
                ntree=300,
                metric='RMSE', 
                tuneGrid =tune.mtry,
                trControl=fit.control)
fin=Sys.time()
print(fin-deb)  
print(rf.grid)
plot(rf.grid)

# ----------------------------------------------------
# VI
# ----------------------------------------------------
rdf=randomForest(formula=Unemployment_Rate~.,data=base6, ntree=300, mtry=5, nodesize = 5, 
                 importance=TRUE)
varImpPlot(rdf)
importance(rdf,scale=TRUE)
VI=importance(rdf,scale=TRUE)[,1]
VI[order(VI,decreasing=TRUE)]

# ----------------------------------------------------
# selection de variables
# ----------------------------------------------------
library(VSURF)
resVSURF<-VSURF(formula=Unemployment_Rate~.,data=base6,mtry=5,
                ntree.thres=100, nfor.thres=20,
                ntree.interp=100,nfor.interp=20,
                ntree.pred=100,nfor.pred=10)
plot(resVSURF)
summary(resVSURF)
colnames(base6)[resVSURF$varselect.interp]
colnames(base6)[resVSURF$varselect.pred]

