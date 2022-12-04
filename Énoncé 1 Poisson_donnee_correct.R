###
### Travail pratique 2
###
##
## Énoncé 1, Modélisation de la fréquence des consultations médicales
##
########################## Importation des données #############################

# récupération des données à partir de analyse_exploratoire
# table complète et corrigé : covadjustdata



# Plus tard, il y aura un réel ajustement si nécessaire
med_train_adjust <- covadjustdata
med_valid <- read.table("~/Documents/TP2_2003/VisiteMedicaleFreq_valid.txt", header = TRUE)
# attach(med_train_adjust) # Pour m'aide à partir les modèles

#################### Loi de Poisson avec lien canonique ########################

glmpoiscomplet <- glm(docvis~female+year+age+hsat+handdum+handper+hhninc+hhkids+educ+
                   married+working+bluec+whitec+self+public+addon+offset(log(ancov)),
               family = poisson, data = med_train_adjust)

(summaryglmpoiscomplet <- summary(glmpoiscomplet))

### Coefficient et écart-type pour a)
summaryglmpoiscomplet$coefficients[,c(1,2)]

### Sélection de variable 99% (page 159 note de cours)
qchisq(0.99,1) # Valeur de référence 6,634897

anova(glmpoiscomplet) # Le plus petit est working avec 0.96

# Modèle sans working
glmpois1 <- glm(docvis~female+year+age+hsat+handdum+handper+hhninc+hhkids+educ+
                   married+bluec+whitec+self+public+addon+offset(log(ancov)),
               family = poisson, data = med_train_adjust)

anova(glmpois1) # Le plus petit est whitec avec 0.00

# Modèle sans whitec et working
glmpois2 <- glm(docvis~female+year+age+hsat+handdum+handper+hhninc+hhkids+educ+
                    married+bluec+self+public+addon+offset(log(ancov)),
                family = poisson, data = med_train_adjust)

anova(glmpois2) # Le plus petit est handdum avec 0.20

# Modèle sans whitec, working et handdum
glmpois3 <- glm(docvis~female+year+age+hsat+handper+hhninc+hhkids+educ+
                    married+bluec+self+public+addon+offset(log(ancov)),
                family = poisson, data = med_train_adjust)

anova(glmpois3) # Le plus petit est addon avec 1.71

# Modèle sans whitec, working, handum et addon
glmpois4 <- glm(docvis~female+year+age+hsat+handper+hhninc+hhkids+educ+
                    married+bluec+self+public+offset(log(ancov)),
                family = poisson, data = med_train_adjust)

anova(glmpois4) # Le plus petit est public avec 2.38

# Modèle courant sans whitec, working, handum, addon et public
glmpoiscourant <- glm(docvis~female+year+age+hsat+handper+hhninc+hhkids+educ+
                    married+bluec+self+offset(log(ancov)),
                family = poisson, data = med_train_adjust)

anova(glmpoiscourant) # Le plus petit est bluec avec 8.70, donc on a notre modèle courant

# Les variables conservées pour le b) se retrouve dans le tableau anova ci-haut

# Vérification
anova(glmpoiscourant,glmpois4,glmpois3,glmpois2,glmpois1,glmpoiscomplet) #En effet, rien de significatif

### Statistique du modèle c) À vérifier revoir les ateliers

# Pearson
qchisq(0.99,length(med_train_adjust)-length(summary(glmpoiscourant)$coefficients[,1])+1) # Référence 20.09024 avec n-p' degré de liberté
sum(residuals(glmpoiscourant,type = "pearson")^2)

# Déviance
qchisq(0.99,length(med_train_adjust)-length(summary(glmpoiscourant)$coefficients[,1])+1) # Référence 20.09024 avec n-p' degré de liberté
summary(glmpoiscourant)$deviance # Selon les notes de cours page 155

# AIC
summary(glmpoiscourant)$aic

## Le modèle poisson ne me semble pas très approprié


## Estimation ponctuelle de la moyenne, écart-type et de la VaR0.99 d)

# Voir atelier 11 question 12 pour plus d'info
# fit est la prédiction
# se.fit semble être l'écart-type
# VaR0.99 dans un cas de 10 données doit être le max

predict_75_a_84 <- predict(glmpoiscourant,med_valid[med_valid$ID>=1075 & med_valid$ID<=1084,],
        type = "link", se.fit = TRUE)

moy_et <- data.frame("Moyenne" = predict_75_a_84$fit,
                     "Écart-type" = predict_75_a_84$se.fit)

round(exp(moy_et),3) # Choisir un des deux

predict(glmpoiscourant,med_valid[med_valid$ID>=1075 & med_valid$ID<=1084,],
                           type = "response", se.fit = TRUE) # Choisir un des deux

# La différence est l'écart-type, une petite investigation doit être faite.

max(predict(glmpoiscourant,med_valid[med_valid$ID>=1075 & med_valid$ID<=1084,],
            type = "response")) # Pour VaR0.99


## Estimation ponctuelle et IC 95% prob que le nombre de consultation soit 0 e)
# Atelier 12 questions 11

lambda_75_a_84 <- predict(glmpoiscourant,med_valid[med_valid$ID>=1075 & med_valid$ID<=1084,],
                  type = "response")
round(exp(-lambda_75_a_84),5)

# Pour l'explication, voir le numéro d'atelier associé

## Graphique fonction de masse de probabilité estimé

lambda_94_et_217 <- predict(glmpoiscourant,med_valid[med_valid$ID ==1094 | med_valid$ID ==1217,],
                            type = "response")/med_valid[med_valid$ID ==1094 | med_valid$ID ==1217, c('ancov')]

# La division est pour mettre ça sur une échelle annuelle

plot(1:20,dpois(1:20,lambda = lambda_94_et_217[1]), type = "h",
     xlab = "Nombre annuel de consultation",
     ylab = "Probabilité",
     sub = "Fonction de masse de probabilité pour le nombre annuel de consultation estimées pour l'ID 1094")

plot(1:25,dpois(1:25,lambda = lambda_94_et_217[2]), type = "h",
     xlab = "Nombre annuel de consultation",
     ylab = "Probabilité",
     sub = "Fonction de masse de probabilité pour le nombre annuel de consultation estimées pour l'ID 1217")

# Graphiques des résidus g)
# Atelier 12 question 9

# Pearson
hist(residuals(glmpoiscourant,type = "pearson"))

# Anscombe revoir pour être sur
hatmu <- fitted(glmpoiscourant)
anscombe <- 1.5*((med_train_adjust$docvis)^(2/3)-hatmu^(2/3))/hatmu^(1/6)
hist(anscombe)

# Déviance
hist(residuals(glmpoiscourant,type = "deviance"))
