###
### Travail pratique 2
###
##
## Énoncé 1, Modélisation de la fréquence des consultations médicales
##
########################## Importation des données #############################

med_train <- read.table("~/Ecole/Universite/modeles lin/tp 2/VisiteMedicaleFreq_train.txt", header = TRUE)
med_valid <- read.table("~/Ecole/Universite/modeles lin/tp 2/VisiteMedicaleFreq_valid.txt", header = TRUE)

# Juste pour me donner une idée, plus grosse analyse dans un autre document
summary(med_train)
summary(med_valid)

# Plus tard, il y aura un réel ajustement si nécessaire
med_train_adjust <- med_train
attach(med_train_adjust) # Pour m'aide à partir les modèles

##################### Free for all (Binomial négative) #########################
library("MASS")
glmbncomplet <- glm.nb(docvis~female+year+age+hsat+handdum+handper+hhninc+hhkids+
                           educ+married+working+bluec+whitec+self+public+addon+
                           offset(log(ancov)), data = med_train_adjust)

summary(glmbncomplet)
anova(glmbncomplet)

drop1(glmbncomplet, test = "Chisq")

glmbn1 <- update(glmbncomplet,~.-bluec)
drop1(glmbn1, test = "Chisq")

glmbn2 <- update(glmbn1,~.-hhkids)
drop1(glmbn2, test = "Chisq")

glmbn3 <- update(glmbn2,~.-addon)
drop1(glmbn3, test = "Chisq")

glmbn4 <- update(glmbn3,~.-hhninc)
drop1(glmbn4, test = "Chisq")

glmbn5 <- update(glmbn4,~.-public)
drop1(glmbn5, test = "Chisq")

glmbn6 <- update(glmbn5,~.-working)
drop1(glmbn6, test = "Chisq")

glmbn7 <- update(glmbn6,~.-whitec)
drop1(glmbn7, test = "Chisq")

glmbn8 <- update(glmbn7,~.-year)
drop1(glmbn8, test = "Chisq")

glmbn9 <- update(glmbn8,~.-handdum)
drop1(glmbn9, test = "Chisq")

glmbn10 <- update(glmbn9,~.-handper)
drop1(glmbn10, test = "Chisq")

glmbn11 <- update(glmbn10,~.-married)
drop1(glmbn11, test = "Chisq")

glmbn12 <- update(glmbn11,~.-age)
drop1(glmbn12, test = "Chisq")

######################## Poison gonflé à zéro ##################################

library(pscl)

m1 <- zeroinfl(docvis~female+age+hsat+handdum+handper+hhkids+
                   educ+married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov)), data = med_train_adjust)

summary(m1)

m2 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   educ+married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov)), data = med_train_adjust)

summary(m2)
#$loglik
# Est-ce qu'on garde hhkids

pchisq(2*(summary(m1)$loglik-summary(m2)$loglik),2,lower.tail = FALSE)<0.01 # Diff. de 2 df

m3 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   educ+married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov))|female+age+hsat+handdum+handper+
                   educ+married+working+bluec+self+public+addon+
                   offset(log(ancov)), data = med_train_adjust)

summary(m3)

# Est-ce qu'on garde whitec pour le pi

pchisq(2*(summary(m2)$loglik-summary(m3)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m4 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   educ+married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov))|female+age+hsat+handdum+handper+
                   educ+married+bluec+self+public+addon+
                   offset(log(ancov)), data = med_train_adjust)

summary(m4)

# Est-ce qu'on garde working pour le pi

pchisq(2*(summary(m3)$loglik-summary(m4)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m5 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   educ+married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov))|female+age+hsat+handdum+handper+
                   educ+bluec+self+public+addon+
                   offset(log(ancov)), data = med_train_adjust)

summary(m5)

# Est-ce qu'on garde married pour le pi

pchisq(2*(summary(m4)$loglik-summary(m5)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m6 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   educ+married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov))|female+age+hsat+handdum+handper+
                   educ+self+public+addon+
                   offset(log(ancov)), data = med_train_adjust)

summary(m6)

# Est-ce qu'on garde bluec pour le pi

pchisq(2*(summary(m5)$loglik-summary(m6)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m7 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov))|female+age+hsat+handdum+handper+
                   educ+self+public+addon+
                   offset(log(ancov)), data = med_train_adjust)

summary(m7)

# Est-ce qu'on garde educ pour poisson

pchisq(2*(summary(m6)$loglik-summary(m7)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m8 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov))|female+age+hsat+handdum+handper+
                   educ+self+public+
                   offset(log(ancov)), data = med_train_adjust)

summary(m8)

# Est-ce qu'on garde addon pour pi

pchisq(2*(summary(m7)$loglik-summary(m8)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m9 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov))|female+hsat+handdum+handper+
                   educ+self+public+
                   offset(log(ancov)), data = med_train_adjust)

summary(m9)

# Est-ce qu'on garde age pour pi

pchisq(2*(summary(m8)$loglik-summary(m9)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m10 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                   married+working+bluec+whitec+self+public+addon+
                   offset(log(ancov))|female+hsat+handdum+handper+
                   educ+public+
                   offset(log(ancov)), data = med_train_adjust)

summary(m10)

# Est-ce qu'on garde self pour pi

pchisq(2*(summary(m9)$loglik-summary(m10)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m11 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                    married+working+whitec+self+public+addon+
                    offset(log(ancov))|female+hsat+handdum+handper+
                    educ+public+
                    offset(log(ancov)), data = med_train_adjust)

summary(m11)

# Est-ce qu'on garde bluec pour poisson

pchisq(2*(summary(m10)$loglik-summary(m11)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m12 <- zeroinfl(docvis~female+age+hsat+handdum+handper+
                    married+whitec+self+public+addon+
                    offset(log(ancov))|female+hsat+handdum+handper+
                    educ+public+
                    offset(log(ancov)), data = med_train_adjust)

summary(m12)

# Est-ce qu'on garde working pour poisson

pchisq(2*(summary(m11)$loglik-summary(m12)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m13 <- zeroinfl(docvis~female+age+hsat+handper+
                    married+whitec+self+public+addon+
                    offset(log(ancov))|female+hsat+handdum+handper+
                    educ+public+
                    offset(log(ancov)), data = med_train_adjust)

summary(m13)

# Est-ce qu'on garde handdum pour poisson

pchisq(2*(summary(m12)$loglik-summary(m13)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m14 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+addon+
                    offset(log(ancov))|female+hsat+handdum+handper+
                    educ+public+
                    offset(log(ancov)), data = med_train_adjust)

summary(m14)

# Est-ce qu'on garde handper pour poisson

pchisq(2*(summary(m13)$loglik-summary(m14)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m15 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+addon+
                    offset(log(ancov))|female+hsat+handdum+handper+
                    educ+
                    offset(log(ancov)), data = med_train_adjust)

summary(m15)

# Est-ce qu'on garde public pour poisson

pchisq(2*(summary(m14)$loglik-summary(m15)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m16 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+addon+
                    offset(log(ancov))|female+hsat+handdum+handper+
                    offset(log(ancov)), data = med_train_adjust)

summary(m16)

# Est-ce qu'on garde educ pour pi

pchisq(2*(summary(m15)$loglik-summary(m16)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m17 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+addon+
                    offset(log(ancov))|female+hsat+handper+
                    offset(log(ancov)), data = med_train_adjust)

summary(m17)

# Est-ce qu'on garde handdum pour pi

pchisq(2*(summary(m16)$loglik-summary(m17)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m18 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+
                    offset(log(ancov))|female+hsat+handper+
                    offset(log(ancov)), data = med_train_adjust)

summary(m18)

# Est-ce qu'on garde addon pour poisson

pchisq(2*(summary(m17)$loglik-summary(m18)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m19 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+addon+
                    offset(log(ancov))|female+hsat+
                    offset(log(ancov)), data = med_train_adjust)

summary(m19)

# Est-ce qu'on garde handper pour pi

pchisq(2*(summary(m17)$loglik-summary(m19)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

# Le modèle conservé est le modèle 17 selon le test du rapport de vraissemblance

# Son AIC est...
-2*summary(m17)$loglik+2*13

# C'est meillleur que le modèle poisson courant qui avait 7030, avec le modèle poisson gonflé
# à zéro on a 6037

# Il reste a essayer des mise en intéraction

m20 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+addon+age:female+
                    offset(log(ancov))|female+hsat+handper+
                    offset(log(ancov)), data = med_train_adjust)

summary(m20)
summary(m17)

# Est-ce qu'on garde age:female pour poisson

pchisq(2*(summary(m20)$loglik-summary(m17)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df


m21 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+age:female+
                    offset(log(ancov))|female+hsat+handper+
                    offset(log(ancov)), data = med_train_adjust)

summary(m21)

# Est-ce qu'on garde addon pour poisson

pchisq(2*(summary(m20)$loglik-summary(m21)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

m22 <- zeroinfl(docvis~female+age+hsat+
                    married+whitec+self+public+addon+age:female+
                    offset(log(ancov))|female+hsat+handper+
                    offset(log(ancov)), data = med_train_adjust)

summary(m22)
summary(m20)

# Est-ce qu'on garde {...} pour poisson

pchisq(2*(summary(m22)$loglik-summary(m20)$loglik),1,lower.tail = FALSE)<0.01 # Diff. de 2 df

## On poursuit avec le modèle 20 pour le moment

modfinal <- m20
summary(modfinal) # b) prendre estimate et std. error

# Pearson
qchisq(0.99,length(med_train_adjust)-13) # Référence 20.09024 avec n-p' degré de liberté
sum(residuals(modfinal,type = "pearson")^2)

# Déviance À revoir lol
qchisq(0.99,length(med_train_adjust)-13) # Référence 20.09024 avec n-p' degré de liberté
summary(modfinal) # Selon les notes de cours page 155

2*(summary(m1)$loglik-summary(m20)$loglik)

# Représenter graphique fonction de prob

lambda_91_et_217 <- predict(modfinal,med_valid[med_valid$ID ==1091 | med_valid$ID ==1217,],
                            type = "response")/med_valid[med_valid$ID ==1091 | med_valid$ID ==1217, c('ancov')]

# La division est pour mettre ça sur une échelle annuelle

plot(1:7,dpois(1:7,lambda = lambda_91_et_217[1]), type = "h",
     xlab = "Nombre annuel de consultation",
     ylab = "Probabilité",
     sub = "Fonction de masse de probabilité pour le nombre annuel de consultation estimées pour l'ID 1091")

plot(1:25,dpois(1:25,lambda = lambda_91_et_217[2]), type = "h",
     xlab = "Nombre annuel de consultation",
     ylab = "Probabilité",
     sub = "Fonction de masse de probabilité pour le nombre annuel de consultation estimées pour l'ID 1217")

## Estimation de la VaR0.99 en haut de 85

lambdaNo3 <- predict(modfinal,med_valid,type = "response")

max(qpois(0.99,lambdaNo3)) # Aucun en haut de 85
max(med_valid$docvis) # Max data de valid, 38. Donc normal de pas avoir rien de full haut
