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

