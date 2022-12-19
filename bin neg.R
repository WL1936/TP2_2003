### Travail pratique 2
###
### Énoncé 1.2

med_train <- 'VisiteMedicaleFreq_train'
med_valid <- 'VisiteMedicaleFreq_valid'

med_train <- read.table("~/VisiteMedicaleFreq_train.txt", header = TRUE)
med_valid <- read.table("~/VisiteMedicaleFreq_valid.txt", header = TRUE)

#Loi bin neg avec lien log


library(MASS)

########thera????????????????
########thera <- glm.nb(docvis~female+year+age+hsat+handdum+handper+hhninc+hhkids+educ+married+working+bluec+whitec+self+public+addon+offset(log(ancov)),data=med_train)
########summary(thera)

glmtout <- glm.nb(docvis~female+year+age+hsat+handdum+handper+hhninc+hhkids+educ+married+working+bluec+whitec+self+public+addon+offset(log(ancov)), data = med_train)


#a)
(summarytout <- summary(glmtout))
summarytout$coefficients[,c(1,2)]

#b)


library("MASS")
summary(glmtout)
anova(glmtout)

drop1(glmtout, test = "Chisq")

glm1 <- update(glmtout,~.-bluec)
drop1(glm1, test = "Chisq")

glm2 <- update(glm1,~.-hhkids)
drop1(glm2, test = "Chisq")

glm3 <- update(glm2,~.-hhninc)
drop1(glm3, test = "Chisq")

glm4 <- update(glm3,~.-addon)
drop1(glm4, test = "Chisq")

glm5 <- update(glm4,~.-public)
drop1(glm5, test = "Chisq")

glm6 <- update(glm5,~.-working)
drop1(glm6, test = "Chisq")

glm7 <- update(glm6,~.-whitec)
drop1(glm7, test = "Chisq")

glm8 <- update(glm7,~.-year)
drop1(glm8, test = "Chisq")

glm9 <- update(glm8,~.-handdum)
drop1(glm9, test = "Chisq")

glm10 <- update(glm9,~.-handper)
drop1(glm10, test = "Chisq")

glm11 <- update(glm10,~.-married)
drop1(glm11, test = "Chisq")

glm12 <- update(glm11,~.-age)
drop1(glm12, test = "Chisq")



glmfinal <- glm12


#c)comparer avec poisson



#d)

qchisq(0.99,length(med_train)-length(summary(glmfinal)$coefficients[,1])+1)
sum(residuals(glmfinal,type = "pearson")^2)

qchisq(0.99,length(med_train)-length(summary(glmfinal)$coefficients[,1])+1)
summary(glmfinal)$deviance

summary(glmfinal)$aic


#e)

moyenne_ecart <- data.frame("moyenne" = prediction$fit,"ecart_type" = prediction$se.fit)

prediction <- predict(glmfinal,med_valid[med_valid$ID>=1075 & med_valid$ID<=1084,],type = "link", se.fit = TRUE)

max(predict(glmfinal,med_valid[med_valid$ID>=1075 & med_valid$ID<=1084,])




#f)
estimation <- predict(glmfinal,med_valid[med_valid$ID>=1075 & med_valid$ID<=1084,],type = "response")



#g)

###########A voir####################