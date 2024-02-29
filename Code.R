
#################################
# Nom : SCHILD                  #             
# Prénom : Nathan               #                 
# Master APE                    #           
# Code dossier                  #
#################################


#----------------------------------------------------------------------------------------------------------------------------------------



#*****************************************************************************
#---- Installation et chargement des packages nécessaires/base de données ----

# Dans un premier temps, il convient d'installer et de charger les packages nécessaires.
install.packages("lmtest")
install.packages("plm")
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("ggthemes")
install.packages("fBasics")
library(plm)
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(stargazer)
library(car)
library(lmtest)
library(ggthemes)
library(fBasics)
library(readxl)

# On charge la base de données
data <- read_excel("C:/Users/natha/OneDrive/Bureau/M1/EDP/datap.xlsx")
View(data)

# On transforme la base de données .
pdata<-pdata.frame(data,index=c("country","year"),drop.index=T)




#**********************************************************
#---- Statistiques descriptives----


# On sélectionne tout d'abord les variables dont on souhaite des statistiques descriptives :

stats<- pdata %>% select(growthrate,gdpcap)

# On visualise les statistiques descriptives à l'aide d'un tableau.
stargazer(basicStats(stats) ,
          type = "text",
          title = "Table: Statistiques descriptives",
          summary=F)


# Pour la présentation, on en extrait le code latex.
stargazer(basicStats(stats), 
          type = "latex",
          title = "Table: Statistiques descriptives",
          summary=F)



#********************************
#---- Estimation des modèles ----


#---- Les MCO ----

# Dans un premier temps, nous omettons le fait que nous travaillons avec des données de panel.
# 3 modèles sont considérés.

MCO <- lm(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr
          ,data=pdata)
summary(MCO)
coeftest(MCO,vcov=vcovHC(MCO,type="HC1"))


#---- Modèle Within----

WITHIN <- plm(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr,
              model="within",effect="twoways",data=pdata)
summary(WITHIN)
coeftest(WITHIN,vcov=vcovHC(WITHIN,type="HC1"))


#---- Modèle GLS----

GLS <- plm(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr,
           model="random",effect="twoways",data=pdata)
summary(GLS)
coeftest(GLS,vcov=vcovHC(GLS,type="HC1"))


# On crée une variable stockant les écarts-type robustes de chaque modèle...
rob_se <- list(sqrt(diag(vcovHC(MCO, type = "HC1"))),
               sqrt(diag(vcovHC(WITHIN, type = "HC1"))),
               sqrt(diag(vcovHC(GLS, type = "HC1"))))

stargazer(list(MCO, WITHIN, GLS),title = "Résultat des estimations OLS, WITHIN et GLS",
          column.labels = c("OLS", "WITHIN", "GLS"), align = T, type = "text",
          no.space = F,se=rob_se)

stargazer(list(MCO, WITHIN, GLS),title = "Résultat des estimations OLS, WITHIN et GLS",
          column.labels = c("OLS", "WITHIN", "GLS"), align = T, type = "latex",
          no.space = F,se=rob_se)



#********************************
#---- Tests d'existence et test d'Hausman----

#---- F-test----

F1<- pFtest(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr,
            effect="twoways",data=pdata)
F2<- pFtest(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr,
            effect="indiv",data=pdata)
F3<- pFtest(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr,
            effect="time",data=pdata)
F1 ; F2 ; F3 # -> modèle à deux effets.


#---- LM test----

L1 <- plmtest(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr,
              effect="twoways",data=pdata,type="bp")
L2 <- plmtest(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr,
              effect="indiv",data=pdata,type="bp")
L3 <- plmtest(growthrate~log_gdp+log_fertility+inv_expec+educ+inf+openr,
              effect="time",data=pdata,type="bp")
L1 ; L2 ; L3 # -> modèle à deux effets -> OLS rejeté...

#---- Test d'Hausman----

phtest(WITHIN, GLS) # Effets fixes => Within avec deux effets est retenu au final




