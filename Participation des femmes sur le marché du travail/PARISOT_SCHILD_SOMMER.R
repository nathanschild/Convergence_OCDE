rm(list=ls())

#############################################
#  MASTER APE                               #
#  Dossier R                                #         
#  Nathan SCHILD, Jade PARISOT, Zoé SOMMER  #        
#############################################



#----------------------------------------------------------------------------------------------------------------------------------------
# La base de données utilisée dans ce dossier comporte plusieurs variables issues de différentes sources.
# Ces sources contiennent notamment : l'OCDE, la Banque Mondiale et l'OIT.
# Tout les données utilisées sont accessibles. 


# Voici une description des variables utilisées :
# fertility : fécondité (nombre d'enfants moyen/femme)
# participation : le taux de participation des femmes sur le marché du travail (15-64 ans).
# meanage : l'âge moyen des femmes à la naissance du premier enfant.
# espbirth : l'espérance de vie à la naissance des femmes.
# dep : les dépenses publiques en prestations familiales.
# married : le taux de participation des femmes mariées sur le marché du travail (15-64 ans).
# pop : la population de femmes âgées de 15 à 64 ans 

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





# On peut désormais importer les données sur R.
data <- read_excel("data.xlsx")

# On visualise la base de données
View(data) 



# On renomme les colonnes .
colnames(data)<-c("code","year","country","fertility","participation","meanage","espbirth","dep","married","pop")


# On transforme la base de données pour que R se rende compte qu'on travaille sur données de panel.
pdata<-pdata.frame(data,index=c("country","year"),drop.index=T)
View(pdata)




#**********************************************************
#---- Statistiques descriptives et analyses graphiques ----


# On sélectionne tout d'abord les variables dont on souhaite des statistiques descriptives :

stats<- data %>% select(participation,fertility)

# On visualise les statistiques descriptives à l'aide d'un tableau.
stargazer(basicStats(stats) ,
          type = "text",
          title = "Table: Statistiques descriptives des variables d'intérêt",
          summary=F)


# Pour la présentation, on en extrait le code latex.
stargazer(basicStats(stats), 
          type = "latex",
          title = "Table: Statistiques descriptives des variables d'intérêt",
          summary=F)

# On veut ensuite créer un diagramme à barres.

# On commence par calculer la moyenne par pays
moyenne_part_pays <- data %>%
  group_by(country) %>%
  summarise(mean_participation = mean(participation, na.rm = TRUE)) %>%
  arrange(desc(mean_participation))

# Puis on peut éditer le diagramme à barres.
ggplot(data = moyenne_part_pays, aes(x = reorder(country, -mean_participation), y = mean_participation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Participation moyenne des femmes par pays",
       x = "Pays",
       y = "Participation moyenne (en %)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x


# On crée un graphique pour représenter les tendances pour chaque pays
ggplot(data, aes(x = year, y = participation, color = country)) +
  geom_line(size=0.75) +
  labs(title = "Évolution de la participation des femmes sur le marché du travail par pays",
       x = "Année",
       y = "Participation (en %)") +
  theme_bw()

# Pour que ce soit plus esthétique, on travaille avec la moyenne globale.

# Calcul de la moyenne par année
moyenne_participation <- data %>%
  group_by(year) %>%
  summarise(moyenne_participation = mean(participation, na.rm = TRUE))

# On crée le graphique
ggplot(moyenne_participation, aes(x = year, y = moyenne_participation)) +
  geom_point(color="blue",size=2,shape=15) +
  geom_line(size=1) +
  labs(title = "Évolution moyenne de la participation (2005-2019)",
       x = "Année",
       y = "Moyenne de la participation (en %)") +
  theme_stata()



# On représente graphiquement l'évolution de la variable fertility.
ggplot(data, aes(x = year, y = fertility, color = country)) +
  geom_line(size=0.75) +
  labs(title = "Évolution de la fécondité au fil du temps",
       x = "Année",
       y = "Fécondité") +
  theme_bw()

# On prend la moyenne de fertility pour que ce soit plus esthétique.
moyenne_fertility <- data %>%
  group_by(year) %>%
  summarise(moyenne_fertility = mean(fertility, na.rm = TRUE))

# On peut éditer le graphique.

ggplot(moyenne_fertility, aes(x = year, y = moyenne_fertility)) +
  geom_point(color="blue",size=2,shape=15) +
  geom_line(size=1) +
  labs(title = "Évolution moyenne de la fécondité (2005-2019)",
       x = "Année",
       y = "Moyenne de la fécondité (2005-2019)") +
  theme_stata()



# Désormais, on analyse graphiquement s'il y a une relation observable entre les deux variables.
ggplot(data, aes(x = fertility, y = participation, color = country)) +
  geom_line(size=0.75) +
  labs(title = "Relation participation-fécondité",
       x = "Fécondité",
       y = "Participation (en %)") +
  theme_bw()

# On peut représenter cette relation en utilisant les moyennes des variables.

    # Tout d'abord, on crée un nouveau dataframe pour stocker les moyennes.

dat_moy<- data.frame(moyenne_fertility$moyenne_fertility,moyenne_participation$moyenne_participation)
colnames(dat_moy) <-c("moyf","moyp")

ggplot(dat_moy, aes(x = moyf, y = moyp)) +
  geom_point(color="blue",size=2,shape=15) +
  geom_line(size=1) +
  labs(title = "Relation participation-fertilité en moyenne",
       x = "Fécondité",
       y = "Participation (en %)") +
  theme_stata()



# On édite une matrice de corrélation (on enlève les trois premières colonnes, inutile dans ce cas)
data_2<-data[c(-1,-2,-3)]
mat_cor<-cor(data_2)
corrplot(mat_cor, method = "color", type = "upper", order = "hclust",
         col = colorRampPalette(c("#ffffff", "#0570b0"))(200), # Exemple de palette de couleurs
         addCoef.col = "black", # Couleur des coefficients
         tl.col = "black", tl.srt = 45, # Couleur et angle des étiquettes
         diag = FALSE)

# Observations : 
# La variable expliquée est plutôt bien corrélée aux variables explicatives.
# Attention, il peut y avoir de la collinéarité entre "espbirth" et "meanage".





#********************************
#---- Estimation des modèles ----


#---- Les MCO ----

# Dans un premier temps, nous omettons le fait que nous travaillons avec des données de panel.
# Nous considérerons des régressions de type log-log afin de pallier à l'hétéroscédasticité au maximum.
# 3 modèles sont considérés.


# Premier modèle :

mco<-lm(log(participation)~log(fertility),data=data)
summary(mco)
# A priori pas de lien direct entre participation et fertilité.


# Deuxième modèle :

mco2<-lm(log(participation)~log(fertility)+log(meanage)+log(dep),data=data)
summary(mco2)
# meanage et dep, sont bien reliés au taux de participation
# La constante et la fertilité sont significatifs au seuil de 10%.

vif(mco2). # Aucun problème de collinéarité.
bptest(mco2) # Présence d'hétéroscédasticité...
coeftest(mco2, vcov = vcovHC(mco2, type = "HC0")) # tout est significatif sauf la fertilité...


# Troisième modèle :

mco3<-lm(log(participation)~log(fertility)+log(meanage)+log(dep)+log(espbirth)+log(pop),data=data)
summary(mco3)

# dep, espbirth, la fertilité et la constante sont significatifs. 

bptest(mco3) # Présence d'hétéroscédasticité...
qchisq(0.95,5)
wh_m<-coeftest(mco3, vcov = vcovHC(mco3, type = "HC0")) # Tout est significatif à 10% sauf la pop.
vif<-vif(mco3) # Pas de collinéarité
vif


# Utilisons la library stargazer pour extraire un tableau descriptif ainsi que le code latex associé.
stargazer(mco,mco2,mco3, 
          type = "text", 
          title = "Estimation par les MCO des déterminants de la participation des femmes sur le marché du travail (2005-2019)")

stargazer(mco,mco2,mco3, 
          type = "latex", 
          title = "Estimation par les MCO des déterminants de la participation des femmes sur le marché du travail (2005-2019)")

stargazer(wh_m, 
          type = "latex", 
          title = "Estimation par les MCO avec l'écart-type de White")

stargazer(vif, 
          type = "latex", 
          title = "Facteur d'inflation de la variance")

# Graphique des résidus , mettre en annexe
mean(residuals(mco3))
hist(residuals(mco3))

# Observations :
# La fertilité semble tout de même liée au taux de participation des femmes sur le marché du travail.
# Prenons le dernier modèle, TCEPA, une hausse de 1% du nombre d'enfants/moyen par femme entraîne une baisse de 0,079% de taux de participation des femmes sur le marché du travail.
# Il semble que  dep et espbirth soient liés au taux de participation des femmes.
# Le R^(2) ajusté augmente entre chaque modèle, notre pouvoir explicatif est de 52.7% au final.
# Il n'y pas de collinéarité entre les variables.
# Il y a tout de même de l'hétéroscédasticité.
# En utilisant les écarts-types robustes, on se rend compte que cela ne change pas énormément la significativité.






#---- Modèles à effets fixes I (Within) ----

# Nous souhaitons voir dans notre travail la relation entre les variables au fil du temps. 
# Pour cela, nous adoptons l'approche "within", ce qui signifie que nous mettons des effets fixes individuels.
# Si nous avions voulu faire une comparaison entre les pays, nous aurions adopté une approche "between".
# Ici, nous souhaitons voir l'évolution de cette relation au fil du temps au sein des pays.

# Nous prenons comme choix le fait de faire un modèle log-log, pour potentiellement éviter l'hétéroscédasticité.


model<-plm(log(participation)~log(fertility), data=pdata,model="within")
summary(model) # A priori aucun lien direct...


# Considérons un deuxième modèle en ajoutant plus de variables.

model2<-plm(log(participation)~log(fertility)+log(meanage)+log(dep), data=pdata,model="within")
summary(model2) # fertility et meanage significatifs...

bptest(model2) # Présence d'hétéroscédasticité 
coeftest(model2, vcov = vcovHC(model2, type = "HC0")) # en mettant les écarts-types robustes de Whiten, juste meanage est significatif.


# Considérons un troisème modèle incluant toutes les variables.
model3<-plm(log(participation)~log(fertility)+log(meanage)+log(dep)+log(espbirth)+log(pop), data=pdata,model="within")
summary(model3)
bptest(model3)
wh_w<-coeftest(model3, vcov = vcovHC(model3, type = "HC0"))
wh_w

stargazer(wh_w, 
          type = "latex", 
          title = "Estimation Within avec l'écart-type de White")




# Utilisons la library stargazer pour extraire un tableau descriptif ainsi que le code latex associé.
stargazer(model,model2,model3, 
          type = "text", 
          title = "Estimation Within des déterminants de la participation des femmes sur le marché du travail (2005-2019)")

stargazer(model,model2,model3, 
          type = "latex", 
          title = "Estimation Within des déterminants de la participation des femmes sur le marché du travail (2005-2019)")

# Observations :
# A première vue, meanage et fertility semblent reliées au taux de participation.
# Néanmoins, mettre un modèle log-log n'aura pas suffi car l'hétéroscédasticité est bien présente.
# En mettant les écarts-types robustes de White, seule la variable meanage semble cohérente.
# Le principal déterminant de la participation des femmes sur le marché du travail serait l'âge moyen des femmes à la naissance du premier enfant ?
# Nous verrons que nous ne sommes pas en manque de problème d'endogénéité dans ce cas...
# Notons que le R^(2) ajusté augmente entre chaque modèle, pour au final arriver au stade de 53%.


#---- Modèle à effets fixes II (Within)----

# Nous proposons une deuxième spécification avec cette fois-ci comme variable explicative "married".
# La méthode reste identique à celle proposée précédemment.

# Commençons par le premier modèle simple.
lm_fit1<-plm(log(married)~log(fertility),data=pdata,model="within")
summary(lm_fit1) 
# fertility semble reliée à married...


# Considérons un deuxième modèle incluant plus de variables explicatives.

lm_fit2<-plm(log(married)~log(fertility) + log(meanage) + log(dep),data=pdata,model="within")
summary(lm_fit2) # seule meanage a du sens encore une fois.

bptest(lm_fit2) # Présence d'hétéroscédasticité
coeftest(lm_fit2, vcov = vcovHC(lm_fit2, type = "HC0")) # seule meanage résiste


# Enfin le dernier modèle.
lm_fit3<-plm(log(married)~log(fertility)+log(dep)+log(meanage)+log(espbirth)+log(pop),data=pdata,model="within")
summary(lm_fit3)

bptest(lm_fit3)# Encore de l'hétéroscédasticité
coeftest(lm_fit3, vcov = vcovHC(lm_fit3, type = "HC0")) # rien ne résiste sauf meanage.


# Utilisons la library stargazer pour extraire un tableau descriptif ainsi que le code latex associé.

stargazer(lm_fit1,lm_fit2,lm_fit3, 
          type = "text", 
          title = "Estimation Within des déterminants de la participation des femmes mariées sur le marché du travail (2005-2019)")

stargazer(lm_fit1,lm_fit2,lm_fit3, 
          type = "latex", 
          title = "Estimation Within des déterminants de la participation des femmes sur le marché du travail (2005-2019)")



# Observations :
# On pourrait croire dans un premier temps que la fertilité semble bien expliquer married.
# Néanmoins, au fil des spécifications, sa significativité statistique n'est plus présente.
# Comme le précédent modèle, le R^(2) ajusté augmente entre chaque modèle mais il est plus faible que dans les premiers modèles.
# On pourrait également croire que meanage et espbirth ont un impact significatif.
# Mais, en mettant les écarts-types robustes afin de respecter (A3), plus rien n'est significatif.
# Finalement, les deux spécifications sont presque identiques mis à part le fait que fertility soit significatif au départ.







