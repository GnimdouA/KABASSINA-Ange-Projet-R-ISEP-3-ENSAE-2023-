# chargement du package nécessaire à l'importation 
library(readxl)

# Importation de la base sous format data.frame da,s l'objet projet 
projet = readxl::read_excel("Partie_1/Base_Partie 1.xlsx")

# chargement du package nécessaire au traitement de la base
library(dplyr)

# selection de toutes les variables sauf "key"
# Importons `magrittr` pour acceder aux pipes
library(magrittr)
projet_select = projet %>% dplyr::select(2:last_col())

# Verification des valeurs manquantes 
## Tabulation du nombre de valeurs manquantes par variables 
## liste pour contenir le nombre de variables manquantes
var_na = c()
for (i in 2:length(projet)){
  var_na = c(var_na, projet %>% dplyr::select(i) %>% is.na() %>% sum())
}
## stocker le résultat (variables + nombre de na) dans un dataframe
tab_na <- data.frame(variable = names(projet)[2:length(projet)], nombre_na = var_na)
## ajouter une colone avec la proportion de NA:
tab_na <- tab_na %>% mutate(proportion_na = nombre_na*100/nrow(projet))

## Afficher le data.frame sous forme de tableau au format Markdown
### nous allons utiliser kable_styling du package"kableExtra" pour mettre en forme notre tableau
library(kableExtra)
knitr::kable(tab_na, format = "markdown") %>%
  kableExtra::kable_styling()

# vefication de l'existance de valeurs manquantes pour la variable "key"
projet %>% dplyr::select("key") %>% is.na() %>% sum()

#visualisation avec vis_miss
library(visdat)
visdat::vis_miss(projet)

# Création de variables 
# renomination de variables 
projet = projet %>% dplyr::rename("region" = "q1", "departement" = "q2", "sexe"="q23")

# Création de la variable "sexe_2"
projet = projet %>% dplyr::mutate(sexe_2 = ifelse(sexe=="Femme",1,0))

# Création du data.frame langue
langue = projet %>% dplyr::select("key",starts_with("q24a_"))

# Création de la variable parle = nombre de lanqgues parlées 
langue = langue %>% dplyr::mutate(parle = rowSums(dplyr::select(langue,starts_with("q24a_")))) 

# Selection des variables les variables key et parle
langues = langue %>% dplyr::select("key","parle")

# fusion des data.frame projet et langues 
projet = merge(projet,langues, by = "key")

# ************** Statistiques descriptives**************************

# Répartition des PME suivant le sexe du dirigeant
# Tableau de fréquences
knitr::kable(prop.table(table(projet$sexe)), format = "markdown") %>%
  kableExtra::kable_styling()

# graphique en secteur
library(plotrix)
plotrix::pie3D(table(projet$sexe),labels = c("Femme","Homme") ,col = c("lightyellow","turquoise2"), main = "Sexe")

# Répartition des PME suivant le niveau d’instruction du dirigeant
# Tableau de fréquences
df = data.frame(prop.table(table(projet$q25))) # tableau de fréquence transformé en data.frame
# renommons les colones de notre data.frame de façon plus significatives
df = df %>% dplyr::rename("Niveau_instruction" = "Var1", "Frequence" = "Freq")

# sortir le tableau sous format markdown 
knitr::kable(df, format = "markdown") %>%
  kableExtra::kable_styling()

### diagramme en baton
library(ggplot2)
ggplot(df, aes(x = Niveau_instruction, y = Frequence*100))+
  geom_col(fill = "turquoise3")+
  geom_text(aes(label = Frequence*100), vjust = 1.6, color = "white")

# Répartition des PME suivant le statut juridique

# Tableau de fréquences
df = data.frame(prop.table(table(projet$q12))) # tableau de fréquence transformé en data.frame
# renommons les colones de notre data.frame de façon plus significative
df = df %>% dplyr::rename("statut_juridique" = "Var1", "Frequence" = "Freq")

# sortir le tableau sous format markdown 
knitr::kable(df, format = "markdown") %>%
  kableExtra::kable_styling()

### diagramme en baton
ggplot(df, aes(x = statut_juridique, y = Frequence*100))+
  geom_col(fill = "aquamarine3")+
  geom_text(aes(label = Frequence*100), vjust = -0.3, color = "black")

### Répartition des PME suivant le statut de propriété par rapport au local 
### de l'entreprise (propriétaire ou locataire)*

# Tableau de fréquences
df = data.frame(prop.table(table(projet$q81))) # tableau de fréquence transformé en data.frame
# renommons les colones de notre data.frame de façon plus significative
df = df %>% dplyr::rename("proprio_locataire" = "Var1", "Frequence" = "Freq")

# sortir le tableau sous format markdown 
knitr::kable(df, format = "markdown") %>%
  kableExtra::kable_styling()


# graphique en secteur
ggplot(df, aes(x = "", y=Frequence, fill = factor(proprio_locataire))) +
    geom_bar(width = 1, stat = "identity") +
    theme(axis.line = element_blank(),
          plot.title = element_text(hjust=0.5)) +
       labs(fill="proprio_locataire",
                    x=NULL,
                    y=NULL, 
                    title="Propriétaire ou locataire",
                    caption="Source: mpg") + 
  coord_polar(theta = "y", start=0)
# Répartition des PME suivant la filière
projet = projet %>% dplyr::mutate(filiere = ifelse(filiere_1==1,"arachide",
                                                   ifelse(filiere_2==1,"anacarde",
                                                          ifelse(filiere_3==1,"mangue",
                                                                 ifelse(filiere_4==1,"riz","n_s_p")))))


## Analyse du temps moyen mis pour recueillir les informations des PME
projet = projet %>% dplyr::mutate(duree = submissiondate - start)
### calcul de la durée minimale, maximale et moyenne 
#### pour l'ensemble 
tab = projet %>% summarise(min_duree = min(duree),
                           max_duree = max(duree),
                           mean_duree = mean(duree))

knitr::kable(tab, format = "markdown")
#### suivant le sexe
tableau_stat <- projet %>%
  group_by(sexe) %>%
  summarise(min_duree = min(duree),
            max_duree = max(duree),
            mean_duree = mean(duree))

knitr::kable(tableau_stat, format = "markdown")

# Analyse bivariée
## Répartion des PME suivant le sexe du dirigeant, 
## son niveau d'instruction, le statut juridique de l'entreprise 
## et le statut de propriété face au local de travail
library(gtsummary) # charger le package gtsummary

# faire le tableau
projet %>%
  dplyr::select(sexe, q24,q12, q25, q81,filiere) %>%
  gtsummary::tbl_summary(
    ## paramètres de tbl_summary
    by = sexe, 
    ## variables qui forme les groupes: sexe
    label = list(
      sexe = "Sexe du dirigeant",
      q24 = "Age du dirigeant",
      q12 = "Statut juridique",
      q25 = "Niveau d'instruction du dirigeant",
      q81 = "Statut de propriété vis à vis du local de travail",
      filiere = "Filière"
    ),
    ## ajouter les étiquettes des variables 
    percent = "column", 
    ## Type de pourcentage affichés dans le tableau 
    statistic = q24 ~ "{median}", 
    ## statistiques à calculer pour l'âge du dirigeant (médiane)
    missing = "always", 
    ## afficher les stat sur les valeurs manquantes
    missing_text = "Missing", 
    ## formatage et nomination de la variable "valeur manquante"
  ) %>% 
  add_difference() %>% 
  ## afficher la différence entre les groupes, le test de significativité de la différence
  add_stat_label() 
  ## afficher une colonne qui signifie les statistiques calculées et leur format d'affichage. Ex: mean (sd)

# *********************** Cartographie *************************************
# chargement des packages nécessaires 
library(sf)
library(ggplot2)
library(ggspatial)
library(tmap)

# conversion du dataframe en objet géographique du format sf
projet_map <- projet  %>% sf::st_as_sf(coords= c("gps_menlongitude","gps_menlatitude"),crs = 4326)

## Répartition spaciale des PME suivant le sexe du dirigeant 
### représentons d'abord le Sénégal
# Chargement des données sur le Sénégal 
sen <- sf::st_read("Partie_1/donnees/Limite_Région.shp")

# Représentation graphique 
ggplot() +
  geom_sf(data = sen, fill = "snow", color = "black") +
  geom_sf(data = projet_map, aes(color = sexe), size = 1.5) +
  scale_color_manual(values = c("hotpink3", "skyblue2")) +
  geom_sf_text(data = sen, aes(label = NOMREG), nudge_y = 0.2, size = 2) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true") +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Sénégal : répartition spatiale des PME suivant le sexe du dirigeant",
       caption = "By KABASSINA Gnimdou Ange",
       x = NULL, y = NULL,
       color = "Sexe")

## Répartition spaciale des PME suivant le niveau d'instruction du dirigeant
ggplot() +
  geom_sf(data = sen, fill = "snow", color = "black") +
  geom_sf(data = projet_map, aes(color = q25), size = 1.5) +
  scale_color_viridis_d() +
  geom_sf_text(data = sen, aes(label = NOMREG), nudge_y = 0.2, size = 2) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true") +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Sénégal : répartition spatiale des PME suivant le niveau d'instruction du dirigeant",
       caption = "By KABASSINA Gnimdou Ange",
       x = NULL, y = NULL,
       color = "Niveau d'instruction ")


# Répartition spaciale des PME suivant la filière 
ggplot() +
  geom_sf(data = sen, fill = "snow", color = "black") +
  geom_sf(data = projet_map, aes(color = filiere), size = 1.5) +
  scale_color_manual(values = c("hotpink3", "skyblue2","darkgreen","yellow")) +
  geom_sf_text(data = sen, aes(label = NOMREG), nudge_y = 0.2, size = 2) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true") +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Sénégal : répartition spatiale des PME suivant les filières d'activité",
       caption = "By KABASSINA Gnimdou Ange",
       x = NULL, y = NULL,
       color = "Filières")


# Répartition spaciale des PME suivant le statut jurique de la PME
ggplot() +
  geom_sf(data = sen, fill = "gray97", color = "black") +
  geom_sf(data = projet_map, aes(color = q12), size = 1) +
  scale_fill_manual(values = cm.colors(6))+
  geom_sf_text(data = sen, aes(label = NOMREG), nudge_y = 0.2, size = 2) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true") +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Sénégal : répartition spatiale des PME suivant le statut jurique de la PME",
       caption = "By KABASSINA Gnimdou Ange",
       x = NULL, y = NULL,
       color = "Statut jurique de la PME")



