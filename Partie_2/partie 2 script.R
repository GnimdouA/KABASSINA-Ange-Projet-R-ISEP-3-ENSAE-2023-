# Partie 2

# importer la base 
# chargement du package nécessaire à l'importation 
library(readxl)

# Importation de la base sous format data.frame dans l'objet projet 
projet2 = data.frame(readxl::read_excel("Partie_2/Base_Partie 2.xlsx"))

# Nettoyage et gestion des données 
# renommons “country_destination” en “destination” 
projet2 = projet2  %>% dplyr::rename("destination" = "country_destination")

# Definir les vleurs négatives de destination comme manquantes
projet2 = projet2  %>% dplyr::mutate(destination = ifelse(destination<0, NA, destination))

# création une nouvelle variable contenant des tranches d’âge de 5 ans en utilisant la variable “age”.
# voyons comment se présente la variable age pour definir le nombre de classe d'âge à créer, par où commencer et où s'arrêter
summary(projet2$age)

# remplacer les 999 par la médiane 
projet2 = projet2  %>% dplyr::mutate(age = ifelse(age==999, median(age), age))

# voyons la nouvelle distribution
summary(projet2$age)

# création des tranches d'âges
bornes = c(14, 20, 25, 30, 35, 40, 45)# bornes des intervales 
etiquettes = c("entre 15 et 20", "entre 20 et 25", "entre 25 et 30","entre 30 et 35",
               "entre 35 et 40", "entre 40 et 45") # valeurs de la nouvelle variable c a d les intervales
projet2 = projet2 %>% dplyr::mutate(tranche_age=cut(age, breaks = bornes, labels = etiquettes))

# Création d'une nouvelle variable contenant le nombre d’entretiens 
# réalisés par chaque agent recenseur
projet2 <- projet2 %>% 
  group_by(enumerator) %>% # séparer en sous groupes suivant l'identifiant de l'agent
  mutate(nb_entretiens = n()) # calculer le nombre de ligne de chaque sous groupe et l'ajouter à la nouvelle varible

# Création d'une nouvelle variable qui affecte aléatoirement chaque répondant à
# un groupe de traitement (1) ou de controle (0).
projet2 <- projet2 %>% dplyr:: rowwise()  %>%  # pour faire l'aléa ligne par ligne
  mutate(g_traitement = sample(c(0,1),size=1))

# labelisation des valeurs de la nouvelle variable créée 
# 1 = groupe de traitement et 0 = groupe de controle.
projet2 <- projet2 %>%
  mutate(g_traitement = factor(g_traitement, levels = c(0, 1),
                                    labels = c("groupe de contrôle", "roupe de traitement")))
# Fusion de la taille de la population de chaque district (feuille 2) avec l’ensemble de données (feuille 1)
## Importation de la base sur la population des districts
district = data.frame(readxl::read_excel("Partie_2/Base_Partie 2.xlsx",sheet = "district"))

## Fusion
projet2 = merge(projet2, district, by="district")

# Calcul de la durée de l’entretien et de la durée moyenne de l’entretien par enquêteur.
## calcul durée de l'entretien
projet2 <- projet2 %>%
  mutate(dure_entretien = endtime - starttime)
                               
# durée moyenne de l'entretien par agent 
# Calculer la durée moyenne de l'entretien par enquêteur
dure_moyenne <- projet2 %>% aggregate(dure_entretien ~ enumerator, FUN = mean)

# Afficher la durée moyenne de l'entretien par enquêteur sous forme de tableau de format Markdown
knitr::kable(dure_moyenne, format = "markdown") %>%
  kableExtra::kable_styling()

# Renommons toutes les variables de l’ensemble de données en ajoutant 
# le préfixe “endline_” à l’aide d’une boucle.

## méthode 1 : boucle for 
old_names = names(projet3)  # vecteur des nom de toutes les variabes
new_names = paste("endline",old_names,sep="_")  # renommer l'ancien nom x par concatenation de "endline_" et x

# faire la boucle sur les noms des variables
for (i in 2:length(old_names)) {
  projet3 = rename(projet3,new_names[i]=old_names[i]) 
}

## méthode 2: lapply()
colnames(projet2) <- lapply(colnames(projet2), function(x) paste("endline", x, sep = "_"))

# Analyse et visualisation des données
## Tableau récapitulatif contenant l’âge moyen et le nombre moyen d’enfants par district.
library(gtsummary) # charger le package gtsummary

# faire le tableau avec gtsummary
projet2 %>%
  dplyr::select(endline_district, endline_age,endline_children_num) %>%
  gtsummary::tbl_summary(
    ## paramètres de tbl_summary
    by = endline_district, 
    ## variables qui forme les groupes: sexe
    label = list(
      endline_district = "District",
      endline_age = "Age du répondant",
      endline_children_num = "Nombre d'enfants du répondant"
    ),
    ## ajouter les étiquettes des variables 
    percent = "column", 
    type = list(endline_children_num ~ 'continuous'),
    ## Type de pourcentage affichés dans le tableau 
    statistic = c(endline_age,endline_children_num) ~ "{mean}", 
    ## statistiques à calculer pour l'âge du dirigeant (moyenne)
  ) %>% 
  add_overall() %>% 
  ## ajouter les statistiques sur la base totale (non par groupe)
  add_stat_label() %>%  
  ## afficher une colonne qui signifie les statistiques calculées et leur format d'affichage. Ex: mean (sd)
  bold_labels() 
  ## Mettre le nom des variables en gras 


# Tester si la différence d’âge entre les sexes est statistiquement significative au niveau de 5 %.
## Statistiques descriptives 
projet2 %>%
  dplyr::select(endline_sex, endline_age) %>%
  gtsummary::tbl_summary(
    ## paramètres de tbl_summary
    by = endline_sex, 
    ## variables qui forme les groupes: sexe
    label = list(
      endline_sex = "Sexe du répondant",
      endline_age = "Age du répondant"
    ),
    ## ajouter les étiquettes des variables 
    statistic = endline_age ~ "
    {median}
    {mean}
    {sd}", 
    ## statistiques à calculer pour l'âge du dirigeant (médiane, moyenne, écart-type)
    ## formatage et nomination de la variable "valeur manquante"
  ) %>% 
  add_overall() %>% 
  ## ajouter les statistiques sur la base totale (non par groupe)
  add_difference() %>% 
  ## afficher la différence entre les groupes, le test de significativité de la différence
  add_stat_label() 
## afficher une colonne qui signifie les statistiques calculées et leur format d'affichage. Ex: mean (sd)

# représentation graphique : âge suivant le sexe
library(viridis)
projet2 %>% 
  ggplot(aes(x = factor(endline_sex), y = endline_age, fill = factor(endline_sex))) +
  geom_boxplot() +
  scale_fill_viridis_d() +  # Utiliser la palette "viridis"
  labs(x = "Sexe", y = "Âge", title = "Boxplot de l'âge par sexe")

## Extraire la distribution de l'âge des hommes et celle des femmes 
dplyr::filter(endline_sex==1)%>% 
  select(endline_age) # distribution de l'âge des hommes 
age_femme = projet2 %>% dplyr::filter(endline_sex==0) %>% 
  select(endline_age) # distribution de l'âge des femmes 

## Test de normalité 
## test graphique : visualisation 
ggplot(age_homme, aes(x = endline_age )) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, color = "black", fill = "turquoise", alpha = 0.5) +
  geom_density(color = "hotpink") +
  labs(title = "Distribution de l'âge des hommes") +
  theme_minimal()

## test de shapiro 
print("Test de shapiro (hommes)")
shapiro.test (age_homme$endline_age)

## Test de normalité 
## test graphique : visualisation 
ggplot(age_femme, aes(x = endline_age )) +
  geom_histogram(aes(y = ..density..),binwidth = 1.5, color = "black", fill = "turquoise", alpha = 0.5) +
  geom_density(color = "hotpink") +
  labs(title = "Distribution de l'âge des femmes") +
  theme_minimal()

## test de shapiro 
print("Test de shapiro (femmes)")
shapiro.test (age_femme$endline_age)

## solution 1 : test non paramétrique (test de Wilcoxon)
wilcox.test(projet2$endline_age ~ projet2$endline_sex)

## solution 2 : normaliser en appliquant le logarithme 

### appliqation du logarithme
age_f_norm = log(age_femme, base = 10)
age_h_norm = log(age_homme, base = 10)


### testons pour voir si les distributions sont bien normalisées
## Test de normalité 
## test graphique : visualisation 
ggplot(age_h_norm, aes(x = endline_age )) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "black", fill = "turquoise", alpha = 0.5) +
  geom_density(color = "hotpink") +
  labs(title = "Distribution de l'âge des hommes") +
  theme_minimal()

## test de shapiro 
print("Test de shapiro (hommes)")
shapiro.test (age_h_norm$endline_age)

## Test de normalité 
## test graphique : visualisation 
ggplot(age_f_norm, aes(x = endline_age )) +
  geom_histogram(aes(y = ..density..),binwidth = 0.1, color = "black", fill = "turquoise", alpha = 0.5) +
  geom_density(color = "hotpink") +
  labs(title = "Distribution de l'âge des femmes") +
  theme_minimal()

## test de shapiro 
print("Test de shapiro (femmes)")
shapiro.test (age_f_norm$endline_age)

## test de student 
t.test(age_h_norm$endline_age,age_f_norm$endline_age)

# Création du nuage de points de l'âge en fonction du nombre d'enfants
# premier nuage de points simple ou général 
plot1 <- projet2 %>%
  ggplot(aes(x = endline_children_num, y = endline_age)) +
  geom_point() +
  labs(title = "Age et nombre d'enfant", x = "Nombre d'enfant", y = "Age") +
  theme_minimal()

# deuxième nuage de points : suivant le sexe
plot2 <- projet2 %>%
  ggplot(aes(x = endline_children_num, y = endline_age, 
             color = factor(endline_sex, levels = c(0,1),labels= c("Femme","Homme")))) +
  geom_point() +
  labs(title = "Age et nombre d'enfant suivant le sexe", 
       x = "Nombre d'enfant", y = "Age", color = "Sexe") +
  scale_color_viridis_d()+
  theme_minimal()

# troisième nuage de points : suivant le groupe
plot3 <- projet2 %>%
  ggplot(aes(x = endline_children_num, y = endline_age, 
             color = factor(endline_g_traitement))) +
  geom_point() +
  labs(title = "Age et nombre d'enfant suivant le sexe", 
       x = "Nombre d'enfant", y = "Age", color = "Groupe") +
  scale_color_viridis_d()+
  theme_minimal()

# quatrième nuage de points : suivant le district
plot4 <- projet2 %>%
  ggplot(aes(x = endline_children_num, y = endline_age, 
             color = factor(endline_district, levels = c(1,2,3,4,5,6,7,8),
                            labels= paste("District", rep(1:8), sep="_")))) +
  geom_point() +
  labs(title = "Age et nombre d'enfant suivant le sexe", 
       x = "Nombre d'enfant", y = "Age", color = "District") +
  scale_color_viridis_d()+
  theme_minimal()

# Afficher les graphiques côte à côte 
library(gridExtra)
gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Estimation de l’effet de l’appartenance au groupe de traitement sur l’intention de migrer

## selection des variables utiles au modèle : intention de migrer, âge, sexe, destination, nombre d'enfant, groupe
base_rlo = projet2 %>% select(endline_intention,endline_district,
                              endline_sex, endline_children_num, 
                              endline_g_traitement, endline_age)

  ## convertir les variables catégorielles en facteurs
base_rlo =  base_rlo %>% mutate(intention=factor(endline_intention, ordered = TRUE, 
                                                         labels = c("1","2","3","4","5","6","7")),
                                district =factor(endline_district,
                                                 levels = c(1,2,3,4,5,6,7,8), 
                                                 labels= paste("District", rep(1:8), sep="_")),
                                sex=factor(endline_sex))

 
## Régression logistique ordinale
library(ordinal)
rl_ordinal =  ordinal::clm(intention~district + 
                                         sex +
                                         endline_g_traitement + endline_age +
                                         endline_children_num, data = base_rlo)

# Création d'un tableau de régression avec 3 modèles. 
## modèle A : Modèle vide - Effet du traitement sur les intentions;
model.a = ordinal::clm(intention ~ endline_g_traitement,data = base_rlo)

## Modèle B : Effet du traitement sur les intentions en tenant compte de l’âge et du sexe;
model.b = ordinal::clm(intention ~ sex + endline_age, data = base_rlo)

## Modèle C : Identique au modèle B mais en contrôlant le district. 
model.c = ordinal::clm(intention ~ endline_sex + 
                                         endline_age + district, data = base_rlo)

## sortir les résultats dans un seul tableau 
### tabuler le modèle A
ta = model.a %>% gtsummary::tbl_regression()

### tabuler le modèle B
tb = model.b %>% gtsummary::tbl_regression()

### tabuler le modèle C
tc = model.c %>% gtsummary::tbl_regression()

### fusionner les 3 tableaux en un seul 
gtsummary::tbl_stack(
  list(ta, tb, tc),
  group_header = c("Modèle A", "Modèle B", "Modèle C") ## intitulé des groupes de tableau associés
)

### Visualisation graphique des modèles

#### Modèle A
library(GGally)
ggcoef_model(model.a, exponentiate = TRUE)
