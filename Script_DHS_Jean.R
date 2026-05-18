
##########################################################################
# ETAPE 1: installer et charger les packages neccessaires à notre analyse
########################################################################
## 1.1. installer les packages 

# packages necessaires à la manipulation des données 

install.packages("haven") # pour lire les fichiers formats SAS, Stata
install.packages("dplyr") # pour manipuler les donnees 
install.packages("labelled") # 
install.packages("survey") # pour les analyses en tenant compte de l'echantillonnage
install.packages("questionr") #

# package necessaire pour la cartographie 
install.packages("sf")    # pour lire les fichiers spatiaux tel que shapefile
install.packages("tmap") # pour faire les cartes 
install.packages("ggplot2") # pour faire egalement les cartes 
install.packages("RcolorBrewer") # pallettes de couleurs 

install.packages("here") # pour simplifier les chemins vers le projet R


## 1.2. telecharger les packages 

library(rdhs)        # Library pour accéder aux de donneées DHS par API
library(tidyverse)   # Ensemble de libraries pour le traitement et analyse des D
library(survey)      # Applicquer la pondaration à la base et aux indicateurs
library(haven)
library(here)
library(sp)          # Manipulation de données spatiales
library(sf)          # Outils de manipultation avancée des données spatiales
library(tmap)        # Outils pour la production de cartes thématiques 
library(cols4all)
library(DHSr)



#-----------------------2-Creation des sous dossiers-------------------------------------
#Defini le chemin

main         = "/Users/GounvidéJeanGBAGUIDI/Documents/My topic/FE_2024/DHS"
datasets     = "datasets"
output       = "outputs"
shapefile    = "shapefiles"
figure       = "figure"
clsgps12     = "BJGE61FL"
clsgps18     = "BJGE71FL"
#-------------------------------------------------------------------------------
#Configuration de mon projet de DHS
library(rdhs)
set_rdhs_config(email = "gouvidejeang@gmail.com",
                project = "Malaria risk modelling and prediction, vulnerability 
                of Communities to malaria in the context of climate change in
                the Norther part of Benin")

#------------------------------------------------------------------------------

#Obtenir la liste des bases de données DHS disponible entre 2011 et 2018

filelist <- dhs_datasets(countryIds = "BJ", surveyYearStart = "2011", 
                         surveyYearEnd = "2018", fileFormat = "DT")

# Telécharger toute la liste de base de données entre 2011 et 2018

dsets <- get_datasets(dataset_filenames = filelist$FileName)


# Astuce pour rechercher des variables avec des mots clés

look_for(dsets, "pr")

# Chargement des bases de données de HR, PR et KR de 2011/2012
# utiliser le chemin absolu ou utiliser l'alia ainsi qu'il suit:
# readRDS(dsets$BJHR61DT)
HR6D <- readRDS(dsets$BJHR61DT)

PR6D <- readRDS(dsets$BJPR61DT)

KR6D <- readRDS(dsets$BJKR61DT)

IR6D <- readRDS(dsets$BJIR61DT)

#------------------------------------------------------------------------------
# Chargement des bases de données de HR, PR et KR de 2017/2018

HR7D <- readRDS(dsets$BJHR71DT)

PR7D <- readRDS(dsets$BJPR71DT)

KR7D <- readRDS(dsets$BJKR71DT)

IR7D <- readRDS(dsets$BJIR71DT)
#=============================================================================
#dblist <-  paste(c("BJ"), c("HR","BR", "KR"), c(61,61,61,71,71,71),
#                 "DT",".rds", sep="")
#dbname <- paste( c("HR","BR", "KR"), c(6,6,6,7,7,7), "DT", sep="")

#i=1
#for (db in dblist){
#  assign(dbname[i],readRDS(here("~/Library/Caches/rdhs/datasets",db),
#                           envir = .GlobalEn)
#  i=i+1
#}
#=============================================================================


#------------------------------------------------------------------------------
##1. Avant toutes analyses, toujours se referer au guide d'analyse DHS
# https://dhsprogram.com/Data/Guide-to-DHS-Statistics/index.cfm

#Listes des variables d'intérêt par base de données'

# ********************LES VARIABLES IMPORTANTES*********************************
# hv042: Menage sélectionné pour le test d'hémoglobine
# hv103: A dormi la nuit dernière dans le menage
# hc1:   Âge de l'enfant en mois
# hml32: Résultat final du test de dépistage du paludisme sur frottis sanguin
# hml35: Résultat du test rapide de dépistage du paludisme
# hv005: Poids de l'échantillon du ménage
# hv000: Code du pays
# hv022: Strate
# hv021: unite primaire de sondage (UPS) : grappes (Zone de denombrement)
# hv024: Région
# hv104: Sexe
# ******************************************************************************


#Menage

hrvarlist  = c("hv000", "hhid", "hv001", "hv002", "hv003", "hv012",
               "hv024", "hv025", "hv005", "hv021", "hv022", "hv220",
               "hv227", "hml1", "hv014", "hv015","hv023", 
               "hv013", "hv270", "hv219", "hv216", "hv009" , 
               "hml10_1","hml10_2", "hml10_3", "hml10_4",
               "hml10_5", "hml10_6", "hml10_7")



#
#Membres de ménages

prvarlist <- c("hv106", "hv115","hml21", "hml12", "hml19", "hml20","hv013",
               "hml10","hvvidx","hml11","hv104","hv105","hv107", 
               "hml16","hml16a","hml32", "hml33", "hml35","hv005",
               "hv024", "hhid", "hv001", "hv023")

# Enfants de 0-5 ans 

krvarlist <- c(paste("h37",c(letters),sep=""),
               paste("ml13",c(letters),sep=""),"h22","hvaa", "hvab",
               "hvda", "hhid", "hvvidx","hm32","ml13", "v005", "v001",
               "v002", "v003", "mdidx", "caseid","b5", "v023", "v024","v017")

irvarlist <- c("m49a_1", "ml1_1", "v005", "v002", "v003", "caseid",
               "v023", "v024","v008", "b3_01", "v001")
#-------------------------------------------------------------------------------
#Extraction des variables d'intéret des diférentes bases de données 

HR6DE <- HR6D %>% dplyr::select_if(names(.) %in% hrvarlist)
HR7DE <- HR7D %>% dplyr::select_if(names(.) %in% hrvarlist)

PR6DE <- PR6D %>% dplyr::select_if(names(.) %in% prvarlist)
PR7DE <- PR7D %>% dplyr::select_if(names(.) %in% prvarlist)

KR6DE <- KR6D %>% dplyr::select_if(names(.) %in% krvarlist)
KR7DE <- KR7D %>% dplyr::select_if(names(.) %in% krvarlist)

IR6DE <- IR6D %>% dplyr::select_if(names(.) %in% irvarlist)
IR7DE <- IR7D %>% dplyr::select_if(names(.) %in% irvarlist)
#-------------------------------------------------------------------------------
# Ajouter les informations sur le nombre de moustiquaire disponible par menage 
# dans la base membre du menage 

nbintvar = c("hhid", "hml10_1", "hml10_2", "hml10_3", "hml10_4",
             "hml10_5", "hml10_6", "hml10_7", "id2")

hintvar12 <- HR6D %>% dplyr::select_if(names(.) %in% nbintvar)
hintvar18 <- HR7D %>% dplyr::select_if(names(.) %in% nbintvar)

PR6DE     <- merge(PR6DE,hintvar12, id=hhid)
PR7DE     <- merge(PR7DE,hintvar18, id=hhid)

#-------------------------------------------------------------------------------

# Transformation des variables : Base menage extraite

#Les functions personnalisées utilisées dans cette analyse 

transformhrvar <- function(df) {
  df <- df %>%
    filter(hv013 > 0) %>%
    mutate(
      hacess_mbn = ifelse(hv227 == 1, 100, 0),  # Access to Bed net
      h_haveitn = rowSums(.[grep("hml10_", names(.))], na.rm = TRUE),  # Number of ITNs
      haccess_itn = ifelse(h_haveitn > 0, 100, 0),  # Ownership of ITNs
      hhaveint_twice = ifelse(h_haveitn / hv013 >= 0.5, 100, 0),  # Household access to ITNs
      hv024 = case_when(
        hv024 == 1 ~ "Alibori",
        hv024 == 2 ~ "Atacora",
        hv024 == 3 ~ "Atlantique",
        hv024 == 4 ~ "Borgou",
        hv024 == 5 ~ "Collines",
        hv024 == 6 ~ "Couffo",
        hv024 == 7 ~ "Donga",
        hv024 == 8 ~ "Littoral",
        hv024 == 9 ~ "Mono",
        hv024 == 10 ~ "Ouémé",
        hv024 == 11 ~ "Plateau",
        hv024 == 12 ~ "Zou"
      ),
      hv005 = hv005 / 1000000  # Scaling hv005
    )
  return(df)
}


transformprvar <- function(df) {
  df <- df %>%
    filter(hv013 > 0) %>%
    mutate(
      prev_bloodtest = ifelse(hml32 == 1, 100, ifelse(hml32 == 0, 0, NA)),
      prev_raptest = ifelse(hml35 == 1, 100, ifelse(hml35 == 0, 0, NA)),
      slept_int = ifelse(hml12 %in% c(1, 2), 100, ifelse(hml12 %in% c(0, 3), 0, NA)),
      hnber_ofitn = rowSums(.[grep("hml10_", names(.))], na.rm = TRUE),
      capacity_int = hnber_ofitn * 2,
      capacity_int_adj = pmin(capacity_int, hv013),  # Adjust to household size
      hv005 = hv005 / 1000000,
      hv024 = case_when(
        hv024 == 1 ~ "Alibori",
        hv024 == 2 ~ "Atacora",
        hv024 == 3 ~ "Atlantique",
        hv024 == 4 ~ "Borgou",
        hv024 == 5 ~ "Collines",
        hv024 == 6 ~ "Couffo",
        hv024 == 7 ~ "Donga",
        hv024 == 8 ~ "Littoral",
        hv024 == 9 ~ "Mono",
        hv024 == 10 ~ "Ouémé",
        hv024 == 11 ~ "Plateau",
        hv024 == 12 ~ "Zou"
      )
    ) %>%
    group_by(hhid) %>%
    arrange(hhid, -slept_int) %>%
    mutate(
      access_ind = c(
        rep(100, times = min(unique(capacity_int_adj), n())),
        rep(0, times = max(n() - min(unique(capacity_int_adj), n()), 0))
      )
    ) %>%
    ungroup()
  return(df)
}

transformkrvar <- function(df) {
  df <- df %>%
    mutate(
      cm_coverage = ifelse(h22 == 1 & ml13e == 1, 100, 
                           ifelse(h22 == 1 & (ml13e == 0 | is.na(ml13e)), 0, NA)),
      v024 = case_when(
        v024 == 1 ~ "Alibori",
        v024 == 2 ~ "Atacora",
        v024 == 3 ~ "Atlantique",
        v024 == 4 ~ "Borgou",
        v024 == 5 ~ "Collines",
        v024 == 6 ~ "Couffo",
        v024 == 7 ~ "Donga",
        v024 == 8 ~ "Littoral",
        v024 == 9 ~ "Mono",
        v024 == 10 ~ "Ouémé",
        v024 == 11 ~ "Plateau",
        v024 == 12 ~ "Zou"
      )
    )
  return(df)
}

transformirvar <- function(df) {
  df <- df %>%
    mutate(
      age = v008 - b3_01,
      
      iptfd = case_when(
        m49a_1==1 & age<24  ~ 100,
        m49a_1!=1 & age<24  ~ 0),
      
      iptsd =case_when(
        m49a_1==1 & ml1_1 >=2 & ml1_1<=97 & age<24  ~ 100,
        !(m49a_1==1 & ml1_1 >=2 & ml1_1<=97) & age<24  ~ 0),
      ipttd =case_when(
        m49a_1==1 & ml1_1 >=3 & ml1_1<=97 & age<24  ~ 100,
        !(m49a_1==1 & ml1_1 >=3 & ml1_1<=97) & age<24  ~ 0),
      
      v024 = case_when(
        v024 == 1 ~ "Alibori",
        v024 == 2 ~ "Atacora",
        v024 == 3 ~ "Atlantique",
        v024 == 4 ~ "Borgou",
        v024 == 5 ~ "Collines",
        v024 == 6 ~ "Couffo",
        v024 == 7 ~ "Donga",
        v024 == 8 ~ "Littoral",
        v024 == 9 ~ "Mono",
        v024 == 10 ~ "Ouémé",
        v024 == 11 ~ "Plateau",
        v024 == 12 ~ "Zou"
      )
    )
  return(df)
}


HR6DET <- transformhrvar(HR6DE)
HR7DET <- transformhrvar(HR7DE)

# Transformation des variables : Base individus extraite

PR6DET <- transformprvar(PR6DE)
PR7DET <- transformprvar(PR7DE)

# Transformation des variables : Base enfants extraite

KR6DET <- transformkrvar(KR6DE)
KR7DET <- transformkrvar(KR7DE)

# Transformation des variables : Base nouveau né

IR6DET <- transformirvar(IR6DE)
IR7DET <- transformirvar(IR7DE)


#-------------------------------------------------------------------------------

# Prise en compte du plan d'echantillonnage :

# utiliser le package "survey" et tenir du poids,UPS et la strate 


#Pondération des bases de données# poids de ponderation : hv005/1000000 

#Menage

HR6DETW <- svydesign(ids= ~hv001, strata = ~hv023, data = HR6DET, 
                     weights = ~hv005)
HR7DETW <- svydesign(ids= ~hv001, strata = ~hv023, data = HR7DET, 
                     weights = ~hv005)
#Individu

PR6DETW <- svydesign(ids= ~hv001, strata = ~hv023, data = PR6DET, 
                     weights = ~hv005)
PR7DETW <- svydesign(ids= ~hv001, strata = ~hv023, data = PR7DET, 
                     weights = ~hv005)
#Enfant
KR6DETW <- svydesign(ids= ~v001, strata = ~v023, data = KR6DET, 
                     weights = ~v005)
KR7DETW <- svydesign(ids= ~v001, strata = ~v023, data = KR7DET, 
                     weights = ~v005)
#Nouveau niveau

IR6DETW <- svydesign(ids= ~v001, strata = ~v023, data = IR6DET, 
                     weights = ~v005)
IR7DETW <- svydesign(ids= ~v001, strata = ~v023, data = IR7DET, 
                     weights = ~v005)

#----------------------#Calcule des indicateurs --------------------------------------


### option : utiliser la svyby  pour la avoir la proportion 


#Calcule des indicateurs : possession et accessibilité des INT dans les Menages

hddf <- list(HR6DETW, HR7DETW)
step=6
for (df in hddf){
  df_name = paste("hrmalaria_ind", step, sep="")
  assign(df_name,svyby(~haccess_itn + hhaveint_twice, ~hv024,df, 
                       svymean, keep.var=FALSE,keep.names = FALSE,
                       na.rm=TRUE), envir = .GlobalEnv )
  step= step+1
  print(df_name)
}

#-------------------------------------------------------------------------------
#Calcule des indicateurs : acces ind, utilisation INT, Pr TDR, et miro 

prdf = list(PR6DETW,PR7DETW)
variables = c("access_ind","slept_int","prev_bloodtest","prev_raptest")
step=6
for (df in prdf){
  df_name = paste("prmalaria_ind", step, sep="")
  resultats <- lapply(variables, function(var) {
    svyby(as.formula(paste("~", var)), ~hv024, df, svymean, na.rm = TRUE,
          keep.var=T,keep.names = T, drop.empty.groups=T)
  })
  assign(df_name, resultats %>% reduce(full_join, by="hv024") %>% 
           select(-starts_with("se.")),envir = .GlobalEnv )
  step= step+1
  print(df_name)
}


#-------------------------------------------------------------------------------
#Calcule des indicateurs : acces au soins

krdf = list(KR6DETW, KR7DETW)

step=6
for (df in krdf){
  df_name = paste("krmalaria_ind", step, sep="")
  assign(df_name,svyby(~cm_coverage, 
                       ~v024,df,svymean, keep.var=FALSE,keep.names = FALSE, na.rm=TRUE), envir = .GlobalEnv )
  step= step+1
  print(df_name)
}


#-------------------------------------------------------------------------------
#Calcule des indicateurs : acces au soins

irdf = list(IR6DETW, IR7DETW)

step=6
for (df in irdf){
  df_name = paste("irmalaria_ind", step, sep="")
  assign(df_name,svyby(~iptfd+iptsd+ipttd, 
                       ~v024,df,svymean, keep.var=FALSE,keep.names = FALSE,
                       na.rm=TRUE), envir = .GlobalEnv )
  step= step+1
  print(df_name)
}


#-------------------------------------------------------------------------------
# Fussionner les indicateurs par édition 

if ("v024" %in% names(krmalaria_ind6)){
  krmalaria_ind6 <- krmalaria_ind6 %>% rename(hv024=v024) #renommer vO24 en hv024
  krmalaria_ind7 <- krmalaria_ind7 %>% rename(hv024=v024) #renommer vO24 en vO24
  
  irmalaria_ind6 <- irmalaria_ind6 %>% rename(hv024=v024) #renommer vO24 en hv024
  irmalaria_ind7 <- irmalaria_ind7 %>% rename(hv024=v024) #renommer vO24 en vO24
  
}

df_inds_2012 <- list(hrmalaria_ind6,prmalaria_ind6,krmalaria_ind6, irmalaria_ind6)
df_inds_2018 <- list(hrmalaria_ind7,prmalaria_ind7,krmalaria_ind7, irmalaria_ind7)


indicateurs_2012 <- df_inds_2012 %>% reduce(full_join, by="hv024") #fusion 2012
indicateurs_2018 <- df_inds_2018 %>% reduce(full_join, by="hv024") #fusion 2018

# write the csv file'
# write.csv(indicateurs_2012, file = "DHS_2012@.csv", row.names = FALSE)
# write.csv(indicateurs_2018, file = "DHS_2018@.csv", row.names = FALSE)

#-------------------------------------------------------------------------------

#Chargement du shapefile du niveau administratif 1 : département pour le Bénin 
#st_crs(admin1sh) <- 4326

# admin1sh <- st_read(here(F:/FE_2024/DHS/BJGE61FL/BEN_adm1.shp"))


admin1sh <- st_read(here("F:/FE_2024/DHS/shapefiles/BEN_adm1.shp"))
admin1sh <- st_transform(admin1sh, crs=4326)


#Fussion avec les indicateurs 
# Mutation de colonne NAME_1 column dans admin1sh
library(dplyr)

# Update NAME_1 values and rename hv024 to NAME_1
admin1sh <- admin1sh %>%
  mutate(NAME_1 = case_when(
    NAME_1 == "Atakora" ~ "Atacora",
    NAME_1 == "Kouffo" ~ "Couffo",
    TRUE ~ as.character(NAME_1)  # Keep the original value if no match
  )) %>%
  rename( hv024 = NAME_1)  # Change hv024 to a new NAME_1 if needed

# Create the list for joining
df_shf2012 <- list(admin1sh, indicateurs_2012)
df_shf2018 <- list(admin1sh, indicateurs_2018)

# Perform the join
library(dplyr)

# Perform a left join
indicateur_2012_shf <- left_join(admin1sh, indicateurs_2012, by ="hv024")
indicateur_2018_shf <- left_join(admin1sh, indicateurs_2018, by ="hv024")
# Check the result
head(indicateur_2012_shf)
head(indicateur_2018_shf)

#-------------------------------------------------------------------------------
##5.3. Faire la cartographie de la prevalence avec le package tmap 


# utiliser les palettes couleur de 'RdYlBu'

display.brewer.pal(n = 11, name = 'RdYlBu')# couleur 

brewer.pal(n = 11, name = "RdYlBu")

indicateur_2012_shf %>%
  tm_shape() +
  tm_polygons("prev_bloodtest", title = "Prévalence du paludisme au Benin",
              breaks = seq(5, 60, by = 5),
              palette = "-RdYlBu") +
  tm_layout(legend.outside = TRUE,
            legend.title.size = 0.5,
            legend.text.size = 0.5, frame = FALSE,
            legend.position = c("left", "bottom")) +
  tm_text("hv024", size = 0.4)


##option 2 : defenir les couleurs 
library(RColorBrewer)

indicateur_2018_shf %>%
  tm_shape() +
  tm_polygons("prev_bloodtest", title = "Prévalence du paludisme au Bénin",
              breaks = seq(5, 60, by = 5),
              palette = brewer.pal(9, "RdYlBu")) +  # Automatic palette from blue to red
  tm_layout(legend.outside = TRUE,
            legend.title.size = 1.0,
            legend.text.size = 0.6, 
            frame = FALSE,
            legend.position = c("left", "bottom")) +
  tm_text("hv024", size = 0.6)


## option utiliser d'autres package de couleurs

library(viridis) # Assurez-vous que le package viridis est installé

indicateur_2018_shf %>%
  tm_shape() +
  tm_polygons("prev_bloodtest", title = "Prévalence du paludisme BEnin",
              breaks = seq(5, 60, by = 5),
              palette = "-viridis", # Utilisation de la palette viridis
              style = "fixed") + # Ajout de style pour les breaks fixes
  tm_layout(legend.outside = TRUE,
            legend.title.size = 1.0,
            legend.text.size = 0.8, frame = FALSE,
            legend.position = c("left", "bottom")) +
  tm_text("hv024" , size = 0.6)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Analyse au niveau des districts ou zones sanitaire 
# lire le shapefile Zone sanitaire
zs_sh <- st_read(here("C:/Users/GounvidéJeanGBAGUIDI/Documents/FE_2024/FE_2024/DHS/zone_sanitaire_sh_2.shp")) 
# Lire shapefile des coordonnées gps des grappe/cluster round6
gcluster12 <-st_read(here(main, shapefile, clsgps12,"BJGE61FL.shp"))
# Lire shapefile des coordonnées gps des grappe/cluster round7
gcluster18 <-st_read(here(main, shapefile, clsgps18,"BJGE71FL.shp")) 

zs_sh <- st_read(here("F:/FE_2024/DHS/ZSShapefilebenin/ZSShapefilebenin/zone_sanitaire_sh_2.shp"))
gcluster12 <-st_read(here("F:/FE_2024/DHS/BJGE61FL/BJGE61FL.shp")) # Lire shapefile des coordonnées gps des grappe/cluster round6

eds_points <- st_as_sf(gcluster12, coords = c("longitude", "latitude"), crs = 4326)


ggplot()+
  geom_sf(zs_sh , mapping = aes(geometry = geometry))+
  #notice we added size inside the aes(), shape and alpha control icon and transparency respectively
  geom_sf(eds_points,  mapping = aes(geometry = geometry), shape = 15, alpha = 0.7)+
  #we added the scale_size() to control the size of points
  scale_size(range = c(0.5, 4))+
  theme_bw()


# obtenir systeme de reference géographique
getcrsben <- st_crs(admin1sh) 

# attibuer le systeme de reference géographique précédement obtenu
zs_sh      <- st_transform(zs_sh, crs=getcrsben) 

# attibuer le systeme de reference géographique précédement obtenu
gcluster12 <- st_transform(gcluster12, crs=getcrsben) 

# attibuer le systeme de reference géographique précédement obtenu
gcluster18 <- st_transform(gcluster18, crs=getcrsben) 

# Faire une fusion de la base zone sanitaire et GPS des clusters 
joined_sf <- st_join(gcluster12,zs_sh,
                     join = st_within) 

# Extraire les variables d'intéret de la fusion, notament l'id de cluster, les zones sanitaire, les régions

zs_cls = joined_sf %>% select(c(DHSCLUST, commune__2,BN_NIV2)) 

# renommer la variable numero grappe de manière à correspondre avec son niom dans la base ménage 
zs_cls <- zs_cls %>% rename(hv001 = DHSCLUST) 

# Fussionner cette manitenant avec la base menage transformées

datamerge <- merge(zs_cls, PR6DET, id = hv001) 

# Writing the merged dataset to a CSV file
write.csv(datamerge, "datamerge.csv", row.names = FALSE)



# Filter the datamerge for rows where commune__2 is "Bk"
filtered_data <- datamerge %>%
  filter(commune__2 == "Bk")

# Display the filtered data
print(filtered_data)


ggplot()+
  geom_sf(admin1sh, mapping = aes(geometry = geometry))+
  #notice we added size inside the aes(), shape and alpha control icon and transparency respectively
  geom_sf(datamerge, mapping = aes(color = h_haveitn), shape = 15, alpha = 0.7)+
  #we added the scale_size() to control the size of points
  scale_size(range = c(0.5, 4))+
  #there are many palettes available, try see which one works for you
  scale_color_distiller(palette = "RdBu")+
  theme_bw()

