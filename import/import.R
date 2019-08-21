library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(stringr)

############ Données consommateurs ##########

#p15_dic <- read_excel(here("data", "consommateurs", "RВponses Profilage 15.xlsx"), sheet = 2, skip = 5)

## On importe les trois bases de 2013 à 2016 dans un même objet (pour map après coup)
p <- list(p13_14 = read_excel(here("data", "consommateurs", "RВponses Profilage 1314.xlsx"), sheet = 3),
          p15 = read_excel(here("data", "consommateurs", "RВponses Profilage 15.xlsx"), sheet = 3),
          p16 = read_excel(here("data", "consommateurs", "RВponses Profilage 16.xlsx"), sheet = 3))

## Renommer les variables pour se débarasser des espaces, etc.
## Tout en lowercase pour éviter quelques cas où les variables ne sont pas nommées
## de la même façon dans les trois bases
clean_names <- function(df){
  rename_all(df, .funs = function(x) {
    tolower(x) %>%  str_replace_all(c("%" = "prop", 
                                      "à" = "a",
                                      "[ -\\'/]" = "_",
                                      "[éèê]" = "e"))
  }
  )
}

p <- map(p, clean_names)
rm(clean_names)

## ARGH! il y a une variable en trop dans p$p13_14$hhkey
## buyer = extrait de la variable hhkey dans p13_14
p$p13_14 <- separate(p$p13_14, hhkey, into = c("hhkey", "buyer"), sep = 8) %>% 
  mutate(hhkey = as.integer(hhkey))

## Il y a 3 variables dans p13-14 non présentes dans p15, dont une que l'on vient de rajouter, buyer
## et 24 présentes dans p15 et pas dans p13-14

sum(is.na(match(names(p$p13_14), names(p$p15))))
sum(is.na(match(names(p$p15), names(p$p13_14))))


## Renommons-les
p$p13_14 <- rename_at(p$p13_14, 
          .vars = vars(names(p$p13_14)[is.na(match(names(p$p13_14), names(p$p15)))]),
          .funs = function(x) {paste0(x, "_p1314")})

p[names(p) %in% c("p15", "p16")] <- map(p[names(p) %in% c("p15", "p16")], 
                                        function(df){
                                          rename_at(df, 
                                                    .vars = vars(names(p$p15)[is.na(match(names(p$p15), names(p$p13_14)))]),
                                                    .funs = function(x) {paste0(x, "_p1516")})
                                          
                                        })



## Pour ces variables: il faudrait les préfacer avec leurs années respectives.

## La correspondance entre p15 et p16 est presque parfaite
## seul problème: deux variables inversées d'une base à l'autre.
## mais ne posera pas de problème de fusion si utilise un vrai join 
## et pas un rbind()
sum(is.na(match(names(p$p15), names(p$p16))))
sum(is.na(match(names(p$p15), names(p$p16))))

## Vérifier les correspondances entre personnes

sum(p$p13_14$hhkey %in% p$p15$hhkey)
sum(p$p15$hhkey %in% p$p16$hhkey)
sum(p$p13_14$hhkey %in% p$p16$hhkey)

# Préparer la fusion
map(p, function(df){
  mutate(df, hhkey = as.integer(hhkey)) #hhkey  est un integer = ne pas risquer un arrondi
})

i <- bind_rows(p, .id = "vague")
rm(p)
#### Recodage consommateurs

i <- mutate_all(i, .funs = ~ ifelse(. == "<undefined>", NA, .))




#### Import consommation #####
## La base est déjà fusionnée dans le fichier 2013-2014
# m15_dic <- read_excel(here("data", "consommateurs", "Achats-2015.xlsx"), sheet = 2, skip = 4, col_names = c("Variable", "Description"))
# 
# m <- list(m13_14 = read_excel(here("data", "consommateurs", "Achats_juillet 2013_decembre_2014.xlsx"), sheet = 7), 
#           m15 = read_excel(here("data", "consommateurs", "Achats-2015.xlsx"), sheet = 7),
#           m16 = read_excel(here("data", "consommateurs", "Achats_janvier2016_juin2016.xlsx"), sheet = 7))
# 
# m <- map(m, clean_names)
# m <- bind_rows(m)

m <- read_excel(here("data", "consommateurs", "Achats_juillet 2013_decembre_2014.xlsx"), sheet = 7, col_names = c("HHKEY",
                                                                                                                  "Identifiant_Déclaration",
                                                                                                                  "Product_group",
                                                                                                                  "VideoRental",
                                                                                                                  "Date_de_l_achat",
                                                                                                                  "Enseigne",
                                                                                                                  "Canal_de_Distribution",
                                                                                                                  "Achat_sur_Internet?",
                                                                                                                  "Location",
                                                                                                                  "Planifié",
                                                                                                                  "Format_1",
                                                                                                                  "Matériel",
                                                                                                                  "Téléchargé",
                                                                                                                  "Téléchargement_HD",
                                                                                                                  "TelechVision_LecteurMP3",
                                                                                                                  "TelechVision_PC/notebook",
                                                                                                                  "TelechVision_Smartphone",
                                                                                                                  "TelechVision_Tablette",
                                                                                                                  "TelechVision_Téléviseur",
                                                                                                                  "Code_EAN",
                                                                                                                  "TITLE",
                                                                                                                  "AUTHOR",
                                                                                                                  "CATEGORY",
                                                                                                                  "SUB_CATEGORY",
                                                                                                                  "SUB_CATEGORY_2",
                                                                                                                  "CINEMA_RELEASE",
                                                                                                                  "DISTRIBUTOR",
                                                                                                                  "Format_2",
                                                                                                                  "LABEL",
                                                                                                                  "LICENSE",
                                                                                                                  "NBUNITES",
                                                                                                                  "PEGI",
                                                                                                                  "PERFORMER",
                                                                                                                  "PUBLISHER",
                                                                                                                  "RELEASE_DATE",
                                                                                                                  "EXTRACURRICULAR",
                                                                                                                  "SCHOOL_LEVEL",
                                                                                                                  "SCHOOL_TOPIC",
                                                                                                                  "SERIE",
                                                                                                                  "SERIES",
                                                                                                                  "NeufOccasion",
                                                                                                                  "Abonnement",
                                                                                                                  "MontantAbonnement",
                                                                                                                  "Prix",
                                                                                                                  "PrixSpécial",
                                                                                                                  "Prépayé",
                                                                                                                  "Cadeau",
                                                                                                                  "GenreLivre",
                                                                                                                  "AchetéPour",
                                                                                                                  "Sexe",
                                                                                                                  "Age",
                                                                                                                  "MembreFamille",
                                                                                                                  "Mobilité",
                                                                                                                  "Connaissance_Actu",
                                                                                                                  "Connaissance_ArticleInternet",
                                                                                                                  "Connaissance_ArticlePresse",
                                                                                                                  "Connaissance_AutrePub",
                                                                                                                  "Connaissance_AutreSource",
                                                                                                                  "Connaissance_BA_cinéma",
                                                                                                                  "Connaissance_BandeAnnonceTV",
                                                                                                                  "Connaissance_BAO",
                                                                                                                  "Connaissance_CatalogueMag",
                                                                                                                  "Connaissance_Concert_artiste",
                                                                                                                  "Connaissance_CourrierElec",
                                                                                                                  "Connaissance_Demande",
                                                                                                                  "Connaissance_Diff_titre",
                                                                                                                  "Connaissance_Emission_de_radio",
                                                                                                                  "Connaissance_Emission/direct",
                                                                                                                  "Connaissance_EmissionTV",
                                                                                                                  "Connaissance_EmissionTVLitt",
                                                                                                                  "Connaissance_Enseignant",
                                                                                                                  "Connaissance_Facebook",
                                                                                                                  "Connaissance_HasardMagasin",
                                                                                                                  "Connaissance_PubInternet",
                                                                                                                  "Connaissance_PubMag",
                                                                                                                  "Connaissance_PubPresse",
                                                                                                                  "Connaissance_PubRadio",
                                                                                                                  "Connaissance_PubTV",
                                                                                                                  "Connaissance_RecoMagasin",
                                                                                                                  "Connaissance_Réseaux_sociaux",
                                                                                                                  "Connaissance_SiteOfficiel",
                                                                                                                  "Connaissance_StreamingAudio",
                                                                                                                  "Connaissance_StreamingVidéo",
                                                                                                                  "Connaissance_TopPresse",
                                                                                                                  "Connaissance_Vidéo_clip",
                                                                                                                  "Connaissance_VisionnéTV",
                                                                                                                  "Connaissance_VuCinéma",
                                                                                                                  "DecAchat_Actu",
                                                                                                                  "DecAchat_ArticleCritiquePresse",
                                                                                                                  "DecAchat_AttraitGenre",
                                                                                                                  "DecAchat_Autres_motifs",
                                                                                                                  "DecAchat_avis_consos_rés._Soc",
                                                                                                                  "DecAchat_Bouche-à-oreille",
                                                                                                                  "DecAchat_ClipTV",
                                                                                                                  "DecAchat_Demande",
                                                                                                                  "DecAchat_DiffusionRadio",
                                                                                                                  "DecAchat_EcouteMagSite",
                                                                                                                  "DecAchat_Emballage",
                                                                                                                  "DecAchat_Enseignant",
                                                                                                                  "DecAchat_EssaiJeu",
                                                                                                                  "DecAchat_ExtraitInternet",
                                                                                                                  "DecAchat_Fan",
                                                                                                                  "DecAchat_FeuilletéLivre",
                                                                                                                  "DecAchat_Internet",
                                                                                                                  "DecAchat_PrixPromo",
                                                                                                                  "DecAchat_ProgTV",
                                                                                                                  "DecAchat_Radio",
                                                                                                                  "DecAchat_StreamingAudio",
                                                                                                                  "DecAchat_StreamingVidéo",
                                                                                                                  "DecAchat_TopPresse",
                                                                                                                  "DecAchat_Vu_ciné_et_à_revoir",
                                                                                                                  "DecAchat_Vu_TV_et_à_revoir"), 
                skip = 1)


#### Recodage consommation ####

m <- mutate_all(m, .funs = ~ifelse(. == "<undefined>", NA, .))

m <- rename_all(m, tolower)

## Le recodage des valeurs manquantes produit un certain nombre de variables inutiles: tout est manquant/undefined.
## On sélectionne seulement les variables dont le compte total des NA est inférieur à nrow:
m <- select_if(m, .predicate = function(x) sum(is.na(x)) < nrow(m))

m <- mutate(m, date_de_l_achat = ymd(date_de_l_achat))

# Catégorie: corriger erreurs d'encodage
m <- mutate(m, category = str_replace_all(category, c("\\+Â®|\\?" = "é")))

# m15 <- read_excel(here("data", "consommateurs", "Achats-2015.xlsx"), sheet = 7)
m <- mutate(m, hhkey = as.integer(hhkey))

### Données distributeurs
# dis <- read_tsv(file = here("data", "distributeurs", "Dico_physique_France_Musique.csv"),
#                 col_types = "cccccccc")

save(i, m, file = here("data", "data.RData"))


