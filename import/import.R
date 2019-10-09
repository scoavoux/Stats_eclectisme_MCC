library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(stringr)

############ Données consommateurs ##########

#p15_dic <- read_excel(here("data", "consommateurs", "RВponses Profilage 15.xlsx"), sheet = 2, skip = 5)

## On importe les trois bases de 2013 à 2016 dans un même objet (pour map après coup)
p <- list(p13 = read_excel(here("data", "consommateurs", "RВponses Profilage 1314.xlsx"), sheet = 3),
          p14 = read_excel(here("data", "consommateurs", "RВponses Profilage 1314.xlsx"), sheet = 4),
          p15 = read_excel(here("data", "consommateurs", "RВponses Profilage 15.xlsx"), sheet = 3),
          p16 = read_excel(here("data", "consommateurs", "RВponses Profilage 16.xlsx"), sheet = 3),
          p17 = read_excel(here("data", "consommateurs", "GfK Panel Consommateurs - Extract Année 2017.xlsx"), sheet = 2),
          p18 = read_excel(here("data", "consommateurs", "GfK Panel Consommateurs - Extract Année 2018.xlsx"), sheet = 2))

## Corriger les erreurs qui se voient à l'importation
p$p14 <- rename(p$p14, HHKEY = "HHKEY...1") %>% 
  select(-`HHKEY...2`)

p$p17 <- rename(p$p17, 
                `Sexe des membres 01 du foyer` = "Sexe des membres 02 du foyer...22",
                `Sexe des membres 02 du foyer` = "Sexe des membres 02 du foyer...23",
                ## Celle-ci n'apparaît pas comme erreur, mais est grossière (première colonne du fichier excel pour 2017)
                HHKEY = "KEY")

## Renommer les variables pour se débarasser des espaces, etc.
## Tout en lowercase pour éviter quelques cas où les variables ne sont pas nommées
## de la même façon dans les trois bases
clean_names <- function(df){
  rename_all(df, .funs = function(x) {
    tolower(x) %>%  str_replace_all(c("%" = "prop", 
                                      "à" = "a",
                                      "[ \\-\\'/]" = "_",
                                      "[éèê]" = "e",
                                      "ç" = "c"))
  }
  )
}

p <- map(p, clean_names)
rm(clean_names)

## NOMS DE VARIABLES
# Le problème: toutes les bases n'ont pas les mêmes variables, mais on veut pouvoir
# distinguer les NA dans chaque variable, ceux qui ne sont pas renseignés
# de ceux qui viennent de ce que la variable n'existait pas dans la base.
# La solution: vérifier les bases, ajouter un prefixe aux variables uniques.

## On fait d'abord le diagnostic dessous, puis on corrige avec les fonctions ici

p$p13 <- rename(p$p13, 
                marque_liseuse_possedee             = "fr_marque_de_liseuse_possede"
)

p[c("p13", "p14", "p15", "p16")] <- lapply(p[c("p13", "p14", "p15", "p16")],
  function(df){
    rename(df, 
           marque_tablette_asus                         = "fr_marque_tablette_asus",
           marque_tablette_autre                        = "fr_marque_tablette_autre",
           marque_tablette_samsung                      = "fr_marque_tablette_samsung",
           possession_console_autre_marque              = "fr_console_poss_autre",
           possession_console_nintendo_3ds              = "fr_console_poss_nintendo_3ds",
           possession_console_nintendo_ds               = "fr_console_poss_nintendo_ds",
           possession_console_ps2                       = "fr_console_poss_ps2",
           possession_console_ps3                       = "fr_console_poss_ps3",
           possession_console_ps4                       = "fr_console_poss_ps4",
           possession_console_ps_vita                   = "fr_console_poss_ps_vita",
           possession_console_psp                       = "fr_console_poss_psp",
           possession_console_wii                       = "fr_console_poss_wii",
           possession_console_wii_u                     = "fr_console_poss_wii_u",
           possession_console_x_box_360                 = "fr_console_poss_x_box_360",
           possession_console_x_box_one                 = "fr_console_poss_x_box_one",
           regardez_vous_dvd_ou_bluray                  = "regardez_vous_dvdoubluray",
           site_musique_streaming_dailymotion           = "site_musi_stream_dailymotion",
           site_musique_streaming_deezer                = "site_musi_stream_deezer",
           site_musique_streaming_google_play           = "site_musi_stream_google_play",
           site_musique_streaming_spotify               = "site_musi_stream_spotify",
           site_musique_streaming_youtube               = "site_musi_stream_youtube",
           tv_connectee_a_la_box                        = "tv_connecte_a_la_box",
           type_de_box_internet                         = "type_box_internet_possede",
           site_musique_compte_utilisateur_autres_sites = "compte_utili_site_autrestream",
           site_musique_compte_utilisateur_deezer       = "compte_utili_site_deezer",
           site_musique_compte_utilisateur_google       = "compte_utili_site_googleplay",
           site_musique_compte_utilisateur_napster      = "compte_utili_site_napster",
           site_musique_compte_utilisateur_qobuz        = "compte_utili_site_qobuz",
           site_musique_compte_utilisateur_spotify      = "compte_utili_site_spotify",
           frequence_revente_jeux_video_physiques       = "fr_revente_jeu_video_physique",
           frequence_revente_de_livres                  = "fr_revente_de_livre",
           possession_liseuse                           = "fr_possession_liseuse",
           possession_ordinateur                        = "fr_ordinateur_possede",
           possession_tablette                          = "fr_possession_tablette",
           autres_sites_musique_streaming               = "site_musi_stream_autres_sites",
           possession_carte_bibliotheque                = "possedez_carte_biblio",
           possession_equipement_box_internet           = "poss_equipement_box_internet",
           possession_equipement_lecteur_video          = "poss_equipement_lecteur_video",
           prop_jeux_telecharges_console                = "prop_jeux_telech_console",
           prop_jeux_telecharges_mobile___smartphone    = "prop_jeux_telech_mob_smart",
           prop_jeux_telecharges_pc                     = "prop_jeux_telech_pc",
           prop_livres_electroniques_gratuits           = "prop_livres_electr_gratuit",
           site_streaming_musique_napster               = "site_musi_stream_napster",
           site_streaming_musique_qobuz                 = "site_musi_stream_qobuz",
           nombre_de_personnes_par_foyer                = "nombre_de_pers._par_foyer"


     )
     
   })

p[c("p15", "p16")] <- lapply(p[c("p15", "p16")],
  function(df){
    rename(df, 
           fr_marque_tablette_apple_ipad                = "fr__marque_tablette_apple_ipad",
           marque_tablette_microsoft_s                  = "fr_marque_tablette_microsoft_s",
           site_musique_streaming_fnac_jukebox          = "site_musi_stream_fnac_jukebox",
           site_musique_streaming_xbox_music            = "site_musi_stream_xbox_music",
           site_musique_compte_utilisateur_fnac_jukebox = "compte_utili_site_fnac_jukebox",
           site_musique_compte_utilisateur_xbox_music   = "compte_utili_site_xbox_music"

    )
  }
)

p[c("p17", "p18")] <- lapply(p[c("p17", "p18")],
  function(df){
    rename(df, 
           ecoute_musique_cd_vinyles           = "ecoute_musique_cd___vinyles",
           ecoute_musique_mp3_telecharge       = "ecoute_musique_mps_telechargee",
           ecoute_musique_radio_web_radio      = "ecoute_musique_radio___web_radio",
           freq_assist_concert                 = "frequence_assiste_a_des_concerts",
           freq_autressites_streaming          = "frequence_autre_sites_streaming",
           freq_dailymotion_streaming          = "frequence_dailymotion_streaming",
           freq_emprunt_livre_biblio           = "frequence_emprunt_livres_bibliotheque",
           freq_films_rattrapage               = "frequence_films_rattrapage",
           freq_ratrappe_feuilletons           = "frequence_rattrape_feuilletons",
           freq_rattrape_6play                 = "frequence_rattrape_6play",
           freq_rattrape_autres_programmes     = "frequence_rattrape_autres_programmes",
           freq_rattrape_autres_services       = "frequence_rattrape_autres_services",
           freq_rattrape_dessins_animes        = "frequence_rattrape_dessins_animes",
           freq_rattrape_documentaires         = "frequence_rattrape_documentaires",
           freq_rattrape_mytf1                 = "frequence_rattrape_mytf1",
           freq_rattrape_pluzz                 = "frequence_rattrape_pluzz",
           freq_rattrape_series_americ.        = "frequence_rattrape_series_americaines",
           freq_rattrape_series_francaises     = "frequence_rattrape_series_francaises",
           freq_telech_ebook                   = "frequence_telechargement_ebook",
           freq_telech_mus_sans_payer          = "frequence_telecharge_musique_sans_payer",
           freq_telecharge_vid_sans_payer      = "frequence_telecharge_videos_sans_payer",
           freq_tv_replay                      = "frequence_tv_replay",
           freq_youtube_streaming              = "frequence_youtube_streaming",
           poss_equip_console_de_jeux          = "possession_equipement_console_de_jeux",
           poss_equip_telephone_portable       = "possession_equipement_telephone_portable",
           possession_console_nintendo_3ds     = "possession_console_nitendo_3ds",
           possession_console_nintendo_ds      = "possession_console_nitendo_ds",
           possession_lecteurs_bluray          = "possession_lecteur_bluray",
           regardez_autres_types_video         = "regardez_vous_autres_types_videos",
           regardez_emissions_ou_series        = "regardez_vous_emmissions_ou_series",
           regardez_films_ou_des_series        = "regardez_vous_films_ou_series",
           regardez_videos_a_la_demande        = "regardez_vous_videos_a_la_demande",
           regardez_videos_telechargees        = "regardez_vous_videos_telechargees",
           taille_d_agglo                      = "taille_d_agglomeration",
           fr_marque_tablette_apple_ipad       = "marque_tablette_apple_ipad"
                               )
                             }
)
p$p17 <- rename(p$p17, 
           ecoute_musique_sites_steaming       = "eoute_musique_site_streaming",
           freq_telech_mus_streaming           = "frequence_telecharge_musique_streaming",
           frequence_revente_jeux_video_physiques = "frequence_revent_jeux_video_physiques"
)

p$p18 <- rename(p$p18, 
           ecoute_musique_sites_steaming       = "ecoute_musique_site_streaming"
)

## Pour les variables non présentes, remplacer le code pour que NA ne se confondent pas


code_missing <- "Question non posée cette année"
code_missing_num <- 99999

## Variables non présentes en 2013 et 2014
p[c("p13", "p14")] <- lapply(p[c("p13", "p14")],
  function(df){
    mutate(df, compte_payant_napster                        = code_missing,
               compte_payant_spotify                        = code_missing,
               freq_films_rattrapage                        = code_missing,
               freq_ratrappe_feuilletons                    = code_missing,
               freq_rattrape_6play                          = code_missing,
               freq_rattrape_autres_programmes              = code_missing,
               freq_rattrape_autres_services                = code_missing,
               freq_rattrape_dessins_animes                 = code_missing,
               freq_rattrape_documentaires                  = code_missing,
               freq_rattrape_mytf1                          = code_missing,
               freq_rattrape_pluzz                          = code_missing,
               freq_rattrape_series_americ.                 = code_missing,
               freq_rattrape_series_francaises              = code_missing,
               freq_telech_ebook                            = code_missing,
               marque_tablette_microsoft_s                  = code_missing,
               site_musique_streaming_fnac_jukebox          = code_missing,
               site_musique_streaming_xbox_music            = code_missing,
               telecharge_jeux_video                        = code_missing,
               site_musique_compte_utilisateur_fnac_jukebox = code_missing,
               site_musique_compte_utilisateur_xbox_music   = code_missing
               )
  }
)

## Variables non présentes en 2013:2016
p[c("p13", "p14", "p15", "p16")] <- lapply(p[c("p13", "p14", "p15", "p16")],
  function(df){
    mutate(df, possession_console_nitendo_switch    = code_missing,
               frequence_rattrape_a_la_demande      = code_missing,
               marque_tablette_acer                 = code_missing,
               marque_tablette_archos               = code_missing,
               marque_tablette_blackberry           = code_missing,
               marque_tablette_mp_man               = code_missing,
               marque_tablette_storex               = code_missing,
               site_musique_streaming_amazon_music  = code_missing,
               site_musique_streaming_apple_music   = code_missing,
               site_musique_streaming_beats_music   = code_missing,
               site_musique_streaming_cstream       = code_missing,
               site_musique_streaming_leclerc_reglo = code_missing
               )
  }
)

## Variables non présentes en 2013:2017
p[c("p13", "p14", "p15", "p16", "p17")] <- lapply(p[c("p13", "p14", "p15", "p16", "p17")],
  function(df){
    mutate(df, frequence_musique_streaming = code_missing
      )
  }
)

## Variables non présentes en 2013, 2017, 2018
p[c("p13", "p17", "p18")] <- lapply(p[c("p13", "p17", "p18")],
  function(df){
    mutate(df, fr_marque_liseuse_autre       = code_missing,
               fr_marque_liseuse_kindle_amaz = code_missing,
               fr_marque_liseuse_kobo_fnac   = code_missing
               )
  }
)

## Variables non présentes en 2017, 2018
p[c("p17", "p18")] <- lapply(p[c("p17", "p18")],
  function(df){
    mutate(df, code_postal = code_missing_num
               )
  }
)

## Variables non présentes en 2018
p[c("p18")] <- lapply(p[c("p18")],
  function(df){
    mutate(df, freq_telech_mus_streaming = code_missing
               )
  }
)

## ARGH! il y a une variable en trop dans p$p13_14$hhkey
## buyer = extrait de la variable hhkey dans p13_14
p$p13 <- separate(p$p13, hhkey, into = c("hhkey", "buyer"), sep = 8) %>% 
  mutate(hhkey = as.integer(hhkey))
p$p14 <- separate(p$p14, hhkey, into = c("hhkey", "buyer"), sep = 8) %>% 
  mutate(hhkey = as.integer(hhkey))
p$p17 <- separate(p$p17, hhkey, into = c("hhkey", "buyer"), sep = 8) %>% 
  mutate(hhkey = as.integer(hhkey))

# Préparer la fusion
p <- map(p, function(df){
  mutate(df, hhkey = as.integer(hhkey)) #hhkey  est un integer = ne pas risquer un arrondi
})

i <- bind_rows(p, .id = "vague")

# Ces variables sont vides => à supprimer (définies uniquement dans p13 et p17)

i <- select(i, 
            -marque_liseuse_possedee,
            -marque_tablette_possedee,
            -sites_musique_compte_utilisateur_dailymotion,
            -site_musique_compte_utilisateur_youtube)

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


