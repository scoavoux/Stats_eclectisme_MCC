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
          p18 = read_excel(here("data", "consommateurs", "GfK Panel Consommateurs - Extract Année 2018.xlsx"), sheet = 2),
          p19 = read_excel(here("data", "consommateurs", "GfK Panel Consommateurs - Extract Année 2019 - Q1.xlsx"), sheet = 2)
          )

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

p[c("p19")] <- lapply(p[c("p19")],
                             function(df){
                               rename(df, 
                                      ecoute_musique_cd_vinyles           = "ecoute_musique_cd___vinyles",
                                      ecoute_musique_mp3_telecharge       = "ecoute_musique_mp3_telechargee",
                                      ecoute_musique_radio_web_radio      = "ecoute_musique_radio___web_radio",
                                      ecoute_musique_sites_steaming       = "ecoute_musique_site_streaming",
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
                                      fr_marque_tablette_apple_ipad       = "marque_tablette_apple_ipad",
                                      site_musique_compte_utilisateur_google       = "site_musique_compte_utilisateur_google_play",
                                      
                               )
                             }
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
p[c("p13", "p17", "p18", "p19")] <- lapply(p[c("p13", "p17", "p18", "p19")],
  function(df){
    mutate(df, fr_marque_liseuse_autre       = code_missing,
               fr_marque_liseuse_kindle_amaz = code_missing,
               fr_marque_liseuse_kobo_fnac   = code_missing
               )
  }
)

## Variables non présentes en 2017, 2018
p[c("p17", "p18", "p19")] <- lapply(p[c("p17", "p18", "p19")],
  function(df){
    mutate(df, code_postal = code_missing_num
               )
  }
)

## Variables non présentes en 2018
p[c("p18", "p19")] <- lapply(p[c("p18", "p19")],
  function(df){
    mutate(df, freq_telech_mus_streaming = code_missing
               )
  }
)

# harmonyser hhkey
p <- map(p, function(df){
  separate(df, hhkey, into = c("hhkey", "buyer"), sep = 8) %>% 
    mutate(hhkey = as.integer(hhkey))
})

i <- bind_rows(p, .id = "vague")

# Ces variables sont vides => à supprimer (définies uniquement dans p13 et p17)

i <- select(i, 
            -marque_liseuse_possedee,
            -marque_tablette_possedee,
            -sites_musique_compte_utilisateur_dailymotion,
            -site_musique_compte_utilisateur_youtube)

i <- rename(i, ecoute_musique_sites_streaming = "ecoute_musique_sites_steaming",
            site_musique_compte_utilisateur_google_play = "site_musique_compte_utilisateur_google")

#### Recodage consommateurs

i <- mutate_all(i, .funs = ~ ifelse(. == "<undefined>", NA, .))

#### Import consommation #####
## La base 2013-2016 est déjà fusionnée dans le fichier 2013-2014

m <- list(m13 = read_excel(here("data", "consommateurs", "Achats_juillet 2013_decembre_2014.xlsx"), sheet = 7),
          m17 = read_excel(path = here("data", "consommateurs", "GfK Panel Consommateurs - Extract Année 2017.xlsx"), sheet = 9),
          m18 = read_excel(path = here("data", "consommateurs", "GfK Panel Consommateurs - Extract Année 2018.xlsx"), sheet = 9),
          m19 = bind_rows(read_excel(here("data", "consommateurs", "GfK Panel Consommateurs - Extract Année 2019 - Q1.xlsx"), sheet = 9),
                          read_excel(here("data", "consommateurs", "GfK Panel Consommateurs - Extract Année 2019 - Q2.xlsx"), sheet = 9))
          )

m$m13 <- rename(m$m13, 
               Format_1 = "Format...11",
               Format_2 = "Format...28")

m <- map(m, clean_names)

# Supprimer variables vides
delete_empty_vars <- function(df){
  mutate_all(df, .funs = ~ifelse(. == "<undefined>", NA, .)) %>% 
    select_if(.predicate = function(x) sum(is.na(x)) < nrow(df))
}

m <- map(m, delete_empty_vars)

## Fusion: noms de variables
m$m13 <- rename(m$m13, 
                achat_pour          = "achetepour",
                canal_distribution  = "canal_de_distribution",
                date_achat          = "date_de_l_achat",
                membre_famille      = "membrefamille",
                montant_abonnement  = "montantabonnement",
                prix_special        = "prixspecial"
)

m$m17 <- rename(m$m17, 
                code_ean          = "ean13",
                distributor       = "distribur",
                neufoccasion      = "neufocca"
)

m$m18 <- rename(m$m18, 
                code_ean          = "ean13",
                distributor       = "distribur",
                neufoccasion      = "neufocca"
)

m$m19 <- rename(m$m19, 
                code_ean          = "ean13",
                distributor       = "distribur",
                neufoccasion      = "neufocca"
)

m$m13 <- mutate(m$m13, 
                occasion_key = code_missing)

m[c("m17", "m18", "m19")] <- lapply(m[c("m17", "m18", "m19")], function(df){
  mutate(df, 
         connaissance_concert_artiste  = code_missing,
         connaissance_diff_titre       = code_missing,
         connaissance_emission_direct  = code_missing,
         connaissance_pubtv            = code_missing,
         connaissance_streamingaudio   = code_missing,
         connaissance_streamingvideo   = code_missing,
         connaissance_video_clip       = code_missing,
         decachat_cliptv               = code_missing,
         decachat_diffusionradio       = code_missing,
         decachat_ecoutemagsite        = code_missing,
         decachat_streamingaudio       = code_missing,
         decachat_streamingvideo       = code_missing,
         identifiant_declaration       = code_missing_num
  ) %>% 
    rename(achat_magasin_ou_internet = "achat_magasin_ou_internet_?")
})

m$m13 <- rename_if(m$m13, str_detect(names(m$m13), "achat_sur_internet"), function(x) "achat_magasin_ou_internet")

## Fusion

m <- bind_rows(m, .id = "vague_conso")

#### Recodage consommation ####
m <- mutate(m, date_achat = ymd(date_achat))

# Catégorie: corriger erreurs d'encodage
m <- mutate(m, category = str_replace_all(category, c("\\+Â®|\\?" = "é",
                                                      "AÂ©" = "é")))

# Todo?
# achat_magasin_ou_internet a des modalités différentes dans les deux bases.

m <- mutate(m, hhkey = as.integer(hhkey))

rm(code_missing, code_missing_num, p, clean_names, delete_empty_vars)
save(i, m, file = here("data", "data.RData"))


