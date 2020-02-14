## Chargement de packages 
library(tidyverse)
library(lubridate) 
library(readxl) 
library(here) 
library(stringr) 
library(DT)
library(knitr)

## Chargement des données
load(here("data", "data.RData"))

## fonction 
`%not in%`<- Negate(`%in%`)

###### Base individu ######

i <- mutate(i, sexe = factor(sexe, levels = c("Femme", "Homme")))

## diplome et occupation de ego non renseigné quand ego est chef de famille
i <- mutate(i, niveau_d_etudes = ifelse(is.na(niveau_d_etudes), niveau_d_etudes_chef_de_famille, niveau_d_etudes),
            profession         = ifelse(is.na(profession),      profession_chef_de_famille,      profession))


### Etudes = NSP to NA:
### besoin de recoder.
i <- mutate(i, niveau_d_etudes = factor(niveau_d_etudes, 
                                        levels = c("Pas d’école",
                                                   "Etudes primaires", 
                                                   "Enseig. Tech ou Pro COURT",
                                                   "Enseig. Tech ou Pro LONG",
                                                   "1er cycle d’enseignement gén",
                                                   "2ème cycle d’enseignement gén",
                                                   "Enseignement Supérieur COURT", 
                                                   "Enseignement Supérieur LONG"
                                        )),
            profession = factor(profession, 
                                levels = c("Agriculteur exploitant", 
                                           "Artisan",
                                           "Commerçant et assimilé", 
                                           "Chef d’entreprise +10 sal.",
                                           "Profession lib. & assimilé", 
                                           "Cadre fonc pub/prof intel/art",
                                           "Cadre d’entreprise",
                                           "inter enseig/santé/fonc. pub",
                                           "Contremaitre/agent maitrise", 
                                           "Prof inter adm/com entrep.",
                                           "Technicien",
                                           "Pers services dir. aux part", 
                                           "Employé admin entreprise",
                                           "Employé de commerce",
                                           "Employé fonction publique", 
                                           "Ouvrier qualifié",
                                           "Ouvrier non qualifié",
                                           "Ouvrier agricole",
                                           "Etudiant",
                                           "Chômeur n’ayant jamais trav.",
                                           "Inactif div autre que retraité", 
                                           "Anc. artis/commerc/chef entrep", 
                                           "Anc. cadre & prof. Inter",
                                           "Ancien agric. exploitant", 
                                           "Ancien employé et ouvrier")))

### Milieu social

i <- mutate(i, milieu = case_when(profession_chef_de_famille == "Agriculteur exploitant"         ~ "Agriculteur, artisan, commerçant", 
                                  profession_chef_de_famille == "Artisan"                        ~ "Agriculteur, artisan, commerçant",
                                  profession_chef_de_famille == "Commerçant et assimilé"         ~ "Agriculteur, artisan, commerçant", 
                                  profession_chef_de_famille == "Anc. artis/commerc/chef entrep" ~ "Agriculteur, artisan, commerçant", 
                                  profession_chef_de_famille == "Ancien agric. exploitant"       ~ "Agriculteur, artisan, commerçant", 
                                  profession_chef_de_famille == "Chef d’entreprise +10 sal."     ~ "Cadre privé, chef d'entreprise, retraité CPIS/PI",
                                  profession_chef_de_famille == "Profession lib. & assimilé"     ~ "Cadre privé, chef d'entreprise, retraité CPIS/PI",
                                  profession_chef_de_famille == "Cadre d’entreprise"             ~ "Cadre privé, chef d'entreprise, retraité CPIS/PI",
                                  profession_chef_de_famille == "Anc. cadre      & prof. Inter"  ~ "Cadre privé, chef d'entreprise, retraité CPIS/PI", 
                                  profession_chef_de_famille == "Cadre fonc pub/prof intel/art"  ~ "Cadres et PI de fonction publique / intell.",
                                  profession_chef_de_famille == "inter enseig/santé/fonc. pub"   ~ "Cadres et PI de fonction publique / intell.",
                                  profession_chef_de_famille == "Contremaitre/agent maitrise"    ~ "Autres professions intermédiaires", 
                                  profession_chef_de_famille == "Prof inter adm/com entrep."     ~ "Autres professions intermédiaires",
                                  profession_chef_de_famille == "Technicien"                     ~ "Autres professions intermédiaires",
                                  profession_chef_de_famille == "Ancien employé et ouvrier"      ~ "Ouvriers et employés",
                                  profession_chef_de_famille == "Pers services dir. aux part"    ~ "Ouvriers et employés", 
                                  profession_chef_de_famille == "Employé admin entreprise"       ~ "Ouvriers et employés",
                                  profession_chef_de_famille == "Employé de commerce"            ~ "Ouvriers et employés",
                                  profession_chef_de_famille == "Employé fonction publique"      ~ "Ouvriers et employés", 
                                  profession_chef_de_famille == "Ouvrier qualifié"               ~ "Ouvriers et employés",
                                  profession_chef_de_famille == "Ouvrier non qualifié"           ~ "Ouvriers et employés",
                                  profession_chef_de_famille == "Ouvrier agricole"               ~ "Ouvriers et employés",
                                  profession_chef_de_famille == "Etudiant"                       ~ "Autres",
                                  profession_chef_de_famille == "Chômeur n’ayant jamais trav."   ~ "Autres",
                                  profession_chef_de_famille == "Inactif div autre que retraité" ~ "Autres"),
            milieu = factor(milieu, levels = c("Agriculteur, artisan, commerçant", 
                                               "Cadre privé, chef d'entreprise, retraité CPIS/PI",
                                               "Cadres et PI de fonction publique / intell.",
                                               "Autres professions intermédiaires", 
                                               "Ouvriers et employés",
                                               "Autres")))

### Études
## À revoir avec le questionnaire
dip <- c("Pas d’école" = "Moins que bac", 
         "Etudes primaires" = "Moins que bac", 
         "Enseig. Tech ou Pro COURT" = "Moins que bac", 
         "Enseig. Tech ou Pro LONG" = "Moins que bac", 
         "1er cycle d’enseignement gén" = "Moins que bac", 
         "2ème cycle d’enseignement gén" = "Bac", 
         "Enseignement Supérieur COURT" = "Enseignement Supérieur COURT", 
         "Enseignement Supérieur LONG" = "Enseignement Supérieur LONG")

i$dipl <- dip[as.character(i$niveau_d_etudes)] %>% 
  factor(levels = c("Moins que bac", "Bac", "Enseignement Supérieur COURT", "Enseignement Supérieur LONG"))



### Fabienne : Mise en classes de l'âge 
i <- mutate(i, agec = case_when(age<=14 ~ "[10, 14]",
                                age>=15 & age<=17 ~ "[15, 17]",
                                age>=18 & age<=29 ~ "[18, 29]",
                                age>=30 & age<=39 ~ "[30, 39]",
                                age>=40 & age<=49 ~ "[40, 49]",
                                age>=50 ~ ">=50"),
            agec = factor(agec, levels = c("[10, 14]", "[15, 17]", "[18, 29]", "[30, 39]", "[40, 49]", ">=50")))


### Fabienne : recodage profession (pour comparaison INSEE RP2015)
i <- mutate(i, prof = case_when(profession == "Agriculteur exploitant"         ~ "Agriculteurs exploitants", 
                                profession == "Artisan"                        ~ "Artisans, commerçants, chef d'entreprise",
                                profession == "Commerçant et assimilé"         ~ "Artisans, commerçants, chef d'entreprise", 
                                profession == "Chef d’entreprise +10 sal."     ~ "Artisans, commerçants, chef d'entreprise",
                                profession == "Profession lib. & assimilé"     ~ "Cadres et professions intellectuelles supérieures",
                                profession == "Cadre d’entreprise"             ~ "Cadres et professions intellectuelles supérieures",
                                profession == "Cadre fonc pub/prof intel/art"  ~ "Cadres et professions intellectuelles supérieures",
                                profession == "inter enseig/santé/fonc. pub"   ~ "Professions intermédiaires",
                                profession == "Contremaitre/agent maitrise"    ~ "Professions intermédiaires", 
                                profession == "Prof inter adm/com entrep."     ~ "Professions intermédiaires",
                                profession == "Technicien"                     ~ "Professions intermédiaires",
                                profession == "Pers services dir. aux part"    ~ "Employés", 
                                profession == "Employé admin entreprise"       ~ "Employés",
                                profession == "Employé de commerce"            ~ "Employés",
                                profession == "Employé fonction publique"      ~ "Employés", 
                                profession == "Ouvrier qualifié"               ~ "Ouvriers",
                                profession == "Ouvrier non qualifié"           ~ "Ouvriers",
                                profession == "Ouvrier agricole"               ~ "Ouvriers",
                                profession == "Etudiant"                       ~ "Autres personnes sans activité professionnelle",
                                profession == "Chômeur n’ayant jamais trav."   ~ "Autres personnes sans activité professionnelle",
                                profession == "Inactif div autre que retraité" ~ "Autres personnes sans activité professionnelle",
                                profession == "Anc. artis/commerc/chef entrep" ~ "Retraités",
                                profession == "Anc. cadre & prof. Inter"       ~ "Retraités",
                                profession == "Ancien agric. exploitant"       ~ "Retraités",
                                profession == "Ancien employé et ouvrier"      ~ "Retraités"),
            prof = factor(prof, levels = c("Agriculteurs exploitants", 
                                           "Artisans, commerçants, chef d'entreprise",
                                           "Cadres et professions intellectuelles supérieures",
                                           "Professions intermédiaires", 
                                           "Employés",
                                           "Ouvriers",
                                           "Autres personnes sans activité professionnelle",
                                           "Retraités")))

### Fabienne : recodage profession pour definition quotas à partir du RP2016
i <- mutate(
  i,
  PCS_mod = case_when(
    profession == "Agriculteur exploitant"         ~ "A. Agr. et ancien agr. exploitant",
    profession == "Artisan"                        ~ "B. Artisan",
    profession == "Commerçant et assimilé"         ~ "C. Commerçant et assimilé",
    profession == "Chef d’entreprise +10 sal."     ~ "D. Chef d'entrep, prof. lib. & assimilé",
    profession == "Profession lib. & assimilé"     ~ "D. Chef d'entrep, prof. lib. & assimilé",
    profession == "Cadre d’entreprise"             ~ "F. Cadre d’entreprise",
    profession == "Cadre fonc pub/prof intel/art"  ~ "E. Cadre fonc pub/prof intel/art",
    profession == "inter enseig/santé/fonc. pub"   ~ "G. inter enseig/santé/fonc. pub",
    profession == "Contremaitre/agent maitrise"    ~ "J. Contremaitre/agent maitrise",
    profession == "Prof inter adm/com entrep."     ~ "H. Prof inter adm/com entrep.",
    profession == "Technicien"                     ~ "I. Technicien",
    profession == "Pers services dir. aux part"    ~ "N. Pers services dir. aux part",
    profession == "Employé admin entreprise"       ~ "L. Employé admin entreprise",
    profession == "Employé de commerce"            ~ "M. Employé de commerce",
    profession == "Employé fonction publique"      ~ "K. Employé fonction publique",
    profession == "Ouvrier qualifié"               ~ "O. Ouvrier qualifié",
    profession == "Ouvrier non qualifié"           ~ "P. Ouvrier non qualifié et agricole",
    profession == "Ouvrier agricole"               ~ "P. Ouvrier non qualifié et agricole",
    profession == "Etudiant"                       ~ "T. Inactif (non retraité)",
    profession == "Chômeur n’ayant jamais trav."   ~ "T. Inactif (non retraité)",
    profession == "Inactif div autre que retraité" ~ "T. Inactif (non retraité)",
    profession == "Anc. artis/commerc/chef entrep" ~ "Q. Anc. artis/commerc/chef entrep",
    profession == "Anc. cadre & prof. Inter"       ~ "R. Anc. cadre & prof. Inter",
    profession == "Ancien agric. exploitant"       ~ "A. Agr. et ancien agr. exploitant",
    profession == "Ancien employé et ouvrier"      ~ "S. Ancien employé et ouvrier"
  ),
  PCS_mod = factor(PCS_mod, levels = c("A. Agr. et ancien agr. exploitant",
                                       "B. Artisan",
                                       "C. Commerçant et assimilé",
                                       "D. Chef d'entrep, prof. lib. & assimilé",
                                       "E. Cadre fonc pub/prof intel/art",
                                       "F. Cadre d’entreprise",
                                       "G. inter enseig/santé/fonc. pub",
                                       "H. Prof inter adm/com entrep.",
                                       "I. Technicien",
                                       "J. Contremaitre/agent maitrise",
                                       "K. Employé fonction publique",
                                       "L. Employé admin entreprise",
                                       "M. Employé de commerce",
                                       "N. Pers services dir. aux part",
                                       "O. Ouvrier qualifié",
                                       "P. Ouvrier non qualifié et agricole",
                                       "Q. Anc. artis/commerc/chef entrep",
                                       "R. Anc. cadre & prof. Inter",
                                       "S. Ancien employé et ouvrier",
                                       "T. Inactif (non retraité)"))
)

### Fabienne : recodage de la taille d'agglomeration (pb modalités pour 4 individus en 2013)
i <- mutate(i, taille_agglo = case_when(
  str_detect(taille_d_agglo, "100\\s000 habitants ou plus") ~ "100 000 habitants et plus",
  str_detect(taille_d_agglo, "2\\s000-19\\s999 habitants")  ~ "De 2 000 à moins de 20 000 h",
  str_detect(taille_d_agglo, "20\\s000 – 99\\s999 habitants") ~ "De 20 000 à moins de 100 000 h",
  TRUE ~ taille_d_agglo),
  taille_agglo = factor(taille_agglo, levels = c("Moins de 2 000 habitants",
                                                 "De 2 000 à moins de 20 000 h",
                                                 "De 20 000 à moins de 100 000 h",
                                                 "100 000 habitants et plus",
                                                 "Paris, RP")))


### Fabienne : recodage des régions à partir du code postal
# longueur du code postal
# Problème : pas de code postal pour 2017 2018 2019

i$CP_num<-as.character(i$code_postal)

i <- mutate(
  i,
  dept = case_when(
    str_length(as.character(code_postal)) == 3 ~ str_sub(as.character(code_postal), 1, 1),
    str_length(as.character(code_postal)) == 4 ~ str_sub(as.character(code_postal), 1, 2),
    str_length(as.character(code_postal)) == 5 ~ str_sub(as.character(code_postal), 1, 2),
    TRUE ~ as.character(code_postal)
  )
)

### Fabienne : recodage région parisienne
i <- mutate(
  i,
  regparis = case_when(
    region == "Paris / Région Parisienne" ~ "Paris / Région Parisienne",
    TRUE ~ "Autre"
  ),
  regparis = factor(regparis, levels = c("Autre", "Paris / Région Parisienne"))
)

### Fabienne : filtre suppression des agriculteurs et anciens agriculteurs
i <- filter(i,profession %not in% c("Agriculteur exploitant","Ancien agric. exploitant"))
