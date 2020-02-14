### IMPORTER RP2016 
# import fichier dictionnaire des modalités (utiliser read_csv2 car séparateur ;)
rp16_mod <- read_csv2(here("data","RP2016","varmod_INDREG_2016.csv"))

# constitution d'un table avec uniquement les noms des variables du RP2016
# doublons <- which(duplicated(rp16_mod$COD_VAR)) #quelles sont les séquences en double dans la colonne COD_VAR de mon jeu de données?
# rp16_var<-rp16_mod[-doublons,] #on enregistre un nouveau tableau ne contenant pas les lignes des doublons
rp16_var <- distinct(rp16_mod, COD_VAR, .keep_all = TRUE)

# z <- filter(rp16_mod, COD_VAR=="DEPT") 

# données détail agrégée sont crées par ce script:
# source(here("ponderation", "preparer_marges.R"))
rp16 <- read_csv(here("data","RP2016","recensement2016_agrege.csv"), 
                           col_types = cols(n_pond = col_number()), 
                           locale = locale(decimal_mark = "."))

#### RECODAGE
# mise en classes de l'âge (catégories GFK)
# max(rp16$AGED)
# min(rp16$AGED)

rp16$AGEDC <- cut(rp16$AGED, c(0, 9, 14, 17, 29, 39, 49, 120), include.lowest = TRUE)

# table(rp16$AGEDC)

# Recodage : région (catégories GFK/union des annonceurs)
# table(rp16$DEPT, rp16$REGION)
# table(rp16$DEPT)

# dept <- filter(rp16_mod, COD_VAR=="DEPT") 
# c(dept$COD_MOD)

# catégories Region GFK : impossible de les retrouver
# Bassin Parisien Ouest --> PB
# Bassin Parisien Est --> PB
# Est --> PB
# Méditerranée --> PB
# Ouest --> PB
# Sud Ouest --> PB

rp16<- mutate(rp16, 
           REG = case_when(
             DEPT == "59" | DEPT == "62" ~ "Nord",
             DEPT == "75" | DEPT == "92" | DEPT == "93" | DEPT == "94" | DEPT == "91" | DEPT == "95" | DEPT == "78" | DEPT == "77"~ "Paris / Région Parisienne",
             REGION == "84" ~ "Sud Est",
             REGION == "94" ~ "Corse",
             REGION == "01" | REGION == "02" | REGION == "03" | REGION == "04" ~ "DOM",
             TRUE ~ "Autre"))
# table(rp16$REG)  

rp16 <- mutate(
  rp16,
  REG_mod = case_when(
    REGION == "11"  ~ "Île-de-France",
    REGION == "24"  ~ "Centre-Val de Loire",
    REGION == "27"  ~ "Bourgogne-Franche-Comté",
    REGION == "28"  ~ "Normandie",
    REGION == "32"  ~ "Hauts-de-France",
    REGION == "44"  ~ "Grand Est",
    REGION == "52"  ~ "Pays de la Loire",
    REGION == "53"  ~ "Bretagne",
    REGION == "75"  ~ "Nouvelle-Aquitaine",
    REGION == "76"  ~ "Occitanie",
    REGION == "84"  ~ "Auvergne-Rhône-Alpes",
    REGION == "93"  ~ "Provence-Alpes-Côte d'Azur",
    REGION == "01" | REGION == "02" | REGION == "03" | REGION == "04" ~ "DOM",
    REGION == "94" ~ "Corse"
  )
)

rp16 <- mutate(
  rp16,
  regparis = case_when(
    REGION == "11"  ~ "Paris / Région Parisienne",
    TRUE ~ "Autre"))

  
# Recodage PCS 
rp16<- mutate(rp16, 
              PCS = case_when(
                CS2 == 10 | CS2 == 71 ~ 10,
                CS2 == 23 | CS2 == 31 ~ 31,
                CS2 == 66 | CS2 == 69 ~ 66,
                CS2 == 81 | CS2 == 82 ~ 80,
                TRUE ~ CS2))

rp16 <- mutate(
  rp16,
  PCS_mod = case_when(
    PCS == 10 ~ "A. Agr. et ancien agr. exploitant",
    PCS == 21 ~ "B. Artisan",
    PCS == 22 ~ "C. Commerçant et assimilé",
    PCS == 31 ~ "D. Chef d'entrep, prof. lib. & assimilé",
    PCS == 32 ~ "E. Cadre fonc pub/prof intel/art",
    PCS == 36 ~ "F. Cadre d’entreprise",
    PCS == 41 ~ "G. inter enseig/santé/fonc. pub",
    PCS == 46 ~ "H. Prof inter adm/com entrep.",
    PCS == 47 ~ "I. Technicien",
    PCS == 48 ~ "J. Contremaitre/agent maitrise",
    PCS == 51 ~ "K. Employé fonction publique",
    PCS == 54 ~ "L. Employé admin entreprise",
    PCS == 55 ~ "M. Employé de commerce",
    PCS == 56 ~ "N. Pers services dir. aux part",
    PCS == 61 ~ "O. Ouvrier qualifié",
    PCS == 66 ~ "P. Ouvrier non qualifié et agricole",
    PCS == 72 ~ "Q. Anc. artis/commerc/chef entrep",
    PCS == 73 ~ "R. Anc. cadre & prof. Inter",
    PCS == 76 ~ "S. Ancien employé et ouvrier",
    PCS == 80 ~ "T. Inactif (non retraité)"
  )
)
            
# table(rp16$PCS)             
# table(rp16$PCS_mod)       


#### Filtre : pop 10 ans et plus, hors Corse et DOM, hors agriculteurs et ancien agriculteurs
r <- filter(rp16, AGED>=10 & REGION %not in% c("94","01","02","03","04") & CS2 %not in% c("10","71"))

# Apliquer la pondération
rw <- svydesign(ids = ~1, data = r, weights = ~r$n_pond)

