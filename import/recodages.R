# Recoder les noms d'artistes à partir des proximités lexico
# load(here("data", "data.RData"))


n_unique(m$performer)
m <- mutate(m, performer = tolower(performer))

## On considère que artiste X feat. artiste Y doit être attribué à artiste X
m <- mutate(m, performer = str_replace(performer, " feat\\..*$", ""))

## Transformer "nom, prénom" en "prénom nom"
m <- mutate(m, performer = str_replace(performer, "^(\\w+), (\\w+)$", "\\2 \\1"))

## Transformer "band (the)" en "the band"
m <- mutate(m, performer = str_replace(performer, "^(\\.+) \\(?the\\)?$", "the \\1"))

## Supprimer les , - '
m <- mutate(m, performer = str_replace_all(performer, "[,\\-'\\.]", " "))

## Corriger les erreurs d'encodage/accents
m <- mutate(m, performer = str_replace_all(performer, "\\+\\?", "e"))
m <- mutate(m, performer = str_replace_all(performer, "\\+â®", "e"))

## remplacer les doubles/triples espaces
m <- mutate(m, performer = str_replace_all(performer, "\\s+", " "))
## trim whitespaces
m <- mutate(m, performer = str_replace_all(performer, "\\s$", ""))
m <- mutate(m, performer = str_replace_all(performer, "^\\s", ""))

## supprimer les artistes vides
m <- mutate(m, performer = ifelse(performer == "", NA, performer))

## Dictionnaire
source(here("import", "dictionnaires_artistes.R"))
for(dic in dics){
  m <- mutate(m, performer = str_replace_all(performer, dic))
}

m <- mutate(m, performer = case_when(
  title == "Changes" & performer == "faul & wad ad" ~ "faul & wad ad vs pnau",
  title == "Jimmy" & performer == "cats on trees" ~ "cats on trees & calogero",
  title == "Jimmy" & performer == "cats on trees / calogero" ~ "cats on trees & calogero",
  title == "Kendji" & is.na(performer) ~ "kendji girac",
  title == "La Bande a Renaud" ~ "la bande a renaud",
  title == "1789, Les Amants De La Bastille" ~ "1789 les amants de la bastille",
  title == "BLACK ICE" & is.na(performer) ~ "ac/dc",
  title == "ALAIN SOUCHON & LAURENT VOULZY" ~ "alain souchon & laurent voulzy",
  title == "Calogero" & performer == "calogero karaoke" ~ "calogero",
  title == "GILBERT BECAUD" & performer == "compilation" ~ "gilbert becaud",
  title == "Goodbye (Feat. Lyse)" & is.na(performer) ~  "feder",
  title == "Liebe lebt: Das Beste von Mireille Mathieu" ~ "mireille mathieu",
  title == "PASSIONS DE L'AME ET DU COEUR" ~ "ricercar consort les ombres and philippe pierlot",
  title == "Prayer in C" & performer == "tribute to lilly wood & the prick and robin schulz" ~ "robin schulz",
  title == "Nos vies paralleles" & performer == "anggun" ~ "florent pagny & anggun",
  title == "Stolen Car" & performer == "mylene farmer" ~ "mylene farmer & sting",
  title == "THE HEIST" & performer == "macklemore" ~ "macklemore & ryan lewis",
  title == "This Is War" & is.na(performer) ~ "30 seconds to mars",
  title == "TROPICAL FAMILY" & performer == "various artists" ~ "tropical family",
  title == "Songs of Anarchy: Music from Sons of Anarchy Seasons 1-4" ~ "artistes divers",
  
  title == "Pulp Fiction" ~ "artistes divers",
  title == "ALADDIN" ~ "artistes divers",
  title == "Violetta en Vivo" ~ "artistes divers",
  title == "Violetta - Hoy somos mas" ~ "artistes divers",
  title == "LA REINE DES NEIGES" ~ "artistes divers",
  title == "LE ROI SOLEIL" & performer == "le roi soleil" ~ "artistes divers",
  title == "UNIVERSAL" & performer == "best of mid oct12" ~ "artistes divers",
  title == "UNIVERSAL" & performer == "best of mid mar11" ~ "artistes divers",
  
  TRUE ~ performer
)
)

m <- mutate(m, performer = ifelse(str_detect(performer, "^compilation"), "artists divers", performer))



rm(dics, dic)

##### Genres #####
# avec quelques interprétations...
m <- mutate(m, genre = case_when(
  category == "Divers"                                  ~ NA_character_,
  category == "Autre"                                   ~ NA_character_,
  category == "AUTRES"                                  ~ NA_character_,
  category == "COMPILATION"                             ~ NA_character_,
  category == "Multimedia"                              ~ NA_character_,
  category == "Compilation Thematiques"                 ~ NA_character_,
  category == "Compilation Thématiques"                 ~ NA_character_,
  category == "Video"                                   ~ NA_character_,
  category == "Texte"                                   ~ NA_character_,
  
  category == "Ambiance"                                ~ "Ambiance",
  category == "New Age"                                 ~ "Ambiance",
  category == "New age/ambient"                         ~ "Ambiance",
  category == "Bruitage / Sonorisation / Bruits Nature" ~ "Ambiance",
  category == "Trip hop"                                ~ "Ambiance",
  category == "Relaxation"                              ~ "Ambiance",
  
  category == "Classique"                               ~ "Classique",
  category == "Classique Concerto"                      ~ "Classique",
  category == "Classique Contemporaine"                 ~ "Classique",
  category == "Classique Liturgie"                      ~ "Classique",
  category == "Classique Lyrique"                       ~ "Classique",
  category == "Classique Messes / Requiem / Passions"   ~ "Classique",
  category == "Classique Musique Baroque"               ~ "Classique",
  category == "Classique Recitals"                      ~ "Classique",
  
  category == "Comedie Musicale"                        ~ "Comedie musicale",
  category == "Comedies Musicales"                      ~ "Comedie musicale",
  
  category == "Dance"                                   ~ "Dance",
  category == "Dance Francaise"                         ~ "Dance",
  category == "Dance Internationale"                    ~ "Dance",
  
  category == "Electro"                                 ~ "Electro",
  category == "Electro Francaise"                       ~ "Electro",
  category == "Electro Internationale"                  ~ "Electro",
  category == "Techno - Jungle - House"                 ~ "Electro",
  category == "Compilation Electro / Dance"             ~ "Electro",
  
  category == "Compilation Enfant"                      ~ "Enfant",
  category == "Enfant"                                  ~ "Enfant",
  category == "Enfant Comptine / Berceuse / Ronde"      ~ "Enfant",
  
  category == "Jazz"                                    ~ "Jazz, blues",
  category == "Jazz Rock"                               ~ "Jazz, blues",
  category == "Compilation Jazz / Blues / Gospel"       ~ "Jazz, blues",
  category == "Blues"                                   ~ "Jazz, blues",
  category == "Gospel"                                  ~ "Jazz, blues",
  
  category == "Metal"                                   ~ "Metal, hard rock",
  category == "Metal Francais"                          ~ "Metal, hard rock",
  category == "Metal International"                     ~ "Metal, hard rock",
  category == "Hard Rock"                               ~ "Metal, hard rock",
  category == "Hard rock/heavy metal"                   ~ "Metal, hard rock",
  
  category == "Compilation Bande Originale"             ~ "OST film",
  category == "Cinema"                                  ~ "OST film",
  category == "BOF"                                     ~ "OST film",
  category == "Musique de Film / Serie TV"              ~ "OST film",
  
  category == "Musiques du Monde"                       ~ "Musique du monde",
  category == "Musiques du Monde Afrique"               ~ "Musique du monde",
  category == "Musiques du Monde Amerique Latine"       ~ "Musique du monde",
  category == "Musiques du Monde Amérique Latine"       ~ "Musique du monde",
  category == "Musiques du Monde Asie / Oceanie"        ~ "Musique du monde",
  category == "Musiques du Monde Europe"                ~ "Musique du monde",
  category == "Musiques du Monde Orient / Pays Arabes"  ~ "Musique du monde",
  
  category == "Rap"                                     ~ "Rap",
  category == "Rap Francais"                            ~ "Rap",
  category == "Rap - Hip Hop"                           ~ "Rap",
  category == "Rap International"                       ~ "Rap",
  
  category == "Reggae"                                  ~ "Reggae",
  category == "Reggae / Ragga"                          ~ "Reggae",
  category == "Reggae / Ragga International"            ~ "Reggae",
  
  category == "Pop rock"                                ~ "Pop rock",
  
  category == "Rock"                                    ~ "Rock",
  category == "Rock Francais"                           ~ "Rock",
  category == "Rock Inde"                               ~ "Rock",
  category == "Rock Indé"                               ~ "Rock",
  category == "Rock International"                      ~ "Rock",
  category == "Inde"                                    ~ "Rock",
  category == "Compilation Rock"                        ~ "Rock",
  category == "Country"                                 ~ "Rock",
  
  category == "Soul / Funk / R&B"                       ~ "Soul, funk, RnB",
  category == "Soul / Funk / R&B Francais"              ~ "Soul, funk, RnB",
  category == "Soul / Funk / R&B International"         ~ "Soul, funk, RnB",
  category == "Soul - Funk - R'n'b"                     ~ "Soul, funk, RnB",
  
  category == "Variete Francaise"                       ~ "Variete francaise",
  category == "Pop Francaise"                           ~ "Variete francaise",
  category == "Variété Francaise"                       ~ "Variete francaise",
  category == "Compilation Variete"                     ~ "Variete francaise",
  category == "Compilation Variété"                     ~ "Variete francaise",
  
  category == "Variete Internationale"                  ~ "Variete internationale",
  category == "Variété Internationale"                  ~ "Variete internationale",
  category == "Pop Internationale"                      ~ "Variete internationale",
  TRUE                                                  ~ NA_character_))


# str_replace_all()
# filter(m, str_detect(performer, "franck")) %>% select(performer, title) %>% distinct() %>% print(n=100)
# filter(m, performer %in% c("john william", "john williams")) %>% select(performer, title) %>% distinct()

###### Base individu ######

## diplome et occupation de ego non renseigné quand ego est chef de famille
i <- mutate(i, niveau_d_etudes = ifelse(is.na(niveau_d_etudes), niveau_d_etudes_chef_de_famille, niveau_d_etudes),
            profession         = ifelse(is.na(profession),      profession_chef_de_famille,      profession))

## Préparer l'appareillement des bases de données
m <- mutate(m, vague = case_when(year(date_de_l_achat) < 2015 ~ "p13_14",
                                 year(date_de_l_achat) == 2015 ~ "p15",
                                 year(date_de_l_achat) == 2016 ~ "p16"))

## Factoriser

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

