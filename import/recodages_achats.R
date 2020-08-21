# load(here("data", "data.RData"))

###### Renommage propre des variables #######

m <- rename(m, achat_pour_sexe = sexe,
            achat_pour_age = age,
            achat_pour_membre_famille = membre_famille)

###### Recodage des genres ######

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
  category == "Texte Humoristique"                      ~ NA_character_,
  
  category == "Ambiance"                                ~ "Ambiance",
  category == "New Age"                                 ~ "Ambiance",
  category == "New age/ambient"                         ~ "Ambiance",
  category == "Bruitage / Sonorisation / Bruits Nature" ~ "Ambiance",
  category == "Relaxation"                              ~ "Ambiance",
  
  category == "Classique"                               ~ "Classique",
  category == "Classique Concerto"                      ~ "Classique",
  category == "Classique Contemporaine"                 ~ "Classique",
  category == "Classique Liturgie"                      ~ "Classique",
  category == "Classique Lyrique"                       ~ "Classique",
  category == "Classique Messes / Requiem / Passions"   ~ "Classique",
  category == "Classique Musique Baroque"               ~ "Classique",
  category == "Classique Recitals"                      ~ "Classique",
  category == "Classique Musique de Chambre"            ~ "Classique",
  category == "Classique Sonate"                        ~ "Classique",
  category == "Classique Symphonie"                     ~ "Classique",
  category == "Compilation Classique"                   ~ "Classique",
  
  category == "Comedie Musicale"                        ~ "Comedie musicale",
  category == "Comedies Musicales"                      ~ "Comedie musicale",
  
  category == "Dance"                                   ~ "Dance",
  category == "Dance Francaise"                         ~ "Dance",
  category == "Dance Internationale"                    ~ "Dance",
  category == "Compilation Electro / Dance"             ~ "Dance",
  
  category == "Electro"                                 ~ "Electro",
  category == "Electro Francaise"                       ~ "Electro",
  category == "Electro Internationale"                  ~ "Electro",
  category == "Techno - Jungle - House"                 ~ "Electro",
  category == "Trip hop"                                ~ "Electro",
  
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
  category == "Musiques du Monde Antilles / Reunion"    ~ "Musique du monde",
  category == "Musiques du Monde Asie / Océanie"        ~ "Musique du monde",
  category == "Compilation Musiques du Monde"           ~ "Musique du monde",
  
  category == "Rap"                                     ~ "Rap",
  category == "Rap Francais"                            ~ "Rap",
  category == "Rap - Hip Hop"                           ~ "Rap",
  category == "Rap International"                       ~ "Rap",
  category == "Compilation Urbain"                      ~ "Rap",
  
  category == "Reggae"                                  ~ "Reggae",
  category == "Reggae / Ragga"                          ~ "Reggae",
  category == "Reggae / Ragga International"            ~ "Reggae",
  category == "Reggae / Ragga Francais"                 ~ "Reggae",
  
  category == "Pop rock"                                ~ "Pop rock",
  category == "Compilation Pop"                         ~ "Pop rock",
  
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

###### Pre-recodage des noms d'artistes ######

# n_unique(m$performer)
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
m <- mutate(m, performer = str_replace_all(performer, "â®", "e"))
m <- mutate(m, performer = str_replace_all(performer, "aâ©", "e"))
m <- mutate(m, performer = str_replace_all(performer, "aâ«", "e"))
m <- mutate(m, performer = str_replace_all(performer, "aâ\\?°", "e"))
m <- mutate(m, performer = str_replace_all(performer, "\\+â«", "i"))
m <- mutate(m, performer = str_replace_all(performer, "aâ®", "i"))
m <- mutate(m, performer = str_replace_all(performer, "aâ§", "c"))
m <- mutate(m, performer = str_replace_all(performer, "aâ¶", "o"))

  ## remplacer les doubles/triples espaces
m <- mutate(m, performer = str_replace_all(performer, "\\s+", " "))
## trim whitespaces
m <- mutate(m, performer = str_replace_all(performer, "\\s$", ""))
m <- mutate(m, performer = str_replace_all(performer, "^\\s", ""))

## supprimer les artistes vides
m <- mutate(m, performer = ifelse(performer == "", NA, performer))

###### Recodage des noms d'artistes ######

## Dictionnaire
source(here("import", "dictionnaires_artistes.R"), encoding = "UTF-8")
for(dic in dics){
  m <- mutate(m, performer = str_replace_all(performer, dic))
}

m <- mutate(m, performer = case_when(
  title == "Changes" & performer == "faul & wad ad" ~ "faul & wad ad vs pnau",
  title == "LOUIS ARMSTRONG MEETS OSC/ARMSTRONG" & performer == "armstrong" ~ "louis armstrong",
  title == "Samson FRANCOIS, piano" ~ "samson francois",
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
  # recodage des NA
  code_ean == "602547498458"     & is.na(performer) ~ "a ha",
  code_ean == "886973382929"     & is.na(performer) ~ "ac dc",
  code_ean == "4029759105077"    & is.na(performer) ~ "ace of base",
  code_ean == "650113300223"     & is.na(performer) ~ "ah neeh mah",
  code_ean == "7611745654132"    & is.na(performer) ~ "alan simon",
  code_ean == "28948205332"      & is.na(performer) ~ "artistes divers",
  code_ean == "600753443880"     & is.na(performer) ~ "artistes divers",
  code_ean == "600753646380"     & is.na(performer) ~ "artistes divers",
  code_ean == "602527571775"     & is.na(performer) ~ "artistes divers",
  code_ean == "3149020822357"    & is.na(performer) ~ "artistes divers",
  code_ean == "3700477826956"    & is.na(performer) ~ "artistes divers",
  code_ean == "5014797760585"    & is.na(performer) ~ "artistes divers",
  code_ean == "887254674429"     & is.na(performer) ~ "asaf avidan",
  code_ean == "3359340159471"    & is.na(performer) ~ "bagdad de vannes",
  code_ean == "723721093655"     & is.na(performer) ~ "beethoven",
  code_ean == "602547010391"     & is.na(performer) ~ "ben howard",
  code_ean == "5036408158529"    & is.na(performer) ~ "bert kaempfert",
  code_ean == "888295516143"     & is.na(performer) ~ "camille and kennerly",
  code_ean == "3383510001888"    & is.na(performer) ~ "capricornus",
  code_ean == "7320470221488"    & is.na(performer) ~ "carptree",
  code_ean == "74646376027"      & is.na(performer) ~ "celine dion",
  code_ean == "3770004998005"    & is.na(performer) ~ "chassol",
  code_ean == "5054196369255"    & is.na(performer) ~ "chica vampiro",
  code_ean == "5425023012048"    & is.na(performer) ~ "colline hill",
  code_ean == "3660341358573"    & is.na(performer) ~ "damien dubois",
  code_ean == "4260075861418"    & is.na(performer) ~ "danny bryant",
  code_ean == "9397601001736"    & is.na(performer) ~ "david bowie",
  code_ean == "602557924022"     & is.na(performer) ~ "diablo swing orchestra",
  code_ean == "3375519980011"    & is.na(performer) ~ "dimoné",
  code_ean == "93624915515"      & is.na(performer) ~ "disturbed",
  code_ean == "50087311896"      & is.na(performer) ~ "dove cameron",
  code_ean == "825646017294"     & is.na(performer) ~ "ed sheeran",
  code_ean == "825646089079"     & is.na(performer) ~ "ed sheeran",
  code_ean == "4011222236302"    & is.na(performer) ~ "edith piaf",
  code_ean == "8429006769292"    & is.na(performer) ~ "efecto pasillo",
  code_ean == "602547217875"     & is.na(performer) ~ "ellie goulding",
  code_ean == "3770001198101"    & is.na(performer) ~ "ellipse",
  code_ean == "650922770125"     & is.na(performer) ~ "elvis presley",
  code_ean == "889397556228"     & is.na(performer) ~ "elvis presley",
  code_ean == "675640915825"     & is.na(performer) ~ "emma louise",
  code_ean == "602547343192"     & is.na(performer) ~ "eskimo callboy",
  code_ean == "93624930754"      & is.na(performer) ~ "ewert and the two dragons",
  code_ean == "640213206815"     & is.na(performer) ~ "extreme noise terror",
  code_ean == "813985011134"     & is.na(performer) ~ "five finger death punch",
  code_ean == "3760127222491"    & is.na(performer) ~ "folle journée de nantes",
  code_ean == "3760061159983"    & is.na(performer) ~ "gabriel saglio et les vieilles pies",
  code_ean == "9991508021622"    & is.na(performer) ~ "gustav holst",
  code_ean == "72435729412"      & is.na(performer) ~ "gustav mahler",
  code_ean == "851147006079"     & is.na(performer) ~ "heitor pereira",
  code_ean == "602547250575"     & is.na(performer) ~ "hollywood undead",
  code_ean == "3456533000265"    & is.na(performer) ~ "i lov dax",
  code_ean == "DLC2630041098261" & is.na(performer) ~ "jacques brel",
  code_ean == "794881770427"     & is.na(performer) ~ "jean marie machado",
  code_ean == "827565061785"     & is.na(performer) ~ "jo harman",
  code_ean == "3298498335125"    & is.na(performer) ~ "joey starr & nathy",
  code_ean == "50087384432"      & is.na(performer) ~ "john williams",
  code_ean == "602567142218"     & is.na(performer) ~ "johnny clegg",
  code_ean == "602537789672"     & is.na(performer) ~ "junkie xl",
  code_ean == "602557283877"     & is.na(performer) ~ "justin hurwitz",
  code_ean == "3760061159204"    & is.na(performer) ~ "kai dina",
  code_ean == "190295932480"     & is.na(performer) ~ "kids united",
  code_ean == "190295992064"     & is.na(performer) ~ "kids united",
  code_ean == "3521383423813"    & is.na(performer) ~ "legendaire",
  code_ean == "888430196827"     & is.na(performer) ~ "leiva",
  code_ean == "889853493821"     & is.na(performer) ~ "leiva",
  code_ean == "2630041098278"    & is.na(performer) ~ "leo ferre",
  code_ean == "887254794622"     & is.na(performer) ~ "leo rojas",
  code_ean == "887654351227"     & is.na(performer) ~ "leo rojas",
  code_ean == "889854360924"     & is.na(performer) ~ "london grammar",
  code_ean == "888430904521"     & is.na(performer) ~ "lower than atlantis",
  code_ean == "888430942622"     & is.na(performer) ~ "mallory knox",
  code_ean == "5700907265261"    & is.na(performer) ~ "manigance",
  code_ean == "602537599646"     & is.na(performer) ~ "marcel amont",
  code_ean == "825646144044"     & is.na(performer) ~ "marina and the diamonds",
  code_ean == "4582275372434"    & is.na(performer) ~ "miku hatsune",
  code_ean == "5414939804762"    & is.na(performer) ~ "milky chance",
  code_ean == "889853021123"     & is.na(performer) ~ "nena",
  code_ean == "889854794729"     & is.na(performer) ~ "nena",
  code_ean == "859381011521"     & is.na(performer) ~ "nick and knight",
  code_ean == "45778719426"      & is.na(performer) ~ "our last night",
  code_ean == "825646372324"     & is.na(performer) ~ "pablo alboran",
  code_ean == "67003111225"      & is.na(performer) ~ "passenger",
  code_ean == "604988092929"     & is.na(performer) ~ "paul and mary peter",
  code_ean == "3220017081343"    & is.na(performer) ~ "paul mauriat",
  code_ean == "4011222236371"    & is.na(performer) ~ "philippe clay",
  code_ean == "887254706120"     & is.na(performer) ~ "pink",
  code_ean == "602537755134"     & is.na(performer) ~ "pixie lott",
  code_ean == "6417138643155"    & is.na(performer) ~ "poets of the fall",
  code_ean == "3760200900377"    & is.na(performer) ~ "ray charles",
  code_ean == "4035719009033"    & is.na(performer) ~ "richard wagner",
  code_ean == "620638057728"     & is.na(performer) ~ "ruth moody",
  code_ean == "602547901132"     & is.na(performer) ~ "sandra",
  code_ean == "3760155960907"    & is.na(performer) ~ "sebastien perrin",
  code_ean == "8436004061723"    & is.na(performer) ~ "sergio et estibaliz",
  code_ean == "710357173325"     & is.na(performer) ~ "shura cherbassky",
  code_ean == "602498677346"     & is.na(performer) ~ "sober",
  code_ean == "708857990122"     & is.na(performer) ~ "stacey kent",
  code_ean == "850721006252"     & is.na(performer) ~ "state champs",
  code_ean == "802644880815"     & is.na(performer) ~ "steven wilson",
  code_ean == "817424014540"     & is.na(performer) ~ "stick to your guns",
  code_ean == "850721006641"     & is.na(performer) ~ "stick to your guns",
  code_ean == "8716059005171"    & is.na(performer) ~ "the wanton bishops",
  code_ean == "794043187223"     & is.na(performer) ~ "theodore shapiro",
  code_ean == "64027249729"      & is.na(performer) ~ "tibz",
  code_ean == "4002587646028"    & is.na(performer) ~ "tom robin",
  code_ean == "888750295323"     & is.na(performer) ~ "trent reznor & atticus ross",
  code_ean == "3760051124724"    & is.na(performer) ~ "tri yann",
  code_ean == "75678762758"      & is.na(performer) ~ "twenty one pilots",
  code_ean == "889854206628"     & is.na(performer) ~ "umberto tozzi",
  code_ean == "602557105209"     & is.na(performer) ~ "unheilig",
  code_ean == "600753567722"     & is.na(performer) ~ "vicky leandros",
  code_ean == "16861751524"      & is.na(performer) ~ "we are harlot",
  TRUE ~ performer
)
)

m <- mutate(m, performer = ifelse(str_detect(performer, "^compilation"), "artists divers", performer))

## netoyer les & / and

m <- mutate(m, performer = str_replace_all(performer, and_dic))

rm(dics, dic)

## Composer classiques
m <- mutate(m, title = tolower(title))

## corriger quelques fautes
m <- mutate_at(m, 
          vars(title, performer), 
          ~str_replace_all(., c("\\bhandel\\b" = "haendel",
                                "tchaikovski" = "tchaikovsky")))

# composers_main <- c("bach", "brahms", "mozart", "vivaldi", "tchaikovsky", "beethoven", "verdi")
# 
# composers <- c("bartok", "chopin", "corelli", "donizetti", 
#                "dvorak", "gershwin", "grieg", "haendel", "haydn", "holst", "liszt", 
#                "lully", "mahler", "marais", "massenet", "mendelssohn", "moussorgski", 
#                "prokofiev", "puccini", "purcell", "rameau", "ravel", 
#                "rimsky-korsakov", "rossini", "scarlatti", "schubert", "schumann", "smetana", 
#                "strauss", "stravinsky", "wagner") 
# 
# select(m, title, performer) %>% 
#   filter(str_detect(title, paste0("\\b", composers_main, "\\b")[1]))
# 
# library(quanteda)
# tt <- mutate(m, title = tolower(title)) %>% 
#   filter(genre == "Classique") %>% 
#   distinct(title)
# cp <- corpus(tt$title)
# 
# dfm <- dfm(cp)

## Préparer l'appareillement des bases de données
m <- mutate(m, vague = case_when(vague_conso == "m17"     ~ "p17",
                                 vague_conso == "m18"     ~ "p18",
                                 vague_conso == "m19"     ~ "p19",
                                 year(date_achat) == 2013 ~ "p13",
                                 year(date_achat) == 2014 ~ "p14",
                                 year(date_achat) == 2015 ~ "p15",
                                 year(date_achat) == 2016 ~ "p16"))

