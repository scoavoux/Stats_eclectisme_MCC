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

# str_replace_all()
# filter(m, str_detect(performer, "franck")) %>% select(performer, title) %>% distinct() %>% print(n=100)
# filter(m, performer %in% c("john william", "john williams")) %>% select(performer, title) %>% distinct()
