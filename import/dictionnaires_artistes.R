dics <- list()

# ordre des noms
dics[["ordre"]] <- c(
    "^albarn damon$" = "damon albarn",
    "^aubert jean louis$" = "jean louis aubert",
    "^berger michel$" = "michel berger",
    "^blunt james$" = "james blunt",
    "^bowie david$" = "david bowie",
    "^brillant dany$" = "dany brillant",
    "^casal luz$" = "luz casal",
    "^charles ray$" = "ray charles",
    "^clerc julien$" = "julien clerc",
    "^daho etienne$" = "etienne daho",
    "^derulo jason$" = "jason derulo",
    "^dessay natalie$" = "natalie dessay",
    "^diamond neil$" = "neil diamond",
    "^ezra george$" = "george ezra",
    "^fersen thomas$" = "thomas fersen",
    "^goldman jean jacques$" = "jean jacques goldman",
    "^groban josh$" = "josh groban",
    "^guetta david$" = "david guetta",
    "^hallyday johnny$" = "johnny hallyday",
    "^hepburn alex$" = "alex hepburn",
    "^jones norah$" = "norah jones",
    "^joplin janis$" = "janis joplin",
    "^knopfler mark$" = "mark knopfler",
    "^lemay lynda$" = "lynda lemay",
    "^lenorman gerard$" = "gerard lenorman",
    "^mae christophe$" = "christophe mae",
    "^mars bruno$" = "bruno mars",
    "^michael frank$" = "frank michael",
    "^pausini laura$" = "laura pausini",
    "^redding otis$" = "otis redding",
    "^sanson veronique$" = "veronique sanson",
    "^santamaria arno$" = "arno santamaria",
    "^sardou michel$" = "michel sardou",
    "^sheeran ed$" = "ed sheeran",
    "^souchon alain$" = "alain souchon",
    "^vassili amaury$" = "amaury vassili",
    "^farlow$" = "tal farlow")

# fautes d'orthographe
dics[["ortho"]] <- c(
    "^2 pac$" = "2pac",
    "^ac dc$" = "ac/dc",
    "^acdc$" = "ac/dc",
    "^anne etchegoyen le choeur aizkoa$" = "anne etchegoyen & le choeur aizkoa",
    "^b b king$" = "bb king",
    "^ben harper and the innocent criminals$" = "ben harper & the innocent criminals",
    "^black eyed peas$" = "the black eyed peas",
    "^bon jovi$" = "jon bon jovi",
    "^bruce springsteen and the e street band$" = "bruce springsteen & the e street band",
    "^buzzcocks$" = "the buzzcocks",
    "^c\\?line dion$" = "celine dion",
    "^chordettes$" = "the chordettes",
    "^christine and the queens$" = "christine & the queens",
    "^cour de pirate$" = "coeur de pirate",
    "^cranberries$" = "the cranberries",
    "^cyndi lauper$" = "cindy lauper",
    "^earth wind & fire$" = "earth wind and fire",
    "^erik frasiak$" = "eric frasiak",
    "^florence \\+ the machine$" = "florence & the machine",
    "^franck zappa$" = "frank zappa",
    "^francks michael$" = "frank michael",
    "^herbert karajan$" = "herbert von karajan",
    "von karajan herbert" = "herbert von karajan",
    "^hugues auf$" = "hugues aufray",
    "^johnny halliday$" = "johnny hallyday",
    "^julian perreta$" = "julian perretta",
    "^knopfler$" = "mark knopfler",
    "^kylie minougue$" = "kylie minogue",
    "^l oldelaf & monsieur d$" = "oldelaf et monsieur d",
    "^la laâ©gende du roi arthur$" = "la legende du roi arthur",
    "^last shadow puppets$" = "the last shadow puppets",
    "^le peuple de lherbe$" = "le peuple de l herbe",
    "^les hurlements dleo$" = "les hurlements d leo",
    "^les ogres de barbak$" = "les ogres de barback",
    "^les pr\\?tres$" = "les pretres",
    "^lilly wood and the prick$" = "lilly wood & the prick",
    "^ma\\+â«tre gims$" = "maitre gims",
    "^mireille matthieu$" = "mireille mathieu",
    "^of monsters and men$" = "of monsters & men",
    "^offspring$" = "the offspring",
    "^p!nk$" = "pink",
    "^paco de luc\\?a$" = "paco de lucia",
    "^pere duval$" = "le pere duval",
    "^pharell williams$" = "pharrell williams",
    "^pharrell willams$" = "pharrell williams",
    "^playing for change band$" = "playing for change",
    "^rockabye baby$" = "rockabye baby!",
    "^rolling stones$" = "the rolling stones",
    "^royksopp and robyn$" = "royksopp & robyn",
    "^shym$" = "shy m",
    "^simon and garfunkel$" = "simon & garfunkel",
    "^the kids united$" = "kids united",
    "^the scorpions$" = "scorpions",
    "^thelonious$" = "thelonious monk",
    "^velvet underground$" = "the velvet underground",
    "^the pink floyd$" = "pink floyd",
    "^cour de pirate$" = "coeur de pirate",
    "^harris calvin feat florence welch$" = "calvin harris",
    "^detroit \\(bertrand cantat pascal humbert\\)$" = "detroit",
    "^maison tellier \\(la\\)$" = "la maison tellier",
    "^brel$" = "jacques brel",
    "^kendji$" = "kendji girac",
    "^presley$" = "elvis presley",
    "^bebo & cigala$" = "bebo valdes & diego el cigala",
    "^cure$" = "the cure",
    "biffy cliro" = "biffy clyro",
    "winston mcanuff & fixiâ" = "winston mcanuff & fixi",
    "yngwie malmsteen" = "yngwie j malmsteen",
    "fredericks goldman jones" = "carole fredericks & jean jacques goldman & michael jones",
    "carole fredericks & jean jacques goldman michael jones" = "carole fredericks & jean jacques goldman & michael jones",
    "^beatles$" = "the beatles",
    "^eagles$" = "the eagles",
    "^cale j j$" = "jj cale",
    "capart \\(louis\\)" = "louis capart",
    "charles aznavour nouveau contrat" = "charles aznavour",
    "^dylan$" = "bob dylan",
    "^dylan bob$" = "bob dylan",
    "^edith piaf chant$" = "edith piaf",
    "^eodm \\(eagles of death metal\\)$" = "eagles of death metal",
    "fatals picards \\(les\\)" = "les fatals picards",
    "gaâ©naâ©ration goldman m pokora tal" = "m pokora & tal",
    "generation goldman" = "artistes divers",
    "georges brassens \\(5cd\\)" = "georges brassens",
    "mickael jackson history futu" = "michael jackson",
    "naked & famous the" = "the naked & famous",
    "pink floyd atom heart mother \\(" = "pink floyd",
    "ritchie blackmore s rainbow" = "rainbow")

# vérification à partir des noms d'albums
dics[["verif"]] <- c(
    "^mahe$" = "yannick mahe",
    "^dion$" = "celine dion")

## artistes divers
dics[["divers"]] <- c(
    "^compilation$" = "artistes divers",
    "^divers$" = "artistes divers",
    "^multi artistes$" = "artistes divers",
    "^multi interpr\\?tes$" = "artistes divers",
    "^multi compositeurs$" = "artistes divers",
    "^multi interpretes$" = "artistes divers",
    "^various artist$" = "artistes divers",
    "^various artists$" = "artistes divers",
    "^various" = "artistes divers",
    "^variete" = "artistes divers",
    "^interpretes divers$" = "artistes divers")

# Simplifications
# on ajoute ici les modification pouvant être considérées
# comme donnant lieu à débat, notamment rapporter 
# des collectifs à leur auteur le plus connu

dics[["simplifications"]] <- c(
    "keith jarrett trio" = "keith jarrett",
    "benabar et associes" = "benabar",
    "ben harper & the innocent criminals" = "ben harper",
    "bob dylan & the band" = "bob dylan",
    "bob marley & the wailers" = "bob marley",
    "bruce springsteen & the e street band" = "bruce springsteen",
    "diana ross & the supremes" = "diana ross",
    "dire straits/knopfler" = "dire straits",
    "frank zappa & the mothers of invention" = "frank zappa",
    "jimi hendrix experience" = "jimi hendrix",
    "louis delort & the sheperds" = "louis delort")

#### Annexes: les scripts pour diagnostiquer les problèmes

## Puis, rechercher les proximités
# library(stringdist)
# 
# performer_dist <- stringdistmatrix(unique(m$performer), method = "lv")
# performer_dist <- as.matrix(performer_dist)
# performer_dist[upper.tri(performer_dist)] <- NA
# str(performer_dist)
# dimnames(performer_dist) <- list(unique(m$performer), unique(m$performer))
# df <- as.data.frame(performer_dist) %>%
#   rownames_to_column("performer") %>%
#   as_tibble(.name_repair = "minimal") %>%
#   gather(key, value, -performer)
# 
# rp <- c("m", " ")
# 
# df <- filter(df, performer != key, !is.na(value),
#              !(performer %in% rp),
#              !(key %in% rp))
# 
# arrange(df, value) %>% write_csv(path = "stringdist.csv")
# 
# 
# group_by(m, title) %>% 
#   distinct(performer, .keep_all = TRUE) %>% 
#   mutate(n = n()) %>% 
#   arrange(desc(n), title) %>% 
#   select(title, performer, n) %>% 
#   ungroup() %>% 
#   write_csv("titres.csv")
# 
# distinct(m, performer) %>%  arrange(performer) %>% write_csv("performers.csv")

# les noms inversé à remettre dans le bon sens (bowie david -> david bowie ; à faire à la main en refaisant un extract)
# les noms de groupes: Marley & the wailers, springsteen & e-street band...
