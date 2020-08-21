
###### Base personne ######

# Pour certaines analyses, on voudrait que chaque ego ait une valeur unique
# sur certaines variables. Nécessaire notamment pour agréger toutes les consommations
# Comment faire?

# On commence par diagnostiquer ce qui ne va pas.
# restreindre la base à ceux qui achètent = pas la peine de se prendre la tête pour tout le monde

hhv <- paste0(m$hhkey, "_", m$vague) %>% unique()

i <- mutate(i, hhvague = paste0(hhkey, "_", vague)) %>% 
  filter(hhvague %in% hhv)

### Réintégrer corrections manuelles
## Sur la façon dont on a produit le diagnostic, cf. la partie
## diagnostic dans le fichier recodages_personne.R

ps <- readxl::read_xlsx(here("data", "persons_solutions.xlsx"))

i <- i %>% 
  left_join(select(ps, hhkey, vague, ends_with("_alt")))

i <- mutate(i, 
            profession = ifelse(!is.na(profession_alt), profession_alt, as.character(profession)),
            profession_chef_de_famille = ifelse(!is.na(profession_chef_de_famille_alt), profession_chef_de_famille_alt, as.character(profession_chef_de_famille)),
            age = ifelse(!is.na(age_alt), age_alt, age),
            sexe = ifelse(!is.na(sexe_alt), sexe_alt, as.character(sexe)),
            dipl = ifelse(!is.na(dipl_alt), dipl_alt, as.character(dipl)))


## Refaire ce qui est défait, les facteurs (copié de recodage_individus)

i <- mutate(i, milieu = case_when(profession_chef_de_famille == "Agriculteur exploitant"         ~ "Agriculteur, artisan, commerçant", 
                                  profession_chef_de_famille == "Artisan"                        ~ "Agriculteur, artisan, commerçant",
                                  profession_chef_de_famille == "Commerçant et assimilé"         ~ "Agriculteur, artisan, commerçant", 
                                  profession_chef_de_famille == "Anc. artis/commerc/chef entrep" ~ "Agriculteur, artisan, commerçant", 
                                  profession_chef_de_famille == "Ancien agric. exploitant"       ~ "Agriculteur, artisan, commerçant", 
                                  profession_chef_de_famille == "Chef d’entreprise +10 sal."     ~ "Cadres et prof. intell. sup., chefs. entrep.",
                                  profession_chef_de_famille == "Profession lib. & assimilé"     ~ "Cadres et prof. intell. sup., chefs. entrep.",
                                  profession_chef_de_famille == "Cadre d’entreprise"             ~ "Cadres et prof. intell. sup., chefs. entrep.",
                                  profession_chef_de_famille == "Anc. cadre & prof. Inter"       ~ "Retraité CPIS ou PI", 
                                  profession_chef_de_famille == "Cadre fonc pub/prof intel/art"  ~ "Cadres et prof. intell. sup., chefs. entrep.",
                                  profession_chef_de_famille == "inter enseig/santé/fonc. pub"   ~ "Professions intermédiaires",
                                  profession_chef_de_famille == "Contremaitre/agent maitrise"    ~ "Professions intermédiaires", 
                                  profession_chef_de_famille == "Prof inter adm/com entrep."     ~ "Professions intermédiaires",
                                  profession_chef_de_famille == "Technicien"                     ~ "Professions intermédiaires",
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
                                               "Cadres et prof. intell. sup., chefs. entrep.",
                                               "Retraité CPIS ou PI",
                                               "Professions intermédiaires", 
                                               "Ouvriers et employés",
                                               "Autres")))

i <- mutate(i, 
            sexe = factor(sexe, levels = c("Homme", "Femme")),
            dipl = factor(dipl, levels = c("Moins que bac", "Bac", 
                                           "Enseignement Supérieur COURT", 
                                           "Enseignement Supérieur LONG"))
            )

## Ajout du 30 juillet 2020
i <- mutate(i, dipl = fct_collapse(dipl, 
                                   "Plus que bac" = c("Enseignement Supérieur COURT",
                                                      "Enseignement Supérieur LONG")))

## Supprimer les réponses fantaisistes
## supprime 10 achat, 5 personnes (un compte)
m <- filter(m, !(hhkey %in% (filter(i, hhkey_alt == "remove") %>% pull(hhkey))))
i <- filter(i, hhkey_alt != "remove" | is.na(hhkey_alt))

## Distinguer 12 personnes qui sont des personnes différentes utilisant
## le même compte

## Note que tous hhkey = integer à 8 chiffres, commençant par 1, 2 ou 3 =>
## En ajoutant 3e7, on force à commencer par 4, 5, 6 et à être unique
i <- mutate(i, hhkey_alt = ifelse(!is.na(hhkey_alt), hhkey + 30000000L, hhkey))

m <- m %>% 
  left_join(select(i, hhkey, vague, hhkey_alt))

i <- select(i, -hhkey) %>% rename(hhkey = "hhkey_alt")
m <- select(m, -hhkey) %>% rename(hhkey = "hhkey_alt")

## Une fois faites les corrections manuelles, reste à produire une base des personnes
p <- arrange(i, hhkey, vague) %>% 
  group_by(hhkey) %>% 
  slice(1)

## Calculer l'année de naissance
p <- mutate(p, 
            vague_num = paste0("20", str_remove(vague, "p")) %>% as.numeric(),
            age_annais = vague_num - age) %>% 
  select(-vague_num)

rm(ps)

###### Diagnostic ######

# hh_sexchange <- distinct(i, hhkey, sexe) %>% 
#   group_by(hhkey) %>% 
#   mutate(n = n_distinct(sexe)) %>% 
#   filter(n > 1) %>%
#   pull(hhkey)
# 
# hh_diplchange <- distinct(i, hhkey, dipl) %>% 
#   group_by(hhkey) %>% 
#   mutate(n = n_distinct(dipl)) %>% 
#   filter(n > 1) %>%
#   pull(hhkey)
# 
# hh_profchange <- distinct(i, hhkey, profession) %>% 
#   group_by(hhkey) %>% 
#   mutate(n = n_distinct(profession)) %>% 
#   filter(n > 1) %>%
#   pull(hhkey)
# 
# hh_agechange <- mutate(i, vague_num = paste0("20", str_remove(vague, "p")) %>% as.numeric(),
#                        age_annais = vague_num - age) %>% 
#   select(hhkey, vague, age_annais, age, sexe, profession, dipl, code_postal) %>% 
#   group_by(hhkey) %>% 
#   mutate(nd_age = n_distinct(age_annais),
#          age_annais_mean = mean(age_annais),
#          age_annais_sd = sd(age_annais),
#          age_annais_range = max(age_annais) - min(age_annais)) %>% 
#   filter(nd_age > 1) %>% 
#   pull(hhkey)
# 
# 
# 
# filter(i, hhkey %in% c(hh_sexchange, hh_diplchange, hh_profchange, hh_agechange)) %>% 
#   select(hhkey, vague, sexe:lien_parente_des_membres_10, -niveau_d_etudes, dipl) %>% 
#   mutate(sexchange = hhkey %in% hh_sexchange,
#          diplchange = hhkey %in% hh_diplchange,
#          profchange = hhkey %in% hh_profchange,
#          agechange = hhkey %in% hh_agechange) %>% 
#   arrange(hhkey) %>% 
#   write_csv(here("data", "persons_problems.csv"))
## Les problèmes concernent 1597 personnes, une bonne partie de l'échantillon donc...

#  write_csv("sexchange.csv")