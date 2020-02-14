# Pré-requis

Avoir les packages listés dans `import/packages.R` installés.

Ajouter un répertoire `data` contenant : 

+ un répertoire `consommateurs` incluant les fichiers panel conso: `Achats_juillet 2013_decembre_2014.xlsx`  `Extract Panel Consommateurs GfK - DEPS - v2016.xlsx`  `RВponses Profilage 1314.xlsx`  `RВponses Profilage 15.xlsx`  `RВponses Profilage 16.xlsx`
+ un répertoire `distributeurs` incluant : `Dico_physique_France_Musique.csv`
+ un répertoire `RP2016` contenant `recensement_agrege.csv` issu du script `preparer_marges.R` ainsi que le fichier `varmod_INDREG_2016.csv` fourni par l'INSEE dans les données individus region du recensement 2016.

# Usage

## Import

`import/import.R` produit les bases de données consommateur et distributeur au format RData, et les stocke dans `./data/data.RData`

`import/recodages.R` nettoie la base.

Le recodage s'appuie sur le script `import/dictionnaires_artistes.R` qui répertorie les noms d'artistes synonymes.

De sorte que chaque analyse peut commencer par le script suivant:

~~~r
library(tidyverse)
library(lubridate) # important de charger lubridate avant here
library(here)
load(here("data", "data.RData"))
source(here("import", "recodages.R"))
~~~

## Pondération

### Préparer les marges

`./ponderation/preparer_marges.R` prend les données individuelles du recensement 2016 en entrée et fabrique `data/RP2016/recensement2016_agrege.csv` pour préparer les marges du redressement.

Les données du recensement sont disponibles sur le site de l'INSEE : https://www.insee.fr/fr/statistiques/4171523?sommaire=4171558

### Redresser

`./ponderation/calage.R` effectue la pondération sur chaque vague. Le script retourne un objet i avec une variable poids correspondant au coefficient de pondération de chaque individu-année.