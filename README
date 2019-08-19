# Pré-requis

Avoir les packages listés dans `import/packages.R` installés.

Ajouter un répertoire `data` contenant : 

+ un répertoire `consommateurs` incluant les fichiers panel conso: `Achats_juillet 2013_decembre_2014.xlsx`  `Extract Panel Consommateurs GfK - DEPS - v2016.xlsx`  `RВponses Profilage 1314.xlsx`  `RВponses Profilage 15.xlsx`  `RВponses Profilage 16.xlsx`
+ un répertoire `distributeurs` incluant : `Dico_physique_France_Musique.csv`

# Usage

`import/import.R` produit les bases de données consommateur et distributeur au format RData, et les stocke dans `./data/data.RData`
<
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


