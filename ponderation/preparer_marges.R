path_to_recensement <- "smb://p-satanas.rd.francetelecom.fr/rgpd/2019_2020_Usages_Consommation_Musicale_DEPS/C_Données/RP2016_INDREG_csv/FD_INDREG_2016.csv"

d <- read_delim(path_to_recensement, 
                delim = ";",
               locale = locale(decimal_mark = ".",
                               grouping_mark = " "),
               col_types = cols_only(
                 DEPT = col_factor(),
                 REGION = col_factor(),
                 AGED = col_double(),
                 CS2 = col_factor(),
                 SEXE = col_factor(),
                 IPONDI = col_double()
               ))

x <- group_by(d, AGED, SEXE, CS2, REGION, DEPT) %>% 
  summarize(n_raw = n(),
            n_pond = sum(IPONDI))

write_csv(x, path = here("data", "RP2016", "recensement2016_agrege.csv"))
