x = c("tidyverse","curl","DBI","arrow","bir","icarus","readxl","melodi","countrycode","httr2",'rvest','countrycode','httr','fixest','here','eurostat','stringr')

lapply(x,library,character.only = T)

files <- c(
  "computations.R",
  "data.R",
  "mapping.R",
  "utils.R"
)

lapply(files, function(f) source(here(f)))

compute_ict_shares = function(year)
{
dataset.sdbs = read.csv(paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_SDBSBSC_ISIC4@DF_SDBS_ISIC4,1.0/A..PROD.._T.?startPeriod=",year,#VAFC
                               "&endPeriod=",year,"&dimensionAtObservation=AllDimensions&format=csvfilewithlabels")) %>%
  mutate(ISIC4 = substring(ACTIVITY,2)) %>%
  filter(ISIC4 %in% c("26","46","62","95","63","61","58",
                      "261","262","263","264","268",
                      "465",
                      "620","951","631","61","582")) %>%
  mutate(level = case_when(nchar(ISIC4) == 2 ~ 1,
                           T ~ 2),
         branch = case_when(substr(ISIC4,1,2) %in% c("62","63") ~ "J62_63",
                            substr(ISIC4,1,2) %in% c("26") ~ "C26",
                            substr(ISIC4,1,2) %in% c("46") ~ "G46",
                            substr(ISIC4,1,2) %in% c("58") ~ "J58",
                            substr(ISIC4,1,2) %in% c("61") ~ "J61",
                            substr(ISIC4,1,2) %in% c("95") ~ "S95")) %>%
  group_by(REF_AREA,substr(ISIC4,1,2)) %>%
  filter(any(level == 1)) %>%
  group_by(REF_AREA,branch) %>%
  summarise(VALUE_LVL2 = unique(sum(OBS_VALUE[level == 2],na.rm=T)),
            VALUE_LVL1 = unique(sum(OBS_VALUE[level == 1],na.rm=T)),.groups = 'drop')

country_shares = dataset.sdbs %>%
  group_by(REF_AREA,branch) %>%
  summarise(value = case_when(branch == "J61" ~ 1,
                              T ~ VALUE_LVL2 / VALUE_LVL1),
            .groups = 'drop')

global_shares = dataset.sdbs %>%
  group_by(branch) %>%
  summarise(value = case_when(branch == "J61" ~ 1,
                              T ~ sum(VALUE_LVL2,na.rm=T) / sum(VALUE_LVL1,na.rm=T)),
            REF_AREA = 'WORLD',
            .groups = 'drop') %>%
  distinct()

shares = rbind(country_shares,global_shares) %>%
  mutate(REF_AREA = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL))

return(shares)
}




compute_macro_fpt_K = function(year,update,verbose = T)
{
  K = format_domestic_capital_tables(year)$CAPITAL_USE

  dbWriteTable(m0_connexion_init(),
               SQL('models.figaro_capital_use'),
               K %>%
                 as.data.frame() %>%
                 rownames_to_column('resource_id') %>%
                 pivot_longer(-1,names_to = 'use_id') %>%
                 mutate(year = year,
                        resource_country = sub("_.*", "", resource_id),
                        resource_industry = sub("^[^_]+_", "", resource_id),
                        use_country = sub("_.*", "", use_id),
                        use_industry = sub("^[^_]+_", "", use_id)),
               append = ifelse(year > 2010,TRUE,FALSE),
               overwrite = ifelse(year > 2010,FALSE,TRUE))

  FIGARO = import_figaro_io_table(year,verbose)

  Z = FIGARO %>% select(any_of(rownames(.))) %>% filter(rownames(.) %in% colnames(.)) %>% as.matrix() # Intermediate Transactions Matrix

  X = FIGARO %>%
    colSums() %>%
    subset(!grepl('P3|P5',names(.))) %>%
    data.frame(production = .) %>%
    as.matrix()

  E = import_ghg_data(year,FIGARO)$value * 1000

  diag(Z)[diag(Z) == colSums(Z, na.rm = TRUE)] <- 0

  B = case_when(X == 0 ~ 0,
                T ~ E/X)
  A <- sweep(Z+K, 2, as.numeric(X), `/`)
  A[is.nan(A) | is.infinite(A)] <- 0

  L <- solve(diag(nrow = nrow(A)) - A)

  L_adj = sweep(L,2,diag(L),`/`)

  distributed_fpt = sweep(L_adj,1,B,`*`)

  #Economic aggregates

  P2 = colSums(Z,na.rm = T)

  P51c = colSums(K,na.rm = T)

  B1G = X - P2

  B1N = B1G - P51c

  #Environmental footprints

  GVA = E / B1G

  NVA = E / B1N

  CFC = case_when(P51c > 0 ~ colSums(distributed_fpt * K,na.rm=T) / P51c,
                  T ~ 0) %>%
  `names<-`(names(P51c))

  IC = case_when(P2 > 0 ~ colSums(distributed_fpt * Z,na.rm=T) / P2,
                  T ~ 0) %>%
    `names<-`(names(P2))

  PRD = colSums(distributed_fpt,na.rm=T)

  results =
    data.frame(row.names = names(PRD),
               IC = as.numeric(IC),
               CFC = as.numeric(CFC),
               NVA = as.numeric(NVA),
               GVA = as.numeric(GVA),
               PRD = as.numeric(PRD)) %>%
    rownames_to_column('id') %>%
    pivot_longer(-1,names_to = 'aggregate',values_transform = list(value = ~round(.x, 1))) %>%
    separate(id,c('country','industry'),sep = "_",remove = F,extra = 'merge') %>%
    mutate(year = year)

  return(results)

}

library(DBI) ; library(bir)

for(i in 2010:2021)
{

  fpt = compute_macro_fpt_K(i)

  dbWriteTable(m0_connexion_init(),
               SQL('macrodata.macro_fpt_k'),
               fpt,
               append = ifelse(i > 2010,TRUE,FALSE),
               overwrite = ifelse(i > 2010,FALSE,TRUE))

  # test = dbGetQuery(m0_connexion_init(),"SELECT * FROM macrodata.macro_fpt_k WHERE country = 'FR'")

}

# A = sweep(Z, 2, as.numeric(X), `/`) ; A[is.nan(A) | is.infinite(A)] <- 0
#
# L = solve(diag(nrow = nrow(A)) - A)
#
# distributed_fpt = sweep(L,1,B,`*`) %>%
#   colSums() %>%
#   as.data.frame() %>%
#   `colnames<-`('value') %>%
#   rownames_to_column('id') %>%
#   filter(grepl('FR',id)) %>%
#   mutate(industry = sub("^[^_]+_", "", id)) %>%
#   select(industry,value)
#
# write.table('C:/Users/Joris/OneDrive - La Société Nouvelle/Documents/Travaux statistiques/Travaux appliqués/F Charpentier - ICT4S/ARCEP ADEME/Empreinte DF 01-2026/FPT.csv',
#             sep = ";",
#             row.names = F,
#             append = case_when())
