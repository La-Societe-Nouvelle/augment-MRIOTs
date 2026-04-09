x = c("tidyverse","curl","arrow","readxl","countrycode","httr2",'rvest','countrycode','httr','fixest','here','eurostat','stringr')

lapply(x,library,character.only = T)

files <- c(
  "computations.R",
  "data.R",
  "mapping.R",
  "utils.R"
)

lapply(files, function(f) source(here(f)))

compute_footprint_K = function(year,pattern = '26|46|58|61|62|63|95',verbose = T)
{
  K = build_capital_use(year)$CAPITAL_USE

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

  if(pattern == '26|46|58|61|62|63|95') # In the specific case of ICT, we refine the output according to the strategy described in the paper (ICT output importance at the 3-digit level).
  {
    ICT_SHARES = compute_ict_shares(year) %>% filter(REF_AREA %in% c('WORLD',
                                                                     substr(colnames(FIGARO),1,2))) %>%
      mutate(id = paste0(REF_AREA,"_",branch))

    X = X %>%
      as.data.frame() %>%
      rownames_to_column('id') %>%
      separate(id,c('country','industry'),sep = '_',extra = 'merge',remove = F) %>%
      filter(grepl(paste0(unique(ICT_SHARES$branch),collapse = "|"),id)) %>%
      mutate(SHARE = case_when(id %in% ICT_SHARES$id ~ ICT_SHARES$value[match(id,ICT_SHARES$id)],
                               T ~ ICT_SHARES$value[match(paste0('WORLD_',industry),ICT_SHARES$id)]),
             ADJUSTED_VALUE = production * SHARE) %>%
      column_to_rownames('id') %>%
      select(ADJUSTED_VALUE)

  }

  selected_industry_num <- grep(pattern, colnames(x = A))

  sub_X = as.matrix(X[grep(pattern, rownames(X)),1, drop = FALSE])

  L <- solve(diag(nrow = nrow(A)) - A)

  sub_invL <- solve(L[selected_industry_num, selected_industry_num])

  prescaling_sub_prod <- sub_invL %*% sub_X

  full_prod <- L[, selected_industry_num] %*% diag(x = as.numeric(prescaling_sub_prod))

  distributed_fpt <- (diag(x = as.numeric(B)) %*% full_prod) %>% `colnames<-`(colnames(A)[selected_industry_num]) %>% `rownames<-`(rownames(A))

  total_fpt <- colSums(distributed_fpt, na.rm = TRUE) %>% `names<-`(colnames(A)[selected_industry_num])

  return(list(DISTRIBUTED = distributed_fpt,
              BY_SECTOR = total_fpt,
              TOTAL = sum(total_fpt)))
}

results = compute_footprint_K(2021)
