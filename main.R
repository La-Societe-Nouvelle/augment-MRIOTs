x = c("tidyverse","curl","arrow","readxl","countrycode","httr2",'rvest','countrycode','httr','fixest','here','eurostat','stringr','Matrix','pdftools')

sapply(x, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
})

lapply(x,library,character.only = T)

files <- c(
  "computations.R",
  "data.R",
  "mapping.R",
  "utils.R",
  "vintage.R"
)

lapply(files, function(f) source(here(f)))

FIGARO_INDUSTRIES = c("A01","A02","A03","B","C10T12","C13T15","C16","C17","C18","C19","C20","C21","C22",
                      "C23","C24","C25","C26","C27","C28","C29","C30","C31_32","C33","D35","E36","E37T39",
                      "F","G45","G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60","J61","J62_63",
                      "K64","K65","K66","L","M69_70","M71","M72","M73","M74_75","N77","N78","N79","N80T82",
                      "O84","P85","Q86","Q87_88","R90T92","R93","S94","S95","S96","T","U")

# Direct GHG intensities of one year (same computation as in compute_footprint_K), used for the valuation by age of the capital
direct_intensities = function(year,verbose = T)
{
  FIGARO = import_figaro_io_table(year,verbose)

  X = FIGARO %>% colSums() %>% subset(!grepl('P3|P5',names(.)))

  E = import_ghg_data(year,FIGARO,verbose)$value * 1000

  case_when(X == 0 ~ 0,
            T ~ E/X) %>% `names<-`(names(X))
}


compute_footprint_K = function(year,pattern = ICT_PATTERN,capital = T,verbose = T,vintage = F)
{
  if(vintage & !capital) stop('The valuation by vintage requires capital = TRUE')

  capital_build = if(capital) build_capital_use(year) else NULL

  K = if(capital) capital_build$CAPITAL_USE else 0

  FIGARO = import_figaro_io_table(year,verbose)

  Z = FIGARO %>% select(any_of(rownames(.))) %>% filter(rownames(.) %in% colnames(.)) %>% as.matrix() # Intermediate Transactions Matrix

  X = FIGARO %>%
    colSums() %>%
    subset(!grepl('P3|P5',names(.))) %>%
    data.frame(production = .) %>%
    as.matrix()

  X_all = X # output of all industries, X is then restricted to the studied industries

  E = import_ghg_data(year,FIGARO)$value * 1000

  diag(Z)[diag(Z) == colSums(Z, na.rm = TRUE)] <- 0

  B = case_when(X == 0 ~ 0,
                T ~ E/X)
  A <- sweep(Z+K, 2, as.numeric(X), `/`)
  A[is.nan(A) | is.infinite(A)] <- 0

  if(pattern == ICT_PATTERN) # In the specific case of ICT, we refine the output according to the strategy described in the paper (ICT output importance at the 3-digit level).
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

  result = list(DISTRIBUTED = distributed_fpt,
                BY_SECTOR = total_fpt,
                TOTAL = sum(total_fpt))

  if(vintage)
  {
    inputs = list(Z = Z,X = X_all,B = `names<-`(as.numeric(B),rownames(X_all)))

    sys = figaro_system(inputs,capital_build,sub_X,pattern)

    vintages = 2010:year # the intensities of each year of acquisition, in the order of the industries

    intensities_by_year = sapply(vintages,function(y) if(y == year) inputs$B else direct_intensities(y,FALSE)) %>% `colnames<-`(vintages)

    v = footprint_by_vintage(sys,capital_use_by_vintage(year,capital_build,first_year = 2010,last_year = year,price = TRUE)$capital_use,intensities_by_year)

    result$VINTAGE = list(CURRENT = sum(sys$B * sys$tau_0), # footprint without the capital term
                          CAPITAL = sum(sys$B * (sys$tau_c - sys$tau_0)), # capital term, intensities of the year of analysis
                          CAPITAL_BY_VINTAGE = v$capital, # capital term, intensities of the year of acquisition
                          BY_VINTAGE = v$by_vintage,
                          TOTAL = v$footprint)
  }

  return(result)
}

#results = compute_footprint_K(2021)
#results = compute_footprint_K(2021,vintage = T)
#results = compute_footprint_K(2021,pattern = STUDIED_PATTERNS$MANUFACTURING,vintage = T)
#results = compute_footprint_K(2021,pattern = STUDIED_PATTERNS$EU,vintage = T)
