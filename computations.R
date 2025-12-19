get_capital_distribution_klems = function(year,classification,verbose = T)
{

  klems_industries = c('A','B',"C10-C12","C13-C15","C16-C18","C19","C20","C21","C22-C23","C24-C25","C26","C27","C28","C29-C30","C31-C33","D","E","F","G45","G46","G47",
                       "H49","H50","H51","H52","H53","I","J58-J60","J61","J62-J63","K","L","M","N","O","P","Q86","Q87-Q88","R","S","T","U")

  raw.klems = get_cache_file("https://www.dropbox.com/s/sp2p4m86et66nfg/capital%20accounts.csv?dl=1", pattern = 'raw.klems') %>%
    read.csv()

  va_klems = get_cache_file("https://www.dropbox.com/s/nkz7mdp0onken1j/national%20accounts.csv?dl=1",pattern = 'va_klems') %>%
    read.csv() %>%
    filter(year == !!year & nace_r2_code %in% klems_industries) %>%
    select(nace_r2_code,geo_code,VA_CP)

  meta.klems = get_cache_file("https://www.dropbox.com/s/srsn4zdb1wta27t/Variable-List-2023.xlsx?dl=1",pattern = 'meta.klems') %>%
    read_xlsx()

  matrix.gfcf = import_figaro_io_table(year) %>%
    filter(!grepl('W2',rownames(.))) %>%
    {.[,grepl('P51G',colnames(.))]}

  vector.gfcf = data.frame(value = rowSums(matrix.gfcf)) %>%
    rownames_to_column('id') %>%
    separate(id,into = c('country','industry'),sep = "_",extra = 'merge')

  #In EU KLEMS, all countries have the same level of granularity

  depreciations.klems = import_depreciation_rates_klems()

  #In EU KLEMS, Value added, investment, and capital stock are valued in M of national currency -> We apply OECD exchange rates

  oecd_rates = get_cache_file(link = paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A....EXC_A.......?startPeriod=',
                                            year,'&endPeriod=',year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels'),
                              pattern = paste0('OECD_EXCHANGE_RATES_',year)) %>%
    read.csv() %>%
    select(REF_AREA,RATE = OBS_VALUE,TIME_PERIOD) %>%
    reframe(EXCH_RATE = RATE / RATE[REF_AREA == 'EU27_2020'],
            ISO2 = countrycode(REF_AREA,"iso3c","iso2c",nomatch = NULL))

  cfc_klems =
    raw.klems %>%
    filter(nace_r2_code %in% klems_industries) %>%
    filter(year == !!year) %>%
    select(nace_r2_code,geo_code,starts_with('I_'),starts_with('K_')) %>%
    pivot_longer(-c(1:2),names_to = 'type') %>%
    separate(type,c('flow','product'),sep = "_",extra = 'merge') %>%
    filter(product != 'GFCF') %>%
    mutate(product = case_when(product == 'Rstruc' ~ 'RStruc',
                               T ~ product),

    #Some codes are unfortunately misaligned between EU KLEMS DB and depreciation rates table


           merge_code = case_when(nace_r2_code == 'C22-C23' ~ 'C22_C23',
                                  nace_r2_code == 'C24-C25' ~ 'C24_C25',
                                  nace_r2_code == 'C29-C30' ~ 'C29_C30',
                                  substr(nace_r2_code,1,1) == 'Q' ~ 'Q',
                                  nace_r2_code == 'J62-J63' ~ 'J62_J63',
                                  T ~ nace_r2_code),
           value = replace_na(value,0)) %>%

    #Add the rates & derive CFC (stocks * rates)

    left_join(depreciations.klems,by = c('merge_code' = 'Code','product' = 'product')) %>%
    group_by(nace_r2_code,geo_code,product) %>%
    summarise(CFC = sum(value,na.rm=T) * unique(rate),.groups = 'drop') %>%

    #We ultimately compute several useful ratios

    left_join(va_klems,by = c('nace_r2_code','geo_code')) %>%
    left_join(oecd_rates %>% mutate(ISO2 = case_when(ISO2 == 'GB' ~ 'UK',ISO2 == 'GR' ~ 'EL', T ~ ISO2)),by = c('geo_code' = 'ISO2')) %>%
    mutate(CFC = CFC / EXCH_RATE,
           VA_CP = VA_CP / EXCH_RATE)

  return(cfc_klems)

}

aggregate_cfc.klems = function(figaro,klems_data,year,verbose)
{

  concordance_nace.klems = get_correspondence_nace.klems(classification = 'FIGARO')

  value_added_figaro = figaro %>%
    filter(grepl('W2',rownames(.))) %>%
    colSums() %>%
    subset(!grepl('P3|P5',names(.))) %>%
    data.frame(value_added = .) %>%
    rownames_to_column('id') %>%
    separate(id,c('country','industry'),extra = 'merge',sep = "_")


  cfc_rates_klems = klems_data %>%
    group_by(geo_code,nace_r2_code) %>%

    #We prevent EU KLEMS based outlier by capping CFC to VALUE_ADDED (max(RATE) = 1)

    summarise(CFC = pmax(pmin(sum(CFC) / unique(VA_CP),1,na.rm=T),0,na.rm=T),.groups = 'drop') %>%
    left_join(concordance_nace.klems,by = 'nace_r2_code',relationship = "many-to-many") %>%

    #Embedded VA in FIGARO is used to calibrate EU KLEMS

    left_join(value_added_figaro %>% mutate(country = case_when(country == 'GB' ~ 'UK',
                                                                T ~ country)),by = c('geo_code' = 'country','industry' = 'industry')) %>%
    reframe(country = geo_code,industry,CFC = CFC * value_added)

  return(cfc_rates_klems)

}

ras_capital_use = function(K,pp_country,pi_country,min_error,n_iter,verbose)
{


  pi_country_adj = pi_country / sum(pi_country) * sum(pp_country)

  for(iter in 1:n_iter)
  {

    r = pp_country / rowSums(K) ; r[!is.finite(r)] = 1

    K = diag(x = r) %*% K

    s = pi_country_adj / colSums(K) ; s[!is.finite(s)] = 1

    K = K %*% diag(x = s)

    accuracy = max(abs(c(r,s)-1))

    if(accuracy < min_error & verbose) message('RAS Procedure : Maximum error for round ',iter," : ",accuracy)

    if(accuracy < min_error) break

  }

  return(K)

}

domestic_capital_use_matrix.klems = function(klems_data,figaro,classification,n_iter,min_error,verbose)
{
  #Import required correspondence tables and format matrices Gphat and Gihat

  Gphat = get_correspondence_product.klems(classification = classification) %>%
    as.data.frame() %>%
    arrange(row.names(.)) %>%
    as.matrix()

  Gihat = get_correspondence_nace.klems(classification = classification) %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = 'industry',values_fill = 0) %>%
    column_to_rownames('nace_r2_code') %>%
    as.matrix() %>%
    t()

  #Format pp and pi, that are used to sort out 'complex' matching situations

  pp = figaro %>% transmute(value = rowSums(across(contains('P51G'))))

  CFC_KLEMS = aggregate_cfc.klems(figaro,klems_data,year,verbose) #KLEMS-based aggregated CFC

  pi = CFC_KLEMS$CFC %>% `names<-`(paste0(CFC_KLEMS$country,"_",CFC_KLEMS$industry))

  storage_K = list()

  for(i in unique(klems_data$geo_code))
  {

    klems_domestic =
      klems_data %>%
      ungroup() %>%
      filter(geo_code == i) %>%
      select(nace_r2_code,product,CFC) %>%
      pivot_wider(names_from = nace_r2_code,values_from = CFC) %>%
      arrange(product) %>%
      column_to_rownames('product') %>%
      arrange(rownames(.)) %>%
      as.matrix()

    figaro_ref = ifelse(i == 'UK','GB',i)

    if(!figaro_ref %in% unique(substr(colnames(figaro),1,2))) next

    pp_country = pp %>% filter(substr(rownames(.),1,2) == figaro_ref & substr(rownames(.),1,5) != 'FIGW1') %>% unlist()

    pi_country = pi %>% subset(substr(names(.),1,2) == i & substr(names(.),1,5) != 'FIGW1')

    pi_country[pi_country == 0] <- 0.1

    Gp = solve(diag(x = as.numeric(Gphat %*% as.matrix(pp_country)))) %*% Gphat %*% diag(x = unlist(pp_country))

    Gi = diag(x = unlist(pi_country)) %*% Gihat %*% solve(diag(x = as.numeric(t(as.matrix(pi_country)) %*% Gihat)))

    K1 = K = t(Gp) %*% klems_domestic %*% t(Gi)

    K = ras_capital_use(K1,pp_country,pi_country,min_error,n_iter,verbose)

    storage_K[[figaro_ref]] = K

  }

  return(storage_K)


}

apply_klems_depreciation.oecd = function(country_data.oecd,depreciation_rates.oecd)
{

  #IF OECD is disseminating both Gross and Net measures, we directly compute the implied CFC

  if(all(c('NET','GROSS') %in% unique(country_data.oecd$Type)))
  {

    country_data.oecd = country_data.oecd %>%
      group_by(ACTIVITY,ASSET,level) %>%
      summarise(OBS_VALUE = unique(OBS_VALUE[Type == 'GROSS'] - OBS_VALUE[Type == 'NET']),.groups = 'drop')

  }else{

  if(all(unique(country_data.oecd$Type) == 'NET'))
  {
    #Else, we apply the KLEMS depreciation rates

    country_data.oecd = country_data.oecd %>%
      ungroup() %>%
      mutate(REF = case_when(ACTIVITY %in% depreciation_rates.oecd$industry ~ depreciation_rates.oecd$Code[match(ACTIVITY,depreciation_rates.oecd$industry)],
                             ACTIVITY %in% depreciation_rates.oecd$Code ~ depreciation_rates.oecd$Code[match(ACTIVITY,depreciation_rates.oecd$Code)],
                             ACTIVITY == '_T' ~ 'TOT',
                             ACTIVITY == 'C16T18' ~ 'C16-C18',
                             ACTIVITY == 'C24_25' ~ 'C24_C25',
                             ACTIVITY == 'C29_30' ~ 'C29_C30',
                             ACTIVITY == 'C31T33' ~ 'C31-C33',
                             ACTIVITY == 'J58T60' ~ 'J58-J60',
                             ACTIVITY %in% c('M69T71','M73_75') ~ 'M')) %>%
      mutate(RATE = depreciation_rates.oecd$rate[match(paste0(REF,ASSET),paste0(depreciation_rates.oecd$Code,depreciation_rates.oecd$ASSET))]) %>%
      mutate(GROSS = OBS_VALUE / (1-RATE),
             NET = OBS_VALUE,
             OBS_VALUE = GROSS - NET) %>%
      select(!c(RATE,REF,GROSS,NET))
  }

  }

  return(country_data.oecd)


}

calibrate_cfc.oecd = function(formatted_country_data.oecd,value_added.oecd,capital_stock.oecd,un_ratios,reference_activity_country,verbose)
{

    calibrated_cfc.oecd = data.frame(CFC = colSums(formatted_country_data.oecd)) %>%
    rownames_to_column('industry') %>%
    mutate(VALUE_ADDED = value_added.oecd$VALUE_ADDED[match(paste0(i,industry),paste0(value_added.oecd$ISO2,value_added.oecd$ACTIVITY))],
           CFC_RATE = CFC / VALUE_ADDED) %>%
    left_join(reference_activity_country %>% select(FIGARO,OECD),by = c('industry' = 'OECD')) %>%
    select(FIGARO,CFC_RATE) %>%
    mutate(FIGARO_VA = value_added_country.oecd$value_added[match(FIGARO,value_added_country.oecd$OECD)],
           FIGARO_CFC = replace_na(CFC_RATE * FIGARO_VA,0))

    #There is a consistency issue with CFC volume by deduction of OECD data. Therefore, if this was the used procedure, we adjust by calibating to UNstats CFC rate

    TYPE_OF_CFC_ASSESSMENT =
      capital_stock.oecd %>%
      ungroup() %>%
      filter(ISO2 == i) %>%
      mutate(Type = case_when(substring(INSTR_ASSET,nchar(INSTR_ASSET)) == 'N' ~ 'NET',
                              T ~ 'GROSS')) %>%
      pull(Type) %>%
      unique() %>%
      length()

    if(TYPE_OF_CFC_ASSESSMENT == 2)
    {
      UN_RATE_CFC = un_ratios %>%
        mutate(industry = sub(".*\\(([^)]+)\\).*", "\\1", `Sub Item`)) %>%
        left_join(get_correspondence_activity.un('FIGARO') %>%
                    rename(FIGARO = industry) %>%
                    pivot_longer(-c(1:2),names_to = 'Option',values_to = 'industry_un'),
                  by = c('industry' = 'industry_un'),
                  relationship = "many-to-many") %>%
        group_by(FIGARO) %>%
        mutate(min_option = min(Option)) %>%
        filter(Option == min_option) %>%
        select(industry,P51C,B1G,FIGARO) %>%
        distinct() %>%
        summarise(CFC_RATE_UN = sum(P51C) / sum(B1G),.groups = 'drop')

      calibrated_cfc.oecd =
        calibrated_cfc.oecd %>%
        left_join(UN_RATE_CFC,by = 'FIGARO') %>%
        mutate(FIGARO_CFC = FIGARO_VA * CFC_RATE_UN) %>%
        select(FIGARO,CFC_RATE = CFC_RATE_UN,FIGARO_VA,FIGARO_CFC)
    }


    return(calibrated_cfc.oecd)

}

domestic_capital_use_matrix.oecd = function(figaro,un_ratios,missing,classification,n_iter,min_error,verbose)
{

  value_added.figaro = figaro %>%
    filter(grepl('W2',rownames(.))) %>%
    colSums() %>%
    subset(!grepl('P3|P5',names(.))) %>%
    data.frame(value_added = .) %>%
    rownames_to_column('id') %>%
    separate(id,c('country','industry'),extra = 'merge',sep = "_")

  capital_stock.oecd = import_oecd_fixed_asset(year,verbose)

  activity_correspondence.oecd = get_correspondence_nace.oecd(classification)

  pp = figaro %>% transmute(value = rowSums(across(contains('P51G'))))

  # AUS  CAN  GRC  KOR  NOR  RUS
  # 176  691 1700   86  811   25

  available_countries.oecd = unique(capital_stock.oecd$ISO2) %>% subset(. %in% missing)

  depreciation_rates.oecd = get_correspondence_nace.oecd_to_klems(classification) %>%
    left_join(import_depreciation_rates_klems(),by = 'Code',relationship = "many-to-many") %>%
    left_join(get_correspondence_product.oecd_to_klems(classification),by = c('product' = 'KLEMS'),relationship = "many-to-many")

  value_added.oecd = import_oecd_value_added(year,verbose)

  storage_K = list()

  for(i in available_countries.oecd %>% subset(!. %in% c('KR','RU'))) #For Korea and Russia, there is no information on sectoral x product level
  {

    #Filter country-specific OECD data & apply depreciation rates

    country_data.oecd =
      capital_stock.oecd %>%
      ungroup() %>%
      filter(ISO2 == i) %>%
      mutate(Type = case_when(substring(INSTR_ASSET,nchar(INSTR_ASSET)) == 'N' ~ 'NET',
                              T ~ 'GROSS')) %>%
      mutate(ACTIVITY = case_when(ACTIVITY == 'O' ~ 'O84',
                                  ACTIVITY == 'P' ~ 'P85',
                                  ACTIVITY == 'D' ~ 'D35',
                                  T ~ ACTIVITY)) %>%
      filter(ACTIVITY %in% unique(unlist(activity_correspondence.oecd))) %>%
      apply_klems_depreciation.oecd(country_data.oecd = .,depreciation_rates.oecd) %>%
      select(ACTIVITY,ASSET,OBS_VALUE,level)


    #Match the OECD column 'ACTIVITY' with FIGARO classification

    reference_activity_country = activity_correspondence.oecd %>%
      mutate(OECD = case_when(Option0 %in% country_data.oecd$ACTIVITY ~ Option0,
                              Option1 %in% country_data.oecd$ACTIVITY ~ Option1,
                              Option2 %in% country_data.oecd$ACTIVITY ~ Option2,
                              Option3 %in% country_data.oecd$ACTIVITY ~ Option3))

    delete_list = c()

    non_financial_asset_tree = get_non_financial_asset_tree()

    #Avoid double counting applying crosswalk

    for(j in ncol(non_financial_asset_tree):1)
    {
      available = non_financial_asset_tree[,j] %>% subset(. %in% country_data.oecd$ASSET)

      linked = non_financial_asset_tree[,j] %>% subset(substr(.,1,nchar(non_financial_asset_tree[,j])-1) %in% substr(available,1,nchar(available)-1)) %>% subset(!. %in% available)

      to_delete = non_financial_asset_tree[non_financial_asset_tree[,j] %in% c(available,linked),(1:5)<j] %>% unlist() %>% unique()

      delete_list = rbind(delete_list,
                          country_data.oecd %>%
                            group_by(ACTIVITY) %>%
                            filter(any(ASSET %in% available)) %>%
                            filter(ASSET %in% to_delete) %>%
                            select(ACTIVITY,ASSET) %>%
                            ungroup()) %>%
        distinct()
    }

    #Filtering and rotating the temporary table

    formatted_country_data.oecd =
      country_data.oecd %>%
      filter(ACTIVITY %in% reference_activity_country$OECD & !paste0(ACTIVITY,ASSET) %in% paste0(delete_list$ACTIVITY,delete_list$ASSET)) %>%
      arrange(ACTIVITY,ASSET) %>%
      select(!level) %>%
      pivot_wider(names_from = ACTIVITY,values_from = OBS_VALUE,values_fill = 0) %>%
      column_to_rownames('ASSET')


    value_added_country.oecd = value_added.figaro %>%
      filter(country == i) %>%
      left_join(reference_activity_country %>% pivot_longer(-1,names_to = 'Option',values_to = 'OECD'),by = c('industry' = 'FIGARO'),relationship = 'many-to-many') %>%
      group_by(OECD) %>%
      filter(
        case_when(
          any(Option == "Option0") ~ Option == "Option0",
          any(Option == "Option1") ~ Option == "Option1",
          any(Option == "Option2") ~ Option == "Option2",
          any(Option == "Option3") ~ Option == "Option3"
        )
      ) %>%
      group_by(OECD) %>%
      summarise(value_added = sum(value_added,na.rm=T))


    calibrated_cfc.oecd = calibrate_cfc.oecd(formatted_country_data.oecd,value_added.oecd,capital_stock.oecd,un_ratios,reference_activity_country,verbose) %>%
      arrange(FIGARO)


    #Ignore total fixed asset (not distributed) and households-related assets (such as N111 Dwellings)

    if(any(rownames(formatted_country_data.oecd) %in% c('N11','N111')))
    {
      formatted_country_data.oecd = formatted_country_data.oecd[-which(rownames(formatted_country_data.oecd) %in% c('N11','N111')),]
    }

    #Build up the full-sized matrix applying disaggregation through pi where applicable

    initial_country_data_oecd = matrix(ncol = nrow(get_correspondence_nace.klems(classification)),nrow = nrow(formatted_country_data.oecd)) %>%
      as.data.frame() %>%
      `rownames<-`(row.names(formatted_country_data.oecd)) %>%
      `colnames<-`(get_correspondence_nace.klems(classification)$industry)

    pp_country = pp %>% filter(substr(rownames(.),1,2) == i & substr(rownames(.),1,5) != 'FIGW1') %>% unlist()

    pi_country = calibrated_cfc.oecd$FIGARO_CFC[match(colnames(initial_country_data_oecd),calibrated_cfc.oecd$FIGARO)] %>%
      `names<-`(calibrated_cfc.oecd$FIGARO[match(colnames(initial_country_data_oecd),calibrated_cfc.oecd$FIGARO)])

    pi_country[pi_country == 0] <- 0.1

    for(j in 1:ncol(initial_country_data_oecd))
    {
      pi_j = pi_country %>% subset(names(.) %in% gsub('C31_C32','C31_32',colnames(initial_country_data_oecd)[j]))

      ref = reference_activity_country %>%
        filter(FIGARO == colnames(initial_country_data_oecd)[j]) %>%
        pull(OECD)

      aggregate_ref = reference_activity_country$FIGARO[which(apply(reference_activity_country, 1, function(row) any(row == ref)))]

      pi_agg = pi_country %>% subset(names(.) %in% gsub('C31_C32','C31_32',aggregate_ref)) %>% sum()

      initial_country_data_oecd[,j] = formatted_country_data.oecd[,ref] / pi_agg * pi_j
    }

    #Pre-adjustment (optional) if CFC were obtained through OECD GROSS - NET

    initial_country_data_oecd = sweep(initial_country_data_oecd,2,calibrated_cfc.oecd$FIGARO_CFC/colSums(initial_country_data_oecd,na.rm = T),`*`)

    initial_country_data_oecd[,colSums(initial_country_data_oecd,na.rm = T) == 0] = 0

    #FIGARO-related test

    if(ncol(initial_country_data_oecd) != 64) stop('Wrong matching')

    #Set up Gphat, to correspond exactly to the country capital use matrix breakdown level

    Gphat = data.frame(ASSET = rownames(initial_country_data_oecd)) %>%
      left_join(get_correspondence_product.oecd(classification),by = 'ASSET') %>%
      column_to_rownames('ASSET') %>%
      as.matrix()

    #Gihat is straightforward (I)

    Gihat = diag(x=1,nrow = 64)

    #Applying Södersten formulas

    Gp = solve(diag(x = as.numeric(Gphat %*% as.matrix(pp_country)))) %*% Gphat %*% diag(x = unlist(pp_country))

    Gi = diag(x = unlist(pi_country)) %*% Gihat %*% solve(diag(x = as.numeric(t(as.matrix(pi_country)) %*% Gihat)))

    K1 = K = t(Gp) %*% as.matrix(initial_country_data_oecd) %*% t(Gi)

    #Lenzen xi

    pi_country_adj = pi_country / sum(pi_country) * sum(pp_country)

    #RAS procedure

    K = ras_capital_use(K1,pp_country,pi_country_adj,min_error,n_iter,verbose)

    storage_K[[i]] = K

  }

  return(storage_K)

}

missing_countries_capital_use = function(domestic_capital_use_list,figaro,un_ratios,min_error,n_iter,verbose)
{

  #Data preparation

  correspondence_nace = get_correspondence_nace.klems(classification)

  value_added.figaro = figaro %>%
    filter(grepl('W2',rownames(.))) %>%
    colSums() %>%
    subset(!grepl('P3|P5',names(.))) %>%
    data.frame(value_added = .) %>%
    rownames_to_column('id') %>%
    separate(id,c('country','industry'),extra = 'merge',sep = "_")

  pp = figaro %>% transmute(value = rowSums(across(contains('P51G'))))

  #Import PWT data to characterize GDP per capita category

  pwt = import_pwt_data(year,verbose) %>%
    filter(country %in% unique(substr(colnames(figaro),1,2))) %>%
    mutate(observed = case_when(country %in% names(domestic_capital_use_list) ~ 1,
                                T ~ 0),
           GDP_CAT = case_when(GDP_CAPITA <= quantile(GDP_CAPITA,probs = 1/3) ~ 1,
                               GDP_CAPITA > quantile(GDP_CAPITA,probs = 2/3) ~ 3,
                               T ~ 2))

  #Matrix to DF

  K_DF = c()

  for(i in names(domestic_capital_use_list))
  {

    K_DF =
      rbind(K_DF,
            domestic_capital_use_list[[i]] %>%
              `dimnames<-`(list(correspondence_nace$industry,correspondence_nace$industry)) %>%
              as.data.frame() %>%
              rownames_to_column('resource_id') %>%
              pivot_longer(-1,names_to = 'use_id') %>%
              mutate(country = i,
                     income_class = pwt$GDP_CAT[pwt$country == i]))

  }

  #Regression Poisson : value of assets = fixed effect asset class, industry, asset class x industry, asset class x income group

  #K_DF is transformed to proportional ratio

  train_poisson_regression <- fepois(
    value ~ 1 |
      resource_id +
      use_id +
      interaction(resource_id, use_id) +
      interaction(resource_id, income_class),
    data = K_DF %>%
      mutate(value_added = value_added.figaro$value_added[match(paste0(country,use_id),paste0(value_added.figaro$country,value_added.figaro$industry))]) %>%
      group_by(use_id,country) %>%
      mutate(value = pmax(value / value_added,0)) %>% ungroup())

  to_predict =
    pwt %>%
    filter(observed == 0) %>%
    select(country,income_class = GDP_CAT) %>%
    rbind(data.frame(country = 'FIGW1',income_class = 1)) %>% #ADDING FIGW1 predict
    merge(data.frame(resource_id = correspondence_nace$industry)) %>%
    merge(data.frame(use_id = correspondence_nace$industry)) %>%
    mutate(value_added = value_added.figaro$value_added[match(paste0(country,use_id),paste0(value_added.figaro$country,value_added.figaro$industry))],
           #NA occurs only when U interacts with U
           value = value_added * predict(train_poisson_regression,newdata = .) %>% replace_na(0)) %>%
    #Just ensure sum(value) <= value_added so that CFC rate is not > 1
    group_by(country,use_id) %>%
    mutate(agg = sum(value,na.rm = T),
           new_agg = pmin(agg,value_added),
           coeff = new_agg / agg) %>%
    mutate(value = case_when(value_added <= 0 ~ 0,
                             T ~ value * coeff)) %>%
    ungroup()

  pi = un_ratios %>%
    mutate(country = case_when(!iso2 %in% c(unique(to_predict$country),unique(K_DF$country)) ~ 'FIGW1',
                               T ~ iso2))  %>%
    mutate(industry = sub(".*\\(([^)]+)\\).*", "\\1", `Sub Item`)) %>%
    left_join(get_correspondence_activity.un('FIGARO') %>%
                rename(FIGARO = industry) %>%
                pivot_longer(-c(1:2),names_to = 'Option',values_to = 'industry_un'),
              by = c('industry' = 'industry_un'),
              relationship = "many-to-many") %>%
    group_by(country,FIGARO) %>%
    mutate(min_option = min(Option)) %>%
    filter(Option == min_option) %>%
    select(country,industry,P51C,B1G,FIGARO) %>%
    distinct() %>%
    summarise(CFC_RATE_UN = pmin(sum(P51C) / sum(B1G),1),.groups = 'drop')

  pi = rbind(pi,
             expand_grid(country = c(unique(to_predict$country),unique(K_DF$country)) %>% subset(!. %in% unique(pi$country)),
                         FIGARO = unique(pi$FIGARO)) %>%
               left_join(pi %>% filter(country == 'FIGW1') %>% select(!country),by = 'FIGARO')) %>%
    left_join(value_added.figaro,by = c('country','FIGARO' = 'industry')) %>%
    mutate(CFC = CFC_RATE_UN * value_added)


  for(i in unique(to_predict$country))
  {

    K = K1 = to_predict %>%
      filter(country == i) %>%
      select(use_id,resource_id,value) %>%
      pivot_wider(names_from = 'use_id') %>%
      column_to_rownames('resource_id')

    pp_country = pp %>% filter(sub("_.*$", "", rownames(.)) == i) %>% unlist()

    pi_country = pi %>% filter(country == i) %>% arrange(FIGARO) %>% pull(CFC)

    pi_country[pi_country == 0] <- 0.1

    pi_country_adj = pi_country / sum(pi_country) * sum(pp_country)

    K = ras_capital_use(as.matrix(K1),pp_country,pi_country_adj,min_error,n_iter,verbose)

    domestic_capital_use_list[[i]] = K

  }

  return(domestic_capital_use_list)


}

distribute_origin_suppliers = function(figaro,complete_domestic_capital_use,verbose)
{
  figaro_investments = figaro %>%
    select(contains('P51G')) %>%
    `colnames<-`(gsub('_P51G','',colnames(.))) %>%
    filter(!grepl("W2",rownames(.)))

  figaro_industries = sub("^[A-Z]{2}_(.*)$", "\\1", rownames(figaro_investments) %>% subset(!grepl('FIGW1|W2',.))) %>% unique()

  K_mat = matrix(0, nrow = length(figaro_industries), ncol = 0,
                 dimnames = list(figaro_industries, NULL))

  n_country = nrow(figaro_investments) / length(figaro_industries)

  share_supply_product = figaro_investments %>%
    rownames_to_column('resource_id') %>%
    pivot_longer(-1,names_to = 'country',values_to = 'value') %>%
    separate(resource_id,into = c('resource_country','resource_industry'),sep = "_",extra = 'merge',remove = F) %>%
    group_by(country,resource_industry) %>%
    mutate(share = value / sum(value,na.rm = T)) %>%
    ungroup() %>%
    select(resource_industry,resource_id,country,share)

  capital_use_pxi = do.call(cbind, complete_domestic_capital_use) %>%
    as.data.frame() %>%
    `dimnames<-`(list(figaro_industries,rownames(figaro_investments))) %>%
    rownames_to_column('resource_industry') %>%
    pivot_longer(-1,names_to = 'use_id') %>%
    separate(use_id,into = c('use_country','use_industry'),sep = "_",extra = 'merge',remove = F) %>%
    left_join(share_supply_product,by = c('use_country' = 'country','resource_industry'),relationship = "many-to-many") %>%
    reframe(resource_id,resource_industry,use_id,use_country,use_industry,value = case_when(value == 0 | share == 0 ~ 0,
                                                                                            is.na(share) | is.na(value) ~ 0,
                                                                                            T ~ value * share)) %>%
    mutate(resource_country = sub("_.*$", "", resource_id)) %>%
    arrange(use_country,use_industry,resource_country,resource_industry) %>%
    select(use_id,resource_id,value) %>%
    pivot_wider(names_from = 'use_id') %>%
    column_to_rownames('resource_id')

  return(capital_use_pxi)
}

compilation_d_fixed_sales_assumption = function(year,complete_domestic_capital_use,verbose)
{
  V = import_figaro_supply_table(year,verbose)

  U = import_figaro_use_table(year,verbose)

  q = rowSums(V,na.rm = T)

  p = colSums(V,na.rm = T)

  Tr = t(U[1:nrow(V),1:ncol(V)]) %*% diag(x = case_when(q == 0 ~ 0,T ~ 1/q))

  capital_use_ixi = Tr %*% as.matrix(multi_regional_capital_use_pxi)

  return(capital_use_ixi)
}

format_domestic_capital_tables = function(year,classification = 'FIGARO',n_iter = 100,min_error = 1e-3,verbose = T)
{
  figaro = import_figaro_io_table(year)

  klems_data = get_capital_distribution_klems(year) %>%
    mutate(geo_code = case_when(geo_code == 'EL' ~ 'GR',
                                T ~ geo_code))

  un_ratios =  import_un_ratios(year)

  domestic_capital_use_list.klems = domestic_capital_use_matrix.klems(klems_data,figaro,classification,n_iter,min_error,verbose)

  missing_countries = sub("_.*$", "", colnames(figaro)) %>% unique() %>% subset(!. %in% names(domestic_capital_use_list.klems))

  domestic_capital_use_list.oecd = domestic_capital_use_matrix.oecd(figaro,un_ratios,missing_countries,classification,n_iter,min_error,verbose)

  domestic_capital_use_list = c(domestic_capital_use_list.klems,domestic_capital_use_list.oecd)

  complete_domestic_capital_use = domestic_capital_use_list %>% missing_countries_capital_use(.,figaro,un_ratios,min_error,n_iter,verbose)

  multi_regional_capital_use_pxi = distribute_origin_suppliers(figaro,complete_domestic_capital_use)

  multi_regional_capital_use_ixi = compilation_d_fixed_sales_assumption(year,complete_domestic_capital_use,verbose)

}
