get_cfc.klems = function(year,klems_data,classification,verbose = T)
{

  raw.klems = klems_data

  va_klems = get_cache_file("https://www.dropbox.com/s/nkz7mdp0onken1j/national%20accounts.csv?dl=1",pattern = 'va_klems') %>%
    read.csv() %>%
    filter(year == !!year & nace_r2_code %in% unique(raw.klems$nace_r2_code)) %>%
    select(nace_r2_code,geo_code,VA_CP)

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
    mutate(product = case_when(product == 'Rstruc' ~ 'RStruc',
                               T ~ product),

    #Some codes are unfortunately misaligned between EU KLEMS DB and depreciation rates table


           merge_code = case_when(nace_r2_code == 'C22-C23' ~ 'C22_C23',
                                  nace_r2_code == 'C24-C25' ~ 'C24_C25',
                                  nace_r2_code == 'C29-C30' ~ 'C29_C30',
                                  nace_r2_code == 'M-N' ~ 'M_N',
                                  substr(nace_r2_code,1,1) == 'Q' ~ 'Q',
                                  nace_r2_code == 'J62-J63' ~ 'J62_J63',
                                  T ~ nace_r2_code),
           value = replace_na(value,0)) %>%

    #Add the rates & derive CFC (stocks * rates)

    left_join(depreciations.klems,by = c('merge_code' = 'Code','product' = 'product'),relationship = "many-to-many") %>%
    group_by(nace_r2_code,geo_code,product) %>%
    summarise(CFC = sum(value,na.rm=T) * unique(rate),.groups = 'drop') %>%

    #We ultimately compute several useful ratios

    left_join(va_klems,by = c('nace_r2_code','geo_code')) %>%
    left_join(oecd_rates %>% mutate(ISO2 = case_when(ISO2 == 'GB' ~ 'UK',ISO2 == 'GR' ~ 'EL', T ~ ISO2)),by = c('geo_code' = 'ISO2')) %>%
    mutate(CFC = CFC,
           VA_CP = VA_CP / EXCH_RATE)

  return(cfc_klems)

}

get_capital_stock.klems = function(year,classification,verbose = T)
{

  raw.klems = get_cache_file("https://www.dropbox.com/s/sp2p4m86et66nfg/capital%20accounts.csv?dl=1", pattern = 'raw.klems') %>%
    read.csv()

  oecd_rates = get_cache_file(link = paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A....EXC_A.......?startPeriod=',
                                            year,'&endPeriod=',year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels'),
                              pattern = paste0('OECD_EXCHANGE_RATES_',year)) %>%
    read.csv() %>%
    select(REF_AREA,RATE = OBS_VALUE,TIME_PERIOD) %>%
    reframe(EXCH_RATE = RATE / RATE[REF_AREA == 'EU27_2020'],
            ISO2 = countrycode(REF_AREA,"iso3c","iso2c",nomatch = NULL))

  crosswalk_klems = get_crosswalk_nace.klems(classification)

  capital_stock_klems =
    raw.klems %>%
    filter(year == !!year) %>%
    select(nace_r2_code,geo_code,starts_with('I_'),starts_with('K_')) %>%
    pivot_longer(-c(1:2),names_to = 'type') %>%
    separate(type,c('flow','product'),sep = "_",extra = 'merge') %>%
    filter(product != 'GFCF' & flow == 'K')

  filtered_capital_stock_klems = c()

  for(country in unique(capital_stock_klems$geo_code))
  {

    if(grepl('EU',country)) next

    country_crosswalk_klems = crosswalk_klems

    country_capital_stock_klems = capital_stock_klems %>%
      filter(geo_code == country)

    for(code in unique(unlist(crosswalk_klems))) country_crosswalk_klems[crosswalk_klems == code & 1:length(unlist(crosswalk_klems)) > nrow(crosswalk_klems)] =
      sum(country_capital_stock_klems$value[country_capital_stock_klems$nace_r2_code == code],na.rm=T)

    country_sectoral_combination_klems = country_crosswalk_klems %>%
      mutate(First_choice_value = case_when(Option1 > 0 ~ Option1,
                                            Option2 > 0 ~ Option2,
                                            Option3 > 0 ~ Option3,
                                            Option4 > 0 ~ Option4,
                                            Option5 > 0 ~ Option5,
                                            Option6 > 0 ~ Option6,
                                            Option7 > 0 ~ Option7,
                                            Option8 > 0 ~ Option8),
             First_choice_ref = case_when(Option1 > 0 ~ crosswalk_klems$Option1,
                                          Option2 > 0 ~ crosswalk_klems$Option2,
                                          Option3 > 0 ~ crosswalk_klems$Option3,
                                          Option4 > 0 ~ crosswalk_klems$Option4,
                                          Option5 > 0 ~ crosswalk_klems$Option5,
                                          Option6 > 0 ~ crosswalk_klems$Option6,
                                          Option7 > 0 ~ crosswalk_klems$Option7,
                                          Option8 > 0 ~ crosswalk_klems$Option8)) %>%
      group_by(First_choice_ref) %>%
      mutate(length_ref = length(First_choice_ref)) %>%
      rowwise() %>%
      mutate(theoretical_ref = sum(length(unique(which(crosswalk_klems == First_choice_ref, arr.ind = TRUE)[,1])))) %>%
      mutate(Final_choice = if(length_ref == theoretical_ref) First_choice_ref else{
        crosswalk_klems[crosswalk_klems$industry == industry,min(which(!is.na(crosswalk_klems[crosswalk_klems$industry == industry,]) &
                                                                         1:ncol(crosswalk_klems) < which(crosswalk_klems[crosswalk_klems$industry == industry,] == First_choice_ref)))]}) %>%
      ungroup() %>%
      select(industry = Final_choice) %>%
      distinct() %>%
      pull()

    filtered_country_capital_stock_klems = country_capital_stock_klems %>%
      filter(nace_r2_code %in% country_sectoral_combination_klems)

    filtered_capital_stock_klems = rbind(filtered_capital_stock_klems,filtered_country_capital_stock_klems)
  }

  filtered_capital_stock_klems =
    filtered_capital_stock_klems %>%
    left_join(oecd_rates %>% mutate(ISO2 = case_when(ISO2 == 'GB' ~ 'UK',ISO2 == 'GR' ~ 'EL', T ~ ISO2)),by = c('geo_code' = 'ISO2')) %>%
    mutate(value = value  / EXCH_RATE)

  return(filtered_capital_stock_klems)


}

get_figaro_margins = function(year,figaro,klems_data,un_ratios,classification,verbose)
{
  figaro_investments = figaro %>%
    select(contains('P51G')) %>%
    `colnames<-`(gsub('_P51G','',colnames(.))) %>%
    filter(!grepl("W2",rownames(.))) %>%
    rowSums(na.rm=T)

  figaro_valueadded = figaro %>%
    filter(grepl('W2',rownames(.))) %>%
    select(-contains('P3'),-contains('P5')) %>%
    colSums(na.rm=T)

  figaro_production = figaro %>%
    select(-contains('P3'),-contains('P5')) %>%
    colSums(na.rm=T)

  oecd_cfc =
    read.csv('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.0/A....P51C+B1G.....V..?startPeriod=2021&endPeriod=2021&dimensionAtObservation=AllDimensions&format=csvfilewithlabels') %>%
    select(ACTIVITY,REF_AREA,TRANSACTION,OBS_VALUE) %>%
    mutate(ISO2 = countrycode(REF_AREA,"iso3c","iso2c",nomatch = NULL)) %>%
    group_by(country = ISO2,code = ACTIVITY) %>%
    summarise(ratio = OBS_VALUE[TRANSACTION == 'P51C'] / OBS_VALUE[TRANSACTION == 'B1G'],.groups = 'drop')

  klems_cfc = get_cfc.klems(year,klems_data,classification) %>%
    group_by(country = geo_code,code = nace_r2_code) %>%
    summarise(ratio = pmax(pmin(sum(CFC) / unique(VA_CP),1,na.rm=T),0,na.rm=T),.groups = 'drop')

  un_cfc = un_ratios %>%
    ungroup() %>%
    mutate(code = case_when(`Sub Item` == 'Total Economy' ~ "_T",
                            T ~ sub(".*\\((.*)\\).*", "\\1", `Sub Item`))) %>%
    select(code,country = iso2,ratio = P51C_B1G)


  all_cfc =
    rbind(oecd_cfc %>% mutate(source = 'OECD'),
          klems_cfc %>% mutate(source = 'KLEMS'),
          un_cfc %>% mutate(source = 'UN'))

  classification_cfc =
    data.frame(industry =
                 c("A01","A02","A03","B","C10T12","C13T15","C16","C17","C18","C19","C20","C21","C22",
                   "C23","C24","C25","C26","C27","C28","C29","C30","C31_32","C33","D35","E36","E37T39",
                   "F","G45","G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60","J61","J62_63",
                   "K64","K65","K66","L","M69_70","M71","M72","M73","M74_75","N77","N78","N79","N80T82",
                   "O84","P85","Q86","Q87_88","R90T92","R93","S94","S95","S96","T","U")) %>%
    mutate(Option1 = industry,
           Option2 = case_when(industry %in% c("A01","A02","A03") ~ substring(industry,2)),
           Option3 = case_when(industry %in% c("C16","C17","C18") ~ 'C16T18',
                               industry %in% c("C22","C23") ~ 'C22_23',
                               industry %in% c("C24","C25") ~ 'C24_25',
                               industry %in% c("C29","C30") ~ 'C29_30',
                               industry %in% c("C31_32","C33") ~ 'C31T33',
                               industry %in% c("J58","J59_60") ~ 'J58T60',
                               industry %in% c("M69_70","M71") ~ 'M69T71',
                               industry %in% c("M73","M74_75") ~ 'M73T75'),
           Option4 = substr(industry,1,1),
           Option5 = case_when(substr(industry,1,1) %in% c('B','C','D','E') ~ "B+C+D+E",
                               substr(industry,1,1) %in% c('R','S','T') ~ "R+S+T",
                               substr(industry,1,1) %in% c('M','N') ~ "M+N",
                               substr(industry,1,1) %in% c('O','P','Q') ~ "O+P+Q"),
           Option6 = "_T")

  figaro_cfc_from_sources = data.frame(str_split_fixed(names(figaro_valueadded), "_", 2)) %>%
    `colnames<-`(c('country','industry')) %>%
    left_join(classification_cfc,by = 'industry') %>%
    mutate(share = case_when(paste0(country,Option1) %in% paste0(all_cfc$country,all_cfc$code) ~ all_cfc$ratio[match(paste0(country,Option1),paste0(all_cfc$country,all_cfc$code))],
                             paste0(country,Option2) %in% paste0(all_cfc$country,all_cfc$code) ~ all_cfc$ratio[match(paste0(country,Option2),paste0(all_cfc$country,all_cfc$code))],
                             paste0(country,Option3) %in% paste0(all_cfc$country,all_cfc$code) ~ all_cfc$ratio[match(paste0(country,Option3),paste0(all_cfc$country,all_cfc$code))],
                             paste0(country,Option4) %in% paste0(all_cfc$country,all_cfc$code) ~ all_cfc$ratio[match(paste0(country,Option4),paste0(all_cfc$country,all_cfc$code))],
                             paste0(country,Option5) %in% paste0(all_cfc$country,all_cfc$code) ~ all_cfc$ratio[match(paste0(country,Option5),paste0(all_cfc$country,all_cfc$code))])) %>% #Option 6 not included and part of the missing value strategy
    mutate(value_added_rate = case_when(figaro_valueadded == 0 ~ 0,
                                        T ~ figaro_valueadded / figaro_production))


  pwt_data = import_pwt_data(year,verbose) %>%
    filter(country %in% unique(figaro_cfc_from_sources$country)) %>%
    mutate(GDP_CAT = case_when(GDP_CAPITA <= quantile(GDP_CAPITA,probs = 1/3) ~ 1,
                               GDP_CAPITA > quantile(GDP_CAPITA,probs = 2/3) ~ 3,
                               T ~ 2))

  missing_cfc = figaro_cfc_from_sources %>%
    filter(is.na(share)) %>%
    select(country,industry,value_added_rate) %>%
    mutate(income_class = pwt_data$GDP_CAT[match(country,pwt_data$country)])

  train_set = figaro_cfc_from_sources %>%
    filter(!is.na(share)) %>%
    select(country,industry,value_added_rate,share) %>%
    mutate(income_class = pwt_data$GDP_CAT[match(country,pwt_data$country)])

  train_ls = feols(share ~ value_added_rate:industry + income_class:industry | industry, data = train_set)

  figaro_cfc = rbind(train_set %>% select(country,industry,share),
                     missing_cfc %>% select(country,industry) %>% mutate(share = predict(train_ls,newdata = missing_cfc))) %>%
    mutate(share = pmax(share,0),
           value_added = figaro_valueadded[match(paste0(country,"_",industry),names(figaro_valueadded))]) %>%
    reframe(country,industry,CFC = pmax(0,share * value_added)) %>%
    arrange(country,industry)

  margins = list(rowwise = figaro_investments,
                 colwise = figaro_cfc$CFC)

  return(margins)


}

domestic_capital_stock_matrix.klems = function(klems_data,figaro,classification,figaro_margins,verbose)
{
  #Import required correspondence tables and format matrices Gphat and Gihat

  Gihat = get_correspondence_nace.klems(classification = classification) %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = 'industry',values_fill = 0) %>%
    column_to_rownames('nace_r2_code') %>%
    as.matrix() %>%
    t()

  pi = figaro_margins$colwise

  crosswalk_klems = get_crosswalk_nace.klems(classification)

  storage_K = list()

  for(i in unique(klems_data$geo_code))
  {

    figaro_ref = ifelse(i == 'UK','GB',i)

    if(!figaro_ref %in% unique(substr(colnames(figaro),1,2))) next

    klems_domestic =
      klems_data %>%
      ungroup() %>%
      filter(geo_code == i) %>%
      select(nace_r2_code,product,value) %>%
      mutate(value = case_when(is.na(value) ~ 0,
                               T ~ value)) %>%
      pivot_wider(names_from = nace_r2_code) %>%
      arrange(product) %>%
      column_to_rownames('product') %>%
      arrange(rownames(.)) %>%
      as.matrix()

    Gihat_domestic = crosswalk_klems %>%
      reframe(industry,
              country_reference = case_when(Option1 %in% colnames(klems_domestic) ~ Option1,
                                            Option2 %in% colnames(klems_domestic) ~ Option2,
                                            Option3 %in% colnames(klems_domestic) ~ Option3,
                                            Option4 %in% colnames(klems_domestic) ~ Option4,
                                            Option5 %in% colnames(klems_domestic) ~ Option5,
                                            Option6 %in% colnames(klems_domestic) ~ Option6,
                                            Option7 %in% colnames(klems_domestic) ~ Option7,
                                            Option8 %in% colnames(klems_domestic) ~ Option8)) %>%
      left_join(Gihat %>%
                  as.data.frame() %>%
                  rownames_to_column('FIGARO') %>%
                  pivot_longer(-1,names_to = 'KLEMS') %>%
                  filter(value == 1),
                by = c('industry' = 'KLEMS')) %>%
      select(country_reference,FIGARO,value) %>%
      pivot_wider(names_from = country_reference,values_fill = 0) %>%
      column_to_rownames('FIGARO') %>%
      as.matrix()

    pi_country = pi %>%
      subset(substr(rownames(figaro),1,2) == figaro_ref & substr(rownames(figaro),1,5) != 'FIGW1')

    pi_country[pi_country == 0] <- 0.1

    Gi = diag(x = unlist(pi_country)) %*% Gihat_domestic %*% solve(diag(x = as.numeric(t(as.matrix(pi_country)) %*% Gihat_domestic)))

    K = klems_domestic %*% t(Gi) %>%
      `colnames<-`(rownames(Gihat_domestic))

    storage_K[[figaro_ref]] = K

  }

  return(storage_K)

}

domestic_capital_stock_matrix.oecd = function(year,figaro,un_ratios,missing,figaro_margins,verbose)
{

  cfc.figaro =  figaro_margins$colwise %>%
    `names<-`(rownames(figaro %>% filter(!grepl('W2',rownames(.))))) %>%
    as.data.frame() %>%
    `colnames<-`('value') %>%
    rownames_to_column('id') %>%
    separate(id,c('country','industry'),sep = "_",extra = 'merge')

  oecd_rates = get_cache_file(link = paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A....EXC_A.......?startPeriod=',
                                            year,'&endPeriod=',year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels'),
                              pattern = paste0('OECD_EXCHANGE_RATES_',year)) %>%
    read.csv() %>%
    select(REF_AREA,RATE = OBS_VALUE,TIME_PERIOD) %>%
    reframe(EXCH_RATE = RATE / RATE[REF_AREA == 'EU27_2020'],
            ISO2 = countrycode(REF_AREA,"iso3c","iso2c",nomatch = NULL))

  capital_stock.oecd = import_oecd_fixed_asset(year,verbose) %>%
    left_join(oecd_rates,by = 'ISO2') %>%
    mutate(OBS_VALUE = OBS_VALUE / EXCH_RATE)

  activity_correspondence.oecd = get_correspondence_nace.oecd(classification = 'FIGARO')

  # AUS  CAN  GRC  KOR  NOR  RUS
  # 176  691 1700   86  811   25

  available_countries.oecd = unique(capital_stock.oecd$ISO2) %>% subset(. %in% missing)

  depreciation_rates.oecd = get_correspondence_nace.oecd_to_klems(classification = 'FIGARO') %>%
    left_join(import_depreciation_rates_klems(),by = 'Code',relationship = "many-to-many") %>%
    mutate(match_product = tolower(product)) %>%
    left_join(get_correspondence_product.oecd_to_klems(classification = 'FIGARO') %>%
                mutate(KLEMS = tolower(KLEMS)),by = c('match_product' = 'KLEMS'),relationship = "many-to-many")

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
      filter(ACTIVITY %in% unique(unlist(activity_correspondence.oecd)))

    if(all(unique(country_data.oecd$Type) %in% 'NET'))
    {
      country_data.oecd =
        country_data.oecd %>%
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
        mutate(NET = OBS_VALUE,
               GROSS = NET / (1-RATE)) %>%
        select(ASSET,ACTIVITY,OBS_VALUE = GROSS)
    }else{
      country_data.oecd = country_data.oecd %>%
        filter(Type == 'GROSS') %>%
        select(ASSET,ACTIVITY,OBS_VALUE)
    }


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
      pivot_wider(names_from = ACTIVITY,values_from = OBS_VALUE,values_fill = 0) %>%
      column_to_rownames('ASSET')

    cfc_country.oecd = cfc.figaro %>%
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
      summarise(CFC = sum(value,na.rm=T))

    #Ignore total fixed asset (not distributed) and households-related assets (such as N111 Dwellings)

    if(any(rownames(formatted_country_data.oecd) %in% c('N11','N111')))
    {
      formatted_country_data.oecd = formatted_country_data.oecd[-which(rownames(formatted_country_data.oecd) %in% c('N11','N111')),]
    }

    #Build up the full-sized matrix applying disaggregation through pi where applicable

    initial_country_data_oecd = matrix(ncol = nrow(get_correspondence_nace.klems(classification = 'FIGARO')),nrow = nrow(formatted_country_data.oecd)) %>%
      as.data.frame() %>%
      `rownames<-`(row.names(formatted_country_data.oecd)) %>%
      `colnames<-`(get_correspondence_nace.klems(classification = 'FIGARO')$industry)

    # pp_country = pp %>% filter(substr(rownames(.),1,2) == i & substr(rownames(.),1,5) != 'FIGW1') %>% unlist()

    pi_country = cfc_country.oecd$CFC[match(colnames(initial_country_data_oecd),cfc_country.oecd$OECD)] %>%
      `names<-`(cfc_country.oecd$OECD[match(colnames(initial_country_data_oecd),cfc_country.oecd$OECD)])

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

    initial_country_data_oecd[is.na(initial_country_data_oecd)] = 0

    #Convert standard non-financial classification to KLEMS classification system.

    K = initial_country_data_oecd %>%
      rownames_to_column('ASSET') %>%
      left_join(get_correspondence_product.oecd_to_klems(),by = 'ASSET') %>%
      group_by(KLEMS) %>%
      summarise(across(where(is.numeric),  \(x) sum(x,na.rm = TRUE))) %>%
      column_to_rownames('KLEMS')

    missing_rows = row.names = c("CT","Cult","IT","OCon","OIPP","OMach","RD","Rstruc","Soft_DB","TraEq") %>% subset(!tolower(.) %in% tolower(rownames(K)))

    K = rbind(K,
              data.frame(matrix(
                rep(0, ncol(K) * length(missing_rows)),
                nrow = length(missing_rows),
                ncol = ncol(K),
                byrow = TRUE,
                dimnames = list(missing_rows, colnames(K))))) %>%
      arrange(tolower(rownames(.)))

    storage_K[[i]] = K

  }

  return(storage_K)

}

missing_countries_capital_stock = function(year,domestic_capital_stock_list,figaro,un_ratios,option = 'Poisson',verbose)
{

  #Data preparation

  correspondence_nace = get_correspondence_nace.klems(classification = 'FIGARO')

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
    mutate(observed = case_when(country %in% names(domestic_capital_stock_list) ~ 1,
                                T ~ 0),
           GDP_CAT = case_when(GDP_CAPITA <= quantile(GDP_CAPITA,probs = 1/3) ~ 1,
                               GDP_CAPITA > quantile(GDP_CAPITA,probs = 2/3) ~ 3,
                               T ~ 2))

  #Matrix to DF

  K_DF = c()

  for(i in names(domestic_capital_stock_list))
  {

    K_DF =
      rbind(K_DF,
            domestic_capital_stock_list[[i]] %>%
              as.data.frame() %>%
              rownames_to_column('resource_id') %>%
              pivot_longer(-1,names_to = 'use_id') %>%
              mutate(country = i,
                     income_class = pwt$GDP_CAT[pwt$country == i]))

  }

  if(tolower(option) == 'average')
  {

  average_capital_stock = K_DF %>%
    group_by(resource_id,use_id) %>%
    mutate(value_added = value_added.figaro$value_added[match(paste0(country,use_id),paste0(value_added.figaro$country,value_added.figaro$industry))]) %>%
    summarise(value = sum(value),
              value_added = sum(value_added),
              .groups = 'drop') %>%
    group_by(use_id) %>%
    mutate(share = value / value_added) %>%
    ungroup()

  to_predict = pwt %>%
    filter(observed == 0) %>%
    select(country,income_class = GDP_CAT) %>%
    rbind(data.frame(country = 'FIGW1',income_class = 1)) %>% #ADDING FIGW1 predict
    merge(data.frame(resource_id = unique(K_DF$resource_id))) %>%
    merge(data.frame(use_id = correspondence_nace$industry)) %>%
    mutate(value_added = value_added.figaro$value_added[match(paste0(country,use_id),paste0(value_added.figaro$country,value_added.figaro$industry))],
           share = average_capital_stock$share[match(paste0(use_id,resource_id),paste0(average_capital_stock$use_id,average_capital_stock$resource_id))],
           value = case_when(share == 0 | value_added == 0 | value_added < 0 ~ 0,
                             T ~ share * value_added))

  }

  if(option == 'Poisson')
  {
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
    merge(data.frame(resource_id = unique(K_DF$resource_id))) %>%
    merge(data.frame(use_id = correspondence_nace$industry)) %>%
    mutate(value_added = value_added.figaro$value_added[match(paste0(country,use_id),paste0(value_added.figaro$country,value_added.figaro$industry))],
           #NA occurs only when U interacts with U
           value = value_added * predict(train_poisson_regression,newdata = .) %>% replace_na(0))

  }


  for(i in unique(to_predict$country))
  {

    K = to_predict %>%
      filter(country == i) %>%
      select(use_id,resource_id,value) %>%
      pivot_wider(names_from = 'use_id') %>%
      column_to_rownames('resource_id') %>%
      arrange(rownames(.))

    domestic_capital_stock_list[[i]] = K

  }

  return(domestic_capital_stock_list)


}

domestic_capital_tables = function(figaro,complete_domestic_capital_stock,verbose)
{

  Gphat = get_correspondence_product.klems(classification = 'FIGARO') %>%
    as.data.frame() %>%
    arrange(row.names(.)) %>%
    as.matrix()

  pp = figaro %>% transmute(value = rowSums(across(contains('P51G'))))

  depreciations.klems = import_depreciation_rates_klems() %>%
    mutate(product = case_when(product == 'RStruc' ~ 'Rstruc',
                               T ~ product)) %>%
    left_join(get_correspondence_nace.klems('FIGARO') %>%
                mutate(nace_r2_code = case_when(nace_r2_code == 'C22-C23' ~ 'C22_C23',
                                                nace_r2_code == 'C24-C25' ~ 'C24_C25',
                                                nace_r2_code == 'C29-C30' ~ 'C29_C30',
                                                substr(nace_r2_code,1,1) == 'Q' ~ 'Q',
                                                nace_r2_code == 'J62-J63' ~ 'J62_J63',
                                                T ~ nace_r2_code)),
              by = c('Code' = 'nace_r2_code'),relationship = "many-to-many") %>%
    select(product,industry,value = rate) %>%
    filter(!is.na(industry)) %>%
    pivot_wider(names_from = 'industry') %>%
    filter(product %in% c("CT","Cult","IT","OCon","OIPP","OMach","RD","Rstruc","Soft_DB","TraEq")) %>%
    arrange(product) %>%
    column_to_rownames('product') %>%
    as.matrix()

  capital_stock_list = depreciations_list = cfc_list = list()

  for(i in names(complete_domestic_capital_stock))
  {
    pp_country = pp %>% filter(sub("_.*", "", rownames(.)) == i) %>% unlist()

    Gphatxpp = as.numeric(Gphat %*% as.matrix(pp_country)) ; Gphatxpp[Gphatxpp == 0] = 1e4

    Gp = solve(diag(x = Gphatxpp)) %*% Gphat %*% diag(x = unlist(pp_country))

    distributed_domestic_capital_stock = t(Gp) %*% as.matrix(complete_domestic_capital_stock[[i]])

    distributed_cfc = (as.matrix(complete_domestic_capital_stock[[i]]) * depreciations.klems)

    distributed_domestic_depreciations_rate = t(Gp) %*% distributed_cfc / distributed_domestic_capital_stock

    distributed_domestic_depreciations_rate[distributed_domestic_capital_stock == 0] = 0

    capital_stock_list[[i]] = distributed_domestic_capital_stock

    # depreciations_list[[i]] = distributed_domestic_depreciations_rate %>% as.matrix()

    cfc_list[[i]] = t(Gp) %*% distributed_cfc

  }

  return(list(Stock = capital_stock_list,
              CFC = cfc_list,
              depreciations_list = depreciations_list))

}

distribute_origin_suppliers = function(figaro,domestic_capital_tables_pxi,verbose)
{


  CFC_PXI = domestic_capital_tables_pxi$CFC

  STOCK_PXI = domestic_capital_tables_pxi$Stock

  figaro_investments = figaro %>%
    select(contains('P51G')) %>%
    `colnames<-`(gsub('_P51G','',colnames(.))) %>%
    filter(!grepl("W2",rownames(.)))

  share_supply_product = figaro_investments %>%
    rownames_to_column('resource_id') %>%
    pivot_longer(-1,names_to = 'country',values_to = 'value') %>%
    separate(resource_id,into = c('resource_country','resource_industry'),sep = "_",extra = 'merge',remove = F) %>%
    group_by(country,resource_industry) %>%
    mutate(share = value / sum(value,na.rm = T)) %>%
    ungroup() %>%
    select(resource_industry,resource_id,country,share)

  STOCK_PXI = CFC_PXI = matrix(0, nrow = nrow(figaro_investments), ncol = 0,
                               dimnames = list(rownames(figaro_investments), NULL)) %>%
    as.data.frame()

  for(i in sort(names(domestic_capital_tables_pxi$Stock)))
  {
    origin_supply_proxy = share_supply_product %>%
      filter(country == i) %>%
      select(!country)

    STOCK_PXI = cbind(STOCK_PXI,domestic_capital_tables_pxi$Stock[[i]] %>%
                        as.data.frame() %>%
                        mutate(resource_industry = unique(sort(origin_supply_proxy$resource_industry))) %>%
                        left_join(origin_supply_proxy,by = 'resource_industry') %>%
                        mutate(across(where(is.numeric) & !share, ~ .x * share)) %>%
                        arrange(sub("_.*", "", resource_id),sub("^[^_]+_", "", resource_id)) %>%
                        column_to_rownames('resource_id') %>%
                        select(!c(resource_industry,share)) %>%
                        `colnames<-`(paste0(i,"_",colnames(.))))

    CFC_PXI = cbind(CFC_PXI,
                    domestic_capital_tables_pxi$CFC[[i]] %>%
                      as.data.frame() %>%
                      mutate(resource_industry = unique(sort(origin_supply_proxy$resource_industry))) %>%
                      left_join(origin_supply_proxy,by = 'resource_industry') %>%
                      mutate(across(where(is.numeric) & !share, ~ .x * share)) %>%
                      arrange(sub("_.*", "", resource_id),sub("^[^_]+_", "", resource_id)) %>%
                      column_to_rownames('resource_id') %>%
                      select(!c(resource_industry,share)) %>%
                      `colnames<-`(paste0(i,"_",colnames(.))))
  }

  STOCK_PXI[is.na(STOCK_PXI)] = 0
  CFC_PXI[is.na(CFC_PXI)] = 0

  return(list(Stock = STOCK_PXI,
              CFC = CFC_PXI))
}

compilation_d_fixed_sales_assumption = function(year,multi_regional_capital_use_pxi,figaro_margins,n_iter,min_error,option,verbose)
{
  V_t = import_figaro_supply_table(year,verbose)

  U = import_figaro_use_table(year,verbose)

  q = rowSums(V_t,na.rm = T)

  p = colSums(V_t,na.rm = T)

  Tr = t(V_t) %*% diag(x = case_when(q == 0 ~ 0,T ~ 1/q))

  STOCK_IXI = Tr %*% as.matrix(multi_regional_capital_use_pxi$Stock)

  CFC_IXI = K = Tr %*% as.matrix(multi_regional_capital_use_pxi$CFC)

  if(option == 'CFC') figaro_margins$rowwise =  figaro_margins$rowwise / sum(figaro_margins$rowwise,na.rm = T) * sum(figaro_margins$colwise)

  if(option == 'GFCF') figaro_margins$colwise =  figaro_margins$colwise / sum(figaro_margins$colwise,na.rm = T) * sum(figaro_margins$rowwise)

  for(iter in 1:n_iter)
  {

    r = figaro_margins$rowwise / rowSums(K) ; r[!is.finite(r)] = 1

    # K = diag(x = r) %*% K
    K = sweep(K, 1, r, `*`)

    s = figaro_margins$colwise / colSums(K) ; s[!is.finite(s)] = 1

    # K = K %*% diag(x = s)
    K = sweep(K, 2, s, `*`)

    accuracy = max(abs(c(r,s)-1))

    if(accuracy < min_error & verbose) message('RAS Procedure : Maximum error for round ',iter," : ",accuracy)

    if(accuracy < min_error) break

  }

  STOCK_IXI_SCALED = K / (CFC_IXI / STOCK_IXI)

  return(return(list(CAPITAL_USE = K,
                     CAPITAL_STOCK = STOCK_IXI_SCALED)))
}

format_domestic_capital_tables = function(year,classification = 'FIGARO',n_iter = 100,min_error = 1e-3,missing_option = 'Poisson',ras_option = 'CFC',verbose = T)
{
  figaro = import_figaro_io_table(year)

  klems_data = get_capital_stock.klems(year) %>%
    mutate(geo_code = case_when(geo_code == 'EL' ~ 'GR',
                                T ~ geo_code))

  un_ratios =  import_un_ratios(year)

  figaro_margins = get_figaro_margins(year,figaro,klems_data,un_ratios,classification,verbose)

  domestic_capital_stock_list.klems = domestic_capital_stock_matrix.klems(klems_data,figaro,classification,figaro_margins,verbose)

  missing_countries = sub("_.*$", "", colnames(figaro)) %>% unique() %>% subset(!. %in% names(domestic_capital_stock_list.klems))

  domestic_capital_stock_list.oecd = domestic_capital_stock_matrix.oecd(year,figaro,un_ratios,missing_countries,figaro_margins,verbose)

  domestic_capital_stock_list = c(domestic_capital_stock_list.klems,domestic_capital_stock_list.oecd)

  complete_domestic_capital_stock = domestic_capital_stock_list %>% missing_countries_capital_stock(year,.,figaro,un_ratios,option = missing_option,verbose)

  domestic_capital_tables_pxi = domestic_capital_tables(figaro,complete_domestic_capital_stock,verbose)

  multi_regional_capital_use_pxi = distribute_origin_suppliers(figaro,domestic_capital_tables_pxi,verbose)

  multi_regional_capital_use_ixi = compilation_d_fixed_sales_assumption(year,multi_regional_capital_use_pxi,figaro_margins,n_iter = n_iter,min_error = min_error-3,option = ras_option,verbose) #RAS CFC GFCF

  return(multi_regional_capital_use_ixi)

}

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

# aggregate_cfc.klems(figaro,klems_data,year,verbose)

#Derive Vk capital supply matrix. Supply of capital by capital industries
#Prior = V supply matrix
#Uc = Stock use matrix that describes the use of existing capital by current industries
#Uk = Investment use matrix which describes how newly invested capital is distributed across purchasing industries
#RAS : rowsum = f' -> column sum of Uc ; colsum = k -> rowsum of Uk
