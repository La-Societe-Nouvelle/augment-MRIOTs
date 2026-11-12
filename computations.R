get_cfc.klems = function(year,klems_data,verbose = T)
{

  raw.klems = klems_data

  va_klems = get_cache_file("https://www.dropbox.com/s/nkz7mdp0onken1j/national%20accounts.csv?dl=1",pattern = 'va_klems') %>%
    read.csv() %>%
    filter(year == !!year & nace_r2_code %in% unique(raw.klems$nace_r2_code)) %>%
    select(nace_r2_code,geo_code,VA_CP)

  #In EU KLEMS, all countries have the same level of granularity

  depreciations.klems = import_depreciation_rates_klems()

  #In EU KLEMS, Value added, investment, and capital stock are valued in M of national currency -> We apply OECD exchange rates

  oecd_rates = import_oecd_exchange_rates() %>% filter(year == !!year) %>% select(ISO2,EXCH_RATE)

  cfc_klems =
    raw.klems %>%
    mutate(product = case_when(product == 'Rstruc' ~ 'RStruc',
                               T ~ product),

    #Some codes are unfortunately misaligned between EU KLEMS DB and depreciation rates table


           merge_code = case_when(nace_r2_code == 'C22-C23' ~ 'C22_C23',
                                  nace_r2_code == 'C24-C25' ~ 'C24_C25',
                                  nace_r2_code == 'C29-C30' ~ 'C29_C30',
                                  nace_r2_code == 'M-N' ~ 'M_N',
                                  nace_r2_code == 'R-S' ~ 'R_S',
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
    left_join(oecd_rates %>% mutate(ISO2 = case_when(ISO2 == 'GB' ~ 'UK', T ~ ISO2)),by = c('geo_code' = 'ISO2')) %>%
    mutate(EXCH_RATE = case_when(geo_code == 'BG' ~ ecb_bgn_rate(year), #EU KLEMS reports Bulgaria in leva
                                 T ~ EXCH_RATE),
           VA_CP = VA_CP / EXCH_RATE)

  return(cfc_klems)

}

get_capital_stock.klems = function(year)
{

  raw.klems = import_klems_capital_accounts()

  oecd_rates = import_oecd_exchange_rates() %>% filter(year == !!year) %>% select(ISO2,EXCH_RATE)

  crosswalk_klems = get_crosswalk_nace.klems()

  crosswalk_long = crosswalk_klems %>%
    pivot_longer(starts_with('Option'),names_to = 'level',names_prefix = 'Option',names_transform = as.integer,values_to = 'code',values_drop_na = TRUE)

  aggregate_members = crosswalk_long %>%
    filter(level %in% 2:4 & !code %in% crosswalk_klems$industry) %>%
    distinct(aggregate = code,industry)

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

    country_capital_stock_klems = capital_stock_klems %>%
      filter(geo_code == country)

    #Missing sector is derived from the closest aggregate.

    missing_aggregates = aggregate_members %>%
      filter(!aggregate %in% unique(country_capital_stock_klems$nace_r2_code[country_capital_stock_klems$value > 0 & !is.na(country_capital_stock_klems$value)])) %>%
      pull(aggregate) %>%
      unique()

    country_capital_stock_klems = country_capital_stock_klems %>%
      filter(!nace_r2_code %in% missing_aggregates) %>%
      rbind(aggregate_members %>%
              filter(aggregate %in% missing_aggregates) %>%
              inner_join(country_capital_stock_klems,by = c('industry' = 'nace_r2_code')) %>%
              group_by(nace_r2_code = aggregate,geo_code,flow,product) %>%
              summarise(value = sum(value,na.rm = T),.groups = 'drop') %>%
              select(names(country_capital_stock_klems)))

    published = country_capital_stock_klems %>%
      group_by(nace_r2_code) %>%
      summarise(stock = sum(value,na.rm = T),.groups = 'drop') %>%
      filter(stock > 0) %>%
      pull(nace_r2_code)

    #First published level of each branch : decreasing level of breakdown.

    choices = crosswalk_long %>%
      filter(code %in% published) %>%
      group_by(industry) %>%
      slice_min(level,n = 1) %>%
      ungroup()

    filtered_country_capital_stock_klems = country_capital_stock_klems %>%
      filter(nace_r2_code %in% choices$code)

    filtered_capital_stock_klems = rbind(filtered_capital_stock_klems,filtered_country_capital_stock_klems)
  }

  filtered_capital_stock_klems =
    filtered_capital_stock_klems %>%
    left_join(oecd_rates %>% mutate(ISO2 = case_when(ISO2 == 'GB' ~ 'UK',ISO2 == 'GR' ~ 'EL', T ~ ISO2)),by = c('geo_code' = 'ISO2')) %>%
    mutate(EXCH_RATE = case_when(geo_code == 'BG' ~ ecb_bgn_rate(year), #EU KLEMS reports Bulgaria in leva while the exchange-rate table is restated in euros
                                 T ~ EXCH_RATE),
           value = value  / EXCH_RATE)

  return(filtered_capital_stock_klems)


}

get_figaro_cfc = function(year,figaro_valueadded,klems_data,un_ratios,classification,verbose = T,exiobase_level = 'none')
{

  figaro_names = data.frame(str_split_fixed(names(figaro_valueadded), "_", 2)) %>%
    `colnames<-`(c('country','industry'))

  #CFC / GVA rates by source.: national sources, Eurostat, OECD , EU KLEMS, UN

  national_cfc = import_cfc_national(year)

  eurostat_cfc = import_cfc_eurostat(year)

  oecd_cfc =
    read_parquet(here('data','oecd_cfc.parquet')) %>%
    filter(TIME_PERIOD == !!year) %>%
    select(ACTIVITY,REF_AREA,TRANSACTION,OBS_VALUE) %>%
    mutate(ISO2 = countrycode(REF_AREA,"iso3c","iso2c",nomatch = NULL)) %>%
    group_by(country = ISO2,code = ACTIVITY) %>%
    summarise(ratio = ifelse(OBS_VALUE[TRANSACTION == 'B1G'] > 1,OBS_VALUE[TRANSACTION == 'P51C'] / OBS_VALUE[TRANSACTION == 'B1G'],NA),.groups = 'drop') # rates on a near-zero GVA are discarded

  klems_cfc = get_cfc.klems(year,klems_data) %>%
    mutate(geo_code = case_when(geo_code == 'UK' ~ 'GB',
                                T ~ geo_code)) %>%
    group_by(country = geo_code,code = nace_r2_code) %>%
    summarise(ratio = ifelse(unique(VA_CP) > 1,sum(CFC) / unique(VA_CP),NA),.groups = 'drop')

  un_cfc = un_ratios %>%
    ungroup() %>%
    mutate(code = case_when(`Sub Item` == 'Total Economy' ~ "_T",
                            T ~ sub(".*\\((.*)\\).*", "\\1", `Sub Item`))) %>%
    select(code,country = iso2,ratio = P51C_B1G)

  all_cfc =
    rbind(national_cfc %>% select(country,code,ratio,source) %>% mutate(priority = 1),
          eurostat_cfc %>% mutate(source = 'Eurostat',priority = 2),
          oecd_cfc %>% mutate(source = 'OECD',priority = 3),
          klems_cfc %>% mutate(source = 'KLEMS',priority = 4),
          un_cfc %>% mutate(source = 'UN',priority = 5)) %>%
    filter(is.finite(ratio)) %>%
    mutate(ratio = pmax(ratio,0))

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
                               substr(industry,1,1) %in% c('O','P','Q') ~ "O+P+Q"))

  #For each cell, the source of highest priority that provides a rate, at its most granular level (Option6, total economy, is not a cell rate)

  figaro_cfc_rates = figaro_names %>%
    left_join(classification_cfc,by = 'industry') %>%
    pivot_longer(starts_with('Option'),names_to = 'option',values_to = 'code',values_drop_na = TRUE) %>%
    inner_join(all_cfc,by = c('country','code'),relationship = 'many-to-many') %>%
    arrange(priority,option) %>%
    group_by(country,industry) %>%
    slice(1) %>%
    ungroup() %>%
    right_join(figaro_names,by = c('country','industry')) %>% # cells that no source rates are kept, with ratio = NA
    arrange(match(paste0(country,'_',industry),paste0(figaro_names$country,'_',figaro_names$industry))) %>% # in the order of the FIGARO columns
    select(country,industry,ratio,source)

  #National CFC total of each country

  cfc_by_country_ratio = rbind(oecd_cfc %>% mutate(priority = 1),
                      un_cfc %>% mutate(priority = 2)) %>%
    filter(code == '_T' & is.finite(ratio)) %>%
    arrange(priority) %>%
    distinct(country,.keep_all = TRUE)

  figaro_va_by_country = figaro_valueadded %>%
    as.data.frame() %>%
    rownames_to_column('id') %>%
    separate(id,c('country','industry'),sep = "_",extra = 'merge') %>%
    group_by(country) %>%
    summarise(value = sum(.),.groups = 'drop')

  cfc_by_country = figaro_va_by_country %>%
    merge(cfc_by_country_ratio,all.x = T) %>%
    group_by(country) %>%
    summarise(value = sum(value * ratio))

  exiobase_all = character(0)

  if(exiobase_level != 'none')
  {
    ratio_exio = import_exiobase_ratios(year,verbose) %>% {.$ratio_exio[match(cfc_by_country$country,.$country)]}

    use_exio = !is.na(ratio_exio) & (exiobase_level == 'all' | (exiobase_level == 'missing' & is.na(cfc_by_country$value)))

    cfc_by_country = cfc_by_country %>%
      mutate(value = ifelse(use_exio,figaro_va_by_country$value * ratio_exio,value))

    if(exiobase_level == 'all') exiobase_all = cfc_by_country$country[use_exio] # countries rescaled to the EXIOBASE level
  }

  return(list(figaro_cfc_rates = figaro_cfc_rates,
              cfc_by_country = `names<-`(cfc_by_country$value,cfc_by_country$country), # named by country, NA where no national total is published
              exiobase_all = exiobase_all))
}

get_figaro_margins = function(year,figaro,klems_data,un_ratios,classification,verbose,exiobase_level = 'none')
{
  figaro = as.data.frame(figaro)

  figaro_investments = figaro %>%
    select(contains('P51G')) %>%
    `colnames<-`(gsub('_P51G','',colnames(.))) %>%
    filter(!grepl("W2",rownames(.))) %>%
    rowSums(na.rm=T)

  figaro_valueadded = figaro %>%
    filter(grepl('W2',rownames(.)) & !grepl('D21X31',rownames(.))) %>% #value added at basic prices : taxes less subsidies on products excluded
    select(-contains('P3'),-contains('P5')) %>%
    colSums(na.rm=T)

  figaro_cfc = get_figaro_cfc(year,figaro_valueadded,klems_data,un_ratios,classification,verbose,exiobase_level)

  #CFC is retrieved from rate x GVA, NA where no source publishes a rate (later imputed)

  col_margin = pmax(figaro_cfc$figaro_cfc_rates$ratio * figaro_valueadded,0) %>% `names<-`(names(figaro_valueadded))

  #Distribution key of a stock published on a merged branch (eq. 6) : the CFC of each cell, with the cells that no source rates set to the average
  #rate of their country (world average if the country has none) times their GVA, so that the key is strictly positive everywhere

  cells = data.frame(id = names(figaro_valueadded),
                     country = sub('_.*','',names(figaro_valueadded)),
                     col_margin = unname(col_margin),
                     value_added = pmax(unname(figaro_valueadded),0))

  average_rate = cells %>%
    filter(!is.na(col_margin)) %>%
    group_by(country) %>%
    summarise(rate = sum(col_margin) / sum(value_added),.groups = 'drop')

  world_rate = sum(cells$col_margin,na.rm = T) / sum(cells$value_added[!is.na(cells$col_margin)])

  key_split_cfc_agg = cells %>%
    left_join(average_rate,by = 'country') %>%
    mutate(value = case_when(!is.na(col_margin) ~ col_margin,
                             !is.na(rate) ~ rate * value_added,
                             T ~ world_rate * value_added)) %>%
    {`names<-`(.$value,.$id)}

  margins = list(row_margin = figaro_investments,
                 col_margin = col_margin,
                 key_split_cfc_agg = key_split_cfc_agg,
                 value_added = figaro_valueadded,
                 cfc_total = figaro_cfc$cfc_by_country,
                 figaro_cfc_rates = figaro_cfc$figaro_cfc_rates,
                 exiobase_all = figaro_cfc$exiobase_all)

  return(margins)

}

complete_cfc_margins = function(figaro_margins,stock_cfc)
{

  #Missing data : CFC = sum(imputed stock x depreciation rates)
  #Countries in which no cell has an official rate : the CFC of the whole economy is set to the national total and shared between the industries in proportion to their stock-based CFC.
  #Without a national total, the stock-based level is kept.
  #Same rescaling for the countries of exiobase_all (EXIOBASE level for all countries).

  completed = data.frame(id = names(figaro_margins$col_margin),
                         country = sub('_.*','',names(figaro_margins$col_margin)),
                         col_margin = unname(figaro_margins$col_margin),
                         stock_cfc = unname(stock_cfc)) %>%
    group_by(country) %>%
    mutate(rated_country = any(!is.na(col_margin)),
           col_margin = case_when(is.na(col_margin) ~ stock_cfc,
                                  T ~ col_margin),
           national_total = unname(figaro_margins$cfc_total[country]),
           col_margin = case_when((!rated_country | country %in% figaro_margins$exiobase_all) & !is.na(national_total) & sum(col_margin,na.rm = T) > 0 ~ col_margin / sum(col_margin,na.rm = T) * national_total,
                                  T ~ col_margin)) %>%
    ungroup()

  figaro_margins$col_margin = `names<-`(completed$col_margin,completed$id)

  return(figaro_margins)

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

  #Industry weights w_C : FIGARO consumption of fixed capital by using industry (eq. 6)

  w_C = figaro_margins$key_split_cfc_agg

  crosswalk_klems = get_crosswalk_nace.klems()

  storage_K = gi_weights = list()

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

    w_C_country = w_C[paste0(figaro_ref,'_',rownames(Gihat_domestic))]

    w_C_country[w_C_country == 0] <- 0.1

    Gi = diag(x = as.numeric(w_C_country)) %*% Gihat_domestic %*% solve(diag(x = as.numeric(t(as.matrix(w_C_country)) %*% Gihat_domestic)))

    K = klems_domestic[,colnames(Gihat_domestic),drop = FALSE] %*% t(Gi) %>%
      `colnames<-`(rownames(Gihat_domestic))

    storage_K[[figaro_ref]] = K

    gi_weights[[figaro_ref]] = Gi %>% `dimnames<-`(dimnames(Gihat_domestic)) # FIGARO industries (rows) x EU KLEMS sectors (columns) : the shares of the stock by vintage are aggregated with the same weights

  }

  attr(storage_K,'gi_weights') = gi_weights

  return(storage_K)

}

build_capital_stock_panel = function(first_year = 2010,last_year = 2021)
{
  years = first_year:last_year

  #Net stock codes of Eurostat and OECD, in EU KLEMS assets (IPP is split afterwards)
  assets = c(N111N = 'Rstruc',N112N = 'OCon',N1131N = 'TraEq',N11321N = 'IT',N11322N = 'CT',N11ON = 'OMach',N115N = 'Cult',N1171N = 'RD',N1173N = 'Soft_DB',N117N = 'IPP')

  klems = import_klems_capital_accounts() %>%
    filter(year %in% years) %>%
    pivot_longer(starts_with('K_'),names_to = 'asset',names_prefix = 'K_',values_to = 'value') %>%
    transmute(source = 'KLEMS',country = recode(geo_code,EL = 'GR',UK = 'GB'),source_code = nace_r2_code,asset,year,value)

  eurostat = get_eurostat('nama_10_nfa_st',cache = FALSE,time_format = 'num') %>%
    filter(unit == 'CRC_MNAC' & asset10 %in% names(assets) & TIME_PERIOD %in% years) %>%
    transmute(source = 'EUROSTAT',country = recode(geo,EL = 'GR',UK = 'GB'),source_code = nace_r2,asset = assets[asset10],year = TIME_PERIOD,value = values)

  oecd = map(years,import_oecd_fixed_asset) %>%
    bind_rows() %>%
    filter(INSTR_ASSET %in% names(assets) & nchar(ISO2) == 2) %>%
    transmute(source = 'OECD',country = ISO2,source_code = ACTIVITY,asset = assets[INSTR_ASSET],year = TIME_PERIOD,value = OBS_VALUE)

  stock = bind_rows(klems,eurostat,oecd) %>%
    filter(nchar(country) == 2) %>%
    pivot_wider(names_from = asset,values_from = value) %>%
    mutate(OIPP = coalesce(OIPP,IPP - RD - Soft_DB)) %>%
    pivot_longer(all_of(KLEMS_ASSETS),names_to = 'asset',values_to = 'value',values_drop_na = TRUE) %>%
    group_by(source,country,source_code,year) %>%
    filter(any(value > 0)) %>% #an industry whose stocks are all zero is not reported
    ungroup()

  #NACE divisions (1 to 99) and the FIGARO industry that contains each of them ; the section is the first letter of the industry
  divisions = data.frame(division = 1:99) %>%
    mutate(industry = case_when(division == 1 ~ 'A01',
                                division == 2 ~ 'A02',
                                division == 3 ~ 'A03',
                                division %in% 5:9 ~ 'B',
                                division %in% 10:12 ~ 'C10T12',
                                division %in% 13:15 ~ 'C13T15',
                                division == 16 ~ 'C16',
                                division == 17 ~ 'C17',
                                division == 18 ~ 'C18',
                                division == 19 ~ 'C19',
                                division == 20 ~ 'C20',
                                division == 21 ~ 'C21',
                                division == 22 ~ 'C22',
                                division == 23 ~ 'C23',
                                division == 24 ~ 'C24',
                                division == 25 ~ 'C25',
                                division == 26 ~ 'C26',
                                division == 27 ~ 'C27',
                                division == 28 ~ 'C28',
                                division == 29 ~ 'C29',
                                division == 30 ~ 'C30',
                                division %in% 31:32 ~ 'C31_32',
                                division == 33 ~ 'C33',
                                division == 35 ~ 'D35',
                                division == 36 ~ 'E36',
                                division %in% 37:39 ~ 'E37T39',
                                division %in% 41:43 ~ 'F',
                                division == 45 ~ 'G45',
                                division == 46 ~ 'G46',
                                division == 47 ~ 'G47',
                                division == 49 ~ 'H49',
                                division == 50 ~ 'H50',
                                division == 51 ~ 'H51',
                                division == 52 ~ 'H52',
                                division == 53 ~ 'H53',
                                division %in% 55:56 ~ 'I',
                                division == 58 ~ 'J58',
                                division %in% 59:60 ~ 'J59_60',
                                division == 61 ~ 'J61',
                                division %in% 62:63 ~ 'J62_63',
                                division == 64 ~ 'K64',
                                division == 65 ~ 'K65',
                                division == 66 ~ 'K66',
                                division == 68 ~ 'L',
                                division %in% 69:70 ~ 'M69_70',
                                division == 71 ~ 'M71',
                                division == 72 ~ 'M72',
                                division == 73 ~ 'M73',
                                division %in% 74:75 ~ 'M74_75',
                                division == 77 ~ 'N77',
                                division == 78 ~ 'N78',
                                division == 79 ~ 'N79',
                                division %in% 80:82 ~ 'N80T82',
                                division == 84 ~ 'O84',
                                division == 85 ~ 'P85',
                                division == 86 ~ 'Q86',
                                division %in% 87:88 ~ 'Q87_88',
                                division %in% 90:92 ~ 'R90T92',
                                division == 93 ~ 'R93',
                                division == 94 ~ 'S94',
                                division == 95 ~ 'S95',
                                division == 96 ~ 'S96',
                                division %in% 97:98 ~ 'T',
                                division == 99 ~ 'U')) %>%
    filter(!is.na(industry)) %>%
    mutate(section = substr(industry,1,1)) %>%
    group_by(industry) %>%
    mutate(n_divisions = n()) %>%
    ungroup()

  #Different cases ruling
  code_industries = data.frame(source_code = unique(stock$source_code)) %>%
    mutate(published = case_when(grepl('^[A-U]$',source_code) ~ 'section',
                                 grepl('^[A-U][-_][A-U]$',source_code) ~ 'two sections',
                                 grepl('[0-9]',source_code) ~ 'divisions',
                                 T ~ 'total'),
           code_section = substr(source_code,1,1),
           code_last_section = substr(source_code,3,3),
           code_first = as.integer(str_extract(source_code,'[0-9]+')),
           code_last = as.integer(str_extract(source_code,'[0-9]+(?=[^0-9]*$)'))) %>%
    cross_join(divisions) %>%
    filter(case_when(published == 'section' ~ section == code_section,
                     published == 'two sections' ~ section >= code_section & section <= code_last_section & code_last_section == LETTERS[match(code_section,LETTERS) + 1],
                     published == 'divisions' ~ section == code_section & division >= code_first & division <= code_last,
                     T ~ FALSE)) %>%
    group_by(source_code,industry) %>%
    filter(n() == n_divisions) %>%
    summarise(.groups = 'drop') %>%
    group_by(source_code) %>%
    mutate(n_industries = n()) %>%
    ungroup()

  #Each industry takes the finest code that reports it
  observed = stock %>%
    distinct(source,country,source_code,year) %>%
    inner_join(code_industries,by = 'source_code',relationship = 'many-to-many') %>%
    arrange(n_industries,source_code) %>%
    group_by(source,country,year,industry) %>%
    slice(1) %>%
    ungroup() %>%
    inner_join(stock,by = c('source','country','source_code','year'),relationship = 'many-to-many')

  chosen_source = observed %>%
    filter(value > 0) %>%
    count(source,country,year) %>%
    group_by(source,country) %>%
    summarise(cells = sum(n) / length(years),.groups = 'drop') %>%
    arrange(country,-round(cells,6),match(source,c('KLEMS','EUROSTAT','OECD'))) %>%
    distinct(country,.keep_all = TRUE)

  exchange_rates = import_oecd_exchange_rates() %>%
    select(country = ISO2,year,EXCH_RATE)

  bgn_rates = map_dbl(years,ecb_bgn_rate) %>% `names<-`(years)

  capital_stock_panel = observed %>%
    semi_join(chosen_source,by = c('country','source')) %>%
    left_join(exchange_rates,by = c('country','year')) %>%
    mutate(EXCH_RATE = case_when(country == 'BG' & source != 'OECD' ~ bgn_rates[as.character(year)], #EU KLEMS and Eurostat report Bulgaria in leva
                                 T ~ EXCH_RATE),
           value_eur = value / EXCH_RATE) %>%
    select(country,industry,asset,year,source,source_code,value_eur)

  return(capital_stock_panel)
}

get_capital_stock_panel = function(selected_year)
{
  cache_file = list.files(dirname(tempdir()),pattern = '^CAPITAL_STOCK_PANEL',full.names = T,recursive = T)

  if(length(cache_file) != 1)
  {
    unlink(cache_file)

    cache_file = tempfile(pattern = 'CAPITAL_STOCK_PANEL_',fileext = '.parquet')

    write_parquet(build_capital_stock_panel(),cache_file)
  }

  capital_stock_panel = read_parquet(cache_file) %>%
    filter(year == selected_year) %>%
    select(country,industry,asset,source_code,value = value_eur)

  return(capital_stock_panel)
}

domestic_capital_stock_matrix.panel = function(panel,figaro,figaro_margins,verbose)
{
  w_C = figaro_margins$key_split_cfc_agg

  assets = sort(unique(panel$asset),method = 'radix')

  industries = sort(get_correspondence_nace.klems('FIGARO')$industry,method = 'radix')

  #A stock published on a merged branch or a section is split over its FIGARO industries with w_C

  K = panel %>%
    filter(country %in% substr(colnames(figaro),1,2)) %>%
    mutate(w = w_C[paste0(country,'_',industry)],
           w = case_when(w == 0 ~ 0.1,
                         T ~ w)) %>%
    group_by(country,asset,source_code) %>%
    mutate(value = value * w / sum(w)) %>%
    ungroup() %>%
    mutate(observed = TRUE) %>%
    select(country,industry,asset,value,observed) %>%
    complete(country,asset = assets,industry = industries,fill = list(value = 0,observed = FALSE))

  #Countries whose assets are all observed in the industry

  actual_observations = K %>%
    group_by(country,industry) %>%
    filter(all(observed)) %>%
    ungroup()

  #Imputation for industries with partial fillings

  shares = actual_observations %>%
    group_by(country,industry) %>%
    mutate(share = value / sum(value)) %>%
    group_by(industry,asset) %>%
    summarise(share = mean(share,na.rm = T),.groups = 'drop')

  K = K %>%
    left_join(shares,by = c('industry','asset')) %>%
    group_by(country,industry) %>%
    mutate(value = case_when(!observed & any(observed) & !anyNA(share) & sum(share[observed]) != 0 ~ sum(value[observed]) / sum(share[observed]) * share,
                             T ~ value)) %>%
    ungroup()

  #Imputation for industries without any data

  median_asset_rates = actual_observations %>%
    mutate(w = w_C[paste0(country,'_',industry)],
           w = case_when(w == 0 ~ 0.1,
                         T ~ w)) %>%
    group_by(industry,asset) %>%
    summarise(rate = median(value / w),.groups = 'drop')

  K = K %>%
    left_join(median_asset_rates,by = c('industry','asset')) %>%
    mutate(w = w_C[paste0(country,'_',industry)],
           w = case_when(w == 0 ~ 0.1,
                         T ~ w)) %>%
    group_by(country,industry) %>%
    mutate(value = case_when(!any(observed) & !anyNA(rate) ~ w * rate,
                             T ~ value)) %>%
    ungroup()

  K_by_country = K %>%
    select(country,asset,industry,value) %>%
    split(.$country)

  K_list = K_by_country %>%
    map(~ .x %>%
          pivot_wider(id_cols = asset,names_from = industry,values_from = value) %>%
          column_to_rownames('asset') %>%
          as.matrix() %>%
          {.[assets,industries]})

  return(K_list)
}

domestic_capital_stock_matrix.oecd = function(year,missing,figaro_margins,verbose)
{

  #Industry weights w_C : FIGARO consumption of fixed capital by using industry

  w_C = figaro_margins$key_split_cfc_agg

  oecd_rates = import_oecd_exchange_rates() %>% filter(year == !!year) %>% select(ISO2,EXCH_RATE)

  capital_stock.oecd = import_oecd_fixed_asset(year,verbose) %>%
    left_join(oecd_rates,by = 'ISO2') %>%
    mutate(OBS_VALUE = OBS_VALUE / EXCH_RATE)

  activity_correspondence.oecd = get_correspondence_nace.oecd(classification = 'FIGARO')

  available_countries.oecd = unique(capital_stock.oecd$ISO2) %>% subset(. %in% missing)

  storage_K = list()

  for(i in available_countries.oecd %>% subset(!. %in% c('KR','RU'))) #For Korea and Russia, there is no information on sectoral x product level
  {

    #Filter country-specific OECD data : stocks are used in net terms, as published (current prices)

    country_data.oecd =
      capital_stock.oecd %>%
      ungroup() %>%
      filter(ISO2 == i) %>%
      mutate(ACTIVITY = case_when(ACTIVITY == 'O' ~ 'O84',
                                  ACTIVITY == 'P' ~ 'P85',
                                  ACTIVITY == 'D' ~ 'D35',
                                  T ~ ACTIVITY)) %>%
      filter(ACTIVITY %in% unique(unlist(activity_correspondence.oecd))) %>%
      select(ASSET,ACTIVITY,OBS_VALUE)


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

      to_delete = non_financial_asset_tree[non_financial_asset_tree[,j] %in% available,(1:5)<j] %>% unlist() %>% unique()

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

    pi_country = w_C[paste0(i,'_',colnames(initial_country_data_oecd))] %>%
      `names<-`(colnames(initial_country_data_oecd))

    pi_country[pi_country == 0] <- 0.1

    for(j in 1:ncol(initial_country_data_oecd))
    {
      pi_j = pi_country %>% subset(names(.) %in% colnames(initial_country_data_oecd)[j])

      ref = reference_activity_country %>%
        filter(FIGARO == colnames(initial_country_data_oecd)[j]) %>%
        pull(OECD)

      aggregate_ref = reference_activity_country$FIGARO[which(apply(reference_activity_country, 1, function(row) any(row == ref)))]

      pi_agg = pi_country %>% subset(names(.) %in% aggregate_ref) %>% sum()

      initial_country_data_oecd[,j] = formatted_country_data.oecd[,ref] / pi_agg * pi_j
    }

    initial_country_data_oecd[is.na(initial_country_data_oecd)] = 0

    #Convert standard non-financial classification to KLEMS classification system.

    K = initial_country_data_oecd %>%
      rownames_to_column('ASSET') %>%
      left_join(get_correspondence_product.oecd_to_klems(),by = 'ASSET') %>%
      group_by(ASSET) %>%
      mutate(across(where(is.numeric),\(x) x / n())) %>% #An asset matching several KLEMS assets is shared equally between them
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

missing_countries_capital_stock = function(year,domestic_capital_stock_list,figaro,option = 'Poisson',verbose)
{

  #Data preparation

  correspondence_nace = get_correspondence_nace.klems(classification = 'FIGARO')

  value_added.figaro = figaro %>%
    filter(grepl('W2',rownames(.)) & !grepl('D21X31',rownames(.))) %>%
    colSums() %>%
    subset(!grepl('P3|P5',names(.))) %>%
    data.frame(value_added = .) %>%
    rownames_to_column('id') %>%
    separate(id,c('country','industry'),extra = 'merge',sep = "_")

  #Import PWT data to characterize GDP per capita category

  pwt = import_pwt_data(year,verbose) %>%
    filter(country %in% unique(sub('_.*','',colnames(figaro)))) %>%
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
           value = pmax(value_added,0) * predict(train_poisson_regression,newdata = .) %>% replace_na(0))

  }


  for(i in unique(to_predict$country))
  {

    K = to_predict %>%
      filter(country == i) %>%
      select(use_id,resource_id,value) %>%
      pivot_wider(names_from = 'use_id') %>%
      column_to_rownames('resource_id') %>%
      .[sort(rownames(.),method = 'radix'),sort(colnames(.),method = 'radix')]

    domestic_capital_stock_list[[i]] = K

  }

  return(domestic_capital_stock_list)


}

domestic_capital_tables = function(figaro,complete_domestic_capital_stock,verbose)
{

  Gphat = get_correspondence_product.klems() %>%
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

  capital_stock_list = depreciations_list = cfc_list = gp_list = cfc_by_asset_list = stock_by_asset_list = list()

  for(i in names(complete_domestic_capital_stock))
  {
    pp_country = pp %>% filter(sub("_.*", "", rownames(.)) == i)

    pp_country = pp_country$value %>% setNames(rownames(pp_country)) #FIGARO rows of country i, GFCF by industry

    #Gphat columns and pp_country entries must be aligned by name (products x industries)

    Gphat_country = Gphat[,sub("^[^_]+_", "", names(pp_country))]

    Gp = Gphat_country %*% diag(x = unlist(pp_country))

    no_gfcf = rowSums(Gp) == 0
    Gp[no_gfcf,] = Gphat_country[no_gfcf,] #An asset whose products receive no GFCF is split equally between them

    Gp = Gp / ifelse(rowSums(Gp) == 0,1,rowSums(Gp))
    dimnames(Gp) = dimnames(Gphat_country) #assets x FIGARO products : the distributed tables carry the FIGARO product codes as row names

    distributed_domestic_capital_stock = t(Gp) %*% as.matrix(complete_domestic_capital_stock[[i]])

    #C = delta (elementwise) K : assets (rows) and industries (columns) of K and of the depreciation matrix must match by name

    K_i = as.matrix(complete_domestic_capital_stock[[i]])

    distributed_cfc = K_i * depreciations.klems[rownames(K_i),colnames(K_i)]

    distributed_domestic_depreciations_rate = t(Gp) %*% distributed_cfc / distributed_domestic_capital_stock

    distributed_domestic_depreciations_rate[distributed_domestic_capital_stock == 0] = 0

    capital_stock_list[[i]] = distributed_domestic_capital_stock

    depreciations_list[[i]] = distributed_domestic_depreciations_rate %>% as.matrix()

    cfc_list[[i]] = t(Gp) %*% distributed_cfc

    gp_list[[i]] = Gp

    cfc_by_asset_list[[i]] = distributed_cfc # assets x FIGARO industries, before the product step Gp

    stock_by_asset_list[[i]] = K_i # assets x FIGARO industries, before the product step Gp

  }

  return(list(Stock = capital_stock_list,
              CFC = cfc_list,
              depreciations_list = depreciations_list,
              Gp = gp_list,
              cfc_by_asset = cfc_by_asset_list,
              stock_by_asset = stock_by_asset_list))

}

distribute_origin_suppliers = function(figaro,domestic_capital_tables_pxi,verbose,origin_option = 'GFCF')
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
                               dimnames = list(rownames(figaro_investments), NULL))

  for(i in sort(names(domestic_capital_tables_pxi$Stock)))
  {
    origin_supply_proxy = share_supply_product %>%
      filter(country == i) %>%
      select(!country)

    #Each row j of the domestic table is split between the supplying countries o, in proportion to their share in the GFCF of product j : row (o,j) = row j x share(o,j)

    if(origin_option == 'GFCF')
    {
      origin_tables = domestic_capital_tables_pxi[c('Stock','CFC')] %>%
        map(~ origin_supply_proxy %>%
              left_join(.x[[i]] %>% as.data.frame() %>% rownames_to_column('resource_industry'),by = 'resource_industry') %>%
              mutate(across(!c(resource_id,resource_industry,share),~ .x * share)) %>%
              column_to_rownames('resource_id') %>%
              select(!c(resource_industry,share)) %>%
              rename_with(~ paste0(i,'_',.x)) %>%
              as.matrix())
    }else{
      using_industries = colnames(domestic_capital_tables_pxi$CFC[[i]])

      purchases = as.matrix(figaro[rownames(figaro_investments),paste0(i,'_',using_industries)]) %>% pmax(0)

      product = sub('^[^_]+_','',rownames(purchases))

      #Share of each supplying country in the purchases of the product by each using industry
      origin_share = purchases / rowsum(purchases,product)[product,]

      origin_share = ifelse(is.finite(origin_share),origin_share,origin_supply_proxy$share[match(rownames(purchases),origin_supply_proxy$resource_id)])

      origin_tables = domestic_capital_tables_pxi[c('Stock','CFC')] %>%
        map(~ (origin_share * as.matrix(.x[[i]])[product,using_industries]) %>% `colnames<-`(paste0(i,'_',using_industries)))
    }

    STOCK_PXI = cbind(STOCK_PXI,origin_tables$Stock)

    CFC_PXI = cbind(CFC_PXI,origin_tables$CFC)
  }

  STOCK_PXI[is.na(STOCK_PXI)] = 0
  CFC_PXI[is.na(CFC_PXI)] = 0

  return(list(Stock = STOCK_PXI,
              CFC = CFC_PXI))
}

compilation_d_fixed_sales_assumption = function(year,multi_regional_capital_use_pxi,figaro_margins,n_iter,min_error,option,verbose)
{
  V_t = import_figaro_supply_table(year,verbose)

  product_id = sub('_CPA_','_',rownames(V_t))
  STOCK_PXI = as.matrix(multi_regional_capital_use_pxi$Stock)[product_id,]
  CFC_PXI = as.matrix(multi_regional_capital_use_pxi$CFC)[product_id,]

  q = rowSums(V_t,na.rm = T)

  Tr = Matrix::Matrix(sweep(t(V_t),2,ifelse(q == 0,0,1/q),`*`),sparse = TRUE,dimnames = list(colnames(V_t),product_id)) #Share of each industry in the output of each product (fixed sales structure, eq. 11)

  STOCK_IXI = as.matrix(Tr %*% STOCK_PXI)

  CFC_IXI = K = as.matrix(Tr %*% CFC_PXI)

  #Margins of the rows and columns of K, matched by name

  rowwise_margins = figaro_margins$row_margin[rownames(K)]
  colwise_margins = figaro_margins$col_margin[colnames(K)]

  #Row or a column of zeros stays zero

  drop_row = rowSums(K) == 0
  drop_col = colSums(K) == 0

  rowwise_margins[drop_row] = 0 ; colwise_margins[drop_col] = 0

  GFCF_o = tapply(rowwise_margins,sub('_.*','',names(rowwise_margins)),sum,na.rm = T)

  CFC_o = tapply(colwise_margins,sub('_.*','',names(colwise_margins)),sum,na.rm = T)

  if(option == 'GFCF')
  {
    #The GFCF rows of each supplying country are rescaled so that their total equals the CFC of that country

    lambda = CFC_o[names(GFCF_o)] / GFCF_o ; lambda[!is.finite(lambda)] = 0

    rowwise_margins = rowwise_margins * as.numeric(lambda[sub('_.*','',names(rowwise_margins))])
  }

  if(option == 'CFC')
  {
    #The CFC columns of each using country are rescaled so that their total equals the GFCF of that country (a single global factor does not converge : non-traded rows cannot reach their target)

    mu = GFCF_o[names(CFC_o)] / CFC_o ; mu[!is.finite(mu)] = 0

    colwise_margins = colwise_margins * as.numeric(mu[sub('_.*','',names(colwise_margins))])
  }

  RAS_ROW = rep(1,nrow(K)) ; names(RAS_ROW) = rownames(K)

  RAS_COL = rep(1,ncol(K)) ; names(RAS_COL) = colnames(K)

  accuracy = NA_real_ ; n_used = 0

  for(iter in 1:n_iter)
  {

    r = rowwise_margins / rowSums(K) ; r[!is.finite(r)] = 1

    K = sweep(K, 1, r, `*`)

    RAS_ROW = RAS_ROW * r

    s = colwise_margins / colSums(K) ; s[!is.finite(s)] = 1

    K = sweep(K, 2, s, `*`)

    RAS_COL = RAS_COL * s

    accuracy = max(abs(c(r,s)-1))

    n_used = iter

    if(accuracy < min_error) break

  }

  converged = accuracy < min_error

  if(verbose) message('RAS procedure : ',n_used,' iteration(s), maximum error ',signif(accuracy,3),ifelse(converged,' (converged)',' (NOT converged)'))

  if(!converged) warning('RAS procedure did not reach the requested accuracy of ',min_error,' after ',n_iter,' iterations')

  STOCK_IXI_SCALED = K / (CFC_IXI / STOCK_IXI)

  STOCK_IXI_SCALED[!is.finite(STOCK_IXI_SCALED)] = 0

  return(list(CAPITAL_USE = K,
              CAPITAL_STOCK = STOCK_IXI_SCALED,
              RAS_FACTORS = list(rows = RAS_ROW,columns = RAS_COL,fixed_sales = Tr),
              RAS_DIAGNOSTICS = list(iterations = n_used,max_error = accuracy,converged = converged)))
}

build_capital_use = function(year,classification = 'FIGARO',n_iter = 100,min_error = 1e-3,missing_option = 'Poisson',ras_margin_adjusted = 'GFCF',format = 'ixi',verbose = T,exiobase_level = 'none',stock_source = 'panel',origin_option = 'GFCF')
{

  figaro = import_figaro_io_table(year)

  klems_data = get_capital_stock.klems(year) %>%
    mutate(geo_code = case_when(geo_code == 'EL' ~ 'GR',
                                T ~ geo_code))

  un_ratios =  import_un_ratios(year)

  figaro_margins = get_figaro_margins(year,figaro,klems_data,un_ratios,classification,verbose,exiobase_level)

  domestic_capital_stock_list.klems = domestic_capital_stock_matrix.klems(klems_data,figaro,classification,figaro_margins,verbose)

  domestic_capital_stock_list = domestic_capital_stock_list.klems

  if(stock_source == 'panel')
  {
    domestic_capital_stock_list.panel = domestic_capital_stock_matrix.panel(get_capital_stock_panel(year),figaro,figaro_margins,verbose)

    panel_countries = names(domestic_capital_stock_list.panel)

    domestic_capital_stock_list = domestic_capital_stock_list[setdiff(names(domestic_capital_stock_list),panel_countries)]

    domestic_capital_stock_list = c(domestic_capital_stock_list,domestic_capital_stock_list.panel)
  }

  missing_countries = sub("_.*$", "", colnames(figaro)) %>% unique() %>% subset(!. %in% names(domestic_capital_stock_list))

  domestic_capital_stock_list.oecd = domestic_capital_stock_matrix.oecd(year,missing_countries,figaro_margins,verbose)

  domestic_capital_stock_list = c(domestic_capital_stock_list,domestic_capital_stock_list.oecd)

  complete_domestic_capital_stock = domestic_capital_stock_list %>% missing_countries_capital_stock(year,.,figaro,option = missing_option,verbose)

  domestic_capital_tables_pxi = domestic_capital_tables(figaro,complete_domestic_capital_stock,verbose)

  #Cells without official CFC rate : stock-based CFC (stock x depreciation rates, summed over assets)

  stock_cfc = domestic_capital_tables_pxi$CFC %>%
    map(colSums) %>%
    unlist() %>%
    `names<-`(sub('.','_',names(.),fixed = T))

  stock_cfc = stock_cfc[names(figaro_margins$col_margin)] %>%
    `names<-`(names(figaro_margins$col_margin))

  stock_cfc[is.na(stock_cfc)] = 0

  figaro_margins = complete_cfc_margins(figaro_margins,stock_cfc)

  multi_regional_capital_use_pxi = distribute_origin_suppliers(figaro,domestic_capital_tables_pxi,verbose,origin_option)

  if(format == 'pxi') return(multi_regional_capital_use_pxi)

  multi_regional_capital_use_ixi = compilation_d_fixed_sales_assumption(year,multi_regional_capital_use_pxi,figaro_margins,n_iter = n_iter,min_error = min_error,option = ras_margin_adjusted,verbose)

  multi_regional_capital_use_ixi$MARGINS = figaro_margins

  multi_regional_capital_use_ixi$ASSET_DETAIL = list(figaro = figaro,
                                                     Gp = domestic_capital_tables_pxi$Gp,
                                                     cfc_by_asset = domestic_capital_tables_pxi$cfc_by_asset,
                                                     stock_by_asset = domestic_capital_tables_pxi$stock_by_asset,
                                                     gi_weights = attr(domestic_capital_stock_list.klems,'gi_weights'))

  if(format == 'both') return(list(pxi = multi_regional_capital_use_pxi,ixi = multi_regional_capital_use_ixi))

  return(multi_regional_capital_use_ixi)

}

compute_ict_shares = function(year)
{

  exchange_rate_oecd = import_oecd_exchange_rates() %>% filter(year == !!year) %>% select(REF_AREA,OBS_VALUE = RATE)

  dataset.sdbs = read_parquet(here('data','oecd_sdbs_ict.parquet')) %>%
    filter(TIME_PERIOD == !!year) %>%
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
    filter(any(level == 1) & (any(level == 2) | substr(ISIC4,1,2) == '61')) %>% #A division published without its 3-digit rows has no share : the world proxy applies
    group_by(REF_AREA,branch) %>%
    filter(branch != 'J62_63' | n_distinct(substr(ISIC4,1,2)) == 2) %>% #J62_63 needs both of its divisions
    summarise(VALUE_LVL2 = unique(sum(OBS_VALUE[level == 2],na.rm=T)),
              VALUE_LVL1 = unique(sum(OBS_VALUE[level == 1],na.rm=T)),.groups = 'drop')

  country_shares = dataset.sdbs %>%
    group_by(REF_AREA,branch) %>%
    summarise(value = case_when(branch == "J61" ~ 1,
                                T ~ VALUE_LVL2 / VALUE_LVL1),
              .groups = 'drop')

  global_shares = dataset.sdbs %>%
    merge(exchange_rate_oecd %>% select(REF_AREA,OBS_VALUE),all.x = T) %>%
    filter(!is.na(OBS_VALUE)) %>%
    mutate(VALUE_LVL1 = VALUE_LVL1 / OBS_VALUE, # OECD EXC_A = national currency per USD: divide to convert to USD
           VALUE_LVL2 = VALUE_LVL2 / OBS_VALUE) %>%
    group_by(branch) %>%
    summarise(value = case_when(branch == "J61" ~ 1,
                                T ~ sum(VALUE_LVL2,na.rm=T) / sum(VALUE_LVL1,na.rm=T)),
              REF_AREA = 'WORLD',
              .groups = 'drop') %>%
    distinct()

  shares = rbind(country_shares,global_shares) %>%
    mutate(REF_AREA = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL)) %>%
    filter(is.finite(value)) # NaN/Inf country share (VALUE_LVL1 = 0): dropped, the world share is used instead

  return(shares)
}

ecb_bgn_rate = function(year)
{
  bgn_rate = get_cache_file(link = 'https://data-api.ecb.europa.eu/service/data/EXR/A.BGN.EUR.SP00.A?startPeriod=2010&endPeriod=2024&format=csvdata', #All years in one request
                            pattern = 'ECB_BGN_2010_2024') %>%
    read.csv() %>%
    filter(TIME_PERIOD == !!year) %>%
    pull(OBS_VALUE)

  return(bgn_rate)
}

figaro_system = function(inputs,built,sub_X,pattern = ICT_PATTERN)
{
  Z = inputs$Z ; x = as.numeric(inputs$X) ; B = inputs$B ; C = built$CAPITAL_USE

  A = sweep(Z,2,x,'/') ; A[!is.finite(A)] = 0
  Th = sweep(C,2,x,'/') ; Th[!is.finite(Th)] = 0 # capital use per euro of output

  n = nrow(A)
  IA = -A ; diag(IA) = diag(IA) + 1 # I - A
  M = IA - Th # I - A - Theta

  S = grep(pattern,colnames(A)) # positions of the studied industries (Omega)

  E = matrix(0,n,length(S)) ; E[cbind(S,seq_along(S))] = 1 # selection matrix of the grouping
  Y = solve(M,E)                                   # columns of the capital-augmented inverse Lc
  y = solve(Y[S,,drop = FALSE],sub_X)              # final demand yielding the grouping production
  tau_c = as.numeric(Y %*% y)                      # production induced in every industry, capital included

  d = numeric(n) ; d[S] = y
  tau_0 = as.numeric(solve(IA,d))                  # production induced by the same final demand, without the capital step

  kappa = as.numeric(Th %*% tau_0) # direct capital needs, by supplying industry

  list(A = A,Th = Th,M = M,IA = IA,x = x,B = B,tau_0 = tau_0,tau_c = tau_c,kappa = kappa,
       rho_A = spectral_radius(A),rho_Ac = spectral_radius(A + Th))
}

footprint_by_vintage = function(sys,vintage_capital_use,intensities_by_year)
{
  vintages = names(vintage_capital_use)

  phi = solve(t(sys$M),intensities_by_year[,vintages,drop = FALSE]) # Lc' s_v : multipliers with the intensities of vintage v, one column per vintage

  parts = sapply(seq_along(vintages),function(k)
  {
    Th_v = sweep(vintage_capital_use[[k]],2,sys$x,'/') ; Th_v[!is.finite(Th_v)] = 0 # capital use of vintage v per euro of output

    kappa_v = Th_v %*% sys$tau_0 # direct capital needs of vintage v, in euros of year v

    sum(phi[,k] * kappa_v) # emissions of the capital of vintage v
  }) %>% `names<-`(vintages)

  list(capital = sum(parts),by_vintage = parts,footprint = sum(sys$B * sys$tau_0) + sum(parts)) # capital term valued by age, split by vintage, and total footprint (current part + capital term)
}
