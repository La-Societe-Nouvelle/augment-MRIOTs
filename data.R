import_figaro_io_table = function(year,verbose = T)
{

  links = matrix(byrow = T,ncol = 2,
                 data = c('2010', 'https://circabc.europa.eu/rest/download/810676bb-e3d8-4abe-a75e-c4fc5d059f1e',
                          '2011', 'https://circabc.europa.eu/rest/download/dcbc0884-f4e4-46af-bf3f-462937f59ee3',
                          '2012', 'https://circabc.europa.eu/rest/download/38e5a3a3-1ae1-408b-8901-153ef6652829',
                          '2013', 'https://circabc.europa.eu/rest/download/42d4f65d-3c5b-4eb3-8391-46314801b359',
                          '2014', 'https://circabc.europa.eu/rest/download/85d3f6de-3510-45ff-96c9-11da9f1e325a',
                          '2015', 'https://circabc.europa.eu/rest/download/e989cc76-52f5-4cd9-ba8c-5764fafde13a',
                          '2016', 'https://circabc.europa.eu/rest/download/2e7bc8e5-ee0c-4e57-a082-60a9fb59ee73',
                          '2017', 'https://circabc.europa.eu/rest/download/0bd4147b-d06b-4800-87e4-c479352e88ba',
                          '2018', 'https://circabc.europa.eu/rest/download/40c2349e-1ba7-48b5-b4a3-4f00b78f2283',
                          '2019', 'https://circabc.europa.eu/rest/download/ba9d7f3a-a381-49f2-998d-64fec81c3d35',
                          '2020', 'https://circabc.europa.eu/rest/download/28d77fbf-50c2-475c-8ba6-c97b02a69039',
                          '2021', 'https://circabc.europa.eu/rest/download/57fd3555-51db-4f03-b765-3cb7fcfe3007',
                          '2022', 'https://circabc.europa.eu/rest/download/f1330ee5-2841-4f3a-949c-20f36d068714',
                          '2023', 'https://circabc.europa.eu/rest/download/836a0346-bdb5-4d8a-bbea-b3bd583a53c9',
                          '2024', 'https://circabc.europa.eu/rest/download/8ac7da34-911e-4a91-ab1c-8aec1a574381')) %>%
    as.data.frame() %>%
    `colnames<-`(c('year','link'))


  # The file is downloaded once per R session (cache of the temporary folder)

  matrix.file = read.csv(get_cache_file(links %>% filter(year == !!year) %>% pull(link),paste0('FIGARO_IO_',year),verbose)) %>%
    column_to_rownames('rowLabels')

  return(matrix.file)
}

import_figaro_supply_table = function(year,verbose = T)
{

  links = data.frame(year = 2010:2024,
                     link = c('https://circabc.europa.eu/rest/download/5d2c05e4-f049-42ee-ab9d-79acde60e897',
                              'https://circabc.europa.eu/rest/download/e8500dee-ea45-4469-b747-65be3a8c378e',
                              'https://circabc.europa.eu/rest/download/bc2806eb-1472-4703-883c-04bdfb13cd04',
                              'https://circabc.europa.eu/rest/download/20cb8d04-235c-4d7c-bef0-1e5e8c73abbc',
                              'https://circabc.europa.eu/rest/download/23e10ca4-0825-420b-a30f-847cc55de28a',
                              'https://circabc.europa.eu/rest/download/a9475586-1602-4108-b39d-f2a7670df4b7',
                              'https://circabc.europa.eu/rest/download/044cf42a-f2ce-466f-a3e3-91752e878039',
                              'https://circabc.europa.eu/rest/download/ea40cf1b-61d8-433d-b156-d58d4d30b359',
                              'https://circabc.europa.eu/rest/download/8baeaa78-5ca5-4c96-8ed9-3c17bf1a61fb',
                              'https://circabc.europa.eu/rest/download/34498144-a30f-4655-b434-73aa5373ad76',
                              'https://circabc.europa.eu/rest/download/18377509-f0f7-4f3c-b230-ce83090fd122',
                              'https://circabc.europa.eu/rest/download/8c17ee84-289a-4e24-8e29-05732dc61f97',
                              'https://circabc.europa.eu/rest/download/c57663e9-da20-4637-9a12-fa80fe023371',
                              'https://circabc.europa.eu/rest/download/42744021-74a4-4e8d-8574-c3b83c4f2a9f',
                              'https://circabc.europa.eu/rest/download/45234789-1ad8-44b8-a3b4-9f8c1919b5d6'))



  # The file is downloaded once per R session (cache of the temporary folder)

  matrix.file = read.csv(get_cache_file(links %>% filter(year == !!year) %>% pull(link),paste0('FIGARO_SUPPLY_',year),verbose)) %>%
    column_to_rownames('rowLabels')

  return(matrix.file)
}

import_depreciation_rates_klems = function(update = F,verbose = T)
{

  #As the table requires treatment, we use a dedicated cache procedure.

  pattern = "klems_depreciations"

  existing = list.files(dirname(tempdir()),recursive = T,pattern = pattern,full.names = T)

  #If the file is already computed and cached

  if (!update && length(existing) == 1) {
    if (verbose) message("Returning cached data from: ", existing)
    return(read_parquet(existing))
  }

  unlink(existing,recursive = T)

  temp.pdf = curl_download("https://euklems-intanprod-llee.luiss.it/wp-content/uploads/2023/02/EUKLEMS_INTANProd_D2.3.1.pdf",tempfile())

  depreciations.klems = pdftools::pdf_text(temp.pdf)[39:41] %>%
    strsplit("\n") %>%
    unlist() %>%
    trimws() %>%
    .[. != ""] %>%
    {.[grep("^Code", .):length(.)]} %>%
    {.[!grepl("^[A-Z]$", .)]} %>%
    read.table(text = ., sep = "", header = TRUE, fill = TRUE, check.names = FALSE) %>%
    filter(!is.na(IT)) %>%
    pivot_longer(-1,names_to = 'product',values_to = 'rate')

  depreciations.klems = rbind(depreciations.klems,
                              depreciations.klems %>%
                                group_by(Code = case_when(Code %in% c('C20','C21') ~ 'C20-C21',
                                                          Code %in% c('C26','C27') ~ 'C26-C27',
                                                          Code %in% c('D','E') ~ 'D-E'),
                                         product) %>%
                                filter(!is.na(Code)) %>%
                                summarise(rate = mean(rate),.groups = 'drop'))

  write_parquet(depreciations.klems,
                tempfile(pattern = pattern))

  if(verbose) message("Data cached in temporary files")

  return(depreciations.klems)
}

import_oecd_fixed_asset = function(year,verbose = T)
{
  fixed_assets_sector = read_parquet(here('data','oecd_fixed_assets_net.parquet')) %>%
    filter(TIME_PERIOD == !!year) %>%
    select(REF_AREA,INSTR_ASSET,OBS_VALUE,ACTIVITY,TIME_PERIOD,CURRENCY) %>%
    mutate(ISO2 = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL)) %>%
    unite(industry, c('REF_AREA','ACTIVITY'),remove = F) %>%
    group_by(industry) %>%
    filter(any(INSTR_ASSET == 'N11N')) %>%
    mutate(ASSET = substr(INSTR_ASSET,1,nchar(INSTR_ASSET)-1),
           level = nchar(ASSET)) %>%
    ungroup()

   return(fixed_assets_sector)


}

import_klems_capital_accounts = function()
{
  klems_capital_accounts = get_cache_file("https://www.dropbox.com/s/sp2p4m86et66nfg/capital%20accounts.csv?dl=1",pattern = 'raw.klems') %>%
    read.csv()

  return(klems_capital_accounts)
}

import_oecd_exchange_rates = function()
{
  oecd_rates = read_parquet(here('data','oecd_exchange_rates.parquet')) %>% #National currency per US dollar, 2010-2024
    select(REF_AREA,CURRENCY,RATE = OBS_VALUE,year = TIME_PERIOD) %>%
    group_by(year) %>%
    reframe(EXCH_RATE = case_when(CURRENCY == 'EUR' ~ 1,
                                  T ~ RATE / RATE[REF_AREA %in% c('EU27_2020','EU')][1]),
            ISO2 = countrycode(REF_AREA,"iso3c","iso2c",nomatch = NULL),
            REF_AREA,RATE) #RATE : national currency per US dollar

  return(oecd_rates)
}

import_pwt_data = function(year,verbose = T)
{
  pwt_data = get_cache_file("https://dataverse.nl/api/access/datafile/554105",
                            pattern = 'PWT_DATA') %>%
    read_xlsx(sheet = 3)

  formatted_pwt =
    pwt_data %>%
    filter(year == !!year) %>%
    select(countrycode,cgdpe,cgdpo,pop) %>%
    reframe(country = countrycode(countrycode,'iso3c','iso2c',nomatch = NULL),
            GDP_CAPITA = ((cgdpe + cgdpo) / 2) / pop)

  return(formatted_pwt)
}

import_un_ratios = function(year)
{
  UN_AGGREGATES =
    data.frame(id = c(2,3,13,16),
               aggregate = c('P2','B1G','P51C','P51G'))

  UN_DATA =
    map_dfr(.x = UN_AGGREGATES$id,
            .f = function(x){
              paste0("https://data.un.org/legacy/Handlers/DownloadHandler.ashx?DataFilter=",
                     "group_code:206;fiscal_year:2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024;",
                     "item_code:",x,"&DataMartId=SNA&Format=scsv") %>%
                get_cache_file(link = .,pattern = paste0('UN_STATS_',x)) %>%
                unzip(exdir = tempdir()) %>%
                read.csv(sep = ";",check.names = F)
            })


  proxy.un_data =
    UN_DATA %>%
    filter(`SNA93 Item Code` %in% c('K.1','P.51','B.1g')) %>%
    mutate(iso2 = countrycode(`Country or Area`,'country.name.en','iso2c',nomatch = NULL),
           diff_year = abs(as.numeric(Year) - as.numeric(year))) %>%
    select(iso2,`Sub Item`,`SNA93 Item Code`,Currency,Value,Year,diff_year,Series) %>%
    group_by(iso2,`Sub Item`) %>%
    filter(diff_year == min(diff_year)) %>% #naligning the reference year
    mutate(max_series = max(Series)) %>%
    filter('K.1' %in% `SNA93 Item Code` & Series == max_series) %>%
    summarise(P51C = Value[`SNA93 Item Code` == 'K.1'],
              P51C_P51G = case_when('P.51' %in% `SNA93 Item Code` ~ Value[`SNA93 Item Code` == 'K.1'] / Value[`SNA93 Item Code` == 'P.51'],
                                    T ~ NA),
              P51C_B1G = case_when('B.1g' %in% `SNA93 Item Code` && isTRUE(Value[`SNA93 Item Code` == 'B.1g'][1] > 1) ~ Value[`SNA93 Item Code` == 'K.1'] / Value[`SNA93 Item Code` == 'B.1g'],
                                   T ~ NA), #Negative or near-zero GVA are removed
              P51G = case_when('P.51' %in% `SNA93 Item Code` ~ Value[`SNA93 Item Code` == 'P.51'],
                               T ~ NA),
              B1G = case_when('B.1g' %in% `SNA93 Item Code` ~ Value[`SNA93 Item Code` == 'B.1g'],
                              T ~ NA))

  return(proxy.un_data)

}

import_cfc_eurostat = function(year)
{

  #CFC / GVA by country and industry from Eurostat nama_10_a64 (P51C / B1G, current prices, M€) mapped to FIGARO codes

  figaro_industry_list = c("A01","A02","A03","B","C10T12","C13T15","C16","C17","C18","C19","C20","C21","C22",
                           "C23","C24","C25","C26","C27","C28","C29","C30","C31_32","C33","D35","E36","E37T39",
                           "F","G45","G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60","J61","J62_63",
                           "K64","K65","K66","L","M69_70","M71","M72","M73","M74_75","N77","N78","N79","N80T82",
                           "O84","P85","Q86","Q87_88","R90T92","R93","S94","S95","S96","T","U",
                           "A","C","F","G","H","I","J","K","L","M","N","R","S","T","U",
                           "B+C+D+E","M+N","O+P+Q","R+S+T","_T")

  cfc.eurostat = get_eurostat("nama_10_a64",
                              filters = list(na_item = c("P51C","B1G"), unit = "CP_MEUR", time_period = year),
                              cache   = FALSE
  ) %>%
    mutate(
      nace_r2 = case_when(
        nace_r2 == "D"     ~ "D35",
        nace_r2 == "O"     ~ "O84",
        nace_r2 == "P"     ~ "P85",
        grepl("-",nace_r2) ~ paste0(substr(nace_r2,1,3),"T",substr(nace_r2,6,7)),
        grepl("_",nace_r2) ~ paste0(substr(nace_r2,1,4),substr(nace_r2,6,7)),
        TRUE ~ nace_r2),
      geo     = case_when(
        geo == "EL" ~ "GR",   # Greece old code
        geo == "UK" ~ "GB",   # United Kingdom ISO-2
        TRUE        ~ geo
      )
    ) %>%
    filter(nace_r2 %in% figaro_industry_list & nchar(geo) == 2) %>%
    distinct(geo,nace_r2,na_item,.keep_all = TRUE) %>%
    select(geo,nace_r2,na_item,values) %>%
    pivot_wider(names_from = na_item,values_from = values) %>%
    filter(B1G > 1) %>% #Negative or near-zero GVA are removed
    transmute(country = geo,
              code = nace_r2,
              ratio = P51C / B1G) %>%
    filter(is.finite(ratio))

  return(cfc.eurostat)

}

import_exiobase_ratios = function(year,verbose = T)
{

  #EXIOBASE 3 (ixi) national consumption of fixed capital over gross value added
  #FIGW1 takes the GVA-weighted ratio of the five regions.

  zip_file = get_cache_file(paste0('https://zenodo.org/api/records/23039074/files/IOT_',year,'_ixi.zip/content'),pattern = paste0('EXIOBASE_',year))

  lines = strsplit(readLines(unz(zip_file,'factor_inputs/F.txt'),warn = FALSE),'\t')

  factors = lines[-(1:3)]

  values = sapply(factors,function(l) suppressWarnings(as.numeric(l[-1])))

  labels = sapply(factors,function(l) l[1])

  totals = data.frame(region = lines[[1]][-1],
                      cfc = rowSums(values[,grepl('Consumption of fixed capital',labels),drop = FALSE]),
                      gva = rowSums(values[,!grepl('Taxes less subsidies on products',labels),drop = FALSE])) %>%
    group_by(region) %>%
    summarise(across(c(cfc,gva),sum),.groups = 'drop') %>%
    mutate(ratio = cfc / gva) %>%
    filter(is.finite(ratio))

  region_of = c(AL = 'WE',ME = 'WE',AR = 'WL',SA = 'WM')

  row_regions = totals %>% filter(region %in% c('WA','WE','WF','WL','WM'))

  return(rbind(totals %>% filter(!region %in% row_regions$region) %>% transmute(country = region,ratio_exio = ratio),
               data.frame(country = names(region_of),ratio_exio = totals$ratio[match(region_of,totals$region)]),
               data.frame(country = 'FIGW1',ratio_exio = sum(row_regions$cfc) / sum(row_regions$gva))))

}

import_ghg_data = function(year,figaro,verbose)
{

  production.figaro = figaro %>%
    colSums() %>%
    subset(!grepl('P3|P5',names(.))) %>%
    data.frame(production = .) %>%
    rownames_to_column('id') %>%
    separate(id,c('country','industry'),extra = 'merge',sep = "_")

  figaro_industry_list = c("A01","A02","A03","B","C10T12","C13T15","C16","C17","C18","C19","C20","C21","C22",
                           "C23","C24","C25","C26","C27","C28","C29","C30","C31_32","C33","D35","E36","E37T39",
                           "F","G45","G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60","J61","J62_63",
                           "K64","K65","K66","L","M69_70","M71","M72","M73","M74_75","N77","N78","N79","N80T82",
                           "O84","P85","Q86","Q87_88","R90T92","R93","S94","S95","S96","T","U")

  ghg.eurostat = get_eurostat("env_ac_ghgfp",
                              filters = list(na_item = "TOTAL", c_dest = "WORLD",time_period = year),
                              cache   = FALSE
  ) %>%
    mutate(
      nace_r2 = case_when(
        nace_r2 == "D"     ~ "D35",
        nace_r2 == "O"     ~ "O84",
        nace_r2 == "P"     ~ "P85",
        grepl("-",nace_r2) ~ paste0(substr(nace_r2,1,3),"T",substr(nace_r2,6,7)),
        grepl("_",nace_r2) ~ paste0(substr(nace_r2,1,4),substr(nace_r2,6,7)),
        TRUE ~ nace_r2),
      c_orig  = case_when(
        c_orig  == "EL"       ~ "GR",   # Greece old code
        c_orig  == "UK"       ~ "GB",   # United Kingdom ISO‑2
        c_orig  == "WRL_REST" ~ "FIGW1",# Rest‑of‑World aggregate
        TRUE                  ~ c_orig
      )
    ) %>%
    filter(nace_r2 %in% c(figaro_industry_list,'HH') & (nchar(c_orig) == 2 | c_orig == 'FIGW1')) %>%
    group_by(time_period = substr(time,1,4),
             ref_area = c_orig,
             industry = nace_r2,
             counterpart_area = c_dest) %>%
    summarise(obs_value = sum(values),
              sto = 'ALL') %>%
    arrange(time_period,ref_area,industry) %>%
    ungroup()

  ghg.eurostat = production.figaro %>%
    left_join(ghg.eurostat %>% select(ref_area,industry,ghg = obs_value),by = c('country' = 'ref_area','industry')) %>%
    mutate(country2 = case_when(is.na(ghg) ~ 'FIGW1', # Countries without emission data are pooled with the rest of the world (FIGARO from 46 -> 50 countries)
                                T ~ country)) %>%
    group_by(country2,industry) %>%
    mutate(production2 = sum(production),
           ghg2 = sum(ghg,na.rm = T)) %>%
    mutate(intensity = case_when(production2 == 0 ~ 0,
                                 T ~ ghg2 / production2)) %>% # Pooled emission intensity = total emissions / total output
    ungroup() %>%
    mutate(modelled_ghg = intensity * production) %>% # Applied to each member's output
    select(country,industry,value = modelled_ghg) %>%
    arrange(country,industry)

  return(ghg.eurostat)


}

import_cfc_national = function(year)
{
  # CFC/GVA rates from national sources China NBS, India MoSPI, Russia Rosstat, Albania INSTAT

  # Albania: INSTAT SUT, 64 industries already in FIGARO codes ('L68' = FIGARO 'L'), nearest year.
  albania = read.csv(here('data','al_instat_sut_cfc.csv')) %>%
    mutate(year = as.numeric(year),
           diff = abs(year - !!year)) %>%
    filter(diff == min(diff)) %>%
    filter(!is.na(p51c) & !is.na(va) & va > 1) %>%
    mutate(industry = case_when(nace == 'L68' ~ 'L',
                                T ~ nace)) %>%
    group_by(industry) %>%
    summarise(ratio = sum(p51c) / sum(va),.groups = 'drop') %>%
    mutate(country = 'AL',source = 'INSTAT')

  # China: NBS input-output tables, 13 groups of industries. Linear interpolation of the group ratio between benchmark years
  china = rbind(read.csv(here('data','cn_nbs_io_shares.csv')) %>%
                  select(year,sector,ratio = cfc_over_va),
                read.csv(here('data','cn_nbs_io_absolute.csv')) %>%
                  select(year,sector,ratio = cfc_over_va)) %>%
    filter(!is.na(ratio)) %>%
    mutate(year = as.numeric(year)) %>%
    left_join(get_correspondence_national_cfc('CN') %>% select(sector = national_code,industry),
              by = 'sector',relationship = 'many-to-many') %>%
    filter(!is.na(industry)) %>%
    group_by(year,industry) %>%
    summarise(ratio = mean(ratio),.groups = 'drop') %>%
    group_by(industry) %>%
    arrange(year,.by_group = T) %>%
    summarise(ratio = if (n() == 1 || !!year <= min(year)) ratio[year == min(year)] else if (!!year >= max(year)) ratio[year == max(year)] else approx(x = year,y = ratio,xout = !!year)$y,.groups = 'drop') %>%
    mutate(country = 'CN',source = 'NBS')

  # India: MoSPI SUT, fiscal year starting in `year`, nearest otherwise.
  india = read.csv(here('data','in_mospi_sut_cfc.csv')) %>%
    mutate(diff = abs(fy_start - !!year)) %>%
    filter(diff == min(diff)) %>%
    filter(!is.na(cfc) & !is.na(gva) & gva > 1) %>%
    left_join(get_correspondence_national_cfc('IN') %>% select(national_code,industry) %>% distinct(),
              by = c('industry' = 'national_code'),relationship = 'many-to-many') %>%
    filter(!is.na(industry)) %>%
    group_by(industry.y) %>%
    summarise(ratio = sum(cfc) / sum(gva),.groups = 'drop') %>%
    rename(industry = industry.y) %>%
    mutate(country = 'IN',source = 'MoSPI')

  # Russia: Rosstat SUT, nearest available year
  russia = read.csv(here('data','ru_rosstat_sut_cfc.csv')) %>%
    mutate(national_classification = classification) %>%
    filter(national_classification == case_when(!!year <= 2015 ~ 'ISIC3',
                                              T ~ 'NACE2')) %>%
    mutate(diff = abs(year - !!year)) %>%
    filter(diff == min(diff)) %>%
    filter(!is.na(p51c) & !is.na(b1g) & b1g > 1) %>%
    left_join(get_correspondence_national_cfc('RU') %>% select(classification,national_code,industry) %>% distinct(),
              by = c('national_classification' = 'classification','code' = 'national_code'),relationship = 'many-to-many') %>%
    filter(!is.na(industry)) %>%
    group_by(industry) %>%
    summarise(ratio = sum(p51c) / sum(b1g),.groups = 'drop') %>%
    mutate(country = 'RU',source = 'Rosstat')

  cfc_national = rbind(albania,china,india,russia) %>%
    select(country,code = industry,ratio,source) %>%
    arrange(country,code)

  return(cfc_national)
}

import_klems_vintage_panel = function(first_year = 2010,last_year = 2021)
{

  klems_vintage_data = import_klems_capital_accounts() %>%
    filter(!grepl('^(EA|EU)',geo_code)) %>%
    arrange(geo_code,nace_r2_code,year) %>%
    filter(year >= (as.numeric(first_year) - 1) & year <= last_year) %>%
    select(nace_r2_code,geo_code,year,all_of(paste0(rep(c('K','Kpyp','Ipyp'),each = length(KLEMS_ASSETS)),'_',KLEMS_ASSETS))) %>%
    pivot_longer(-c(1:3),names_to = c('FLOW','ASSET'),names_pattern = "^(K|Kpyp|Ipyp)_(.*)$") %>%
    group_by(nace_r2_code,geo_code,ASSET)

  #One row = one country, one EU KLEMS sector and one asset
  country_sector_asset = group_keys(klems_vintage_data)

  cell_data = group_split(klems_vintage_data)

  # The stock of the year before the first vintage is needed, except when the first vintage is the first year of EU KLEMS (1995)

  years = max(first_year - 1,min(klems_vintage_data$year)):last_year

  vintages = first_year:last_year

  #Asset vintages
  V = array(0,c(nrow(country_sector_asset),length(years),length(vintages)),dimnames = list(NULL,years,vintages))

  #Chained-prices converter
  rho = matrix(1,nrow(country_sector_asset),length(years),dimnames = list(NULL,years))

  for(i in 1:nrow(country_sector_asset))
  {

    sub_data = cell_data[[i]]

    #Before its first positive stock, the asset is absent from the sector
    positive_stock_years = sub_data$year[sub_data$FLOW == 'K' & is.finite(sub_data$value) & sub_data$value > 0]

    if(length(positive_stock_years) == 0) next

    first_stock_year = min(positive_stock_years)

    stock = data.frame(acquisition_year = first_stock_year,
                       value = sub_data$value[sub_data$FLOW == 'K' & sub_data$year == first_stock_year]) %>%
      `colnames<-`(c('acquisition_year', as.character(first_stock_year)))

    for(j in years[years > first_stock_year])
    {
      previous_year = j - 1

      K_previous = sub_data$value[sub_data$FLOW == 'K'    & sub_data$year == previous_year]
      K_j        = sub_data$value[sub_data$FLOW == 'K'    & sub_data$year == j]
      Kpyp_j     = sub_data$value[sub_data$FLOW == 'Kpyp' & sub_data$year == j]
      Ipyp_j     = sub_data$value[sub_data$FLOW == 'Ipyp' & sub_data$year == j]

      #usable = handling several time series punctual missing data.

      usable = length(K_previous) == 1 && length(K_j) == 1 && length(Kpyp_j) == 1 && length(Ipyp_j) == 1 &&
        is.finite(K_previous) && is.finite(K_j) && is.finite(Kpyp_j) && is.finite(Ipyp_j) && K_previous > 0 && Kpyp_j > 0

      if(!usable)
      {
        #No stock to carry over : the series restarts from the stock of the year, if it is positive
        if(length(K_j) == 1 && is.finite(K_j) && K_j > 0)
        {
          new_column = c(rep(0, nrow(stock)), K_j)
          stock[nrow(stock) + 1, ] = 0
          stock$acquisition_year[nrow(stock)] = j
          stock[[as.character(j)]] = new_column
        }
        next
      }
      #Negative investment is treated as no investment, and a vintage cannot lose more than its value : the vintages keep adding up to the stock
      implicit_rate   = min(1 - (Kpyp_j - max(Ipyp_j,0)) / K_previous,1)
      price_valuation = K_j / Kpyp_j

      # All vintages are depreciated and revalued before accounting for new investment

      new_column = c(stock[[as.character(previous_year)]] * (1 - implicit_rate) * price_valuation,
                     max(Kpyp_j - (1 - implicit_rate) * K_previous, 0) * price_valuation)
      stock[nrow(stock) + 1, ] = 0
      stock$acquisition_year[nrow(stock)] = j
      stock[[as.character(j)]] = new_column
    }

    stock_acquisition_years = stock$acquisition_year

    stock = as.matrix(stock[,-1,drop = FALSE])
    rownames(stock) = stock_acquisition_years

    years_covered = match(colnames(stock),as.character(years))

    # The first vintage is a bucket : all the vintages acquired up to first_year

    bucket = rownames(stock)[as.numeric(rownames(stock)) <= first_year]

    V[i,years_covered,1] = colSums(stock[bucket,,drop = FALSE])

    # The next vintages keep their own column, from their acquisition year on

    for(v in rownames(stock)[as.numeric(rownames(stock)) > first_year])
    {
      years_since_v = colnames(stock)[as.numeric(colnames(stock)) >= as.numeric(v)]

      V[i,match(years_since_v,as.character(years)),v] = stock[v,years_since_v]
    }

    #Change of the price of the stock, 1 when the stock is missing

    price_change = sub_data$value[sub_data$FLOW == 'K' & sub_data$year %in% years] /
      sub_data$value[sub_data$FLOW == 'Kpyp' & sub_data$year %in% years]

    rho[i,] = ifelse(is.finite(price_change) & price_change > 0,price_change,1)

  }

  return(list(country_sector_asset = country_sector_asset,V = V,rho = rho,years = years,vintages = vintages))
}
