import_figaro_io_table = function(year,verbose = T)
{

  links =
    matrix(byrow = T,ncol = 2,
           data = c('2010', 'https://circabc.europa.eu/rest/download/fc80f855-d144-476e-b4bf-5cfba946819c',
                    '2011', 'https://circabc.europa.eu/rest/download/1bcb2624-04ed-43e1-8588-df6680ed352a',
                    '2012', 'https://circabc.europa.eu/rest/download/399671ad-cbb3-493e-ad5f-83e989f1eecc',
                    '2013', 'https://circabc.europa.eu/rest/download/a2b4746d-1d11-4a44-ab1c-50ac956f0849',
                    '2014', 'https://circabc.europa.eu/rest/download/beba57b2-2696-497a-b92f-2a5beca724c7',
                    '2015', 'https://circabc.europa.eu/rest/download/1a194b8c-6ea1-4bec-9c73-0cd599febcc3',
                    '2016', 'https://circabc.europa.eu/rest/download/2cdd74fc-0bce-4ae0-8bf2-34d34546d86d',
                    '2017', 'https://circabc.europa.eu/rest/download/4a11c796-4186-4cce-a02f-12d353fc5e59',
                    '2018', 'https://circabc.europa.eu/rest/download/7a57a374-2200-498c-bb5f-cee24202b0b8',
                    '2019', 'https://circabc.europa.eu/rest/download/c3467617-8a00-44a0-9b6b-ccad8a2ab58d',
                    '2020', 'https://circabc.europa.eu/rest/download/4df668e1-2a8a-4e84-ae57-00309d8bc760',
                    '2021', 'https://circabc.europa.eu/rest/download/6736dea8-da14-450f-b212-a791baf238c8',
                    '2022', 'https://circabc.europa.eu/rest/download/b20c339d-984f-413c-a499-54ff76beb90c',
                    '2023', 'https://circabc.europa.eu/rest/download/21557f49-1e94-431c-8523-d972fec020b8')) %>%
    as.data.frame() %>%
    `colnames<-`(c('year','link'))

  main.file = curl_fetch_memory(links %>% filter(year == !!year) %>% pull(link))

  header.file = sub('.*filename="?([^"]+)"?', '\\1', parse_headers_list(main.file$headers)$`content-disposition`)

  matrix.file = read.csv(text = rawToChar(main.file$content)) %>%
    column_to_rownames('rowLabels')

  if(verbose) message('FIGARO input-output table successfully fetched for year ', year,' : ',header.file)

  return(matrix.file)
}

import_figaro_supply_table = function(year,verbose = T)
{

  links = data.frame(year = 2010:2023,
                     link = c('https://circabc.europa.eu/rest/download/f3696c33-2805-4987-97d3-97ee1272d242',
                              'https://circabc.europa.eu/rest/download/87ea271a-ed39-4f20-a9a9-4801d0b8f94b',
                              'https://circabc.europa.eu/rest/download/ed13e4b8-1c7f-4dd2-90cd-b255700f9b7a',
                              'https://circabc.europa.eu/rest/download/41578433-53b9-4df3-a302-ca181aa6bd18',
                              'https://circabc.europa.eu/rest/download/baf38842-8436-4304-8427-67a6bbe43f98',
                              'https://circabc.europa.eu/rest/download/cbda3980-8e14-4ae0-aad5-0b0e2d29f7cf',
                              'https://circabc.europa.eu/rest/download/2c21a1a8-1707-4f5a-8490-baee61459f00',
                              'https://circabc.europa.eu/rest/download/17e838f1-002b-485f-af28-c71df9c73734',
                              'https://circabc.europa.eu/rest/download/e4f62e56-4750-455f-bafb-36a15328f2e8',
                              'https://circabc.europa.eu/rest/download/2519eec0-356f-43c9-bb20-272b5421fe92',
                              'https://circabc.europa.eu/rest/download/08130533-528f-46b9-9573-e416bbe3a359',
                              'https://circabc.europa.eu/rest/download/1c336d72-12bd-45f6-b088-e27f0b1d3847',
                              'https://circabc.europa.eu/rest/download/76ccc1ab-6e8b-42fa-bf42-d3ba7a3dbea2',
                              'https://circabc.europa.eu/rest/download/53d49275-70fa-4f24-8d5a-5516208e590d'))



  main.file = curl_fetch_memory(links %>% filter(year == !!year) %>% pull(link))

  header.file = sub('.*filename="?([^"]+)"?', '\\1', parse_headers_list(main.file$headers)$`content-disposition`)

  matrix.file = read.csv(text = rawToChar(main.file$content)) %>%
    column_to_rownames('rowLabels')

  if(verbose) message('FIGARO supply table successfully fetched for year ', year,' : ',header.file)

  return(matrix.file)
}

import_figaro_use_table = function(year,verbose = T)
{

  links = data.frame(year = 2010:2023,
                     link = c('https://circabc.europa.eu/rest/download/676e18a8-0d71-42b2-a02a-f5c7410a0f9c',
                              'https://circabc.europa.eu/rest/download/6f278072-43a2-432c-8941-006bb58783d2',
                              'https://circabc.europa.eu/rest/download/a74c8d7c-10ea-45fe-86f5-095739f3caf8',
                              'https://circabc.europa.eu/rest/download/8ef90aea-3af2-45ab-9798-fc63bf40957d',
                              'https://circabc.europa.eu/rest/download/28c880a6-804e-4658-a329-c8a53de6fa93',
                              'https://circabc.europa.eu/rest/download/05bcbd7e-3c69-41ef-b39a-42c42aae2aed',
                              'https://circabc.europa.eu/rest/download/82f057d7-2c86-46c9-88a6-9cde93bd3c94',
                              'https://circabc.europa.eu/rest/download/f21820f2-1fab-40b6-81af-3cf0bbd25e7b',
                              'https://circabc.europa.eu/rest/download/4c347027-f8b6-45ee-9760-b544c249fd33',
                              'https://circabc.europa.eu/rest/download/21f555fd-bf43-41ca-a9fe-94078544262f',
                              'https://circabc.europa.eu/rest/download/2920767b-92eb-4da7-88e8-cd9f96ee1649',
                              'https://circabc.europa.eu/rest/download/b3b3e7a0-c1f5-4279-a941-17d6f45acd29',
                              'https://circabc.europa.eu/rest/download/e0722dfb-42ef-4520-b365-d675559f2100',
                              'https://circabc.europa.eu/rest/download/35a4d178-3285-46c8-befd-2e7f26803686'))



  main.file = curl_fetch_memory(links %>% filter(year == !!year) %>% pull(link))

  header.file = sub('.*filename="?([^"]+)"?', '\\1', parse_headers_list(main.file$headers)$`content-disposition`)

  matrix.file = read.csv(text = rawToChar(main.file$content)) %>%
    column_to_rownames('rowLabels')

  if(verbose) message('FIGARO use table successfully fetched for year ', year,' : ',header.file)

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

  write_parquet(depreciations.klems,
                tempfile(pattern = pattern))

  if(verbose) message("Data cached in temporary files")

  return(depreciations.klems)
}

import_oecd_fixed_asset = function(year,verbose = T)
{
  fixed_assets_sector = get_cache_file(paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE9A,/A.........V..?startPeriod=',
                                              year,"&endPeriod=",year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels'),
                                       pattern = 'OECD_FIXED_ASSETS_SECTORS') %>%
    read.csv(check.names = F) %>%
    select(REF_AREA,INSTR_ASSET,`Financial instruments and non-financial assets`,OBS_VALUE,ACTIVITY,TIME_PERIOD,CURRENCY) %>%
    mutate(ISO2 = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL)) %>%
    unite(industry, c('REF_AREA','ACTIVITY'),remove = F) %>%
    group_by(industry) %>%
    filter(any(INSTR_ASSET == 'N11N')) %>%
    mutate(ASSET = substr(INSTR_ASSET,1,nchar(INSTR_ASSET)-1),
           level = nchar(ASSET)) %>%
    ungroup()

  #We delete aggregate when details are available in order to avoid double counting

  for(i in sort(unique(fixed_assets_sector$level),decreasing = T))
  {

    asset_level = fixed_assets_sector %>% filter(level == i) %>% pull(ASSET)

    asset_aggregate = substr(asset_level,1,nchar(asset_level)-1) %>% unique()

    to_delete = fixed_assets_sector %>% group_by(industry) %>% filter(any(ASSET %in% asset_level)) %>% filter(ASSET %in% asset_aggregate) %>% ungroup() %>% distinct(industry,INSTR_ASSET)

    fixed_assets_sector = fixed_assets_sector %>% filter(!paste0(industry,INSTR_ASSET) %in% paste0(to_delete$industry,to_delete$INSTR_ASSET))

  }

   return(fixed_assets_sector)


}

import_pwt_data = function(year,verbose = T)
{
  pwt_data = get_cache_file("https://dataverse.nl/api/access/datafile/554105",
                            pattern = 'PWT_DATA') %>%
    read_xlsx(sheet = 3)

  formatted_pwt =
    pwt_data %>%
    filter(year == !!year) %>%
    select(countrycode,cn,delta,cgdpe,cgdpo,pop) %>%
    reframe(country = countrycode(countrycode,'iso3c','iso2c',nomatch = NULL),
            GDP_CAPITA = ((cgdpe + cgdpo) / 2) / pop,
            CFC = cn * delta,
            CFC_VA = CFC / ((cgdpe + cgdpo) / 2))

  return(formatted_pwt)

  #cn capital stock million US$
  #delta average depreciation rate of the capital stock
  #cgdpe/cgdpo real gdp
}

import_oecd_value_added = function(year,verbose = T)
{
  oecd_value_added = get_cache_file(paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,2.0/A....B1G....XDC.V..?startPeriod=',
                                           year,'&endPeriod=',year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels'),
                                    pattern = paste0("OECD_VALUE_ADDED_",year)) %>%
    read.csv() %>%
    select(REF_AREA,ACTIVITY,VALUE_ADDED = OBS_VALUE) %>%
    mutate(ISO2 = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL)) %>%
    mutate(ACTIVITY = case_when(ACTIVITY == 'D' ~ 'D35',
                                ACTIVITY == 'O' ~ 'O84',
                                ACTIVITY == 'P' ~ 'P85',
                                T ~ ACTIVITY))

  return(oecd_value_added)

}

import_oecd_gfcf = function(year,verbose = T)
{

  oecd_investment = get_cache_file(paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE8,2.0/A....P51G....XDC.V..?startPeriod=',
                                           year,'&endPeriod=',year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels'),
                                    pattern = paste0("OECD_INVESTMENT_",year)) %>%
    read.csv() %>%
    select(REF_AREA,ACTIVITY,INVESTMENT = OBS_VALUE,INSTR_ASSET) %>%
    mutate(ISO2 = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL)) %>%
    mutate(ACTIVITY = case_when(ACTIVITY == 'D' ~ 'D35',
                                ACTIVITY == 'O' ~ 'O84',
                                ACTIVITY == 'P' ~ 'P85',
                                T ~ ACTIVITY))

  return(oecd_investment)

}

import_un_ratios = function(year)
{
  UN_AGGREGATES =
    data.frame(id = c(2,3,13,16),
               aggregate = c('P2','B1G','P51C','P51G'))

  UN_DATA =
    map_dfr(.x = UN_AGGREGATES$id,
            .f = function(x){
              paste0("https://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=",
                     "group_code:206;fiscal_year:2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024;",
                     "item_code:",x,"&DataMartId=SNA&Format=scsv") %>% #We fetch P2,B1G,P51C,P51G
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
    group_by(iso2) %>%
    filter(diff_year == min(diff_year)) %>%
    group_by(iso2,`Sub Item`) %>%
    mutate(max_series = max(Series)) %>%
    filter('K.1' %in% `SNA93 Item Code` & Series == max_series) %>%
    summarise(P51C = Value[`SNA93 Item Code` == 'K.1'],
              P51C_P51G = case_when('P.51' %in% `SNA93 Item Code` ~ Value[`SNA93 Item Code` == 'K.1'] / Value[`SNA93 Item Code` == 'P.51'],
                                    T ~ NA),
              P51C_B1G = case_when('B.1g' %in% `SNA93 Item Code` ~ Value[`SNA93 Item Code` == 'K.1'] / Value[`SNA93 Item Code` == 'B.1g'],
                                   T ~ NA),
              P51G = case_when('P.51' %in% `SNA93 Item Code` ~ Value[`SNA93 Item Code` == 'P.51'],
                               T ~ NA),
              B1G = case_when('B.1g' %in% `SNA93 Item Code` ~ Value[`SNA93 Item Code` == 'B.1g'],
                              T ~ NA))

  return(proxy.un_data)

}

import_ghg_data = function(year,figaro,verbose)
{

  value_added.figaro = figaro %>%
    filter(grepl('W2',rownames(.))) %>%
    colSums() %>%
    subset(!grepl('P3|P5',names(.))) %>%
    data.frame(value_added = .) %>%
    rownames_to_column('id') %>%
    separate(id,c('country','industry'),extra = 'merge',sep = "_")

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

  ghg.eurostat = value_added.figaro %>%
    merge(production.figaro) %>%
    left_join(ghg.eurostat %>% select(ref_area,industry,ghg = obs_value),by = c('country' = 'ref_area','industry')) %>%
    mutate(country2 = case_when(is.na(ghg) ~ 'FIGW1',
                                T ~ country)) %>%
    group_by(country2,industry) %>%
    mutate(value_added2 = sum(value_added),
           production2 = sum(production),
           ghg2 = sum(ghg,na.rm = T)) %>%
    mutate(GHG_VA = ghg2 / value_added2,
           GHG_PRD = ghg2 / production2) %>%
    mutate(intensity_type = case_when(value_added2 <= 0 & production2 > 0 ~ 'GHG_PRD',
                                      T ~ 'GHG_VA'),
           intensity = case_when(production2 == 0 ~ 0,
                                 value_added2 <= 0 & production2 > 0 ~ GHG_PRD,
                                 T ~ GHG_VA)) %>%
    ungroup() %>%
    mutate(modelled_ghg = intensity * case_when(intensity_type == 'GHG_VA' ~ value_added,
                                                T ~ production)) %>%
    select(country,industry,value = modelled_ghg) %>%
    arrange(country,industry)

  return(ghg.eurostat)


}
