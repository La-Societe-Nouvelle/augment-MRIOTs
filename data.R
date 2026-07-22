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


  main.file = curl_fetch_memory(links %>% filter(year == !!year) %>% pull(link))

  header.file = sub('.*filename="?([^"]+)"?', '\\1', parse_headers_list(main.file$headers)$`content-disposition`)

  matrix.file = read.csv(text = rawToChar(main.file$content)) %>%
    column_to_rownames('rowLabels')

  if(verbose) message('FIGARO input-output table successfully fetched for year ', year,' : ',header.file)

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



  main.file = curl_fetch_memory(links %>% filter(year == !!year) %>% pull(link))

  header.file = sub('.*filename="?([^"]+)"?', '\\1', parse_headers_list(main.file$headers)$`content-disposition`)

  matrix.file = read.csv(text = rawToChar(main.file$content)) %>%
    column_to_rownames('rowLabels')

  if(verbose) message('FIGARO supply table successfully fetched for year ', year,' : ',header.file)

  return(matrix.file)
}

import_figaro_use_table = function(year,verbose = T)
{

  links = data.frame(year = 2010:2024,
                     link = c('https://circabc.europa.eu/rest/download/db38347b-7e46-4955-af4d-98c3ba43ae8d',
                              'https://circabc.europa.eu/rest/download/8cb37f01-c3ce-45a7-9dd3-278e5297a7e1',
                              'https://circabc.europa.eu/rest/download/d95771ae-34a2-496b-8cdc-63248637e520',
                              'https://circabc.europa.eu/rest/download/7b76a69b-6e34-42ec-8f50-b105f01c3a39',
                              'https://circabc.europa.eu/rest/download/b5e4d84d-a3b0-486a-8634-40119435141a',
                              'https://circabc.europa.eu/rest/download/534f1856-46cb-4860-a8b1-c6f4fb2ff43d',
                              'https://circabc.europa.eu/rest/download/b9ef08dd-835d-4c97-bcaa-f4ec09f3c289',
                              'https://circabc.europa.eu/rest/download/3c946634-83c7-4fa3-8a0d-7c4ebd36fb2f',
                              'https://circabc.europa.eu/rest/download/428ebe8c-5358-48e1-a264-fd79460cefda',
                              'https://circabc.europa.eu/rest/download/0e611ff2-4669-4efb-8d0a-b4ef9016f260',
                              'https://circabc.europa.eu/rest/download/a0a606de-c1cc-4a50-b536-b8e5975a1db4',
                              'https://circabc.europa.eu/rest/download/242f18be-5df2-4c90-b5a6-ee838990d464',
                              'https://circabc.europa.eu/rest/download/6f0df065-c4e1-459b-9da3-3f89d2b9ebf5',
                              'https://circabc.europa.eu/rest/download/f6c0570a-f0d3-44ee-8604-a352d9d67006',
                              'https://circabc.europa.eu/rest/download/ad5952ca-53a9-4950-a4fd-b11f30540e80'))



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

  depreciations.klems = rbind(depreciations.klems,
                              depreciations.klems %>%
                                group_by(Code = case_when(Code %in% c('C20','C21') ~ 'C20-C21',
                                                          Code %in% c('C26','C27') ~ 'C26-C27',
                                                          Code %in% c('D','E') ~ 'D-E'),
                                         product) %>%
                                filter(!is.na(Code)) %>%
                                mutate(rate = mean(rate)) %>%
                                ungroup())

  write_parquet(depreciations.klems,
                tempfile(pattern = pattern))

  if(verbose) message("Data cached in temporary files")

  return(depreciations.klems)
}

import_oecd_fixed_asset = function(year,verbose = T)
{
  fixed_assets_sector = get_cache_file(paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE9A,/A.........V..?startPeriod=',
                                              year,"&endPeriod=",year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels'),
                                       pattern = paste0('OECD_FIXED_ASSETS_SECTORS',year)) %>%
    read.csv(check.names = F) %>%
    select(REF_AREA,INSTR_ASSET,`Financial instruments and non-financial assets`,OBS_VALUE,ACTIVITY,TIME_PERIOD,CURRENCY) %>%
    mutate(ISO2 = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL)) %>%
    unite(industry, c('REF_AREA','ACTIVITY'),remove = F) %>%
    group_by(industry) %>%
    filter(any(INSTR_ASSET == 'N11N')) %>%
    mutate(ASSET = substr(INSTR_ASSET,1,nchar(INSTR_ASSET)-1),
           level = nchar(ASSET)) %>%
    ungroup()

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

import_exchange_rate_oecd <- function(year)
{

  oecd_exch = read.csv(paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAAG@DF_NAAG_IX,/A..EXC_A.XDC_USD.?startPeriod=",
                              year,"&endPeriod=", year,
                              "&dimensionAtObservation=AllDimensions&format=csvfile"))

  return(oecd_exch)

}
