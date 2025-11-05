x = c("tidyverse","curl","DBI","arrow","bir","icarus","readxl","melodi","countrycode","httr2",'rvest','countrycode','httr')

lapply(x,library,character.only = T)


#####FUNCTIONS#####

#From domestic currencies to euro

#Industry-technology construct (compilation)

#Build mapping functions for total capital (Total GCFC)

get_concordance_nace.klems = function(classification)
{
  if(toupper(classification) == 'FIGARO')
  {
  concordance_nace.klems = data.frame(nace_r2_code = c("A","B","C","C10-C12","C13-C15","C16-C18","C19","C20","C20-C21","C21","C22-C23","C24-C25","C26","C26-C27","C27","C28","C29-C30",
                                                       "C31-C33","D","D-E","E","F","G","G45","G46","G47","H","H49","H50","H51","H52","H53","I","J",
                                                       "J58-J60","J61","J62-J63","K","L","L68A","M","M-N","MARKT","MARKTxAG","N","O","O-Q","P","Q","Q86","Q87-Q88",
                                                       "R","R-S","S","T","TOT","TOT_IND","U")) %>%
    mutate(industry = case_when(nace_r2_code == 'A' ~ list(c('A01','A02','A03')),
                                nace_r2_code == 'B' ~ list('B'),
                                nace_r2_code == 'C10-C12' ~ list('C10T12'),
                                nace_r2_code == 'C13-C15' ~ list('C13T15'),
                                nace_r2_code == 'C16-C18' ~ list(c('C16','C17','C18')),
                                nace_r2_code == 'C22-C23' ~ list(c('C22','C23')),
                                nace_r2_code == 'C24-C25' ~ list(c('C24','C25')),
                                nace_r2_code == 'C29-C30' ~ list(c('C29','C30')),
                                nace_r2_code == 'C31-C33' ~ list(c('C31_C32','C33')),
                                nace_r2_code == 'D' ~ list('D35'),
                                nace_r2_code == 'E' ~ list(c('E36','E37T39')),
                                nace_r2_code == 'J58-J60' ~ list(c("J58",'J59_50')),
                                nace_r2_code == 'J62-J63' ~ list('J63_63'),
                                nace_r2_code == 'K' ~ list(c("K64","K65","K66")),
                                nace_r2_code == 'M' ~ list(c("M69_70","M71","M72","M73","M74_75")),
                                nace_r2_code == 'N' ~ list(c("N77","N78","N79","N80T82")),
                                nace_r2_code == 'O' ~ list("O84"),
                                nace_r2_code == 'P' ~ list("P85"),
                                nace_r2_code == 'Q87-Q88' ~ list("Q87_88"),
                                nace_r2_code == 'R' ~ list(c("R90T92","R93")),
                                nace_r2_code == 'S' ~ list(c("S94","S95","S96")),
                                T ~ NA))

  concordance_nace.klems$industry[concordance_nace.klems$nace_r2_code %in%
                                    c('C19','C20','C21','C26','C27','C28','F','G45','G46','G47','H49','H50','H51','H52','H53','I','J61','L','Q86','T','U')] =
    concordance_nace.klems$nace_r2_code[concordance_nace.klems$nace_r2_code %in%
                                          c('C19','C20','C21','C26','C27','C28','F','G45','G46','G47','H49','H50','H51','H52','H53','I','J61','L','Q86','T','U')]

  concordance_nace.klems = concordance_nace.klems %>%
    unnest(industry)
  }

  return(concordance_nace.klems)

}

get_concordance_product.klems = function(classification)
{

  if(toupper(classification) == 'FIGARO')
  {

  concordance_product.klems <- tibble(
  product = product <- c("CT", "Cult", "IT", "OCon", "OIPP", "OMach",
                         "RD", "Rstruc", "Soft_DB", "TraEq"),
  industry = case_when(
    product == "CT"       ~ list("C26"),                                           # Communications equipment
    product == "Cult"     ~ list(c("A01", "A02", "A03")),                         # Cultivated assets
    product == "IT"       ~ list("C26"),                                           # Computing equipment
    product == "OCon"     ~ list("F"),                                             # Total non-residential investment
    product == "OIPP"     ~ list(c("J58", "J59_60", "J61", "J62_63", "M69_70",
                                   "M71", "M72", "M73", "M74_75", "R90T92", "R93")), # Other IPP assets
    product == "OMach"    ~ list(c("C26", "C27", "C28")),                         # Other Machinery and Equipment
    product == "RD"       ~ list("M72"),                                           # Research and development
    product == "Rstruc"   ~ list("F"),                                             # Residential structures
    product == "Soft_DB"  ~ list("J62_63"),                                        # Computer software and databases
    product == "TraEq"    ~ list(c("C29", "C30")),                                 # Transport equipment
    TRUE ~ list(NA_character_)
  )
) %>%
  unnest(industry)

  }

  return(concordance_product.klems)
}

model_capital_distribution.klems = function(raw.klems,vector.gfcf,concordance_nace.klems,concordance_product.klems,verbose = T)
{
  modelling_results = c()

  if(!all(c('country','industry','value') %in% colnames(vector.gfcf))) stop('`vector.gfcf` is not formatted properly')

  if(!all(c('nace_r2_code','industry') %in% colnames(concordance_nace.klems))) stop('`concordance_nace.klems` is not formatted properly')

  if(!all(c('product','industry') %in% colnames(concordance_product.klems))) stop('`concordance_product.klems` is not formatted properly')

  for(country in unique(raw.klems$geo_code[raw.klems$geo_code %in% unique(vector.gfcf$country)]))
  {
    for(industry in unique(raw.klems$nace_r2_code))
    {
      distribution = concordance_product.klems %>%
        left_join(vector.gfcf %>% filter(country == !!country) %>% select(industry,GFCF = value),by = 'industry') %>%
        group_by(product) %>%
        mutate(share = GFCF / sum(GFCF,na.rm = T)) %>%
        select(!GFCF)

      country_industry.klems = raw.klems %>% filter(nace_r2_code == !!industry & geo_code == !!country) %>%
        filter(year == !!year) %>%
        pivot_longer(-c(1:6),names_to = 'type') %>%
        separate(type,c('flow','product'),sep = "_",extra = 'merge') %>%
        filter(product != 'GFCF')

      total_investment = raw.klems %>% filter(nace_r2_code == !!industry & geo_code == !!country & year == !!year) %>% pull(I_GFCF)

      investment = country_industry.klems %>% filter(flow == 'I') %>% select(product,value) %>%
        left_join(distribution,by = 'product') %>%
        group_by(industry) %>%
        summarise(value = sum(value * share,na.rm=T))

      if(!is.na(total_investment)) investment$value = investment$value / sum(investment$value,na.rm=T) * total_investment

      #Ensure distributed GFCF = total GFCF (margin)

      total_stock = raw.klems %>% filter(nace_r2_code == !!industry & geo_code == !!country & year == !!year) %>% pull(K_GFCF)

      stock = country_industry.klems %>% filter(flow == 'K') %>% select(product,value) %>% mutate(product = toupper(product))

      if(!is.na(total_stock)) stock$value = stock$value / sum(stock$value,na.rm=T) * total_stock

      #Ensure capital stock = total capital stock (margin)

      cfc = stock %>%
        left_join(depreciations.klems %>% filter(Code == !!industry) %>% mutate(product = toupper(product)),by = 'product') %>%
        reframe(product,value = rate * value) %>%
        left_join(distribution %>% mutate(product = toupper(product)),by = 'product') %>%
        group_by(industry) %>%
        summarise(value = sum(value * share,na.rm=T))

      modelling_results =
        rbind(modelling_results,
              investment %>%
                mutate(flow = 'GFCF') %>%
                rbind(cfc %>% mutate(flow = 'CFC')) %>%
                rename(product = industry) %>%
                filter(value > 0) %>%
                merge(data.frame(country = country,industry = industry)))
    }

    if(verbose) print(country)
  }

  return(modelling_results)

}

get_concordance_nace.un = function()
{
  concordance_nace.un =  data.frame(description = c("Total Economy","Agriculture, forestry and fishing (A)","Manufacturing, mining and quarrying and other industrial activities (B+C+D+E)",
                                                    "Mining and quarrying (B)","Manufacturing (C)","Electricity, gas, steam and air conditioning supply (D)",
                                                    "Water supply; sewerage, waste management and remediation activities (E)","Construction (F)",
                                                    "Wholesale and retail trade, transportation and storage, accommodation and food service activities (G+H+I)",
                                                    "Wholesale and retail trade; repair of motor vehicles and motorcycles (G)",
                                                    "Transportation and storage (H)","Accommodation and food service activities (I)","Information and communication (J)",
                                                    "Financial and insurance activities (K)","Real estate activities (L)",
                                                    "Professional, scientific, technical, administrative and support service activities (M+N)",
                                                    "Professional, scientific and technical activities (M)","Administrative and support service activities (N)",
                                                    "Public administration and defence, education, human health and social work activities (O+P+Q)",
                                                    "Public administration and defence; compulsory social security (O)","Education (P)","Human health and social work activities (Q)",
                                                    "Other service activities (R+S+T)","Arts, entertainment and recreation (R)","Other service activities (S)",
                                                    "Crop and animal production, hunting and related service activities (01)","Fishing and aquaculture (03)",
                                                    "Private households with employed persons (T)","Forestry and logging (02)","")) %>%

    mutate(group = sapply(description, function(x)sub(".*\\(([^)]+)\\).*", "\\1", x)),
           condition = gsub("\\+","|",group)) %>%
    rowwise() %>%
    mutate(industry = case_when(description == 'Total Economy' ~ list(unique(vector.gfcf$industry)),
                                group %in% c('01','02','03') ~ list(paste0('A',group)),
                                T ~ list(unique(vector.gfcf$industry[grepl(condition,substr(vector.gfcf$industry,1,1))])))) %>%
    filter(description != '')

  return(concordance_nace.un)
}

get_un_stats_cfc = function(year)
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
                curl_download(url = .,destfile = tempfile()) %>%
                unzip(exdir = tempdir()) %>%
                read.csv(sep = ";",check.names = F)
            })


  proxy.un_data =
    UN_DATA %>%
    filter(`SNA93 Item Code` %in% c('K.1','P.51')) %>%
    mutate(iso2 = countrycode(`Country or Area`,'country.name.en','iso2c',nomatch = NULL),
           diff_year = abs(as.numeric(Year) - as.numeric(year))) %>%
    filter(iso2 %in% unique(vector.gfcf$country)) %>%
    select(iso2,`Sub Item`,`SNA93 Item Code`,Currency,Value,Year,diff_year) %>%
    group_by(iso2) %>%
    filter(diff_year == min(diff_year)) %>%
    group_by(iso2,`Sub Item`) %>%
    summarise(P51C_P51G = unique(Value[`SNA93 Item Code` == 'K.1'] / Value[`SNA93 Item Code` == 'P.51']))

  return(proxy.un_data)

}

#Build mapping functions for origin-distributed capital (tbd)

#Footprint computations formula

#####FIGARO - IMPORT SUT#####

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

# matrix.use = import_figaro_use_table(year)
#
# matrix.supply = import_figaro_supply_table(year)
#
# matrix.input_output = import_figaro_io_table(year)

#####KLEMS - IMPORT CFC AND GFCF data#####

#LA KLEMS not accessible http://laklems.net/

#EU KLEMS https://euklems-intanprod-llee.luiss.it/download/

get_depreciations_rate_klems = function(update = F,verbose = T)
{

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

get_capital_distribution_klems = function(year,classification,verbose = T)
{
raw.klems = read.csv("https://www.dropbox.com/s/sp2p4m86et66nfg/capital%20accounts.csv?dl=1")

meta.klems = curl_download("https://www.dropbox.com/s/srsn4zdb1wta27t/Variable-List-2023.xlsx?dl=1",tempfile()) %>%
  read_xlsx()

matrix.gfcf = import_figaro_io_table(year) %>% filter(!grepl('W2',rownames(.))) %>% {.[,grepl('P51G',colnames(.))]} #TBD - When other IO

vector.gfcf = rowSums(matrix.gfcf) %>%
  as.data.frame() %>%
  `colnames<-`('value') %>%
  `rownames<-`(gsub("CPA_","",row.names(.))) %>%
  rownames_to_column('id') %>%
  separate(id,into = c('country','industry'),sep = "_",extra = 'merge')

#When KLEMS products map to more than one EXIOBASE product, the values are disaggregated and distributed
#among the different destinations using a proxy p is needed. We use total GFCF values from the existing EXIOBASE data as proxy values.
#Södersten, Carl-Johan; Wood, Richard; Hertwich, Edgar G. . (2017). Environmental Impacts of Capital Formation. Journal of Industrial Ecology, (), –. doi:10.1111/jiec.12532

depreciations.klems = get_depreciations_rate_klems()

concordance_nace.klems = get_concordance_nace.klems(classification = classification)

concordance_product.klems = get_concordance_product.klems(classification = classification)

capital_distribution.klems = model_capital_distribution.klems(raw.klems,vector.gfcf,concordance_nace.klems,concordance_product.klems)

return(capital_distribution.klems)
}

capital_distribution_klems = get_capital_distribution_klems(2021,classification = 'FIGARO')

un_cfc = get_un_stats_cfc(2021)


get_cn_cfc_proxy = function()
{
file.chinese_2011_iot = tempfile()

download.file("https://data.adb.org/media/1036/download", destfile = file.chinese_2011_iot, mode = "wb")

matrix.chinese_2011_iot = read_xlsx(file.chinese_2011_iot,sheet = 'Use_82x64')

cfc.chinese_2011_iot = matrix.chinese_2011_iot %>%
  filter(.[,2] == 'Consumption of Fixed Capital' | .[,3] == 'CPC/ISIC') %>%
  {.[,-c(1:3)]} %>%
  t() %>%
  `colnames<-`(c('ISIC','value')) %>%
  as.data.frame() %>%
  filter(!is.na(ISIC)) %>%
  mutate(value = as.numeric(value))

p51g.chinese_2011_iot = matrix.chinese_2011_iot %>%
  select(3,which(.[4,] == 'Gross Fixed Capital Formation')) %>%
  {.[-c(1:5),]} %>%
  `colnames<-`(c('ISIC','value')) %>%
  mutate(value = replace_na(as.numeric(value),0)) %>%
  filter(!is.na(ISIC))

#Unfortunately, a lot of sectors have P51G < 0. We'd rather take P51G / B1G as proxy.

b1g.chinese_2011_iot = matrix.chinese_2011_iot %>%
  filter(.[,2] == 'Total Gross Value Added/Gross Domestic Product' | .[,3] == 'CPC/ISIC') %>%
  {.[,-c(1:3)]} %>%
  t() %>%
  `colnames<-`(c('ISIC','value')) %>%
  as.data.frame() %>%
  filter(!is.na(ISIC)) %>%
  mutate(value = as.numeric(value))

cn_p51c_b1g = cfc.chinese_2011_iot %>% rename(P51C = value) %>%
  merge(b1g.chinese_2011_iot %>% rename(B1G = value)) %>%
  mutate(ratio = P51C / B1G)

return(cn_p51c_b1g)

}

cn_cfc = get_cn_cfc_proxy() #To do : concordance table

# korea

file.india_klems = tempfile()

GET('https://rbidocs.rbi.org.in/rdocs/content/docs/INDIAKLEMS08072024.xlsx',
    write_disk(file.india_klems, overwrite = TRUE),user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) Firefox/119.0"))

capital_matrix.india_klems = read_xlsx(file.india_klems,sheet = 'K',skip = 1)

#KLEMS = 28 countries out of FIGARO's 50.

#For missings, CFC can be estimated through UNstats


#UN data covers 38 countries. But importantly, China is missing


#The proxy consists of setting an accurate ratio between capital depreciation and capital formation.


#Norway : https://www.ssb.no/en/statbank/table/11189

# K = capital stock
# I = GFCF

#Machinery : Jiang, M., Wang, R., Wood, R., Rasul, K., Zhu, B., & Hertwich, E. (2023). Material and carbon footprints of machinery capital. Environmental Science & Technology, 57(50), 21124-21135.

#IT = Computing equipment ; CT = Communications equipment ; Soft_DB

#Japan Industrial Productivity Database (JIP) https://www.rieti.go.jp/en/database/JIP2023/index.html #already in EUKLEMS

#China Industrial Productivity Database (CIP) https://www.rieti.go.jp/en/database/CIP2023/index.html

#India KLEMS database : https://www.rbi.org.in/Scripts/PublicationReportDetails.aspx?UrlPage=&ID=1275

#Australia ? https://www.abs.gov.au/statistics/industry/industry-overview/estimates-industry-level-klems-multifactor-productivity/latest-release#data-downloads

#Mexico https://en.www.inegi.org.mx/programas/ptf/2018/#tabular_data

#South Korea

#Russia

#Deal with non observed countries

#####INTEGRATE DATA#####


