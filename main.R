x = c("tidyverse","curl","DBI","arrow","bir","icarus","readxl","melodi","countrycode","httr2",'rvest','countrycode','httr','fixest')

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
                                nace_r2_code == 'J58-J60' ~ list(c("J58",'J59_60')),
                                nace_r2_code == 'J62-J63' ~ list('J62_63'),
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

  Ye_al_2021 = curl_download('https://github.com/yequanliang1993/capital-endogenized-input-output-model/raw/refs/heads/main/Global_version/data_input/CapitalProject_Concordances.xlsx',
                             tempfile()) %>%
    read_xlsx(sheet = 'Asset_Concordance') %>%
    {.[-c(1,3:4),-c(1,3,4)]} %>%
    `colnames<-`(c("product",.[1,-1])) %>%
    {.[-1,]} %>%
    select(contains("p")) %>%
    filter(grepl("I_",product)) %>%
    mutate(product = gsub('I_','',product)) %>%
    column_to_rownames('product')

  Bourgeois_al_2023 = read.csv('https://raw.githubusercontent.com/La-Societe-Nouvelle/Comparing_MRIOTs_in_R/refs/heads/main/TabPass%20Exio3%20to%20FIGARO.csv',sep = ";") %>%
    filter(Pays == 'FR') %>%
    select(vrai_code,starts_with('D')) %>%
    pivot_longer(-1,names_to = 'raw_industry') %>%
    mutate(figaro = case_when(raw_industry == "D01T02" ~ list(c("A01","A02")),
                              raw_industry == "D03" ~ list("A03"),
                              raw_industry %in% c('D05T06','D07T08','D09') ~ list('B'),
                              raw_industry == "D10T12" ~ list("C10T12"),
                              raw_industry == "D13T15" ~ list("C13T15"),
                              raw_industry == "D16" ~ list("C16"),
                              raw_industry == "D17T18" ~ list(c("C17","C18")),
                              raw_industry =="D19" ~ list("C19"),
                              raw_industry == "D20" ~ list("C20"),
                              raw_industry == "D21" ~ list("C21"),
                              raw_industry == "D22" ~ list("C22"),
                              raw_industry == "D23" ~ list("C23"),
                              raw_industry == "D24" ~ list("C24"),
                              raw_industry == "D25" ~ list("C25"),
                              raw_industry == "D26" ~ list("C26"),
                              raw_industry == "D27" ~ list("C27"),
                              raw_industry == "D28" ~ list("C28"),
                              raw_industry == "D29" ~ list("C29"),
                              raw_industry == "D30" ~ list("C30"),
                              raw_industry == 'D31T33' ~ list(c("C31_32","C33")),
                              raw_industry == "D35" ~ list("D35"),
                              raw_industry == 'D36T39' ~ list(c("E36","E37T39")),
                              raw_industry == 'D41T43' ~ list('F'),
                              raw_industry == 'D45T47' ~ list(c("G45","G46","G47")),
                              raw_industry == "D49" ~ list("H49"),
                              raw_industry == "D50" ~ list("H50"),
                              raw_industry == "D51" ~ list("H51"),
                              raw_industry == "D52" ~ list("H52"),
                              raw_industry == "D53" ~ list("H53"),
                              raw_industry == 'D55T56' ~ list('I'),
                              raw_industry == 'D58T60' ~ list(c("J58","J59_60")),
                              raw_industry == 'D61' ~ list('J61'),
                              raw_industry == 'D62T63' ~ list('J62_63'),
                              raw_industry == 'D64T66' ~ list(c("K64","K65","K66")),
                              raw_industry == 'D68' ~ list('L'),
                              raw_industry == 'D69T75' ~ list(c("M69_70","M71","M72","M73","M74_75")),
                              raw_industry == 'D77T82' ~ list(c("N77","N78","N79",'N80T82')),
                              raw_industry == 'D84' ~ list('O84'),
                              raw_industry == 'D85' ~ list('P85'),
                              raw_industry == 'D86T88' ~ list(c("Q86","Q87_88")),
                              raw_industry == 'D90T93' ~ list(c("R90T92","R93")),
                              raw_industry == 'D94T96' ~ list(c("S94","S95","S96")),
                              raw_industry == 'D97T98' ~ list('T'),
                              raw_industry == 'D99' ~ list('U'))) %>%
    unnest(figaro) %>%
    select(!raw_industry) %>%
    group_by(vrai_code,figaro) %>%
    summarise(value = case_when(any(value) == 1 ~ 1,
                                T ~ 0)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'figaro',values_fill = 0) %>%
    column_to_rownames('vrai_code') %>%
    `rownames<-`(paste0("p",substring(rownames(.),2))) %>%
    cbind(U = 0)

  missing = colnames(Ye_al_2021) %>% subset(!colnames(Ye_al_2021) %in% rownames(Bourgeois_al_2023))

  remove_2l = missing %>% subset(substr(missing,1,nchar(missing)-2) %in% rownames(Bourgeois_al_2023))

  remove_3l = missing %>% subset((substr(missing,1,nchar(missing)-3) %in% rownames(Bourgeois_al_2023)) & (!missing %in% remove_2l))

  colnames(Ye_al_2021) = case_when(colnames(Ye_al_2021) %in% remove_2l ~ substr(colnames(Ye_al_2021),1,nchar(colnames(Ye_al_2021))-2),
                                   colnames(Ye_al_2021) %in% remove_3l ~ substr(colnames(Ye_al_2021),1,nchar(colnames(Ye_al_2021))-3),
                                   colnames(Ye_al_2021) %in% c("p24.e","p24.f","p24.g","p24.h","p24.i") ~ "p24.d",
                                   colnames(Ye_al_2021) == 'p26.w.1' ~ 'p26.a.w',
                                   T ~ colnames(Ye_al_2021))

  Ye_al_formatted = sapply(unique(colnames(Ye_al_2021)), function(x) {
    cols <- which(colnames(Ye_al_2021) == x)
    apply(Ye_al_2021[, cols, drop = FALSE], 1, max)
  }) %>% as.data.frame()

  Ye_al_formatted[] <- lapply(Ye_al_formatted, function(x) as.numeric(x))

  concordance_product.klems = (as.matrix(Ye_al_formatted) %*% as.matrix(Bourgeois_al_2023)) %>%
    {(. > 0) * 1}

  return(concordance_product.klems)


  if(toupper(classification) == 'FIGARO')
  {

  concordance_product.klems <- tibble(
  product = product <- c("CT", "Cult", "IT", "OCon", "OIPP", "OMach",
                         "RD", "Rstruc", "Soft_DB", "TraEq"),
  industry = case_when(
    product == "CT"       ~ list("C26"),                                           # Communications equipment #Telecommunications equipment
    product == "Cult"     ~ list(c("A01", "A02", "A03","C10T12")), #+Wine                        # Cultivated assets #Cultivated biological resources
    product == "IT"       ~ list("C26"),                                           # Computing equipment #Computer hardware
    product == "OCon"     ~ list("F"),                                             # Total non-residential investment #Other buildings and structures
    product == "OIPP"     ~ list(c("J58", "J59_60","R90T92")), # Other IPP assets #Entertain.& Artistic Originals
    product == "OMach"    ~ list(c("C26", "C27", "C28")),                         # Other Machinery and Equipment #Other machinery and equip. &weapons
    product == "RD"       ~ list("M72"),                                           # Research and development
    product == "Rstruc"   ~ list("F"),                                             # Residential structures #Dwellings
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

klems_industries = c('A','B',"C10-C12","C13-C15","C16-C18","C19","C20","C21","C22-C23","C24-C25","C26","C27","C28","C29-C30","C31-C33","D","E","F","G45","G46","G47",
                     "H49","H50","H51","H52","H53","I","J58-J60","J61","J62-J63","K","L","M","N","O","P","Q86","Q87-Q88","R","S","T","U")

raw.klems = read.csv("https://www.dropbox.com/s/sp2p4m86et66nfg/capital%20accounts.csv?dl=1")

va_klems = read.csv("https://www.dropbox.com/s/nkz7mdp0onken1j/national%20accounts.csv?dl=1") %>%
  filter(year == !!year & nace_r2_code %in% klems_industries) %>%
  select(nace_r2_code,geo_code,VA_CP)

meta.klems = curl_download("https://www.dropbox.com/s/srsn4zdb1wta27t/Variable-List-2023.xlsx?dl=1",tempfile()) %>%
  read_xlsx()

matrix.gfcf = import_figaro_io_table(year) %>% filter(!grepl('W2',rownames(.))) %>% {.[,grepl('P51G',colnames(.))]} #TBD - When other IO

vector.gfcf = rowSums(matrix.gfcf) %>%
  as.data.frame() %>%
  `colnames<-`('value') %>%
  `rownames<-`(gsub("CPA_","",row.names(.))) %>%
  rownames_to_column('id') %>%
  separate(id,into = c('country','industry'),sep = "_",extra = 'merge')

#In EU KLEMS, all countries have the same level of granularity

depreciations.klems = get_depreciations_rate_klems()

oecd_rates = read.csv(paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A....EXC_A.......?startPeriod=',
                             year,'&endPeriod=',year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels')) %>%
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
         merge_code = case_when(nace_r2_code == 'C22-C23' ~ 'C22_C23',
                                nace_r2_code == 'C24-C25' ~ 'C24_C25',
                                nace_r2_code == 'C29-C30' ~ 'C29_C30',
                                substr(nace_r2_code,1,1) == 'Q' ~ 'Q',
                                nace_r2_code == 'J62-J63' ~ 'J62_J63',
                                T ~ nace_r2_code)) %>%
  left_join(depreciations.klems,by = c('merge_code' = 'Code','product' = 'product')) %>%
  group_by(nace_r2_code,geo_code,product) %>%
  summarise(CFC = sum(value,na.rm=T) * unique(rate),.groups = 'drop') %>%
  left_join(va_klems,by = c('nace_r2_code','geo_code')) %>%
  left_join(oecd_rates,by = c('geo_code' = 'ISO2')) %>%
  mutate(CFC = CFC / EXCH_RATE,
         VA_CP = VA_CP / EXCH_RATE)

#When KLEMS products map to more than one EXIOBASE product, the values are disaggregated and distributed
#among the different destinations using a proxy p is needed. We use total GFCF values from the existing EXIOBASE data as proxy values.
#Södersten, Carl-Johan; Wood, Richard; Hertwich, Edgar G. . (2017). Environmental Impacts of Capital Formation. Journal of Industrial Ecology, (), –. doi:10.1111/jiec.12532

return(cfc_klems)

}

un_cfc = get_un_stats_cfc(2021)


get_cn_cfc_proxy = function()
{

matrix.chinese_2011_iot = read_xlsx('prc-sut-2011.xlsx',sheet = 'Use_82x64')

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

#Investment in "equipment" by industrial enterprises at or above "designated size"
#Investment in "non-residential structures" by industrial enterprises at or above "designated size"
#Capital stock in "equipment" by all industrial enterprises
#Capital stock in "non-residential structures" by all industrial enterprises

return(cn_p51c_b1g)

}

cn_cfc = get_cn_cfc_proxy() #To do : concordance table

exchange_rate = function(df)
{

  oecd_rates = read.csv('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A....EXC_A.......?startPeriod=2010&dimensionAtObservation=AllDimensions&format=csvfilewithlabels') %>%
    select(REF_AREA,RATE = OBS_VALUE,TIME_PERIOD) %>%
    group_by(TIME_PERIOD) %>%
    mutate(RATE = RATE / RATE[REF_AREA == 'EU27_2020'])

  df = df %>%
    left_join(oecd_rates,by = c('REF_AREA','TIME_PERIOD')) %>%
    mutate(OBS_VALUE = OBS_VALUE / RATE) %>%
    select(!RATE)

  return(df)

}

get_oecd_data = function(year,verbose = T)
{
  fixed_assets_sector = curl_download('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE9A,/A.........V..?startPeriod=2010&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels',
                                      tempfile()) %>%
    read.csv(check.names = F)

  # CORR_FIGARO =
  #   data.frame(ASSET = substr(unique(fixed_assets_sector$INSTR_ASSET),1,nchar(unique(fixed_assets_sector$INSTR_ASSET))-1)) %>%
  #   mutate(REF = case_when(ASSET %in% c('N111','N112') ~ list('F'),
  #                          ASSET == 'N1131' ~ list(c('C29','C30')),
  #                          ASSET %in% c('N1132','N11321','N11322') ~ list('C26'),
  #                          ASSET == 'N115' ~ list(c('A01','A02','A03')),
  #                          ASSET == 'N1171' ~ list('M72'),
  #                          ASSET == 'N1172' ~ list('B'),
  #                          ASSET == 'N1173' ~ list('J62_63'),
  #                          ASSET == 'N11731' ~ list('J62_63'),
  #                          ASSET == 'N11732' ~ list('J62_63'),
  #                          ASSET == 'N117' ~ list(c('M72','J62_63')),
  #                          ASSET == 'N1174' ~ list(c('R90T92','R93')),
  #                          ASSET == 'N1179' ~ list('TOTAL'),
  #                          ASSET == 'N11' ~ list('TOTAL'),
  #                          ASSET == 'N11K' ~ list('F'),
  #                          ASSET == 'N11M' ~ list(c('C25','C26','C27','C28')),
  #                          ASSET == 'N11O' ~ list(c('C25','C26','C27','C28'))))

  filtered_fixed_asset = fixed_assets_sector %>%
    select(REF_AREA,INSTR_ASSET,`Financial instruments and non-financial assets`,OBS_VALUE,ACTIVITY,TIME_PERIOD,CURRENCY) %>%
    filter(TIME_PERIOD == year) %>%
    mutate(ISO2 = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL)) %>%
    unite(industry, c('REF_AREA','ACTIVITY'),remove = F) %>%
    group_by(industry) %>%
    filter(any(INSTR_ASSET == 'N11N')) %>%
    mutate(ASSET = substr(INSTR_ASSET,1,nchar(INSTR_ASSET)-1),
           level = nchar(ASSET))
    #select(industry,INSTR_ASSET,OBS_VALUE) %>% pivot_wider(names_from = industry,values_from = OBS_VALUE)

  for(i in sort(unique(filtered_fixed_asset$level),decreasing = T))
  {

    asset_level = filtered_fixed_asset %>% filter(level == i) %>% pull(ASSET)

    asset_aggregate = substr(asset_level,1,nchar(asset_level)-1) %>% unique()

    to_delete = filtered_fixed_asset %>% group_by(industry) %>% filter(any(ASSET %in% asset_level)) %>% filter(ASSET %in% asset_aggregate) %>% ungroup() %>% distinct(industry,INSTR_ASSET)

    filtered_fixed_asset = filtered_fixed_asset %>% filter(!paste0(industry,INSTR_ASSET) %in% paste0(to_delete$industry,to_delete$INSTR_ASSET))

    if(verbose) print(i)
  }

  return(filtered_fixed_asset %>% exchange_rate())


}

get_pwt_data = function(year,verbose = T)
{
  pwt_data = curl_download("https://dataverse.nl/api/access/datafile/554105",destfile = tempfile()) %>% read_xlsx(sheet = 3)

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

depreciation_oecd = function(country_data_oecd,DEPR_RATES)
{

  if(length(unique(country_data_oecd$Type)) == 2)
  {

    country_data_oecd = country_data_oecd %>%
      group_by(ACTIVITY,ASSET,level) %>%
      summarise(OBS_VALUE = unique(OBS_VALUE[Type == 'GROSS'] - OBS_VALUE[Type == 'NET']),.groups = 'drop')

  }else{

    country_data_oecd = country_data_oecd %>%
      mutate(REF = case_when(ACTIVITY %in% DEPR_RATES$industry ~ DEPR_RATES$Code[match(ACTIVITY,DEPR_RATES$industry)],
                             ACTIVITY %in% DEPR_RATES$Code ~ DEPR_RATES$Code[match(ACTIVITY,DEPR_RATES$Code)],
                             ACTIVITY == '_T' ~ 'TOT',
                             ACTIVITY == 'C16T18' ~ 'C16-C18',
                             ACTIVITY == 'C24_25' ~ 'C24_C25',
                             ACTIVITY == 'C29_30' ~ 'C29_C30',
                             ACTIVITY == 'C31T33' ~ 'C31-C33',
                             ACTIVITY == 'J58T60' ~ 'J58-J60',
                             ACTIVITY %in% c('M69T71','M73_75') ~ 'M')) %>%
      mutate(RATE = DEPR_RATES$rate[match(paste0(REF,ASSET),paste0(DEPR_RATES$Code,DEPR_RATES$ASSET))]) %>%
      mutate(OBS_VALUE = pmax(OBS_VALUE * RATE,0)) %>%
      select(!c(RATE,REF))
  }

  return(country_data_oecd)


}

non_financial_asset_tree = function()
{

  matrix(c(
    'N11','N11K','N111','','',
    'N11','N11K','N112','','',
    'N11','N11M','N113','N1131','',
    'N11','N11M','N113','N1132','N11321',
    'N11','N11M','N113','N1132','N11322',
    'N11','N11M','N114','','',
    'N11','N11M','N11O','','',
    'N11','N115','','','',
    'N11','N117','N1171','','',
    'N11','N117','N1172','','',
    'N11','N117','N1173','N11731','',
    'N11','N117','N1173','N11732','',
    'N11','N117','N1174','','',
    'N11','N117','N1179','',''
  ),
  ncol = 5,
  byrow=T) %>%
    as.data.frame() %>%
    setNames(c(1:5))

  #N11
  #   N11K
  #       N111
  #       N112
  #   N11M
  #       N113
  #           N1131
  #           N1132
  #                 N11321
  #                 N11322
  #       N114
  #       N11O
  #   N115
  #   N117
  #       N1171
  #       N1172
  #       N1173
  #           N11731
  #           N11732
  #       N1174
  #       N1179

}

ras_capital_use = function(K1,pp_country,pi_country,min_error,n_iter,verbose)
{

  pi_country_adj = pi_country / sum(pi_country) * sum(pp_country)

  for(iter in 1:n_iter)
  {

    r = pp_country / rowSums(K) ; r[!is.finite(r)] = 1

    K = diag(x = r) %*% K

    s = pi_country_adj / colSums(K) ; s[!is.finite(s)] = 1

    K = K %*% diag(x = s)

    accuracy = max(abs(c(r,s)-1))

    if(accuracy < min_error & verbose) message('Maximum error for country ',i,' for round ',iter," : ",accuracy)

    if(accuracy < min_error) break

  }

  return(K)

}

year = 2018 ; classification = 'FIGARO' ; n_iter = 100 ; min_error = 1e-3 ; verbose = T

format_domestic_capital_tables(year,classification = 'FIGARO',n_iter = 100,min_error = 1e-3,verbose = T)
{
  figaro = import_figaro_io_table(year)

  klems_data = get_capital_distribution_klems(year)

  concordance_nace.klems = get_concordance_nace.klems(classification = classification)

  concordance_product.klems = Gphat = get_concordance_product.klems(classification = classification) %>%
    as.data.frame() %>%
    arrange(row.names(.)) %>%
    as.matrix()

  Gihat =  concordance_nace.klems %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = 'industry',values_fill = 0) %>%
    column_to_rownames('nace_r2_code') %>%
    as.matrix() %>%
    t()

  pp = figaro %>% transmute(value = rowSums(across(contains('P51G'))))

  get_cfc_klems = function(figaro,klems_data,year,verbose)
  {

    value_added_figaro = figaro %>%
      filter(grepl('W2',rownames(.))) %>%
      colSums() %>%
      subset(!grepl('P3|P5',names(.))) %>%
      data.frame(value_added = .) %>%
      rownames_to_column('id') %>%
      separate(id,c('country','industry'),extra = 'merge',sep = "_")


    cfc_rates_klems = klems_data %>%
      filter(geo_code %in% countries) %>%
      group_by(geo_code,nace_r2_code) %>%
      summarise(CFC = pmax(pmin(sum(CFC) / unique(VA_CP),1),0),.groups = 'drop') %>%
      left_join(concordance_nace.klems,by = 'nace_r2_code',relationship = "many-to-many") %>%
      left_join(value_added_figaro,by = c('geo_code' = 'country','industry' = 'industry')) %>%
      reframe(country = geo_code,industry,CFC = CFC * value_added)

    return(cfc_rates_klems)

  }

  CFC_KLEMS = get_cfc_klems(figaro,klems_data,year,verbose)

  pi = CFC_KLEMS$CFC %>% `names<-`(paste0(CFC_KLEMS$country,"_",CFC_KLEMS$industry))

  # pi = figaro %>% filter(grepl('W2',rownames(.))) %>% colSums() %>% subset(!grepl('P3|P5',names(.)))

  storage_K = list()

  for(i in unique(klems_data$geo_code))
  {

    klems_domestic =
      klems_data %>%
      ungroup() %>%
      filter(geo_code == i) %>%
      select(!geo_code) %>%
      pivot_wider(names_from = nace_r2_code,values_from = CFC) %>%
      arrange(product) %>%
      column_to_rownames('product') %>%
      arrange(rownames(.)) %>%
      as.matrix()

    figaro_ref = ifelse(i == 'UK','GB',i)

    if(!figaro_ref %in% unique(substr(colnames(figaro),1,2))) next

    pp_country = pp %>% filter(substr(rownames(.),1,2) == figaro_ref & substr(rownames(.),1,5) != 'FIGW1') %>% unlist()

    pi_country = pi %>% subset(substr(names(.),1,2) == figaro_ref & substr(names(.),1,5) != 'FIGW1')

    pi_country[pi_country == 0] <- 0.1

    Gp = solve(diag(x = as.numeric(Gphat %*% as.matrix(pp_country)))) %*% Gphat %*% diag(x = unlist(pp_country))

    Gi = diag(x = unlist(pi_country)) %*% Gihat %*% solve(diag(x = as.numeric(t(as.matrix(pi_country)) %*% Gihat)))

    K1 = K = t(Gp) %*% klems_domestic %*% t(Gi)

    K = ras_capital_use(K1,pp_country,pi_country,min_error,n_iter,verbose)

    storage_K[[figaro_ref]] = K

  }

  oecd_data = get_oecd_data(year,verbose) %>% ungroup()

  supplementary_oecd = unique(oecd_data$ISO2) %>% subset(!. %in% names(storage_K) & . %in% unique(substr(colnames(figaro),1,2)))

  #Build up the correspondence tables between OECD - KLEMS - FIGARO

  OECD_TO_KLEMS =
    oecd_data %>%
    filter(ISO2 %in% supplementary_oecd) %>%
    mutate(`Financial instruments and non-financial assets` = `Financial instruments and non-financial assets` %>% gsub(', net','',.) %>% gsub(', gross','',.)) %>%
    distinct(ASSET,`Financial instruments and non-financial assets`) %>%
    mutate(KLEMS = case_when(ASSET == 'N111' ~ list('RStruc'),
                             ASSET == 'N112' ~ list('OCon'),
                             ASSET == 'N1131' ~ list('TraEq'),
                             ASSET == 'N11321' ~ list('IT'),
                             ASSET == 'N11322' ~ list('CT'),
                             ASSET == 'N115' ~ list('Cult'),
                             ASSET %in% c('N1171','N1172') ~ list('RD'),
                             ASSET %in% c('N1173','N11731','N11732') ~ list('Soft_DB'), #Software = 58 & 62 #Database = 63
                             ASSET %in% c('N1179','N1174') ~ list('OIPP'),
                             ASSET == 'N11K' ~ list(c('RStruc','OCon')),
                             ASSET %in% c('N11M','N11O') ~ list('OMach'))) %>% #The reference are straightforwardly taken from KLEMS documentation (Bontadini et al. 02/2023)
    select(!`Financial instruments and non-financial assets`) %>%
    unnest(KLEMS) %>%
    distinct()

  OECD_KLEMS_CORRESPONDENCE =
    OECD_TO_KLEMS %>%
    left_join(Gphat %>% as.data.frame() %>% rownames_to_column('KLEMS'),by = 'KLEMS') %>%
    group_by(ASSET) %>%
    summarise(across(-KLEMS, ~ pmin(sum(.), 1)),.groups = 'drop')

  klems_depreciations_rate = get_depreciations_rate_klems() %>%
    left_join(concordance_nace.klems %>% mutate(nace_r2_code = case_when(nace_r2_code %in% c('C22-C23','C24-C25','C29-C30','J62-J63') ~ gsub("-","_",nace_r2_code),
                                                                         nace_r2_code %in% c('Q86','Q87-Q88') ~ 'Q',
                                                                         T ~ nace_r2_code)),by = c('Code' = 'nace_r2_code'),
              relationship = "many-to-many")

  DEPR_RATES = OECD_TO_KLEMS %>%
    left_join(klems_depreciations_rate,by = c('KLEMS' = 'product'),relationship = "many-to-many")

    # mutate(case_when(KLEMS %in% c('IT','CT') ~ S94 = S96 = 0,
    #                  KLEMS == 'TraEq' ~ G46=G47=0) -> To work ON

  oecd_activity_correspondance = data.frame(FIGARO = concordance_nace.klems$industry) %>%
      mutate(Option1 = case_when(FIGARO %in% c('C16','C17','C18') ~ 'C16T18',
                                 FIGARO %in% c('C24','C25') ~ 'C24_25',
                                 FIGARO %in% c('C29','C30') ~ 'C29_30',
                                 FIGARO %in% c('C31_C32','C33') ~ 'C31T33',
                                 FIGARO %in% c('J58','J59_60') ~ 'J58T60',
                                 FIGARO %in% c('M69_70','M71') ~ 'M69T71',
                                 FIGARO %in% c('M73','M74_75') ~ 'M73_75',
                                 T ~ ''),
             Option2 = substr(FIGARO,1,1),
             Option3 = '_T') %>%
    mutate(Option0 = FIGARO)

  # AUS  CAN  GRC  KOR  NOR  RUS
  # 176  691 1700   86  811   25

  for(i in supplementary_oecd %>% subset(!. %in% c('KR','RU'))) #For Korea and Russia, there is no information on sectoral x product level
  {

    #Filter country-specific OECD data

    country_data_oecd =
      oecd_data %>%
      filter(ISO2 == i) %>%
      mutate(Type = case_when(grepl(", net",`Financial instruments and non-financial assets`) ~ 'NET',
                              T ~ 'GROSS')) %>%
      mutate(ACTIVITY = case_when(ACTIVITY == 'O' ~ 'O84',
                                  ACTIVITY == 'P' ~ 'P85',
                                  ACTIVITY == 'D' ~ 'D35',
                                  ACTIVITY == 'C31_32' ~ 'C31_C32',
                                  T ~ ACTIVITY)) %>%
      filter(ACTIVITY %in% unique(unlist(oecd_activity_correspondance))) %>%
      depreciation_oecd(.,DEPR_RATES = DEPR_RATES)

    #Match the OECD column 'ACTIVITY' with FIGARO classification

    reference_activity_country = oecd_activity_correspondance %>%
      mutate(OECD = case_when(Option0 %in% country_data_oecd$ACTIVITY ~ Option0,
                              Option1 %in% country_data_oecd$ACTIVITY ~ Option1,
                              Option2 %in% country_data_oecd$ACTIVITY ~ Option2,
                              Option3 %in% country_data_oecd$ACTIVITY ~ Option3))

    country_data_oecd = country_data_oecd  %>%
      select(ACTIVITY,ASSET,OBS_VALUE,level)

    delete_list = c()

    CPATNF = non_financial_asset_tree()

    oecd_value_added = read.csv(paste0('https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,2.0/A....B1G....XDC.V..?startPeriod=',
                                       year,'&endPeriod=',year,'&dimensionAtObservation=AllDimensions&format=csvfilewithlabels')) %>%
      select(REF_AREA,ACTIVITY,VALUE_ADDED = OBS_VALUE) %>%
      mutate(ISO2 = countrycode(REF_AREA,'iso3c','iso2c',nomatch = NULL))

    #Avoid double counting applying crosswalk

    for(j in ncol(CPATNF):1)
    {
      available = CPATNF[,j] %>% subset(. %in% country_data_oecd$ASSET)
      linked = CPATNF[,j] %>% subset(substr(.,1,nchar(CPATNF[,j])-1) %in% substr(available,1,nchar(available)-1)) %>% subset(!. %in% available)

      to_delete = CPATNF[CPATNF[,j] %in% c(available,linked),(1:5)<j] %>% unlist() %>% unique()

      delete_list = rbind(delete_list,
                          country_data_oecd %>%
                            group_by(ACTIVITY) %>%
                            filter(any(ASSET %in% available)) %>%
                            filter(ASSET %in% to_delete) %>%
                            select(ACTIVITY,ASSET) %>%
                            ungroup()) %>%
        distinct()
    }

    #Rotating the temporary table

    formatted_country_data_oecd =
      country_data_oecd %>%
      filter(ACTIVITY %in% reference_activity_country$OECD) %>%
      filter(!paste0(ACTIVITY,ASSET) %in% paste0(delete_list$ACTIVITY,delete_list$ASSET)) %>%
      arrange(ACTIVITY,ASSET) %>%
      select(!level) %>%
      pivot_wider(names_from = ACTIVITY,values_from = OBS_VALUE,values_fill = 0) %>%
      column_to_rownames('ASSET')

    get_cfc_oecd = function(initial_country_data_oecd,oecd_value_added)
    {

      data.frame(CFC = colSums(initial_country_data_oecd,na.rm = T)) %>%
        rownames_to_column()

      oecd_value_added %>%
        filter(ISO2 == i)

      #Group by OECD COLS, rejoin, left_join value_added, compute the ratio

      countries = sub("_.*$", "", rownames(figaro)) %>% unique() %>% subset(. != 'W2')

    }


    #Ignore total fixed asset (not distributed) and households-related assets (such as N111 Dwellings)

    if(any(rownames(formatted_country_data_oecd) %in% c('N11','N111')))
    {
      formatted_country_data_oecd = formatted_country_data_oecd[-which(rownames(formatted_country_data_oecd) %in% c('N11','N111')),]
    }

    #Build up the full-sized matrix applying disaggregation through pi where applicable

    initial_country_data_oecd = matrix(ncol = nrow(concordance_nace.klems),nrow = nrow(formatted_country_data_oecd)) %>%
      as.data.frame() %>%
      `rownames<-`(row.names(formatted_country_data_oecd)) %>%
      `colnames<-`(concordance_nace.klems$industry)

    pp_country = pp %>% filter(substr(rownames(.),1,2) == i & substr(rownames(.),1,5) != 'FIGW1') %>% unlist()

    pi_country = pi %>% subset(substr(names(.),1,2) == i & substr(names(.),1,5) != 'FIGW1') %>% `names<-`(substring(names(.),4))

    pi_country[pi_country == 0] <- 0.1

    for(j in 1:ncol(initial_country_data_oecd))
    {
      pi_j = pi_country %>% subset(names(.) %in% gsub('C31_C32','C31_32',colnames(initial_country_data_oecd)[j]))

      ref = reference_activity_country %>%
        filter(FIGARO == colnames(initial_country_data_oecd)[j]) %>%
        pull(OECD)

      aggregate_ref = reference_activity_country$FIGARO[which(apply(reference_activity_country, 1, function(row) any(row == ref)))]

      pi_agg = pi_country %>% subset(names(.) %in% gsub('C31_C32','C31_32',aggregate_ref)) %>% sum()

      initial_country_data_oecd[,j] = formatted_country_data_oecd[,ref] / pi_agg * pi_j
    }

    #FIGARO-related test

    if(ncol(initial_country_data_oecd) != 64) stop('Wrong matching')

    #Set up Gphat, to correspond exactly to the country capital use matrix breakdown level

    Gphat = data.frame(ASSET = rownames(initial_country_data_oecd)) %>%
      left_join(OECD_KLEMS_CORRESPONDENCE,by = 'ASSET') %>%
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

  pwt = get_pwt_data(year,verbose) %>%
    filter(country %in% unique(substr(colnames(figaro),1,2))) %>%
    mutate(observed = case_when(country %in% names(storage_K) ~ 1,
                                T ~ 0),
           GDP_CAT = case_when(GDP_CAPITA <= quantile(GDP_CAPITA,probs = 1/3) ~ 1,
                               GDP_CAPITA > quantile(GDP_CAPITA,probs = 2/3) ~ 3,
                               T ~ 2))

  K_DF = c()

  for(i in names(storage_K))
  {

    K_DF =
      rbind(K_DF,
            storage_K[[i]] %>%
              `dimnames<-`(list(concordance_nace.klems$industry,concordance_nace.klems$industry)) %>%
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
    data = K_DF %>% mutate(value = pmax(value,0)) %>% group_by(use_id,country) %>% mutate(value = value / sum(value)) %>% ungroup())

  to_predict =
    pwt %>%
    filter(observed == 0) %>%
    select(country,income_class = GDP_CAT) %>%
    rbind(data.frame(country = 'FIGW1',income_class = 1)) %>% #ADDING FIGW1 predict
    merge(data.frame(resource_id = concordance_nace.klems$industry)) %>%
    merge(data.frame(use_id = concordance_nace.klems$industry)) %>%
    mutate(pi = pi[match(paste0(country,"_",gsub('C31_C32','C31_32',use_id)),names(pi))])

  to_predict$value = to_predict$pi * predict(train_poisson_regression,newdata = to_predict) %>% replace_na(0) #NA occurs only when U interacts with U

  sto_K = storage_K

  for(i in unique(to_predict$country))
  {

    K = K1 = to_predict %>%
      filter(country == i) %>%
      select(use_id,resource_id,value) %>%
      pivot_wider(names_from = 'use_id') %>%
      column_to_rownames('resource_id') %>%
      as.matrix()

    pp_country = pp %>% filter(sub("_.*$", "", rownames(.)) == i) %>% unlist()

    pi_country = pi %>% subset(sub("_.*$", "", names(.)) == i)

    pi_country[pi_country == 0] <- 0.1

    pi_country_adj = pi_country / sum(pi_country) * sum(pp_country)

    K = ras_capital_use(K1,pp_country,pi_country_adj,min_error,n_iter,verbose)

    storage_K[[i]] = K

  }

  figaro_investments = figaro %>%
    select(contains('P51G')) %>%
    `colnames<-`(gsub('_P51G','',colnames(.)))

  figaro_industries = sub("^[A-Z]{2}_(.*)$", "\\1", rownames(figaro_investments) %>% subset(!grepl('FIGW1|W2',.))) %>% unique()

  K_mat = matrix(0, nrow = length(figaro_industries), ncol = 0,
                 dimnames = list(figaro_industries, NULL))

  for(i in sort(names(storage_K)))
  {

    country_sector_destination = figaro_investments %>%
      filter(sub("_.*$", "", rownames(.)) == i) %>%
      {sweep(.,1,rowSums(.),`/`)} %>%
      rownames_to_column('industry') %>%
      mutate(industry = sub("^[^_]*_(.*)$", "\\1", industry)) %>%
      pivot_longer(-1,names_to = 'destination',values_to = 'share')


    distributed_K = storage_K[[i]] %>%
      `dimnames<-`(list(figaro_industries,figaro_industries)) %>%
      as.data.frame() %>%
      rownames_to_column('resource_id') %>%
      pivot_longer(-1,names_to = 'use_id') %>%
      left_join(country_sector_destination,by = c('use_id' = 'industry'),relationship = "many-to-many") %>%
      mutate(distributed = value * share) %>%
      arrange(destination,resource_id) %>%
      reframe(resource_id = paste0(destination,"_",resource_id),use_id = paste0(i,"_",use_id),distributed) %>%
      pivot_wider(names_from = 'use_id',values_from = 'distributed') %>%
      column_to_rownames('resource_id')

    distributed_K[is.na(distributed_K)] = 0

    K_mat = cbind(K_mat,distributed_K)

  }

  return(K_mat)

}


# Alsamawi, A. et al. (2020-09-30), “Returns to intangible capital
# in global value chains: New evidence on trends and policy
# determinants”, OECD Trade Policy Papers, No. 240, OECD
# Publishing, Paris.
# http://dx.doi.org/10.1787/4cd06f19-en

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


