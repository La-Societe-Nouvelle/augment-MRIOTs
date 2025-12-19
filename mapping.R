get_correspondence_nace.klems = function(classification)
{
  if(toupper(classification) == 'FIGARO')
  {
    correspondence_nace.klems = data.frame(nace_r2_code = c("A","B","C","C10-C12","C13-C15","C16-C18","C19","C20","C20-C21","C21","C22-C23","C24-C25","C26","C26-C27","C27","C28","C29-C30",
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
                                  nace_r2_code == 'C31-C33' ~ list(c('C31_32','C33')),
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

    correspondence_nace.klems$industry[correspondence_nace.klems$nace_r2_code %in%
                                      c('C19','C20','C21','C26','C27','C28','F','G45','G46','G47','H49','H50','H51','H52','H53','I','J61','L','Q86','T','U')] =
      correspondence_nace.klems$nace_r2_code[correspondence_nace.klems$nace_r2_code %in%
                                            c('C19','C20','C21','C26','C27','C28','F','G45','G46','G47','H49','H50','H51','H52','H53','I','J61','L','Q86','T','U')]

    correspondence_nace.klems = correspondence_nace.klems %>%
      unnest(industry)
  }

  return(correspondence_nace.klems)

}

get_correspondence_product.klems = function(classification,verbose = T)
{

  Ye_al_2021 = get_cache_file(link = 'https://github.com/yequanliang1993/capital-endogenized-input-output-model/raw/refs/heads/main/Global_version/data_input/CapitalProject_Concordances.xlsx',
                              pattern = 'Ye_al_2021',
                              verbose = verbose) %>%
    read_xlsx(sheet = 'Asset_Concordance') %>%
    {.[-c(1,3:4),-c(1,3,4)]} %>%
    `colnames<-`(c("product",.[1,-1])) %>%
    {.[-1,]} %>%
    select(contains("p")) %>%
    filter(grepl("I_",product)) %>%
    mutate(product = gsub('I_','',product)) %>%
    column_to_rownames('product')

  Bourgeois_al_2023 = get_cache_file(link = 'https://raw.githubusercontent.com/La-Societe-Nouvelle/Comparing_MRIOTs_in_R/refs/heads/main/TabPass%20Exio3%20to%20FIGARO.csv',
                                     pattern = 'Bourgeois_al_2023',
                                     verbose = verbose) %>%
    read.csv(sep = ";") %>%
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

  correspondence_product.klems = (as.matrix(Ye_al_formatted) %*% as.matrix(Bourgeois_al_2023)) %>%
    {(. > 0) * 1}

  return(correspondence_product.klems)

}

get_correspondence_nace.oecd = function(classification)
{
  if(classification == 'FIGARO')
  {
  concordance_nace.klems = get_correspondence_nace.klems(classification = classification)

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

  return(oecd_activity_correspondance)
  }

}

get_correspondence_product.oecd = function(classification)
{
  if(classification == 'FIGARO')
  {

  OECD_TO_KLEMS =
    get_correspondence_product.oecd_to_klems(classification)

  KLEMS_TO_FIGARO =
    get_correspondence_product.klems(classification = classification)

  OECD_TO_FIGARO =
    OECD_TO_KLEMS %>%
    left_join(KLEMS_TO_FIGARO %>% as.data.frame() %>% rownames_to_column('KLEMS'),by = 'KLEMS') %>%
    group_by(ASSET) %>%
    summarise(across(-KLEMS, ~ pmin(sum(.), 1)),.groups = 'drop')


  return(OECD_TO_FIGARO)
  }


}

get_correspondence_nace.oecd_to_klems = function(classification)
{

  KLEMS_CODE_DEPRECIATIONS = c("TOT","TOT_IN","MARKT","A","B","C","C10-C12","C13-C15","C16-C18","C19","C20","C21","C22_C23","C24_C25","C26","C27","C28","C29_C30",
                               "C31-C33","D","E","F","G","G45","G46","G47","H","H49","H50","H51","H52","H53","I","J","J58-J60","J61","J62_J63","K","L","M_N","M","N",
                               "O-Q","O","P","Q","R_S","R","S","T","U")

  concordance_nace.klems = get_correspondence_nace.klems(classification = classification)


  OECD_TO_KLEMS_DEPRECIATIONS_CORRESPONDENCE = data.frame(Code = KLEMS_CODE_DEPRECIATIONS) %>%
    left_join(concordance_nace.klems %>% mutate(nace_r2_code = case_when(nace_r2_code %in% c('C22-C23','C24-C25','C29-C30','J62-J63') ~ gsub("-","_",nace_r2_code),
                                                                         nace_r2_code %in% c('Q86','Q87-Q88') ~ 'Q',
                                                                         T ~ nace_r2_code)),by = c('Code' = 'nace_r2_code'),
              relationship = "many-to-many")

  return(OECD_TO_KLEMS_DEPRECIATIONS_CORRESPONDENCE)
}

get_non_financial_asset_tree = function()
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

get_correspondence_product.oecd_to_klems = function(classification)
{

  OECD_TO_KLEMS =
    data.frame(ASSET = c("N11","N111","N112","N1131","N11321","N11322","N115","N1171","N1172","N1173","N11731","N11732","N1174","N1179","N11K","N11M","N11O")) %>%
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
    unnest(KLEMS) %>%
    distinct()

  return(OECD_TO_KLEMS)

}

get_correspondence_activity.un = function(classification)
{
  correspondence_nace = get_correspondence_nace.klems(classification = classification) %>%
    mutate(Option1 = case_when(industry %in% c('A01','A02','A03') ~ substr(industry,2,3),
                               substr(industry,1,1) %in% c('T','U') ~ NA,
                               T ~ substr(industry,1,1)),
           Option2 = case_when(industry %in% c('A01','A02','A03') ~ 'A',
                               substr(industry,1,1) %in% c('B','C','D','E') ~ 'B+C+D+E',
                               substr(industry,1,1) %in% c('G','H','I') ~ 'G+H+I',
                               substr(industry,1,1) %in% c('M','N') ~ 'M+N',
                               substr(industry,1,1) %in% c('B','C','D','E') ~ 'B+C+D+E',
                               substr(industry,1,1) %in% c('R','S','T') ~ 'R+S+T',
                               T ~ NA),
           Option3 = 'Total Economy')

  return(correspondence_nace)
}
