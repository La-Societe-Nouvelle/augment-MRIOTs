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

get_crosswalk_nace.klems = function()
{
  crosswalk_klems = data.frame(industry = c('A','B',"C10-C12","C13-C15","C16-C18","C19","C20","C21","C22-C23","C24-C25","C26","C27","C28","C29-C30","C31-C33","D","E","F","G45","G46","G47","H49","H50","H51","H52","H53","I",
                                            "J58-J60","J61","J62-J63","K","L","M","N","O","P","Q86","Q87-Q88","R","S","T","U")) %>%
    mutate(Option1 = industry,
           Option2 = case_when(industry %in% c("C20","C21") ~ "C20-C21",
                               industry %in% c("C26","C27") ~ "C26-C27",
                               TRUE ~ NA),
           Option3 = substr(industry,1,1),
           Option4 = case_when(industry %in% c('D','E') ~ 'D-E',
                               industry %in% c('M','N') ~ 'M-N',
                               industry %in% c('O','P','Q') ~ 'O-Q',
                               industry %in% c('R','S') ~ 'R-S',
                               TRUE ~ NA),
           Option5 = case_when(substr(industry,1,1) %in% c('B','C','D','E','F','G','H','I','J','K','M','N','R','S') ~ 'MARKTxAG',
                               TRUE ~ NA),
           Option6 = case_when(substr(industry,1,1) %in% c('A','B','C','D','E','F','G','H','I','J','K','M','N','R','S') ~ 'MARKT',
                               TRUE ~ NA),
           Option7 = case_when(substr(industry,1,1) %in% c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S') ~ 'TOT_IND',
                               TRUE ~ NA),
           Option8 = 'TOT')

  return(crosswalk_klems)
}

get_correspondence_product.klems = function(verbose = T)
{

  get_asset_classification() %>%
    select(KLEMS,INDUSTRY) %>%
    filter(lengths(KLEMS) <= 2) %>%
    unnest(c(INDUSTRY, KLEMS)) %>%
    merge(data.frame(INDUSTRY = c("A01","A02","A03","B","C10T12","C13T15","C16","C17","C18","C19","C20","C21","C22",
                                  "C23","C24","C25","C26","C27","C28","C29","C30","C31_32","C33","D35","E36","E37T39",
                                  "F","G45","G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60","J61","J62_63",
                                  "K64","K65","K66","L","M69_70","M71","M72","M73","M74_75","N77","N78","N79","N80T82",
                                  "O84","P85","Q86","Q87_88","R90T92","R93","S94","S95","S96","T","U")),all = T) %>%
    mutate(value = 1) %>%
    distinct() %>%
    pivot_wider(names_from = INDUSTRY,values_fill = 0) %>%
    filter(!is.na(KLEMS)) %>%
    column_to_rownames('KLEMS') %>%
    arrange(tolower(row.names(.)))

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
                               FIGARO %in% c('C22','C23') ~ 'C22_23',
                               FIGARO %in% c('C31_32','C33') ~ 'C31T33',
                               FIGARO %in% c('J58','J59_60') ~ 'J58T60',
                               FIGARO %in% c('M69_70','M71') ~ 'M69T71',
                               FIGARO %in% c('M73','M74_75') ~ 'M73T75',
                               T ~ ''),
           Option2 = substr(FIGARO,1,1),
           Option3 = '_T') %>%
    mutate(Option0 = FIGARO)

  return(oecd_activity_correspondance)
  }

}

get_asset_classification = function()
{

  data.frame(ASSET = c("N11","N111","N112","N1131","N1132","N11321","N11322","N1139","N115","N1171","N1172","N1173","N11731","N11732","N1174","N1179","N11K","N11M","N11O")) %>%
    mutate(INDUSTRY = case_when(ASSET == 'N111' ~ list('F'),
                                ASSET == 'N112' ~ list('F'),
                                ASSET == 'N1131' ~ list(c('C29','C30')), #Transport equipment 29_30
                                ASSET == 'N1132' ~ list('C26'),#ICT equipment, when published without its two components
                                ASSET == 'N11321' ~ list('C26'),#Information and communication technologies (ICT) equipment 26
                                ASSET == 'N11322' ~ list('C26'),#Information and communication technologies (ICT) equipment 26
                                ASSET == 'N1139' ~ list(c('C26','C27','C28','C31_32')),#Machinery and equipment not elsewhere classified 26_27_28_31_32
                                ASSET == 'N115' ~ list(c('A01','A02','A03')), #Animals whose natural growth and regeneration are under the direct control, responsibility and management of institutional units A01 A02 A03
                                ASSET == 'N1171' ~ list('M72'), #Consists of the value of expenditure on creative work undertaken on a systematic basis in order to increase the stock of knowledge, including knowledge of man, culture and society, and use of this stock of knowledge to devise new applications.
                                #One can put N1172 -> B ; but this would mean that N117 is a weighted aggregate of CPA_B CPA_M72 -> FALSE in most cases.
                                ASSET == 'N1172' ~ list('M72'), #The value of expenditure on exploration for petroleum and natural gas and for non-petroleum deposits and subsequent evaluation of the discoveries made
                                ASSET == 'N11731' ~ list(c('J58','J62_63')), #Computer programs, program descriptions and supporting materials for both systems and applications software ; J58 carries the publishing of software (CPA 58.2)
                                ASSET == 'N11732' ~ list(c('J58','J62_63')), #Files of data organised to permit resource-effective access and use of the data
                                ASSET == 'N1174' ~ list(c('J59_60','R90T92','R93')),#Original films, sound recordings, manuscripts, tapes, models, etc., on which drama performances, radio and television programmes, musical performances, sporting events, literary and artistic output, etc. are recorded or embodied
                                ASSET == 'N1179' ~ list('M72'),
                                ASSET == 'N11K' ~ list('F'),
                                ASSET == 'N11M' ~ list(c('C26','C29','C30')),
                                ASSET == 'N11O' ~ list(c('C25','C26','C27','C28','C31_32','C33'))),#Other machinery and equipment n.e.c. : ESA 2010 Annex 7.1 also lists 25 (metal structures, 25.3) and 33 (installation, repair and maintenance)

           KLEMS = case_when(ASSET == 'N111' ~ list('Rstruc'),
                             ASSET == 'N112' ~ list('OCon'),
                             ASSET == 'N1131' ~ list('TraEq'), #Transport equipment 29_30
                             ASSET == 'N1132' ~ list(c('IT','CT')),#ICT equipment, when published without its two components
                             ASSET == 'N11321' ~ list('IT'),#Information and communication technologies (ICT) equipment 26
                             ASSET == 'N11322' ~ list('CT'),#Information and communication technologies (ICT) equipment 26
                             ASSET == 'N1139' ~ list('OMach'),#Machinery and equipment not elsewhere classified 26_27_28_31_32
                             ASSET == 'N115' ~ list('Cult'), #Animals whose natural growth and regeneration are under the direct control, responsibility and management of institutional units A01 A02 A03
                             ASSET == 'N1171' ~ list('RD'), #Consists of the value of expenditure on creative work undertaken on a systematic basis in order to increase the stock of knowledge, including knowledge of man, culture and society, and use of this stock of knowledge to devise new applications.
                             ASSET == 'N1172' ~ list('RD'), #The value of expenditure on exploration for petroleum and natural gas and for non-petroleum deposits and subsequent evaluation of the discoveries made
                             ASSET == 'N11731' ~ list('Soft_DB'), #Computer programs, program descriptions and supporting materials for both systems and applications software
                             ASSET == 'N11732' ~ list('Soft_DB'), #Files of data organised to permit resource-effective access and use of the data
                             ASSET == 'N1174' ~ list('OIPP'), #Original films, sound recordings, manuscripts, tapes, models, etc., on which drama performances, radio and television programmes, musical performances, sporting events, literary and artistic output, etc. are recorded or embodied
                             ASSET == 'N1179' ~ list('OIPP'),
                             ASSET == 'N11K' ~ list(c('Rstruc','OCon')),
                             ASSET == 'N11M' ~ list(c('IT','CT','TraEq')),
                             ASSET == 'N11O' ~ list('OMach')))

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
    'N11','N11M','N113','N11O','',
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
  #           N11O
  #       N114
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

get_correspondence_product.oecd_to_klems = function()
{

  OECD_TO_KLEMS =
    get_asset_classification() %>%
    select(ASSET,KLEMS) %>%
    unnest(KLEMS) %>%
    distinct() %>%
    rbind(data.frame(ASSET = 'N1173',KLEMS = 'Soft_DB'))

  return(OECD_TO_KLEMS)

}

get_correspondence_national_cfc = function(country)
{
  # Correspondence between the industries of the national CFC sources and the 64 FIGARO industries.
  # CN: NBS input-output tables, 13 groups of industries.
  # IN: MoSPI Supply-Use Tables, groups of industries (matched by industry name).
  # RU: Rosstat SUT, ISIC Rev.3 divisions (2012-2015) or OKVED2 = NACE Rev.2 (2016-2018).

  if(country == 'CN')
  {
    # 13 NBS groups. Manufacturing is split into 8 subgroups (shares file) or 7 groups (absolute file,
    # oil refining, coking and chemicals merged); both are mapped to the same FIGARO industries.
    correspondence = data.frame(national_code = c(
      'Agriculture',
      'Mining',
      'Foods and tobacco','Textile and leather','Wood paper printing','Coking gas and petroleum',
      'Oil refining coking chemicals','Chemical industry','Nonmetallic minerals','Metals and metal products',
      'Machinery and equipment','Other manufacturing',
      'Electricity heat gas water','Construction',
      'Wholesale and retail trades','Wholesale and retail','Transport storage post','Transport storage post information',
      'Wholesale retail transport post',
      'Hotels and catering','Wholesale retail hotels catering',
      'Information transmission software IT','Information and software',
      'Financial intermediation','Finance and real estate','Real estate',
      'Leasing and business services','Real estate leasing business services',
      'Scientific research and technical services','Research and technical services',
      'Water conservancy environment public facilities','Services to households repair other','Education',
      'Health and social service','Culture sports entertainment','Public management social security','Other services')) %>%
      mutate(industry = case_when(
        national_code == 'Agriculture' ~ list(c('A01','A02','A03')),
        national_code == 'Mining' ~ list('B'),
        national_code %in% c('Foods and tobacco') ~ list('C10T12'),
        national_code %in% c('Textile and leather') ~ list('C13T15'),
        national_code %in% c('Wood paper printing') ~ list(c('C16','C17','C18')),
        national_code %in% c('Coking gas and petroleum') ~ list('C19'),
        national_code %in% c('Oil refining coking chemicals') ~ list(c('C19','C20','C21','C22')),
        national_code %in% c('Chemical industry') ~ list(c('C20','C21','C22')),
        national_code %in% c('Nonmetallic minerals') ~ list('C23'),
        national_code %in% c('Metals and metal products') ~ list(c('C24','C25')),
        national_code %in% c('Machinery and equipment') ~ list(c('C26','C27','C28','C29','C30')),
        national_code %in% c('Other manufacturing') ~ list(c('C31_32','C33')),
        national_code %in% c('Electricity heat gas water') ~ list(c('D35','E36')),
        national_code %in% c('Construction') ~ list('F'),
        national_code %in% c('Wholesale and retail trades','Wholesale and retail') ~ list(c('G45','G46','G47')),
        national_code %in% c('Wholesale retail transport post') ~ list(c('G45','G46','G47','H49','H50','H51','H52','H53')),
        national_code %in% c('Transport storage post','Transport storage post information') ~ list(c('H49','H50','H51','H52','H53')),
        national_code %in% c('Hotels and catering') ~ list('I'),
        national_code %in% c('Wholesale retail hotels catering') ~ list(c('G45','G46','G47','I')),
        national_code %in% c('Information transmission software IT','Information and software') ~ list(c('J58','J59_60','J61','J62_63')),
        national_code %in% c('Financial intermediation') ~ list(c('K64','K65','K66')),
        national_code %in% c('Finance and real estate') ~ list(c('K64','K65','K66','L')),
        national_code %in% c('Real estate','Real estate leasing business services') ~ list('L'),
        national_code %in% c('Leasing and business services') ~ list(c('M69_70','M71','M72','M73','M74_75','N77','N78','N79','N80T82')),
        national_code %in% c('Scientific research and technical services','Research and technical services') ~ list('M72'),
        national_code %in% c('Water conservancy environment public facilities') ~ list('E37T39'),
        national_code %in% c('Services to households repair other') ~ list(c('S95','S96','T')),
        national_code %in% c('Education') ~ list('P85'),
        national_code %in% c('Health and social service') ~ list(c('Q86','Q87_88')),
        national_code %in% c('Culture sports entertainment') ~ list(c('R90T92','R93','J59_60')),
        national_code %in% c('Public management social security') ~ list('O84'),
        national_code %in% c('Other services') ~ list(c('S94','S96','M74_75','N80T82','R93','T','U')))) %>%
      unnest(industry)
  }

  if(country == 'IN')
  {
    # MoSPI SUT groups of industries, matched by industry name.
    correspondence = data.frame(national_code = c(
      'Agriculture','Livestock','Forestry and Logging','Fishing & Aquaculture',
      'Coal & Lignite','Crude Petroleum','Natural Gas','Iron Ore','Non ferrous metal ores','Other Mining',
      'Production, processing and preservation of meat, fish, fruit, vegetables, oils and fats',
      'Manufacture of dairy products','Manufacture of grain mill products, etc. and animal feeds',
      'Manufacture of other food products','Manufacture of beverages','Manufacture of tobacco products',
      'Manufacture of textiles + cotton ginning','Manufacture of wearing apparel, except custom tailoring',
      'Manufacture of leather and related products',
      'Manufacture of Basic Iron and Steel + Casting of iron and steel',
      'Manufacture of basic precious and non-ferrous metals + Casting of non-ferrous metals',
      'Manufacture of fabricated metal products, except machinery and equipments',
      'Manufacture of electronic component, consumer electronics, magnetic and optical media',
      'Manufacture of computer and peripheral equipment','Manufacture of communication equipments',
      'Manufacture of communication equipment',
      'Manufacture of optical and electronics products n.e.c.','Manufacture of Electrical equipments',
      'Manufacture of machinery and equipments n.e.c.',
      'Manufacture of Transport','Manufacture of Transport equipment',
      'Manufacture of coke and refined petroleum products',
      'Manufacture of chemical and chemical products except pharmaceuticals, medicinal and botanical products',
      'Manufacture of pharmaceutical; medicinal chemicals and botanical products',
      'Manufacture of rubber & plastic products','Manufacture of other non-metallic mineral products',
      'Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting material',
      'Manufacture of paper and paper products','Printing and reproduction of recorded media except publishing',
      'Manufacture of furniture','Other Manufacturing','Repair and installation of machinery and equipments',
      'Construction','Electricity','Gas','Water supply','Other Utility Services',
      'Railway Transport','Land Transport','Water Transport','Air Transport',
      'Supportive & Auxiliary transport activities','Storage & warehousing',
      'Communication','Trade','Hotels & Restaurant',
      'Financial Services','Insurance Services','Ownership of dwellings',
      'Education & Research','Medical and Health','Legal Services',
      'Computer related services','Computer and related services','Other Business services',
      'Renting of machinery and equipment','Real estate activities',
      'Community, Social & personal services','Other services','Public administration. & defence')) %>%
      mutate(industry = case_when(
        national_code %in% c('Agriculture') ~ list('A01'),
        national_code %in% c('Livestock') ~ list('A01'),
        national_code %in% c('Forestry and Logging') ~ list('A02'),
        national_code %in% c('Fishing & Aquaculture') ~ list('A03'),
        national_code %in% c('Coal & Lignite','Crude Petroleum','Natural Gas','Iron Ore','Non ferrous metal ores','Other Mining') ~ list('B'),
        national_code %in% c('Production, processing and preservation of meat, fish, fruit, vegetables, oils and fats',
                             'Manufacture of dairy products','Manufacture of grain mill products, etc. and animal feeds',
                             'Manufacture of other food products','Manufacture of beverages','Manufacture of tobacco products') ~ list('C10T12'),
        national_code %in% c('Manufacture of textiles + cotton ginning',
                             'Manufacture of wearing apparel, except custom tailoring',
                             'Manufacture of leather and related products') ~ list('C13T15'),
        national_code %in% c('Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting material') ~ list('C16'),
        national_code %in% c('Manufacture of paper and paper products') ~ list('C17'),
        national_code %in% c('Printing and reproduction of recorded media except publishing') ~ list('C18'),
        national_code %in% c('Manufacture of coke and refined petroleum products') ~ list('C19'),
        national_code %in% c('Manufacture of chemical and chemical products except pharmaceuticals, medicinal and botanical products') ~ list('C20'),
        national_code %in% c('Manufacture of pharmaceutical; medicinal chemicals and botanical products') ~ list('C21'),
        national_code %in% c('Manufacture of rubber & plastic products') ~ list('C22'),
        national_code %in% c('Manufacture of other non-metallic mineral products') ~ list('C23'),
        national_code %in% c('Manufacture of Basic Iron and Steel + Casting of iron and steel',
                             'Manufacture of basic precious and non-ferrous metals + Casting of non-ferrous metals') ~ list('C24'),
        national_code %in% c('Manufacture of fabricated metal products, except machinery and equipments') ~ list('C25'),
        national_code %in% c('Manufacture of electronic component, consumer electronics, magnetic and optical media',
                             'Manufacture of computer and peripheral equipment',
                             'Manufacture of communication equipments','Manufacture of communication equipment',
                             'Manufacture of optical and electronics products n.e.c.') ~ list('C26'),
        national_code %in% c('Manufacture of Electrical equipments') ~ list('C27'),
        national_code %in% c('Manufacture of machinery and equipments n.e.c.') ~ list('C28'),
        national_code %in% c('Manufacture of Transport','Manufacture of Transport equipment') ~ list(c('C29','C30')),
        national_code %in% c('Manufacture of furniture') ~ list('C31_32'),
        national_code %in% c('Other Manufacturing','Repair and installation of machinery and equipments') ~ list(c('C31_32','C33')),
        national_code %in% c('Construction') ~ list('F'),
        national_code %in% c('Electricity') ~ list('D35'),
        national_code %in% c('Gas') ~ list('D35'),
        national_code %in% c('Water supply') ~ list('E36'),
        national_code %in% c('Other Utility Services') ~ list('E37T39'),
        national_code %in% c('Railway Transport','Land Transport') ~ list('H49'),
        national_code %in% c('Water Transport') ~ list('H50'),
        national_code %in% c('Air Transport') ~ list('H51'),
        national_code %in% c('Supportive & Auxiliary transport activities','Storage & warehousing') ~ list(c('H52','H53')),
        national_code %in% c('Communication') ~ list('J61'),
        national_code %in% c('Trade') ~ list(c('G45','G46','G47')),
        national_code %in% c('Hotels & Restaurant') ~ list('I'),
        national_code %in% c('Financial Services','Insurance Services') ~ list(c('K64','K65')),
        national_code %in% c('Ownership of dwellings','Real estate activities') ~ list('L'),
        national_code %in% c('Education & Research') ~ list(c('P85','M72')),
        national_code %in% c('Medical and Health') ~ list(c('Q86','Q87_88')),
        national_code %in% c('Legal Services','Other Business services','Renting of machinery and equipment') ~ list(c('M69_70','M71','M73','M74_75','N77','N78','N79','N80T82')),
        national_code %in% c('Computer related services','Computer and related services') ~ list(c('J58','J59_60','J62_63')),
        national_code %in% c('Community, Social & personal services','Other services') ~ list(c('R90T92','R93','S94','S95','S96')),
        national_code %in% c('Public administration. & defence') ~ list('O84'))) %>%
      unnest(industry)
  }

  if(country == 'RU')
  {
    # ISIC Rev.3 divisions (2012-2015) and OKVED2 = NACE Rev.2 sections/divisions/groups (2016-2018).
    correspondence = data.frame(classification = c(rep('ISIC3',59),rep('NACE2',61)),
                                national_code = c(
      # ISIC Rev.3
      '01','02','05','10','11','12','13','14','15','16','17','18','19','20','21','22','23*','24*','25','26','27','28',
      '29*','30','31','32','33','34','35*','36','37','40','41','45','50*','51','52*','55','60','61','62','63','64',
      '65','66','67','70','71','72','73','74','75','80','85','90','91','92','93','95',
      # OKVED2 (NACE Rev.2)
      'A 01','A 02','A 03','B (05-09)','C 19','C (10-12)','C (13-15)','C 16','C 17','C 18','C 20','C 21','C 22','C 23',
      'C 24','C 25','C 26','C 27','C 28','C 29','C 30','C (31-32)','C 33','D 35','E 36','E (37-39)','F (41-43)',
      'G 45','G 46','G 47','H 49','H 50','H 51','H 52','H 53','I (55-56)','J 58','J (59-60)','J 61','J (62-63)',
      'K (64-66)','L 68','M (69-70)','M 71','M 72','M 73','M (74-75)','N 77','N 78','N 79','N (80-82)',
      'O 84','P 85','Q 86','Q (87-88)','R (90-92)','R 93','S 94','S 95','S 96','T')) %>%
      mutate(industry = case_when(
        # ISIC Rev.3 divisions
        classification == 'ISIC3' & national_code %in% c('01') ~ list('A01'),
        classification == 'ISIC3' & national_code %in% c('02') ~ list('A02'),
        classification == 'ISIC3' & national_code %in% c('05') ~ list('A03'),
        classification == 'ISIC3' & national_code %in% c('10','11','12','13','14') ~ list('B'),
        classification == 'ISIC3' & national_code %in% c('15','16') ~ list('C10T12'),
        classification == 'ISIC3' & national_code %in% c('17') ~ list('C13T15'),
        classification == 'ISIC3' & national_code %in% c('18','19') ~ list('C13T15'),
        classification == 'ISIC3' & national_code %in% c('20') ~ list('C16'),
        classification == 'ISIC3' & national_code %in% c('21','22') ~ list(c('C17','C18')),
        classification == 'ISIC3' & national_code %in% c('23*') ~ list('C19'),
        classification == 'ISIC3' & national_code %in% c('24*') ~ list(c('C20','C21','C22')),
        classification == 'ISIC3' & national_code %in% c('25') ~ list('C22'),
        classification == 'ISIC3' & national_code %in% c('26') ~ list('C23'),
        classification == 'ISIC3' & national_code %in% c('27','28') ~ list(c('C24','C25')),
        classification == 'ISIC3' & national_code %in% c('29*','30','31','32') ~ list(c('C26','C27','C28')),
        classification == 'ISIC3' & national_code %in% c('33') ~ list(c('C26','C31_32')),
        classification == 'ISIC3' & national_code %in% c('34','35*') ~ list(c('C29','C30')),
        classification == 'ISIC3' & national_code %in% c('36','37') ~ list(c('C31_32','C33')),
        classification == 'ISIC3' & national_code %in% c('40') ~ list('D35'),
        classification == 'ISIC3' & national_code %in% c('41') ~ list('E36'),
        classification == 'ISIC3' & national_code %in% c('45') ~ list('F'),
        classification == 'ISIC3' & national_code %in% c('50*','51','52*') ~ list(c('G45','G46','G47')),
        classification == 'ISIC3' & national_code %in% c('55') ~ list('I'),
        classification == 'ISIC3' & national_code %in% c('60') ~ list('H49'),
        classification == 'ISIC3' & national_code %in% c('61') ~ list('H50'),
        classification == 'ISIC3' & national_code %in% c('62') ~ list('H51'),
        classification == 'ISIC3' & national_code %in% c('63') ~ list(c('H52','N79')),
        classification == 'ISIC3' & national_code %in% c('64') ~ list(c('J61','H53')),
        classification == 'ISIC3' & national_code %in% c('65','66','67') ~ list(c('K64','K65','K66')),
        classification == 'ISIC3' & national_code %in% c('70') ~ list('L'),
        classification == 'ISIC3' & national_code %in% c('71') ~ list('N77'),
        classification == 'ISIC3' & national_code %in% c('72') ~ list('J62_63'),
        classification == 'ISIC3' & national_code %in% c('73') ~ list('M72'),
        classification == 'ISIC3' & national_code %in% c('74') ~ list(c('M69_70','M71','M73','M74_75','N77','N78','N79','N80T82')),
        classification == 'ISIC3' & national_code %in% c('75') ~ list('O84'),
        classification == 'ISIC3' & national_code %in% c('80') ~ list('P85'),
        classification == 'ISIC3' & national_code %in% c('85') ~ list(c('Q86','Q87_88')),
        classification == 'ISIC3' & national_code %in% c('90') ~ list('E37T39'),
        classification == 'ISIC3' & national_code %in% c('91') ~ list('S94'),
        classification == 'ISIC3' & national_code %in% c('92') ~ list(c('R90T92','R93')),
        classification == 'ISIC3' & national_code %in% c('93') ~ list(c('S95','S96')),
        classification == 'ISIC3' & national_code %in% c('95') ~ list('T'),
        # OKVED2 (NACE Rev.2)
        classification == 'NACE2' & national_code %in% c('A 01') ~ list('A01'),
        classification == 'NACE2' & national_code %in% c('A 02') ~ list('A02'),
        classification == 'NACE2' & national_code %in% c('A 03') ~ list('A03'),
        classification == 'NACE2' & national_code %in% c('B (05-09)') ~ list('B'),
        classification == 'NACE2' & national_code %in% c('C 19') ~ list('C19'),
        classification == 'NACE2' & national_code %in% c('C (10-12)') ~ list('C10T12'),
        classification == 'NACE2' & national_code %in% c('C (13-15)') ~ list('C13T15'),
        classification == 'NACE2' & national_code %in% c('C 16') ~ list('C16'),
        classification == 'NACE2' & national_code %in% c('C 17') ~ list('C17'),
        classification == 'NACE2' & national_code %in% c('C 18') ~ list('C18'),
        classification == 'NACE2' & national_code %in% c('C 20') ~ list('C20'),
        classification == 'NACE2' & national_code %in% c('C 21') ~ list('C21'),
        classification == 'NACE2' & national_code %in% c('C 22') ~ list('C22'),
        classification == 'NACE2' & national_code %in% c('C 23') ~ list('C23'),
        classification == 'NACE2' & national_code %in% c('C 24') ~ list('C24'),
        classification == 'NACE2' & national_code %in% c('C 25') ~ list('C25'),
        classification == 'NACE2' & national_code %in% c('C 26') ~ list('C26'),
        classification == 'NACE2' & national_code %in% c('C 27') ~ list('C27'),
        classification == 'NACE2' & national_code %in% c('C 28') ~ list('C28'),
        classification == 'NACE2' & national_code %in% c('C 29') ~ list('C29'),
        classification == 'NACE2' & national_code %in% c('C 30') ~ list('C30'),
        classification == 'NACE2' & national_code %in% c('C (31-32)') ~ list('C31_32'),
        classification == 'NACE2' & national_code %in% c('C 33') ~ list('C33'),
        classification == 'NACE2' & national_code %in% c('D 35') ~ list('D35'),
        classification == 'NACE2' & national_code %in% c('E 36') ~ list('E36'),
        classification == 'NACE2' & national_code %in% c('E (37-39)') ~ list('E37T39'),
        classification == 'NACE2' & national_code %in% c('F (41-43)') ~ list('F'),
        classification == 'NACE2' & national_code %in% c('G 45') ~ list('G45'),
        classification == 'NACE2' & national_code %in% c('G 46') ~ list('G46'),
        classification == 'NACE2' & national_code %in% c('G 47') ~ list('G47'),
        classification == 'NACE2' & national_code %in% c('H 49') ~ list('H49'),
        classification == 'NACE2' & national_code %in% c('H 50') ~ list('H50'),
        classification == 'NACE2' & national_code %in% c('H 51') ~ list('H51'),
        classification == 'NACE2' & national_code %in% c('H 52') ~ list('H52'),
        classification == 'NACE2' & national_code %in% c('H 53') ~ list('H53'),
        classification == 'NACE2' & national_code %in% c('I (55-56)') ~ list('I'),
        classification == 'NACE2' & national_code %in% c('J 58') ~ list('J58'),
        classification == 'NACE2' & national_code %in% c('J (59-60)') ~ list('J59_60'),
        classification == 'NACE2' & national_code %in% c('J 61') ~ list('J61'),
        classification == 'NACE2' & national_code %in% c('J (62-63)') ~ list('J62_63'),
        classification == 'NACE2' & national_code %in% c('K (64-66)') ~ list(c('K64','K65','K66')),
        classification == 'NACE2' & national_code %in% c('L 68') ~ list('L'),
        classification == 'NACE2' & national_code %in% c('M (69-70)') ~ list('M69_70'),
        classification == 'NACE2' & national_code %in% c('M 71') ~ list('M71'),
        classification == 'NACE2' & national_code %in% c('M 72') ~ list('M72'),
        classification == 'NACE2' & national_code %in% c('M 73') ~ list('M73'),
        classification == 'NACE2' & national_code %in% c('M (74-75)') ~ list('M74_75'),
        classification == 'NACE2' & national_code %in% c('N 77') ~ list('N77'),
        classification == 'NACE2' & national_code %in% c('N 78') ~ list('N78'),
        classification == 'NACE2' & national_code %in% c('N 79') ~ list('N79'),
        classification == 'NACE2' & national_code %in% c('N (80-82)') ~ list('N80T82'),
        classification == 'NACE2' & national_code %in% c('O 84') ~ list('O84'),
        classification == 'NACE2' & national_code %in% c('P 85') ~ list('P85'),
        classification == 'NACE2' & national_code %in% c('Q 86') ~ list('Q86'),
        classification == 'NACE2' & national_code %in% c('Q (87-88)') ~ list('Q87_88'),
        classification == 'NACE2' & national_code %in% c('R (90-92)') ~ list('R90T92'),
        classification == 'NACE2' & national_code %in% c('R 93') ~ list('R93'),
        classification == 'NACE2' & national_code %in% c('S 94') ~ list('S94'),
        classification == 'NACE2' & national_code %in% c('S 95') ~ list('S95'),
        classification == 'NACE2' & national_code %in% c('S 96') ~ list('S96'),
        classification == 'NACE2' & national_code %in% c('T') ~ list('T'))) %>%
      unnest(industry)
  }

  return(correspondence)
}
