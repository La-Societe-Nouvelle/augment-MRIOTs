
# Capital use of each vintage of the CFC of `year`, in the FIGARO format. The vintages add up to the capital use of the build.

capital_use_by_vintage = function(year,built_capital_use,first_year = 2010,last_year = 2021,price = FALSE)
{
  asset_detail = built_capital_use$ASSET_DETAIL

  countries = names(asset_detail$cfc_by_asset)

  # 1. Raw data : stock by vintage and price factor of each vintage, for every country x EU KLEMS sector x asset

  panel = import_klems_vintage_panel(first_year,last_year)

  vintages = panel$vintages[panel$vintages <= year]

  country_sector_asset = panel$country_sector_asset

  stock_by_vintage = matrix(panel$V[,match(year,panel$years),],nrow = nrow(country_sector_asset))

  #A vintage bought in year v is worth 1 / (change of the stock price after v, up to year) euros of the year of v
  price_by_vintage = sapply(panel$vintages,function(v) 1 / apply(panel$rho[,panel$years > v & panel$years <= year,drop = FALSE],1,prod))

  observed = expand.grid(combination = 1:nrow(country_sector_asset),vintage = panel$vintages) %>%
    mutate(country = country_sector_asset$geo_code[combination],
           sector = country_sector_asset$nace_r2_code[combination],
           asset = factor(country_sector_asset$ASSET[combination],KLEMS_ASSETS),
           stock = as.vector(stock_by_vintage),
           price = as.vector(price_by_vintage))

  stock_array = xtabs(stock ~ country + asset + sector + vintage,observed)

  price_array = xtabs(price ~ country + asset + sector + vintage,observed)

  #Total economy of the EU : mean of the shares by vintage of the EU countries (national stocks are in different currencies)
  eu_total = country_sector_asset$geo_code %in% EU27_KLEMS & country_sector_asset$nace_r2_code == 'TOT' & rowSums(stock_by_vintage) > 0 & rowSums(!is.finite(price_by_vintage)) == 0

  share_by_vintage = stock_by_vintage / rowSums(stock_by_vintage)

  total_share = sapply(KLEMS_ASSETS,function(a) colMeans(share_by_vintage[eu_total & country_sector_asset$ASSET == a,,drop = FALSE]))

  total_price = sapply(KLEMS_ASSETS,function(a) colMeans(price_by_vintage[eu_total & country_sector_asset$ASSET == a,,drop = FALSE]))

  vintage_shares = list()

  for(country in countries)
  {
    industries = colnames(asset_detail$cfc_by_asset[[country]])

    share = array(NA_real_,c(length(panel$vintages),length(KLEMS_ASSETS),length(industries)),dimnames = list(panel$vintages,KLEMS_ASSETS,industries))

    price_factor = array(NA_real_,dim(share),dimnames = dimnames(share))

    stock_weights = asset_detail$gi_weights[[country]]

    # 2. Observed data : EU KLEMS countries, the stock of each sector goes to the industries with the weights of the stock (stock_weights : industries x sectors)

    if(!is.null(stock_weights))
    {
      klems_code = ifelse(country == 'GB','UK',ifelse(country == 'GR','EL',country))

      sectors = intersect(colnames(stock_weights),dimnames(stock_array)$sector)

      for(asset in KLEMS_ASSETS)
      {
        sector_stock = matrix(stock_array[klems_code,asset,sectors,],nrow = length(sectors))
        sector_price = matrix(price_array[klems_code,asset,sectors,],nrow = length(sectors))

        industry_stock = stock_weights[,sectors,drop = FALSE] %*% sector_stock

        industry_price = stock_weights[,sectors,drop = FALSE] %*% (sector_stock * sector_price) / industry_stock

        for(industry in industries)
        {
          if(is.finite(sum(industry_stock[industry,])) && sum(industry_stock[industry,]) > 0)
          {
            share[,asset,industry] = industry_stock[industry,] / sum(industry_stock[industry,])

            price_factor[,asset,industry] = ifelse(industry_stock[industry,] > 0,industry_price[industry,],1)
          }
        }
      }
    }

    vintage_shares[[country]] = list(share = share,price_factor = price_factor)
  }

  # 3. Imputation : a missing cell takes the mean of the EU countries that have it, or the total economy of the EU if none has it

  eu_sum_share = eu_sum_price = eu_count = 0

  for(country in intersect(ifelse(EU27_KLEMS == 'EL','GR',EU27_KLEMS),countries))
  {
    observed = !is.na(vintage_shares[[country]]$share)

    eu_sum_share = eu_sum_share + ifelse(observed,vintage_shares[[country]]$share,0)

    eu_sum_price = eu_sum_price + ifelse(observed,vintage_shares[[country]]$price_factor,0)

    eu_count = eu_count + observed
  }

  eu_share = ifelse(eu_count > 0,eu_sum_share / eu_count,array(total_share,dim(eu_count)))

  eu_price = ifelse(eu_count > 0,eu_sum_price / eu_count,array(total_price,dim(eu_count)))

  for(country in countries)
  {
    missing = is.na(vintage_shares[[country]]$share)

    vintage_shares[[country]]$share[missing] = eu_share[missing]

    vintage_shares[[country]]$price_factor[missing] = eu_price[missing]
  }

  # 4. Capital use of each vintage

  capital_use = list()

  for(vintage in vintages)
  {
    k = match(vintage,panel$vintages)

    cfc_share = list()

    for(country in countries)
    {
      cfc_share[[country]] = vintage_shares[[country]]$share[k,,]

      if(price) cfc_share[[country]] = cfc_share[[country]] * vintage_shares[[country]]$price_factor[k,,]
    }

    capital_use[[as.character(vintage)]] = capital_use_of_cfc_share(cfc_share,built_capital_use)
  }

  return(list(share = vintage_shares,capital_use = capital_use))
}

# Capital use of a part of the CFC. Conversion pipeline : compilation and RAS

capital_use_of_cfc_share = function(cfc_share,built_capital_use)
{
  asset_detail = built_capital_use$ASSET_DETAIL

  product_tables = list()

  for(country in names(cfc_share))
  {
    cfc = as.matrix(asset_detail$cfc_by_asset[[country]])

    kept_cfc = cfc * cfc_share[[country]][rownames(cfc),colnames(cfc)]

    product_tables[[country]] = t(asset_detail$Gp[[country]]) %*% kept_cfc
  }

  product_by_industry = distribute_origin_suppliers(asset_detail$figaro,list(Stock = product_tables,CFC = product_tables),verbose = FALSE)$CFC

  industry_by_industry = as.matrix(built_capital_use$RAS_FACTORS$fixed_sales[,rownames(product_by_industry)] %*% as.matrix(product_by_industry))

  capital_use = industry_by_industry %>%
    sweep(1,built_capital_use$RAS_FACTORS$rows,`*`) %>%
    sweep(2,built_capital_use$RAS_FACTORS$columns,`*`)

  return(capital_use)
}
