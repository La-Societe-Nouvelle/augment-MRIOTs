
EU27_KLEMS = c('AT','BE','BG','CY','CZ','DE','DK','EE','EL','ES','FI','FR','HR','HU','IE','IT','LT','LU','LV','MT','NL','PL','PT','RO','SE','SI','SK')

KLEMS_ASSETS = c('CT','Cult','IT','OCon','OIPP','OMach','RD','Rstruc','Soft_DB','TraEq')

#Industries of the ICT sector, of manufacturing and of the EU27 countries, the groups studied

ICT_PATTERN = '26|46|58|61|62|63|95'

EU27_FIGARO = c('AT','BE','BG','CY','CZ','DE','DK','EE','ES','FI','FR','GR','HR','HU','IE','IT','LT','LU','LV','MT','NL','PL','PT','RO','SE','SI','SK')

STUDIED_PATTERNS = list(ICT = ICT_PATTERN,MANUFACTURING = '_C[0-9]',EU = paste0('^(',paste(EU27_FIGARO,collapse = '|'),')_'))

get_cache_file = function(link,pattern,verbose = TRUE)
{
  search_in_temp = list.files(dirname(tempdir()),pattern = paste0('^',pattern),full.names = T,recursive = T)

  if(length(search_in_temp) == 1)
  {
    if(verbose) message("Cached data found for pattern : ",pattern)

    return(search_in_temp)
  }else{

    if(length(search_in_temp) > 1) unlink(search_in_temp,recursive = T)

    dest_file = tempfile(pattern = paste0(pattern,'_'))

    part = tempfile() # the file takes its cache name only once the download is complete

    response = request(link) %>% req_timeout(300) %>% req_retry(max_tries = 5,retry_on_failure = TRUE,backoff = ~ 10) %>% req_perform(path = part) #retries on timeouts and server errors

    if(grepl('html',resp_content_type(response))) { unlink(part) ; stop('HTML page received instead of data : ',link) }

    file.rename(part,dest_file)

    return(dest_file)

  }


}

# Spectral radius of a non-negative matrix by power iteration. A spectral radius below one is the Hawkins-Simon condition :
# the Leontief inverse (I - A)^-1 then exists and is non-negative, i.e. the economy stays productive.

spectral_radius = function(A,iterations = 500,tol = 1e-10)
{
  v = rep(1,ncol(A)) / ncol(A) ; r = 0

  for(i in seq_len(iterations))
  {
    w = as.numeric(A %*% v) ; r_new = sqrt(sum(w^2)) / sqrt(sum(v^2))

    if(!is.finite(r_new) || r_new == 0) return(0)

    v = w / sqrt(sum(w^2))

    if(abs(r_new - r) < tol * r_new) { r = r_new ; break }

    r = r_new
  }

  r
}
