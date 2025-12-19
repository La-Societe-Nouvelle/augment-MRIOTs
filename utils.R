get_cache_file = function(link,pattern,verbose = TRUE)
{

  search_in_temp = list.files(dirname(tempdir()),pattern = pattern,full.names = T,recursive = T)

  if(length(search_in_temp) == 1)
  {

    if(verbose) message("Cached data found for pattern : ",pattern)

    return(search_in_temp)
  }else{

    if(length(search_in_temp) > 1) unlink(search_in_temp,recursive = T)

    dest_file = curl_download(link,destfile = tempfile(pattern = pattern))

    return(dest_file)

  }


}
