# Resolve
#
# Resolve the address of a user, playlist, set, comments, groups etc.
#
# Parameters:
#
# Returns:
#
# Sample use:

resolve <- function(client_id, soundcloud_url){
  # Base
  base <- paste0("http://api.soundcloud.com/resolve?url=", soundcloud_url,
                 "&client_id=", client_id)
  # Get redirect
  curl = getCurlHandle()
  res <- fromJSON(getURL(base, curl = curl))
  rm(curl) # release curl
  # Control if not found
  if(length(res$errors) != 0) {
    stop("The url you entered did not yield results. Please check your input and try again.")
  }
  # Get redirect url
  red_url <- res$location
  # Return
  return(red_url)
}

