# Resolve
#
# Resolve the address of a user, playlist, set, comments, groups etc.
#
# Parameters:
#     - client_id: soundcloud client id. See loginDetails() for more information
#     - soundcloud_url: soundcloud url to resolve
#
# Returns:
#     - character string with the resolved soundcloud url
#
# Sample use:
#     res <- resolve(client_id, "https://soundcloud.com/the-bugle/bonus-bugle-satirist-in-soho-3")

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

