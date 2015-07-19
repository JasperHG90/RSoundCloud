
SCapi_specific <- function(client_id,
                         soundcloud_search,
                         type = c("url", "id", "name"),
                         limit = 50,
                         ...) {

  '
  Check legality of parameters
  '

  # if type is not chosen
  if(length(type) > 1) {
    stop(paste0("Please specify the type of your search term (e.g. url to soundcloud page,",
                "a soundcloud ID, or a user name."))
  } else{
    type <- match.arg(type)
  }
  # If type is not one of possibilities
  if(!type %in% c("url", "id", "name")) {
    stop(paste0("You can only search by soundcloud ID, soundcloud user name, or a url to",
                "a track, user or playlist etc."))
  }
  # Check if client ID is legal
  curl = getCurlHandle()
  res <- fromJSON(getURL(url, curl = curl))
  rm(curl)
  # Check for errors
  error <- errorHandling(res)
  if(!is.null(error)) {
    stop(error)
  }
  # Construct url
  # if type is url, then OK
  if




  '
  Define helper functions
  '

  # FUNCTION 1: Check for errors. Takes result of a 'getURL' json file

  errorHandling <- function(JSON) {
    # Check for errors
    if(length(JSON$errors) != 0) {
      errorMes <- JSON$errors[[1]]$error_message
      return(errorMes)
    } else{
      return(NULL)
    }
  }

  # FUNCTION 2: Check if result is empty

  emptyRes <- function(JSON) {
    # Check if empty
    if(length(JSON) < 1) {
      return("Error: Empty JSON file (no results)")
    } else{
      return(NULL)
    }
  }

  # FUNCTION 2: Resolve urls via the SC api

  resolve <- function(client_id, soundcloud_url){
    # Base
    base <- paste0("http://api.soundcloud.com/resolve?url=", soundcloud_url,
                   "&client_id=", client_id)
    # Get redirect
    curl = getCurlHandle()
    res <- fromJSON(getURL(base, curl = curl))
    rm(curl) # release curl
    # Error handling
    error <- errorHandling(res)
    if(!is.null(error)) {
      stop(error)
    }
    # Empty JSON?
    empty <- emptyRes(res)
    if(!is.null(empty)) {
      stop(empty)
    }
    # Get redirect url
    red_url <- res$location
    # Change https to http
    red_url <- gsub("https", "http", red_url, fixed=T)
    # Return
    return(red_url)
  }

  # FUNCTION 2: Construct search urls

  constructURL <- function(client_id, soundcloud_search, type) {
    # If url, resolve
    if(type == "url") {
      # Resolve
      url <- resolve(client_id, soundcloud_search)
      # Replace https with http and return

      return(url)
    }
    if(type == "name") {
      # Resolve
    }
  }


}

SCapiGeneral("3242342", "user", limit = 50)
