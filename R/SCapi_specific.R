
SCapi_specific <- function(client_id,
                           soundcloud_search,
                           type = c("url", "id", "name"),
                           query_type = c("users", "tracks", "playlists", "groups", "comments"),
                           limit = 50,
                           get = NULL,
                           filter = NULL,
                           ...) {

  '
  ++++++++++++++++++++++++++++++++++++++++++++++
  Check legality of type & query_type parameters
  ++++++++++++++++++++++++++++++++++++++++++++++
  '

  # TYPE

  # if type is not chosen
  if(length(type) > 1) {
    stop(paste0("Please specify the type of your search term (e.g. url to soundcloud page,",
                "a soundcloud ID, or a user name.)"))
  } else{
    type <- match.arg(type)
  }
  # If type is not one of possibilities
  if(!type %in% c("url", "id", "name")) {
    stop(paste0("You can only search by soundcloud ID, soundcloud user name, or a url to",
                "a track, user or playlist etc."))
  }

  # QUERY_TYPE

  # if query_type is not chosen
  if(length(query_type) > 1) {
    stop(paste0("Please choose a query type (e.g. users, playlists etc.)"))
  } else{
    type <- match.arg(type)
  }
  # If query_type is not one of possibilities
  if(!query_type %in% c("users", "tracks", "playlists", "groups", "comments")) {
    stop(paste0("Please choose one of ",
                "users ", "tracks ", "playlists ", "groups ", "comments ",
                "as a query type."))
  }

  # GET
  if(length(get) > 1) {
    stop(paste0("'get' can only contain 1 argument."))
  }

  '
  ++++++++++++++++
  Helper functions
  ++++++++++++++++
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

  # FUNCTION 3: Resolve urls via the SC api

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

  # FUNCTION 4: Construct search urls

  constructURL <- function(client_id, soundcloud_search, type, limit, query_type, get, addon) {
    # If url, resolve
    if(type == "url" | type == "name") {
      # If user name, create url
      if(type == "name") {
        soundcloud_search <- paste0("https://soundcloud.com/",
                                    soundcloud_search)
      }

      # If query_type = user then take any url but reduce to generic SC page
      if(query_type == "users") {
        # Strip to bare
        soundcloud_search <- gsub(".*soundcloud.com/", "", soundcloud_search)
        if(grepl("/", soundcloud_search)) {
          soundcloud_search <- unlist(strsplit(soundcloud_search, "/"))[1]
        }
        soundcloud_search <- paste0("https://soundcloud.com/",
                                    soundcloud_search)
      }

      # Resolve
      url <- resolve(client_id, soundcloud_search)
      # Add get arguments
      if(!is.null(get)) {
        # Split
        sp <- unlist(strsplit(url, "\\?"))
        url <- paste0(sp[1], "/",
                      get, "?",
                      sp[2])
      }
      # Add filters
      if(length(addon) != 0) {
        # Take client id
        cliID <- unlist(strsplit(url, "\\?"))[2]
        url <- unlist(strsplit(url, "\\?"))[1]
        for(i in 1:length(addon)) {
          cons <- paste0("&", names(addon[i]), "=", unname(unlist(addon[i])))
          url <- paste0(url, cons)
        }
        # Stick client id back on
        url <- paste0(url, "?", cliID)
      }
    }
    if(type == "id") {
      # Create url
      url <- paste0("http://api.soundcloud.com/",
                    query_type, "/",
                    soundcloud_search,
                    "?client_id=",
                    client_id)
    }
    # Add limit and return
    url <- paste0(url, "&limit=", limit)
    return(url)
  }

  # FUNCTION 5: Paginate through results if limit > 200

  paginate <- function(jsonDoc, res_link, limit) {
    # Check length of jsonDoc. If < 200, then there are no more results
    if(length(jsonDoc) < 200) {
      warning(paste0("You specified the query limit to be ",
                     limit,
                     ", but there are only ",
                     length(jsonDoc),
                     " results. Returning ",
                     length(jsonDoc), " results."))
      return(NULL)
    }
    # Decrease limit
    limit <- limit - 200
    # Offset value (i.e. where to start)
    offset <- 200
    # While limit > 0, make new calls.
    while(limit > 0) {
      # if limit < 200, limit == remainder of modulo
      if(limit < 200) {
        tempLim <- limit %% 200
        # Change limit
        res_link <- paste0(gsub("limit=[0-9].*", "", res_link), "limit=", tempLim)
      }
      # Temp url
      tempURL <- paste0(res_link, "&offset=", offset)
      # Call
      tempCall <- fromJSON(file = tempURL, method='C')
      # If empty, break
      if(is.null(emptyRes(tempCall))) {
        warning(paste0("End of results. Returning results up to now."))
        return(jsonDoc)
      }
      # If error, break & warning
      if(!is.null(errorHandling(tempCall))) {
        warning("There occurred an error. You may have exceeded your API requests.")
        return(jsonDoc)
      }
      # Add to master
      jsonDoc <- c(jsonDoc, tempCall)
      # Increase offset
      offset <- offset + 200
      # Decrese limit
      limit <- limit - 200
    }
    # Return
    return(jsonDoc)
  }

  '
  ++++++++++++++++
  CONSTRUCT URL
  ++++++++++++++++
  '

  # Construct url
  # if type is url, then OK. Add list of filters
  page_url <- constructURL(client_id, soundcloud_search, type, limit, query_type, get, filter)

  '
  +++++++++++++++
  Client ID check
  +++++++++++++++
  '

  # Check if client ID is legal
  curl = getCurlHandle()
  res <- fromJSON(getURL(page_url, curl = curl))
  rm(curl)
  # Check for errors
  error <- errorHandling(res)
  if(!is.null(error)) {
    stop(paste0(error,
                " (your client ID is not valid. Please check your details and try again.)"))
  }

  '
  ++++++++++++++++
  QUERY SOUNDCLOUD
  ++++++++++++++++
  '

  # Get results
  results <- fromJSON(file = page_url, method = "C")
  # If limit > 200
  if(limit > 200) {
    # Paginate
    res <- paginate(results, page_url, limit)
    # If results, then bind with master
    if(!is.null(res)) {
      results <- c(results, res)
    }
  }

  # Return
  return(results)

}

v <- SCapi_specific(client_id,
               "https://soundcloud.com/the-source-2013/sets",
               type = "url",
               query_type = "playlists",
               limit = 50,
               get = NULL)
