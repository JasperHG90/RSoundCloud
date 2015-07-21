
SCapi_general <- function(user_id,
                          type = c("users", "tracks", "groups", "comments"),
                          limit = 50,
                          filter = NULL) {

  '
  +++++++++++++++++++++++++++++++++++
  Check legality of type parameter
  +++++++++++++++++++++++++++++++++++
  '

  # TYPE

  # if type is not chosen
  if(length(type) > 1) {
    stop(paste0("Please specify the type of your search term (e.g. users, tracks, groups or comments)"))
  } else{
    type <- match.arg(type)
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

  # FUNCTION 3: Paginate through results if limit > 200

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
