# SCapi_general()
#
# Return results from users, tracks, groups, comments. General
#
# Parameters:
#   - client_id: The soundcloud client id of your application. See loginDetails() for more information
#   - type: What would you like to query? Must be one of "users", "tracks", "groups", "comments"
#   - limit: how many results should be returned? Soundcloud allows you to query 200 results per query. If you want more results,
#            this is possible by paginating (done automatically).
#   - filter: Use if you want to add filters to the query. See: http://bit.ly/1OwCaUC for more information. Filters must be added
#             as a list, e.g. filter = list("q" = "the-bugle").
#   - offset: determines at which result we start searching. For example, if we do a generic query for "users", which returns all
#             users for the limit we set (e.g. 1000), and we use offset = 1000, then we begin our search at the 1001st user, thus
#             returning the information of users between 1000 and 2000.
#
# Returns: list with soundcloud query results. Length of list depends on query and limit specified by user.
#
# Sample use:
#        sc_res <- SCapi_general(client_id, type="users", limit=50, filter=list("q" = "em-mcrae", "q" = "the-bugle"))

SCapi_general <- function(client_id,
                          type = c("users", "tracks", "groups", "comments"),
                          limit = 50,
                          filter = NULL,
                          offset = NULL) {

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
      # If empty, break and return what we have until now
      if(!is.null(emptyRes(tempCall))) {
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
  CREATE URL
  ++++++++++++++++
  '

  # Add filters
  addon <- filter
  # If filters exist, then create
  if(length(addon) != 0) {
    # Master
    filters <- c()
    for(i in 1:length(addon)) {
      cons <- paste0("&", names(addon[i]), "=", unname(unlist(addon[i])))
      filters <- c(filters,cons)
    }
    # Decompose
    filters <- paste0(filters, collapse="")
  }
  if(length(addon) != 0) {
    # Create url
    page_url <- paste0("http://api.soundcloud.com/", type, "?client_id=", client_id, "&limit=", limit, filters)
    if(!is.null(offset)) {
      page_url <- paste0(page_url, "&offset=", offset)
    }
  } else{
    # Create url
    page_url <- paste0("http://api.soundcloud.com/", type, "?client_id=", client_id, "&limit=", limit)
    if(!is.null(offset)) {
      page_url <- paste0(page_url, "&offset=", offset)
    }
  }

  '
  +++++++++++++++
  Client ID check
  +++++++++++++++
  '

  # Check if client ID is legal
  curl = getCurlHandle()
  res <- fromJSON(getURL(paste0("http://api.soundcloud.com/", type, "?client_id=", client_id), curl = curl))
  rm(curl)
  # Check for errors
  error <- errorHandling(res)
  if(!is.null(error)) {
    stop(paste0(error,
                " (your client ID is not valid. Please check your details and try again.)"))
  }

  '
  +++++++++++++++++
  QUERY SOUNDCLOUD
  +++++++++++++++++
  '

  # Get results
  res <- fromJSON(file = page_url, method = "C")
  # If limit > 200
  if(limit > 200) {
    # Paginate
    res <- paginate(res, page_url, limit)
    # If results, then bind with master
    if(!is.null(res)) {
      res <- res
    }
  }

  # Return
  return(res)

}
