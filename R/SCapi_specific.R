#' Return query results for specific users / tracks / playlists/ groups or comments.
#'
#' A client ID is necessary to query results from the SoundCloud API. See \code{\link{loginDetails}}
#'
#' For additional information about filters and the soundcloud API, see: http://bit.ly/1OwCaUC
#' @param client_id The soundcloud client id of your application. See loginDetails() for more information
#' @param soundcloud_search Primary search query. Must be a name of a soundcloud track or user, a url pointing to a search query, or a soundcloud id.
#' @param type Type of your soundcloud_search. Must be one of "url", "id", or "name".
#' @param query_type What would you like to query? depends on your soundcloud_search input value. E.g. if you are querying a user, use "users". If you are querying tracks, use "tracks" etc. See documentation for more information.
#' @param limit how many results should be returned? Soundcloud allows you to query 200 results per query. If you want more results, this is possible by paginating (done automatically).
#' @param verbose prints the url to query for every request.
#' @param get Any additional information for 'users'. For example, if you want to query all tracks for a user, choose 'tracks'. For more information on get requests, see section 'subresources' under 'users': http://bit.ly/1OwCaUC. Defaults to NULL.
#' @param filter Use if you want to add filters to the query. See: http://bit.ly/1OwCaUC for more information. Filters must be added as a list, e.g. filter = list("q" = "the-bugle").
#' @param ... any other arguments. Currently not used.
#' @seealso \code{\link{loginDetails}}
#' @examples \dontrun{
#' Query up to 50 results of the playlists from url provided.
#' results <- SCapi_specific(client_id, "https://soundcloud.com/the-source-2013/sets",
#'                           type = "url", query_type = "playlists", limit = 50, get = NULL,
#'                           filter=NULL)
#' }
#' @author Jasper Ginn
#' @importFrom RCurl getCurlHandle
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @export

SCapi_specific <- function(client_id,
                           soundcloud_search,
                           type = c("url", "id", "name"),
                           query_type = c("users", "tracks", "playlists", "groups", "comments"),
                           limit = 50,
                           verbose = FALSE,
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

  # If filter isn't a list, then error
  if(!is.null(filter)) {
    if(mode(filter) != "list") {
      stop(paste0("'filter' argument only takes a list"))
    }
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
    # If user name, create url
    if(type == "name" | type == "url") {
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
    }
    # If ID, create URL directly
    if(type == "id") {
      # Create url
      url <- paste0("http://api.soundcloud.com/",
                                  query_type, "/",
                                  soundcloud_search,
                                  "?client_id=",
                                  client_id)
    }
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
      #cliID <- unlist(strsplit(url, "\\?"))[2]
      #url <- unlist(strsplit(url, "\\?"))[1]
      for(i in 1:length(addon)) {
        cons <- paste0("&", names(addon[i]), "=", unname(unlist(addon[i])))
        url <- paste0(url, cons)
      }
      # Stick client id back on
      #url <- paste0(url, "?", cliID)
    }
  # Add limit and return
  url <- paste0(url, "&limit=", limit)
  return(url)
  }

  # FUNCTION 5: Paginate through results if limit > 200

  paginate <- function(res_link, limit, verbose) {
    # move from limit to offset
    uppLim <- limit - 200
    limit <- 200
    # Query first batch
    res_link <- gsub("limit=[0-9].", "limit=200", res_link)
    # if verbose is TRUE
    if(verbose == TRUE) {
      cat(paste0("\nFetching results for ", page_url))
    }
    # Get
    jsonDoc <- fromJSON(file = res_link, method = "C")
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
    # Offset value (i.e. where to start)
    offset <- 200
    # While limit > 0, make new calls.
    while(uppLim > 0) {
      # if limit < 200, limit == remainder of modulo
      if(uppLim < 200) {
        tempLim <- uppLim %% 200
        # Change limit
        res_link <- gsub("limit=[0-9].", paste0("limit=", tempLim), res_link)
      }
      # Temp url
      tempURL <- paste0(res_link, "&offset=", offset)
      # if verbose is TRUE
      if(verbose == TRUE) {
        cat(paste0("\nFetching results for ", tempURL))
      }
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
      # Decrease limit
      uppLim <- uppLim - 200
    }
    # Return
    return(jsonDoc)
  }

  '
  ++++++++++++++++
  CONSTRUCT URL
  ++++++++++++++++
  '

  # if type is url, then OK. Add list of filters
  page_url <- constructURL(client_id, soundcloud_search, type, limit, query_type, get, filter)

  '
  ++++++++++++++++
  QUERY SOUNDCLOUD
  ++++++++++++++++
  '

  # Get results
  if(limit <= 200) {
    # if verbose is TRUE
    if(verbose == TRUE) {
      cat(paste0("\nFetching results for ", page_url))
    }
    # get results
    results <- fromJSON(file = page_url, method = "C")
  }
  # If limit > 200
  if(limit > 200) {
    # Paginate
    results <- paginate(page_url, limit, verbose)
    # If results, then bind with master
    if(!is.null(res)) {
      return(NULL)
    }
  }
  # Return
  return(results)

}
