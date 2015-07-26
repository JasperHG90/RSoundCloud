#' Return results from users, tracks, groups, comments.
#'
#' A client ID is necessary to query results from the SoundCloud API. See \code{\link{loginDetails}}. The SCapi_general function differs from the SCapi_specific function in that it only queries "generic" results. To query e.g. the tracks of a specific user, or to get the results for a specific user, see \code{\link{SCapi_specific}}.
#'
#' For additional information about filters and the soundcloud API, see: http://bit.ly/1OwCaUC
#' @param client_id The soundcloud client id of your application. See loginDetails() for more information
#' @param type What would you like to query? Must be one of "users", "tracks", "groups", "comments"
#' @param limit how many results should be returned? Soundcloud allows you to query 200 results per query. If you want more results, this is possible by paginating (done automatically).
#' @param filter Use if you want to add filters to the query. See: http://bit.ly/1OwCaUC for more information. Filters must be added as a list, e.g. filter = list("q" = "the-bugle").
#' @param offset determines at which result we start searching. For example, if we do a generic query for "users", which returns all users for the limit we set (e.g. 1000), and we use offset = 1000, then we begin our search at the 1001st user, thus returning the information of users between 1000 and 2000.
#' @seealso \code{\link{loginDetails}}
#' @examples \dontrun{
#' Query up to 50 results of users that conform to the query "em-mcrae" or "the-bugle"
#' sc_res <- SCapi_general(client_id, type="users", limit=50,
#'                         filter=list("q" = "em-mcrae", "q" = "the-bugle"))
#' }
#' @author Jasper Ginn
#' @importFrom RCurl getCurlHandle
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @export

SCapi_general <- function(client_id,
                          type = c("users", "tracks", "groups", "comments"),
                          limit = 50,
                          filter = NULL,
                          offset = NULL) {

  '
  +++++++++++++++++++++++++++++++++++
  Check legality of parameters
  +++++++++++++++++++++++++++++++++++
  '

  # if type is not chosen
  if(length(type) > 1) {
    stop(paste0("Please specify the type of your search term (e.g. users, tracks, groups or comments)"))
  } else{
    type <- match.arg(type)
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

  # FUNCTION 3: Paginate through results if limit > 200

  paginate <- function(res_link, limit) {
    # move from limit to offset
    uppLim <- limit - 200
    limit <- 200
    # Query first batch
    res_link <- gsub("limit=[0-9].", "limit=200", res_link)
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

  # FUNCTION 4: CONSTRUCT URL
  constructURL <- function(filter, type, client_id, limit, offset) {
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
      page_url <- paste0("http://api.soundcloud.com/", type, "?client_id=", client_id, "&limit=",
                         limit, filters)
      if(!is.null(offset)) {
        page_url <- paste0(page_url, "&offset=", offset)
      }
    } else{
      # Create url
      page_url <- paste0("http://api.soundcloud.com/", type, "?client_id=", client_id, "&limit=",
                         limit)
      if(!is.null(offset)) {
        page_url <- paste0(page_url, "&offset=", offset)
      }
    }
    # Return
    return(page_url)
  }

  '
  ++++++++++++++++
  CREATE URL
  ++++++++++++++++
  '

  page_url <- constructURL(filter, type, client_id, limit, offset)

  '
  +++++++++++++++++
  QUERY SOUNDCLOUD
  +++++++++++++++++
  '

  # if verbose is TRUE
  if(verbose == TRUE) {
    cat(paste0("Fetching results for ", page_url))
  }

  # Get results
  if(limit <= 200) {
    res <- fromJSON(file = page_url, method = "C")
  }
  # If limit > 200
  if(limit > 200) {
    # Paginate
    res <- paginate(page_url, limit)
    # If results, then bind with master
    if(is.null(res)) {
      return(NULL)
    }
  }
  # Return
  return(res)

}
