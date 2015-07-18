# SCapi()
#
# Query the soundcloud API
#
# Parameters:
#
# Returns:
#
# Sample use:


SCapi <- function(client_id,
                  searchterm = NULL,
                  soundcloud_id = NULL,
                  soundcloud_link = NULL,
                  limit = 50,
                  ... ) {

  # Control
  if(all(is.null(soundcloud_id), is.null(soundcloud_link), is.null(searchterm))) {
    stop("You need to specify either a soundcloud id or a soundcloud link to that item.")
  }

  '
  Define paginater function
  '

  paginate <- function(jsonDoc, res_link, limit) {
    # Decrease limit
    limit <- limit - 200
    # Offset value (i.e. where to start)
    offset <- 200
    # While limit > 0, make new calls.
    while(limit > 0) {
      # if limit < 200, limit == remainder of modulo
      if(limit <= 200) {
        tempLim <- limit %% 200
        # Change limit
        res_link <- paste0(gsub("limit=[0-9].*", "", res_link), "limit=", tempLim)
      }
      # Temp url
      tempURL <- paste0(res_link, "&offset=", offset)
      # Call
      tempCall <- fromJSON(file = tempURL, method='C')
      # If empty, break
      if(length(tempCall) == 0) {
        break
      }
      # If error, break & warning
      if(length(tempCall$errors) != 0) {
        warning("There was an error. You may have exceeded your API requests.")
        break
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
  Construct calls
  '

  # if searchterm
  if(!is.null(searchterm)) {
    # Construct url
    page_url <- paste0("http://api.soundcloud.com/",
                       searchterm, "?client_id=",
                       client_id, "&limit=",
                       limit)
    # get json with query
    jsonDoc <- fromJSON(file = page_url, method='C')
    # If empty
    if(length(jsonDoc) == 0) {
      stop("There are no results for this query. [empty JSON]")
    }
    # If error
    if(length(jsonDoc$errors) != 0) {
      stop("400 - Bad Request error")
    }
    if(limit > 200) {
      if(length(jsonDoc) == 200) {
        jsonDoc <- paginate(jsonDoc, page_url, limit)
      } else {
        warning(paste0("You specified the query limit to be ",
                       limit,
                       ", but there are only ",
                       length(jsonDoc),
                       " results. Returning ",
                       length(jsonDoc), " results."))
      }
    }
  }

  # If link, then resolve
  res_link <- resolve(client_id, soundcloud_link)
  # Additional information?
  if(length(...) >= 1) {
    # Add to list
    addit <- list(...)

    "
    Add information depending on the type of query
    "

    # USERS

    if(grepl("users", res_link)) {
      if(length(addit) > 1) {
        warning("You can only query one type (e.g. playlists, track etc) at a time, and only the first element will be used")
      }
      # Adapt link
      temp <- unlist(strsplit(res_link, "?", fixed=T))
      # Paste
      res_link <- paste0(temp[1], "/", addit[[1]], "?", temp[2], "&limit=", limit)
      # get json with query
      jsonDoc <- fromJSON(file = res_link, method='C')
      # If empty
      if(length(jsonDoc) == 0) {
        stop("There are no results for this query. [empty JSON]")
      }
      # If error
      if(length(jsonDoc$errors) != 0) {
        stop("400 - Bad Request error")
      }
      # If limit is larger than 200, use offset (multiple calls). Only do this when the length of the          first query is 200 (otherwise there aren't any more results)
      if(limit > 200) {
        if(length(jsonDoc) == 200) {
          jsonDoc <- paginate(jsonDoc, res_link, limit)
        } else {
          warning(paste0("You specified the query limit to be ", limit,
                         ", but there are only ", length(jsonDoc),
                         " results. Returning ", length(jsonDoc), " results."))
        }
      }
      # Stop if errors
      if(length(res$errors) != 0) {
        stop("Could not fetch result. Please check your input and try again.")
      }
      # Return
      return(jsonDoc)
    }
  }


  # If no info

}

res <- SCapi(client_id,
             soundcloud_id = NULL,
             soundcloud_link = "https://soundcloud.com/the-bugle", "tracks")
