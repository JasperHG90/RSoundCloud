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
                  soundcloud_id = NULL,
                  soundcloud_link = NULL,
                  limit = 50,
                  ... ) {
  # Control
  if(all(is.null(soundcloud_id), is.null(soundcloud_link))) {
    stop("You need to specify either a soundcloud id or a soundcloud link to that item.")
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
      # If limit is larger than 200, use offset (multiple calls)
      if(limit > 200) {
        # Decrease limit
        limit <- limit - 200
        # Offset value (i.e. where to start)
        offset <- 200
        while(limit > 0) {
          # Construct calls
          if(limit %% 200 != 0) {
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
          # Add to master
          jsonDoc <- c(jsonDoc, tempCall)
          # Increase offset
          offset <- offset + 200
          # Decrese limit
          limit <- limit - 200
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
