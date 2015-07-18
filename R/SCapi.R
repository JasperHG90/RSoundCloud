# SCapi()
#
# Query the soundcloud API
#
# Parameters:
#
# Returns:
#
# Sample use:


SCapi <- function(client_id, soundcloud_id = NULL, soundcloud_link = NULL, ... ) {
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
      res_link <- paste0(temp[1], "/", addit[[1]], "?", temp[2])
      # get json with query
      jsonDoc <- fromJSON(file = res_link, method='C')
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
