SCuserInfo <- function(client_id, user_name = NULL, soundcloud_link = NULL) {
  # Control
  if(all(is.null(user_name), is.null(soundcloud_link))) {
    stop("You need to specify either the user name or you need to add the user's soundcloud page.")
  }
  # If link, get info
  if(!is.null(soundcloud_link)) {
    user_name <- gsub("https://soundcloud.com/", "", soundcloud_link, fixed=TRUE)
    if(grepl("/", sc_link)) {
      user_name <- gsub("/.*", "", sc_link)
    }
  }
  # Get user info
  pt1 <- paste0("http://api.soundcloud.com/resolve.json?url=http://soundcloud.com/", user_name)
  pt2 <- paste0("&client_id=", client_id)
  url <- paste0(pt1,pt2)
  # Need to handle redirect to user page with RCurl

  # get json
  jsonDoc <- fromJSON(file = url, method='C')

}

curl = getCurlHandle()
r <- getURL("http://api.soundcloud.com/resolve.json?url=http://soundcloud.com/the-bugle&client_id=45afab70863909e48cfb2f1314d9380a", curl=curl)
