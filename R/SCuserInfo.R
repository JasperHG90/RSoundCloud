# SCuserInfo(client_id, user_id=NULL, user_name=NULL, soundcloud_link=NULL, get.id=FALSE)
#
# return either the soundcloud ID of a user, or all their details. Accepts SC user id, user name or
# a URL to the user's soundcloud page to extract this information.
#
# Parameters:
#   - client_id: The soundcloud client id of your application. See loginDetails() for more information
#   - user_id: User id of the user for which you want to get data. Defaults to NULL
#   - user_name: User name of the user for which you want to get data. Defaults to NULL
#   - soundcloud_link: URL to the user's soundcloud page for which you want to get the data. Defaults
#                      to NULL
#   - get.id: Get only a user's unique soundcloud ID (value is TRUE) OR get all information for a user
#             (value is FALSE). Defaults to FALSE
#
# Returns: A 1-dimensional vector with a user's soundcloud user id OR a list with a user's
#          information.
#
# Sample use:
#     Uinfo <- SCuserInfo(client_id, user_id = 98765)
#     Uid <- SCuserInfo(client_id, user_name = "the-bugle", get.id = TRUE)
#     Uinfo <- SCuserInfo(client_id, soundcloud_link = "https://soundcloud.com/the-bugle/sets")

SCuserInfo <- function(client_id,
                       user_id = NULL,
                       user_name = NULL,
                       soundcloud_link = NULL,
                       get.id = FALSE) {
  # Control
  if(all(is.null(user_id), is.null(user_name), is.null(soundcloud_link))) {
    stop("You need to specify the user name, a URL to the user's soundcloud page, or a user id.")
  }
  # If user id is not given, then work around.
  if(is.null(user_id)) {
    # If link, get info
    if(!is.null(soundcloud_link)) {
      user_name <- gsub("https://soundcloud.com/", "", soundcloud_link, fixed=TRUE)
      if(grepl("/", user_name)) {
        user_name <- gsub("/.*", "", user_name)
      }
    }
    # Construct url for redirect to uid
    pt1 <- paste0("http://api.soundcloud.com/resolve.json?url=http://soundcloud.com/", user_name)
    pt2 <- paste0("&client_id=", client_id)
    url <- paste0(pt1,pt2)
    # Need to handle redirect to user page with RCurl
    curl = getCurlHandle()
    res <- fromJSON(getURL(url, curl = curl))
    rm(curl) # release curl
    # Control if not found
    if(length(res$errors) != 0) {
      stop("That user does not exist. Please check your input and try again.")
    }
    # Get redirect url
    red_url <- res$location
    # If we only want user id, then return
    if(get.id == TRUE) {
      uid <- gsub("https://api.soundcloud.com/users/", "", red_url, fixed=T)
      uid <- gsub("\\..*", "", uid)
      return(uid)
    }
  } else{
    # If user id is given, then use that
    red_url <- paste0("https://api.soundcloud.com/users/",
                      user_id, ".json?client_id=", client_id)
  }
  # get json with info
  jsonDoc <- fromJSON(file = red_url, method='C')
  # Stop if errors
  if(length(res$errors) != 0) {
    stop("That user does not exist. Please check your input and try again.")
  }
  # Return
  return(jsonDoc)
}
