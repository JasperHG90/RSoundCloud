#' Quick access to your soundcloud app client ID
#'
#' Redirects the user to http://soundcloud.com/you/apps. User must then supply client ID of a soundcloud application to the Rstudio console. Returns a list with a client ID. Usually, you only need to do this once. This function opens a tab in your webbrowser and navigates to the soundcloud developers page. Log into your account and create a test application (if you don't have any yet). Then, copy the Client ID into the R console. You will need this ID for each function as it is used to make requests to the souncloud API
#'
#' @examples \dontrun{
#' Visit soundcloud developers, log in and supply client id to Rstudio console
#' client_id <- loginDetails()
#' }
#' @author Jasper Ginn
#' @export

loginDetails <- function() {
  # Open soundcloud developers page
  browseURL("http://soundcloud.com/you/apps")
  # Prompt
  cat("Either use an existing application or register a new application. Then, copy the 'client ID' in the R Console below")
  # Get input
  readClientID <- function() {
    n <- readline(prompt = "Enter the client ID of your soundcloud application: ")
    return(n)
  }
  # Run
  cID <- readClientID()
  # Controls
  while(nchar(cID) != 32) {
    print("The ID you entered is not valid.")
    cID <- readClientID()
  }
  # Return
  return(list(clientID = cID))
}
