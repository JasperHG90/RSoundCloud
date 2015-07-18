# loginDetails()
#
# This function opens a tab in your webbrowser and navigates to the soundcloud developers page.
# Log into your account and create a test application (if you don't have any yet). Then, copy
# the Client ID into the R console. You will need this ID for each function as it is used to make
# Requests to the souncloud API
#
# Parameters: None
#
# Returns: list with your client id
#
# Sample use:
#     CiD <- loginDetails()

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
