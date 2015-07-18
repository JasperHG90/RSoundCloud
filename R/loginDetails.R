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
