\name{SCapi_general}
\alias{SCapi_general}
\title{Query from all soundcloud users, tracks, groups or comments}
\usage{
SCapi_general <- function(client_id, type = c("users", "tracks", "groups", "comments"), limit = 50, filter = NULL, offset = NULL)
}
\arguments{
\item{client_id}{The soundcloud client id of your application. See loginDetails() for more information.}

\item{type}{What would you like to query? Must be one of "users", "tracks", "groups", "comments".}

\item{limit}{how many results should be returned? Soundcloud allows you to query 200 results per query. If you want more results, this is possible by paginating (done automatically).}

\item{filter}{Use if you want to add filters to the query. See: http://bit.ly/1OwCaUC for more information. Filters must be added as a list, e.g. filter = list("q" = "the-bugle").}

\item{offset}{determines at which result we start searching. For example, if we do a generic query for "users", which returns all users for the limit we set (e.g. 1000), and we use offset = 1000, then we begin our search at the 1001st user, thus returning the information of users between 1000 and 2000.}
}
\description{
Return results from users, tracks, groups, comments.
}
\details{
For additional information about filters and the soundcloud API, see: http://bit.ly/1OwCaUC
}
\examples{
# Query up to 50 results of users that conform to the query "em-mcrae" or "the-bugle"
sc_res <- SCapi_general(client_id, type="users", limit=50, filter=list("q" = "em-mcrae", "q" = "the-bugle"))
}
\author{
Jasper Ginn (jasperginn@gmail.com)
}
\seealso{
\code{\link{loginDetails}}
}
