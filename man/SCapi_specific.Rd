\name{SCapi_specific}
\alias{SCapi_specific}
\title{Query specific soundcloud users, tracks, groups, comments, playlists by url, name or soundcloud ID}
\usage{
SCapi_specific <- function(client_id, soundcloud_search, type = c("url", "id", "name"), query_type = c("users", "tracks", "playlists", "groups", "comments"), limit = 50, get = NULL, filter = NULL, ...)
}
\arguments{
\item{client_id}{The soundcloud client id of your application. See loginDetails() for more information.}

\item{soundcloud_search}{Primary search query. Must be a name of a soundcloud track or user, a url pointing to a search query, or a soundcloud id.}

\item{type}{Type of your soundcloud_search. Must be one of "url", "id", or "name".}

\item{query_type}{What would you like to query? depends on your soundcloud_search input value. E.g. if you are querying a user, use "users". If you are querying tracks, use "tracks" etc. See documentation for more information.}

\item{limit}{how many results should be returned? Soundcloud allows you to query 200 results per query. If you want more results, this is possible by paginating (done automatically).}

\item{get}{Any additional information for 'users'. For example, if you want to query all tracks for a user, choose 'tracks'. For more information on get requests, see section 'subresources' under 'users': http://bit.ly/1OwCaUC. Defaults to NULL.}

\item{filter}{Use if you want to add filters to the query. See: http://bit.ly/1OwCaUC for more information. Filters must be added as a list, e.g. filter = list("q" = "the-bugle").}

\item{...}{any other arguments. Use with caution. Currently not used.}
}
\description{
Return query results for specific users, tracks, playlists, groups or comments.
}
\details{
For additional information about get requests, filters and the soundcloud API, see: http://bit.ly/1OwCaUC
}
\examples{
# Query up to 50 results of the playlists from url provided.
results <- SCapi_specific(client_id, "https://soundcloud.com/the-source-2013/sets", type = "url",
                          query_type = "playlists", limit = 50, get = NULL, filter=NULL)
}
\author{
Jasper Ginn (jasperginn@gmail.com)
}
\seealso{
\code{\link{loginDetails}}
}
