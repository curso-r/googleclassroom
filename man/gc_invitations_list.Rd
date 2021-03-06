% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_invitations_list}
\alias{gc_invitations_list}
\title{Invitations List}
\usage{
gc_invitations_list(
  courseId = NULL,
  userId = NULL,
  pageToken = NULL,
  pageSize = NULL
)
}
\arguments{
\item{courseId}{Restricts returned invitations to those for a course with the specified}

\item{userId}{Restricts returned invitations to those for a specific user}

\item{pageToken}{nextPageToken}

\item{pageSize}{Maximum number of items to return}
}
\description{
Returns a list of invitations that the requesting user is permitted toview, restricted to those that
match the list request.
\emph{Note:} At least one of \code{user_id} or \code{course_id} must be supplied. Bothfields can be supplied.
}
\details{
This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} for access errors.
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.rosters
\item https://www.googleapis.com/auth/classroom.rosters.readonly
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}
}
