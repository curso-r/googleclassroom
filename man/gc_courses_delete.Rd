% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_courses_delete}
\alias{gc_courses_delete}
\title{Delete Course}
\usage{
gc_courses_delete(id)
}
\arguments{
\item{id}{Identifier of the course to delete}
}
\description{
Deletes a course.
}
\details{
This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} if the requesting user is not permitted to delete therequested course or for access errors.
\item \code{NOT_FOUND} if no course exists with the requested ID.
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.courses
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}
}
