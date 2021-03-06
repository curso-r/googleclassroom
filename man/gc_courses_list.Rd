% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_courses_list}
\alias{gc_courses_list}
\title{Courses List}
\usage{
gc_courses_list(
  studentId = NULL,
  pageToken = NULL,
  pageSize = NULL,
  teacherId = NULL,
  courseStates = NULL
)
}
\arguments{
\item{studentId}{Restricts returned courses to those having a student with the specified}

\item{pageToken}{nextPageToken}

\item{pageSize}{Maximum number of items to return}

\item{teacherId}{Restricts returned courses to those having a teacher with the specified}

\item{courseStates}{Restricts returned courses to those in one of the specified states}
}
\description{
Returns a list of courses that the requesting user is permitted to view,restricted to those that
match the request. Returned courses are ordered bycreation time, with the most recently created
coming first.
}
\details{
This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} for access errors.
\item \code{INVALID_ARGUMENT} if the query argument is malformed.
\item \code{NOT_FOUND} if any users specified in the query arguments do not exist.
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.courses
\item https://www.googleapis.com/auth/classroom.courses.readonly
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses", "https://www.googleapis.com/auth/classroom.courses.readonly")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}
}
