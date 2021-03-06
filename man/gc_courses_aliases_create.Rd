% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_courses_aliases_create}
\alias{gc_courses_aliases_create}
\title{Create Courses Aliases}
\usage{
gc_courses_aliases_create(CourseAlias, courseId)
}
\arguments{
\item{CourseAlias}{The \link{gc_course_alias} object to pass to this method}

\item{courseId}{Identifier of the course to alias}
}
\description{
Creates an alias for a course.
}
\details{
This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} if the requesting user is not permitted to create thealias or for access errors.
\item \code{NOT_FOUND} if the course does not exist.
\item \code{ALREADY_EXISTS} if the alias already exists.
\item \code{FAILED_PRECONDITION} if the alias requested does not make sense for the  requesting user or course (for example, if a user not in a domain  attempts to access a domain-scoped alias).
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

Other CourseAlias functions: 
\code{\link{gc_course_alias}()}
}
\concept{CourseAlias functions}
