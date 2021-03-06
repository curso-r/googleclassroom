% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_courses_course_work_get}
\alias{gc_courses_course_work_get}
\title{Get Course Work}
\usage{
gc_courses_course_work_get(courseId, id)
}
\arguments{
\item{courseId}{Identifier of the course}

\item{id}{Identifier of the course work}
}
\description{
Returns course work.
}
\details{
This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} if the requesting user is not permitted to access therequested course or course work, or for access errors.
\item \code{INVALID_ARGUMENT} if the request is malformed.
\item \code{NOT_FOUND} if the requested course or course work does not exist.
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.coursework.me
\item https://www.googleapis.com/auth/classroom.coursework.me.readonly
\item https://www.googleapis.com/auth/classroom.coursework.students
\item https://www.googleapis.com/auth/classroom.coursework.students.readonly
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me", "https://www.googleapis.com/auth/classroom.coursework.me.readonly", "https://www.googleapis.com/auth/classroom.coursework.students", "https://www.googleapis.com/auth/classroom.coursework.students.readonly")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}
}
