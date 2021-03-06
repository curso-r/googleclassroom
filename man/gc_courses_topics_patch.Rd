% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_courses_topics_patch}
\alias{gc_courses_topics_patch}
\title{Topics Patch}
\usage{
gc_courses_topics_patch(Topic, courseId, id, updateMask = NULL)
}
\arguments{
\item{Topic}{The \link{gc_Topic} object to pass to this method}

\item{courseId}{Identifier of the course}

\item{id}{Identifier of the topic}

\item{updateMask}{Mask that identifies which fields on the topic to update}
}
\description{
Updates one or more fields of a topic
}
\details{
This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} if the requesting developer project did not createthe corresponding topic or for access errors.
\item \code{INVALID_ARGUMENT} if the request is malformed.
\item \code{NOT_FOUND} if the requested course or topic does not exist
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.topics
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.topics")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}

Other Topic functions: 
\code{\link{gc_courses_topics_create}()},
\code{\link{gc_topic}()}
}
\concept{Topic functions}
