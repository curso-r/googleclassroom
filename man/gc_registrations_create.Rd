% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_registrations_create}
\alias{gc_registrations_create}
\title{create Registrations}
\usage{
gc_registrations_create(Registration)
}
\arguments{
\item{Registration}{The \link{gc_registration} object to pass to this method}
}
\description{
Creates a \code{Registration}, causing Classroom to start sending notificationsfrom the provided \code{feed} to the
destination provided in \code{cloudPubSubTopic}.Returns the created \code{Registration}. Currently, this will be the
same asthe argument, but with server-assigned fields such as \code{expiry_time} and\code{id} filled in. Note that any
value specified for the \code{expiry_time} or \code{id} fields will beignored. While Classroom may validate the
\code{cloudPubSubTopic} and return errors on abest effort basis, it is the caller's responsibility to ensure that
itexists and that Classroom has permission to publish to it.
}
\details{
This method may return the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} if:
\itemize{
\item the authenticated user does not have permission to receive notifications from the requested field; or
\item the credential provided does not include the appropriate scope for the requested feed.
\item another access error is encountered.
}
}
\itemize{
\item \code{INVALID_ARGUMENT} if:
\itemize{
\item no \code{cloudPubsubTopic} is specified, or the specified \code{cloudPubsubTopic} is not valid; or
\item no \code{feed} is specified, or the specified \code{feed} is not valid.* \code{NOT_FOUND} if:
\item the specified \code{feed} cannot be located, or the requesting user does not have permission to determine whether or not it exists; or
\item the specified \code{cloudPubsubTopic} cannot be located, or Classroom has not been granted permission to publish to it.
}
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.push-notifications
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.push-notifications")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}

Other Registration functions: 
\code{\link{gc_registration}()}
}
\concept{Registration functions}
