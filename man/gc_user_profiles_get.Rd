% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_user_profiles_get}
\alias{gc_user_profiles_get}
\title{Get Profile}
\usage{
gc_user_profiles_get(userId)
}
\arguments{
\item{userId}{Identifier of the profile to return}
}
\description{
Returns a user profile
}
\details{
This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} if the requesting user is not permitted to accessthis user profile, if no profile exists with the requested ID, or foraccess errors.
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.profile.emails
\item https://www.googleapis.com/auth/classroom.profile.photos
\item https://www.googleapis.com/auth/classroom.rosters
\item https://www.googleapis.com/auth/classroom.rosters.readonly
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.profile.emails", "https://www.googleapis.com/auth/classroom.profile.photos", "https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}
}
