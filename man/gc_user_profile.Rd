% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_objects.R
\name{gc_user_profile}
\alias{gc_user_profile}
\title{UserProfile Object}
\usage{
gc_user_profile(
  emailAddress = NULL,
  photoUrl = NULL,
  permissions = NULL,
  name = NULL,
  id = NULL,
  verifiedTeacher = NULL
)
}
\arguments{
\item{emailAddress}{Email address of the user}

\item{photoUrl}{URL of user's profile photo}

\item{permissions}{Global permissions of the user}

\item{name}{Name of the user}

\item{id}{Identifier of the user}

\item{verifiedTeacher}{Represents whether a G Suite for Education user's domain administrator has}
}
\value{
UserProfile object
}
\description{
UserProfile Object
}
\details{
Global information for a user.
}
\concept{UserProfile functions}
