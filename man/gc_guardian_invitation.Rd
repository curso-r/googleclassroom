% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_objects.R
\name{gc_guardian_invitation}
\alias{gc_guardian_invitation}
\title{GuardianInvitation Object}
\usage{
gc_guardian_invitation(
  studentId = NULL,
  state = NULL,
  invitedEmailAddress = NULL,
  creationTime = NULL,
  invitationId = NULL
)
}
\arguments{
\item{studentId}{ID of the student (in standard format)}

\item{state}{The state that this invitation is in}

\item{invitedEmailAddress}{Email address that the invitation was sent to}

\item{creationTime}{The time that this invitation was created}

\item{invitationId}{Unique identifier for this invitation}
}
\value{
GuardianInvitation object
}
\description{
GuardianInvitation Object
}
\details{
An invitation to become the guardian of a specified user, sent to a specifiedemail address.
}
\seealso{
Other GuardianInvitation functions: 
\code{\link{gc_user_profiles_guardian_invitations_create}()},
\code{\link{gc_user_profiles_guardian_invitations_patch}()}
}
\concept{GuardianInvitation functions}
