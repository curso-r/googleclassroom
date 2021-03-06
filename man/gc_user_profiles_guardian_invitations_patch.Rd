% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_user_profiles_guardian_invitations_patch}
\alias{gc_user_profiles_guardian_invitations_patch}
\title{Modifies a guardian invitation}
\usage{
gc_user_profiles_guardian_invitations_patch(
  GuardianInvitation,
  studentId,
  invitationId,
  updateMask = NULL
)
}
\arguments{
\item{GuardianInvitation}{The \link{gc_guardian_invitation} object to pass to this method}

\item{studentId}{The ID of the student whose guardian invitation is to be modified}

\item{invitationId}{The \code{id} field of the \code{GuardianInvitation} to be modified}

\item{updateMask}{Mask that identifies which fields on the course to update}
}
\description{
Currently, the only valid modification is to change the \code{state} from\code{PENDING} to \code{COMPLETE}. This has
the effect of withdrawing the invitation.
}
\details{
This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} if the current user does not have permission to  manage guardians, if guardians are not enabled for the domain in question  or for other access errors.
\item \code{FAILED_PRECONDITION} if the guardian link is not in the \code{PENDING} state.
\item \code{INVALID_ARGUMENT} if the format of the student ID provided  cannot be recognized (it is not an email address, nor a \code{user_id} from  this API), or if the passed \code{GuardianInvitation} has a \code{state} other than  \code{COMPLETE}, or if it modifies fields other than \code{state}.
\item \code{NOT_FOUND} if the student ID provided is a valid student ID, but  Classroom has no record of that student, or if the \code{id} field does not  refer to a guardian invitation known to Classroom.
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.guardianlinks.students
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.guardianlinks.students")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}

Other GuardianInvitation functions: 
\code{\link{gc_guardian_invitation}()},
\code{\link{gc_user_profiles_guardian_invitations_create}()}
}
\concept{GuardianInvitation functions}
