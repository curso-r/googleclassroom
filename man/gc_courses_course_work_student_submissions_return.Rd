% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_functions.R
\name{gc_courses_course_work_student_submissions_return}
\alias{gc_courses_course_work_student_submissions_return}
\title{Submissions Return}
\usage{
gc_courses_course_work_student_submissions_return(
  ReturnStudentSubmissionRequest,
  courseId,
  courseWorkId,
  id
)
}
\arguments{
\item{ReturnStudentSubmissionRequest}{The \link{gc_return_student_submission_request} object to pass to this method}

\item{courseId}{Identifier of the course}

\item{courseWorkId}{Identifier of the course work}

\item{id}{Identifier of the student submission}
}
\description{
Returns a student submission.
}
\details{
Returning a student submission transfers ownership of attached Drivefiles to the student and may
also update the submission state. Unlike the Classroom application, returning a student submission
does notset assignedGrade to the draftGrade value. Only a teacher of the course that contains the
requested student submissionmay call this method. This request must be made by the Developer Console
project of the\href{https://support.google.com/cloud/answer/6158849}{OAuth client ID} used tocreate the
corresponding course work item.

This method returns the following error codes:
\itemize{
\item \code{PERMISSION_DENIED} if the requesting user is not permitted to access therequested course or course work, return the requested student submission,or for access errors.
\item \code{INVALID_ARGUMENT} if the request is malformed.
\item \code{NOT_FOUND} if the requested course, course work, or student submissiondoes not exist.
}

Authentication scopes used by this function are:
\itemize{
\item https://www.googleapis.com/auth/classroom.coursework.students
}

Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.students")}
Then run \code{googleAuthR::gar_auth()} to authenticate.
See \code{\link[googleAuthR]{gar_auth}} for details.
}
\seealso{
\href{https://developers.google.com/classroom/}{Google Documentation}

Other ReturnStudentSubmissionRequest functions: 
\code{\link{gc_return_student_submission_request}()}
}
\concept{ReturnStudentSubmissionRequest functions}
