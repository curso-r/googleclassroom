% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom_objects.R
\name{gc_state_history}
\alias{gc_state_history}
\title{StateHistory Object}
\usage{
gc_state_history(state = NULL, stateTimestamp = NULL, actorUserId = NULL)
}
\arguments{
\item{state}{The workflow pipeline stage}

\item{stateTimestamp}{When the submission entered this state}

\item{actorUserId}{The teacher or student who made the change}
}
\value{
StateHistory object
}
\description{
StateHistory Object
}
\details{
The history of each state this submission has been in.
}
\concept{StateHistory functions}
