#' Google Classroom API
#' Manages classes, rosters, and invitations in Google Classroom.
#'
#' @details
#' Authentication scopes used are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.push-notifications
#' \item https://www.googleapis.com/auth/classroom.rosters
#' \item https://www.googleapis.com/auth/classroom.topics
#' \item https://www.googleapis.com/auth/classroom.topics.readonly
#' \item https://www.googleapis.com/auth/classroom.student-submissions.students.readonly
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly
#' \item https://www.googleapis.com/auth/classroom.courses
#' \item https://www.googleapis.com/auth/classroom.courses.readonly
#' \item https://www.googleapis.com/auth/classroom.announcements
#' \item https://www.googleapis.com/auth/classroom.profile.photos
#' \item https://www.googleapis.com/auth/classroom.rosters.readonly
#' \item https://www.googleapis.com/auth/classroom.announcements.readonly
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.students
#' \item https://www.googleapis.com/auth/classroom.student-submissions.me.readonly
#' \item https://www.googleapis.com/auth/classroom.coursework.students
#' \item https://www.googleapis.com/auth/classroom.coursework.students.readonly
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.me.readonly
#' \item https://www.googleapis.com/auth/classroom.coursework.me.readonly
#' \item https://www.googleapis.com/auth/classroom.profile.emails
#' \item https://www.googleapis.com/auth/classroom.coursework.me
#' }
#'
#' @docType package
#' @name classroom
#'
NULL
## NULL

#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x))
        rmNullObs(x) else x)
}

#' Deletes an invitation
#'
#' This method returns the following error codes:* `PERMISSION_DENIED` if the requesting user is not permitted to delete therequested invitation or for access errors.* `NOT_FOUND` if no invitation exists with the requested ID.
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.rosters
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.rosters")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param id Identifier of the invitation to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_invitations_delete <- function(id) {
    url <- sprintf("https://classroom.googleapis.com/v1/invitations/%s", id)
    # classroom.invitations.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' Returns an invitation
#'
#' This method returns the following error codes:
#'
#' - `PERMISSION_DENIED` if the requesting user is not permitted to view therequested invitation or for access errors.
#' - `NOT_FOUND` if no invitation exists with the requested ID.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.rosters
#' \item https://www.googleapis.com/auth/classroom.rosters.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param id Identifier of the invitation to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_invitations_get <- function(id) {
    url <- sprintf("https://classroom.googleapis.com/v1/invitations/%s", id)
    # classroom.invitations.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Invitations List
#'
#'
#' Returns a list of invitations that the requesting user is permitted toview, restricted to those that
#' match the list request.
#' *Note:* At least one of `user_id` or `course_id` must be supplied. Bothfields can be supplied.
#'
#' This method returns the following error codes:
#'
#' - `PERMISSION_DENIED` for access errors.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.rosters
#' \item https://www.googleapis.com/auth/classroom.rosters.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Restricts returned invitations to those for a course with the specified
#' @param userId Restricts returned invitations to those for a specific user
#' @param pageToken nextPageToken
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_invitations_list <- function(courseId = NULL, userId = NULL, pageToken = NULL, pageSize = NULL) {
    url <- "https://classroom.googleapis.com/v1/invitations"
    # classroom.invitations.list
    pars = list(courseId = courseId, userId = userId, pageToken = pageToken, pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Creates an invitation
#'
#' Only one invitation for a user and course may existat a time. Delete and re-create an invitation to
#' make changes.
#'
#' This method returns the following error codes:
#' - `PERMISSION_DENIED` if the requesting user is not permitted to createinvitations for this course or for access errors.
#' - `NOT_FOUND` if the course or the user does not exist.
#' - `FAILED_PRECONDITION` if the requested user's account is disabled or ifthe user already has this role or a role with greater permissions.
#' - `ALREADY_EXISTS` if an invitation for the specified user and coursealready exists.
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.rosters
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.rosters")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Invitation The \link{gc_invitation} object to pass to this method
#' @importFrom googleAuthR gar_api_generator
#' @family Invitation functions
#' @export
gc_invitations_create <- function(Invitation) {
    url <- "https://classroom.googleapis.com/v1/invitations"
    # classroom.invitations.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(Invitation, "gar_Invitation"))

    f(the_body = Invitation)

}

#' Accepts an invitation
#'
#' Accepts an invitation, removing it and adding the invited user to theteachers or students (as appropriate) of
#' the specified course. Only theinvited user may accept an invitation. This method returns the following error
#'  codes:
#'
#'  - `PERMISSION_DENIED` if the requesting user is not permitted to accept therequested invitation or for access errors.
#'  - `FAILED_PRECONDITION` for the following request errors:
#'      - CourseMemberLimitReached
#'      - CourseNotModifiable
#'      - CourseTeacherLimitReached
#'      - UserGroupsMembershipLimitReached
#'  - `NOT_FOUND` if no invitation exists with the requested ID.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.rosters
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.rosters")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param id Identifier of the invitation to accept
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_invitations_accept <- function(id) {
    url <- sprintf("https://classroom.googleapis.com/v1/invitations/%s:accept", id)
    # classroom.invitations.accept
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    f()

}

#' Delete Course
#'
#' Deletes a course.
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting user is not permitted to delete therequested course or for access errors.
#' * `NOT_FOUND` if no course exists with the requested ID.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param id Identifier of the course to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_delete <- function(id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s", id)
    # classroom.courses.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' Courses List
#'
#' Returns a list of courses that the requesting user is permitted to view,restricted to those that
#' match the request. Returned courses are ordered bycreation time, with the most recently created
#' coming first.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` for access errors.
#' * `INVALID_ARGUMENT` if the query argument is malformed.
#' * `NOT_FOUND` if any users specified in the query arguments do not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' \item https://www.googleapis.com/auth/classroom.courses.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses", "https://www.googleapis.com/auth/classroom.courses.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param studentId Restricts returned courses to those having a student with the specified
#' @param pageToken nextPageToken
#' @param pageSize Maximum number of items to return
#' @param teacherId Restricts returned courses to those having a teacher with the specified
#' @param courseStates Restricts returned courses to those in one of the specified states
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_list <- function(studentId = NULL, pageToken = NULL, pageSize = NULL, teacherId = NULL,
    courseStates = NULL) {
    url <- "https://classroom.googleapis.com/v1/courses"
    # classroom.courses.list
    pars = list(studentId = studentId, pageToken = pageToken, pageSize = pageSize,
        teacherId = teacherId, courseStates = courseStates)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()
}

#' Create Course
#'
#' Creates a course
#'
#' The user specified in `ownerId` is the owner of the created courseand added as a teacher.
#'
#' This method returns the following error codes:
#'
#' - `PERMISSION_DENIED` if the requesting user is not permitted to createcourses or for access errors.
#' - `NOT_FOUND` if the primary teacher is not a valid user.
#' - `FAILED_PRECONDITION` if the course owner's account is disabled or forthe following request errors:
#'     - UserGroupsMembershipLimitReached
#' - `ALREADY_EXISTS` if an alias was specified in the `id` andalready exists.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Course The \link{gc_course} object to pass to this method
#' @importFrom googleAuthR gar_api_generator
#' @family Course functions
#' @export
gc_courses_create <- function(Course) {
    url <- "https://classroom.googleapis.com/v1/courses"
    # classroom.courses.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(Course, "gar_Course"))

    f(the_body = Course)

}

#' Get Courses
#'
#' Returns a course.
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or for access errors.
#' * `NOT_FOUND` if no course exists with the requested ID.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' \item https://www.googleapis.com/auth/classroom.courses.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses", "https://www.googleapis.com/auth/classroom.courses.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param id Identifier of the course to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_get <- function(id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s", id)
    # classroom.courses.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Courses Patch
#'
#' Updates one or more fields in a course.This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to modify therequested course or for access errors.
#' * `NOT_FOUND` if no course exists with the requested ID.
#' * `INVALID_ARGUMENT` if invalid fields are specified in the update mask orif no update mask is supplied.
#' * `FAILED_PRECONDITION` for the following request errors:
#'     - CourseNotModifiable
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Course The \link{gc_course} object to pass to this method
#' @param id Identifier of the course to update
#' @param updateMask Mask that identifies which fields on the course to update
#' @importFrom googleAuthR gar_api_generator
#' @family Course functions
#' @export
gc_courses_patch <- function(Course, id, updateMask = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s", id)
    # classroom.courses.patch
    pars = list(updateMask = updateMask)
    f <- googleAuthR::gar_api_generator(url, "PATCH", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    stopifnot(inherits(Course, "gar_Course"))

    f(the_body = Course)

}

#' Course Update
#'
#' Updates a course.
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting user is not permitted to modify therequested course or for access errors.
#' * `NOT_FOUND` if no course exists with the requested ID.
#' * `FAILED_PRECONDITION` for the following request errors:
#'     - CourseNotModifiable
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Course The \link{gc_course} object to pass to this method
#' @param id Identifier of the course to update
#' @importFrom googleAuthR gar_api_generator
#' @family Course functions
#' @export
gc_courses_update <- function(Course, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s", id)
    # classroom.courses.update
    f <- googleAuthR::gar_api_generator(url, "PUT", data_parse_function = function(x) x)
    stopifnot(inherits(Course, "gar_Course"))

    f(the_body = Course)

}

#' Create Topic
#'
#' Creates a topic
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course, create a topic in the requested course,or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.topics
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.topics")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Topic The \link{gc_topic} object to pass to this method
#' @param courseId Identifier of the course
#' @importFrom googleAuthR gar_api_generator
#' @family Topic functions
#' @export
gc_courses_topics_create <- function(Topic, courseId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/topics", courseId)
    # classroom.courses.topics.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(Topic, "gar_Topic"))

    f(the_body = Topic)

}

#' Delete Topic
#'
#' Deletes a topic
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting user is not allowed to delete therequested topic or for access errors.
#' * `FAILED_PRECONDITION` if the requested topic has already beendeleted.
#' * `NOT_FOUND` if no course or topic exists with the requested ID.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.topics
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.topics")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param id Identifier of the topic to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_topics_delete <- function(courseId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/topics/%s", courseId,
        id)
    # classroom.courses.topics.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' Topics Patch
#'
#' Updates one or more fields of a topic
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting developer project did not createthe corresponding topic or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course or topic does not exist
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.topics
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.topics")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Topic The \link{gc_Topic} object to pass to this method
#' @param courseId Identifier of the course
#' @param id Identifier of the topic
#' @param updateMask Mask that identifies which fields on the topic to update
#' @importFrom googleAuthR gar_api_generator
#' @family Topic functions
#' @export
gc_courses_topics_patch <- function(Topic, courseId, id, updateMask = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/topics/%s", courseId,
        id)
    # classroom.courses.topics.patch
    pars = list(updateMask = updateMask)
    f <- googleAuthR::gar_api_generator(url, "PATCH", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    stopifnot(inherits(Topic, "gar_Topic"))

    f(the_body = Topic)

}

#' Get Topics
#'
#' Returns a topic.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or topic, or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course or topic does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.topics
#' \item https://www.googleapis.com/auth/classroom.topics.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.topics", "https://www.googleapis.com/auth/classroom.topics.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param id Identifier of the topic
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_topics_get <- function(courseId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/topics/%s", courseId,
        id)
    # classroom.courses.topics.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Courses Topics List
#'
#' Returns the list of topics that the requester is permitted to view.
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting user is not permitted to accessthe requested course or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.topics
#' \item https://www.googleapis.com/auth/classroom.topics.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.topics", "https://www.googleapis.com/auth/classroom.topics.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param pageToken nextPageToken
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_topics_list <- function(courseId, pageToken = NULL, pageSize = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/topics", courseId)
    # classroom.courses.topics.list
    pars = list(pageToken = pageToken, pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Delete Aliases
#'
#' Deletes an alias of a course
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to remove thealias or for access errors.
#' * `NOT_FOUND` if the alias does not exist.
#' * `FAILED_PRECONDITION` if the alias requested does not make sense for the  requesting user or course (for example, if a user not in a domain  attempts to delete a domain-scoped alias).
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course whose alias should be deleted
#' @param alias Alias to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_aliases_delete <- function(courseId, alias) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/aliases/%s", courseId,
        alias)
    # classroom.courses.aliases.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' Aliases List
#'
#' Returns a list of aliases for a course. This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access thecourse or for access errors.
#' * `NOT_FOUND` if the course does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' \item https://www.googleapis.com/auth/classroom.courses.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses", "https://www.googleapis.com/auth/classroom.courses.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId The identifier of the course
#' @param pageToken nextPageToken
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_aliases_list <- function(courseId, pageToken = NULL, pageSize = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/aliases", courseId)
    # classroom.courses.aliases.list
    pars = list(pageToken = pageToken, pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Create Courses Aliases
#'
#' Creates an alias for a course.
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting user is not permitted to create thealias or for access errors.
#' * `NOT_FOUND` if the course does not exist.
#' * `ALREADY_EXISTS` if the alias already exists.
#' * `FAILED_PRECONDITION` if the alias requested does not make sense for the  requesting user or course (for example, if a user not in a domain  attempts to access a domain-scoped alias).
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.courses
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.courses")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param CourseAlias The \link{gc_course_alias} object to pass to this method
#' @param courseId Identifier of the course to alias
#' @importFrom googleAuthR gar_api_generator
#' @family CourseAlias functions
#' @export
gc_courses_aliases_create <- function(CourseAlias, courseId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/aliases", courseId)
    # classroom.courses.aliases.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(CourseAlias, "gar_CourseAlias"))

    f(the_body = CourseAlias)

}

#' Delete Course Work
#'
#' Deletes a course work.
#'
#' This request must be made by the Developer Console project of the[OAuth client ID](https://support.google.com/cloud/answer/6158849) used tocreate the corresponding course work item. This method returns the following error codes:* `PERMISSION_DENIED` if the requesting developer project did not createthe corresponding course work, if the requesting user is not permitted to delete the requested course or for access errors.
#' * `FAILED_PRECONDITION` if the requested course work has already beendeleted.
#' * `NOT_FOUND` if no course exists with the requested ID.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param id Identifier of the course work to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_course_work_delete <- function(courseId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s",
        courseId, id)
    # classroom.courses.courseWork.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' Course Work List
#'
#' Returns a list of course work that the requester is permitted to view. Course students may only view `PUBLISHED`
#' course work. Course teachersand domain administrators may view all course work.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to accessthe requested course or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.me
#' \item https://www.googleapis.com/auth/classroom.coursework.me.readonly
#' \item https://www.googleapis.com/auth/classroom.coursework.students
#' \item https://www.googleapis.com/auth/classroom.coursework.students.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me", "https://www.googleapis.com/auth/classroom.coursework.me.readonly", "https://www.googleapis.com/auth/classroom.coursework.students", "https://www.googleapis.com/auth/classroom.coursework.students.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param orderBy Optional sort ordering for results
#' @param pageToken nextPageToken
#' @param pageSize Maximum number of items to return
#' @param courseWorkStates Restriction on the work status to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_course_work_list <- function(courseId, orderBy = NULL, pageToken = NULL, pageSize = NULL,
    courseWorkStates = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork", courseId)
    # classroom.courses.courseWork.list
    pars = list(orderBy = orderBy, pageToken = pageToken, pageSize = pageSize, courseWorkStates = courseWorkStates)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Create Course Work
#'
#' Creates course work. The resulting course work (and corresponding student submissions) areassociated with
#' the Developer Console project of the[OAuth client ID](https://support.google.com/cloud/answer/6158849) used
#' to make the request. Classroom API requests to modify course work and studentsubmissions must be made with an
#' OAuth client ID from the associatedDeveloper Console project.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course, create course work in the requested course, share aDrive attachment, or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course does not exist.
#' * `FAILED_PRECONDITION` for the following request error:
#'     - AttachmentNotVisible
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param CourseWork The \link{gc_course_work} object to pass to this method
#' @param courseId Identifier of the course
#' @importFrom googleAuthR gar_api_generator
#' @family CourseWork functions
#' @export
gc_courses_course_work_create <- function(CourseWork, courseId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork", courseId)
    # classroom.courses.courseWork.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(CourseWork, "gar_CourseWork"))

    f(the_body = CourseWork)

}

#' Modify Assignee
#'
#' Modifies assignee mode and options of a coursework. Only a teacher of the course that contains the coursework maycall this method.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course or course work does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param ModifyCourseWorkAssigneesRequest The \link{gc_modify_course_work_assignees_request} object to pass to this method
#' @param courseId Identifier of the course
#' @param id Identifier of the coursework
#' @importFrom googleAuthR gar_api_generator
#' @family ModifyCourseWorkAssigneesRequest functions
#' @export
gc_courses_course_work_modify_assignees <- function(ModifyCourseWorkAssigneesRequest,
    courseId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s:modifyAssignees",
        courseId, id)
    # classroom.courses.courseWork.modifyAssignees
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(ModifyCourseWorkAssigneesRequest, "gar_ModifyCourseWorkAssigneesRequest"))

    f(the_body = ModifyCourseWorkAssigneesRequest)

}

#' Course Work Patch
#'
#' Updates one or more fields of a course work. See google.classroom.v1.CourseWork for detailsof which fields may be updated and who may change them.
#' This request must be made by the Developer Console project of the [OAuth client ID](https://support.google.com/cloud/answer/6158849) used to
#' create the corresponding course work item.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course or course work does not exist.
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param CourseWork The \link{gc_course_work} object to pass to this method
#' @param courseId Identifier of the course
#' @param id Identifier of the course work
#' @param updateMask Mask that identifies which fields on the course work to update
#' @importFrom googleAuthR gar_api_generator
#' @family CourseWork functions
#' @export
gc_courses_course_work_patch <- function(CourseWork, courseId, id, updateMask = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s",
        courseId, id)
    # classroom.courses.courseWork.patch
    pars = list(updateMask = updateMask)
    f <- googleAuthR::gar_api_generator(url, "PATCH", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    stopifnot(inherits(CourseWork, "gar_CourseWork"))

    f(the_body = CourseWork)

}

#' Get Course Work
#'
#' Returns course work.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work, or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course or course work does not exist.
#'
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.me
#' \item https://www.googleapis.com/auth/classroom.coursework.me.readonly
#' \item https://www.googleapis.com/auth/classroom.coursework.students
#' \item https://www.googleapis.com/auth/classroom.coursework.students.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me", "https://www.googleapis.com/auth/classroom.coursework.me.readonly", "https://www.googleapis.com/auth/classroom.coursework.students", "https://www.googleapis.com/auth/classroom.coursework.students.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param id Identifier of the course work
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_course_work_get <- function(courseId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s",
        courseId, id)
    # classroom.courses.courseWork.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Get Submissions
#'
#' Returns a student submission.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course, course work, or student submission or foraccess errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course, course work, or student submissiondoes not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#' \item https://www.googleapis.com/auth/classroom.coursework.me
#' \item https://www.googleapis.com/auth/classroom.coursework.me.readonly
#' \item https://www.googleapis.com/auth/classroom.coursework.students
#' \item https://www.googleapis.com/auth/classroom.coursework.students.readonly
#' \item https://www.googleapis.com/auth/classroom.student-submissions.me.readonly
#' \item https://www.googleapis.com/auth/classroom.student-submissions.students.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me", "https://www.googleapis.com/auth/classroom.coursework.me.readonly", "https://www.googleapis.com/auth/classroom.coursework.students", "https://www.googleapis.com/auth/classroom.coursework.students.readonly", "https://www.googleapis.com/auth/classroom.student-submissions.me.readonly", "https://www.googleapis.com/auth/classroom.student-submissions.students.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param courseWorkId Identifier of the course work
#' @param id Identifier of the student submission
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_course_work_student_submissions_get <- function(courseId, courseWorkId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s/studentSubmissions/%s",
        courseId, courseWorkId, id)
    # classroom.courses.courseWork.studentSubmissions.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Submissions Patch
#'
#' Updates one or more fields of a student submission. See google.classroom.v1.StudentSubmission
#' for details of which fields may be updated and who may change them. This request must be made
#' by the Developer Console project of the [OAuth client ID](https://support.google.com/cloud/answer/6158849)
#' used tocreate the corresponding course work item.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting developer project did not createthe corresponding course work, if the user is not permitted to make therequested modification to the student submission, or foraccess errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course, course work, or student submissiondoes not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.me
#' \item https://www.googleapis.com/auth/classroom.coursework.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me", "https://www.googleapis.com/auth/classroom.coursework.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param StudentSubmission The \link{gc_student_submission} object to pass to this method
#' @param courseId Identifier of the course
#' @param courseWorkId Identifier of the course work
#' @param id Identifier of the student submission
#' @param updateMask Mask that identifies which fields on the student submission to update
#' @importFrom googleAuthR gar_api_generator
#' @family StudentSubmission functions
#' @export
gc_courses_course_work_student_submissions_patch <- function(StudentSubmission, courseId,
    courseWorkId, id, updateMask = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s/studentSubmissions/%s",
        courseId, courseWorkId, id)
    # classroom.courses.courseWork.studentSubmissions.patch
    pars = list(updateMask = updateMask)
    f <- googleAuthR::gar_api_generator(url, "PATCH", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    stopifnot(inherits(StudentSubmission, "gar_StudentSubmission"))

    f(the_body = StudentSubmission)

}

#' Submissions Return
#'
#' Returns a student submission.
#'
#' Returning a student submission transfers ownership of attached Drivefiles to the student and may
#' also update the submission state. Unlike the Classroom application, returning a student submission
#' does notset assignedGrade to the draftGrade value. Only a teacher of the course that contains the
#' requested student submissionmay call this method. This request must be made by the Developer Console
#'  project of the[OAuth client ID](https://support.google.com/cloud/answer/6158849) used tocreate the
#'  corresponding course work item.
#'
#'  This method returns the following error codes:
#'  * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work, return the requested student submission,or for access errors.
#'  * `INVALID_ARGUMENT` if the request is malformed.
#'  * `NOT_FOUND` if the requested course, course work, or student submissiondoes not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param ReturnStudentSubmissionRequest The \link{gc_return_student_submission_request} object to pass to this method
#' @param courseId Identifier of the course
#' @param courseWorkId Identifier of the course work
#' @param id Identifier of the student submission
#' @importFrom googleAuthR gar_api_generator
#' @family ReturnStudentSubmissionRequest functions
#' @export
gc_courses_course_work_student_submissions_return <- function(ReturnStudentSubmissionRequest,
    courseId, courseWorkId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s/studentSubmissions/%s:return",
        courseId, courseWorkId, id)
    # classroom.courses.courseWork.studentSubmissions.return
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(ReturnStudentSubmissionRequest, "gar_ReturnStudentSubmissionRequest"))

    f(the_body = ReturnStudentSubmissionRequest)

}

#' Submissions Reclaim
#'
#' Reclaims a student submission on behalf of the student that owns it. Reclaiming a student submission transfers
#' ownership of attached Drivefiles to the student and updates the submission state. Only the student that owns
#' the requested student submission may call thismethod, and only for a student submission that has been turned in.
#' This request must be made by the Developer Console project of the [OAuth client ID](https://support.google.com/cloud/answer/6158849)
#' used tocreate the corresponding course work item.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work, unsubmit the requested student submission,or for access errors.
#' * `FAILED_PRECONDITION` if the student submission has not been turned in.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course, course work, or student submissiondoes not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.me
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param ReclaimStudentSubmissionRequest The \link{gc_reclaim_student_submission_request} object to pass to this method
#' @param courseId Identifier of the course
#' @param courseWorkId Identifier of the course work
#' @param id Identifier of the student submission
#' @importFrom googleAuthR gar_api_generator
#' @family ReclaimStudentSubmissionRequest functions
#' @export
gc_courses_course_work_student_submissions_reclaim <- function(ReclaimStudentSubmissionRequest,
    courseId, courseWorkId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s/studentSubmissions/%s:reclaim",
        courseId, courseWorkId, id)
    # classroom.courses.courseWork.studentSubmissions.reclaim
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(ReclaimStudentSubmissionRequest, "gar_ReclaimStudentSubmissionRequest"))

    f(the_body = ReclaimStudentSubmissionRequest)

}

#' Submissions Turn In
#'
#' Turns in a student submission
#'
#' Turning in a student submission transfers ownership of attached Drivefiles to the teacher and may also
#' update the submission state. This may only be called by the student that owns the specified studentsubmission.
#' This request must be made by the Developer Console project of the [OAuth client ID](https://support.google.com/cloud/answer/6158849)
#' used tocreate the corresponding course work item.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work, turn in the requested student submission,or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course, course work, or student submissiondoes not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.me
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param TurnInStudentSubmissionRequest The \link{gc_turn_in_student_submission_request} object to pass to this method
#' @param courseId Identifier of the course
#' @param courseWorkId Identifier of the course work
#' @param id Identifier of the student submission
#' @importFrom googleAuthR gar_api_generator
#' @family TurnInStudentSubmissionRequest functions
#' @export
gc_courses_course_work_student_submissions_turn_in <- function(TurnInStudentSubmissionRequest,
    courseId, courseWorkId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s/studentSubmissions/%s:turnIn",
        courseId, courseWorkId, id)
    # classroom.courses.courseWork.studentSubmissions.turnIn
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(TurnInStudentSubmissionRequest, "gar_TurnInStudentSubmissionRequest"))

    f(the_body = TurnInStudentSubmissionRequest)

}

#' Submissions List
#'
#' Returns a list of student submissions that the requester is permitted toview, factoring in the OAuth scopes
#' of the request. `-` may be specified as the `course_work_id` to include studentsubmissions for multiple course
#' work items. Course students may only view their own work. Course teachersand domain administrators may view all
#' student submissions.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work, or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.me
#' \item https://www.googleapis.com/auth/classroom.coursework.me.readonly
#' \item https://www.googleapis.com/auth/classroom.coursework.students
#' \item https://www.googleapis.com/auth/classroom.coursework.students.readonly
#' \item https://www.googleapis.com/auth/classroom.student-submissions.me.readonly
#' \item https://www.googleapis.com/auth/classroom.student-submissions.students.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me", "https://www.googleapis.com/auth/classroom.coursework.me.readonly", "https://www.googleapis.com/auth/classroom.coursework.students", "https://www.googleapis.com/auth/classroom.coursework.students.readonly", "https://www.googleapis.com/auth/classroom.student-submissions.me.readonly", "https://www.googleapis.com/auth/classroom.student-submissions.students.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param courseWorkId Identifier of the student work to request
#' @param userId Optional argument to restrict returned student work to those owned by the
#' @param late Requested lateness value
#' @param pageToken nextPageToken
#' @param states Requested submission states
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_course_work_student_submissions_list <- function(courseId, courseWorkId, userId = NULL,
    late = NULL, pageToken = NULL, states = NULL, pageSize = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s/studentSubmissions",
        courseId, courseWorkId)
    # classroom.courses.courseWork.studentSubmissions.list
    pars = list(userId = userId, late = late, pageToken = pageToken, states = states,
        pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Modify Attachments
#'
#' Modifies attachments of student submission. Attachments may only be added to student submissions belonging to
#' coursework objects with a `workType` of `ASSIGNMENT`. This request must be made by the Developer Console
#' project of the [OAuth client ID](https://support.google.com/cloud/answer/6158849) used tocreate the corresponding
#' course work item.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work, if the user is not permitted to modifyattachments on the requested student submission, or foraccess errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course, course work, or student submissiondoes not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.coursework.me
#' \item https://www.googleapis.com/auth/classroom.coursework.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.coursework.me", "https://www.googleapis.com/auth/classroom.coursework.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param ModifyAttachmentsRequest The \link{gc_modify_attachments_request} object to pass to this method
#' @param courseId Identifier of the course
#' @param courseWorkId Identifier of the course work
#' @param id Identifier of the student submission
#' @importFrom googleAuthR gar_api_generator
#' @family ModifyAttachmentsRequest functions
#' @export
gc_courses_course_work_student_submissions_modify_attachments <- function(ModifyAttachmentsRequest,
    courseId, courseWorkId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/courseWork/%s/studentSubmissions/%s:modifyAttachments",
        courseId, courseWorkId, id)
    # classroom.courses.courseWork.studentSubmissions.modifyAttachments
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(ModifyAttachmentsRequest, "gar_ModifyAttachmentsRequest"))

    f(the_body = ModifyAttachmentsRequest)

}

#' Create Teachers
#'
#' Creates a teacher of a course.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not  permitted to createteachers in this course or for access errors.
#' * `NOT_FOUND` if the requested course ID does not exist.
#' * `FAILED_PRECONDITION` if the requested user's account is disabled,for the following request errors:
#'     - CourseMemberLimitReached
#'     - CourseNotModifiable
#'     - CourseTeacherLimitReached
#'     - UserGroupsMembershipLimitReached
#' * `ALREADY_EXISTS` if the user is already a teacher or student in thecourse.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.profile.emails
#' \item https://www.googleapis.com/auth/classroom.profile.photos
#' \item https://www.googleapis.com/auth/classroom.rosters
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.profile.emails", "https://www.googleapis.com/auth/classroom.profile.photos", "https://www.googleapis.com/auth/classroom.rosters")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Teacher The \link{gc_teacher} object to pass to this method
#' @param courseId Identifier of the course
#' @importFrom googleAuthR gar_api_generator
#' @family Teacher functions
#' @export
gc_courses_teachers_create <- function(Teacher, courseId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/teachers", courseId)
    # classroom.courses.teachers.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(Teacher, "gar_Teacher"))

    f(the_body = Teacher)

}

#' Delete Teachers
#'
#' Deletes a teacher of a course.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to deleteteachers of this course or for access errors.
#' * `NOT_FOUND` if no teacher of this course has the requested ID or if thecourse does not exist.
#' * `FAILED_PRECONDITION` if the requested ID belongs to the primary teacherof this course.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.rosters
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.rosters")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param userId Identifier of the teacher to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_teachers_delete <- function(courseId, userId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/teachers/%s",
        courseId, userId)
    # classroom.courses.teachers.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' Get Teachers
#'
#' Returns a teacher of a course.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to viewteachers of this course or for access errors.
#' * `NOT_FOUND` if no teacher of this course has the requested ID or if thecourse does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.profile.emails
#' \item https://www.googleapis.com/auth/classroom.profile.photos
#' \item https://www.googleapis.com/auth/classroom.rosters
#' \item https://www.googleapis.com/auth/classroom.rosters.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.profile.emails", "https://www.googleapis.com/auth/classroom.profile.photos", "https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param userId Identifier of the teacher to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_teachers_get <- function(courseId, userId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/teachers/%s",
        courseId, userId)
    # classroom.courses.teachers.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Teachers List
#'
#' Returns a list of teachers of this course that the requesteris permitted to view.
#'
#' This method returns the following error codes:
#' * `NOT_FOUND` if the course does not exist.
#' * `PERMISSION_DENIED` for access errors.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.profile.emails
#' \item https://www.googleapis.com/auth/classroom.profile.photos
#' \item https://www.googleapis.com/auth/classroom.rosters
#' \item https://www.googleapis.com/auth/classroom.rosters.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.profile.emails", "https://www.googleapis.com/auth/classroom.profile.photos", "https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param pageToken nextPageToken
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_teachers_list <- function(courseId, pageToken = NULL, pageSize = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/teachers", courseId)
    # classroom.courses.teachers.list
    pars = list(pageToken = pageToken, pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Creates an announcement
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course, create announcements in the requested course, share aDrive attachment, or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course does not exist.
#' * `FAILED_PRECONDITION` for the following request error:
#'     - AttachmentNotVisible
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.announcements
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.announcements")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Announcement The \link{gc_announcement} object to pass to this method
#' @param courseId Identifier of the course
#' @importFrom googleAuthR gar_api_generator
#' @family Announcement functions
#' @export
gc_courses_announcements_create <- function(Announcement, courseId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/announcements",
        courseId)
    # classroom.courses.announcements.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(Announcement, "gar_Announcement"))

    f(the_body = Announcement)

}

#' Modify Assignees
#'
#' Modifies assignee mode and options of an announcement. Only a teacher of the course that contains the
#' announcement may call this method.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or course work or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course or course work does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.announcements
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.announcements")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param ModifyAnnouncementAssigneesRequest The \link{gc_modify_announcement_assignees_request} object to pass to this method
#' @param courseId Identifier of the course
#' @param id Identifier of the announcement
#' @importFrom googleAuthR gar_api_generator
#' @family ModifyAnnouncementAssigneesRequest functions
#' @export
gc_courses_announcements_modify_assignees <- function(ModifyAnnouncementAssigneesRequest,
    courseId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/announcements/%s:modifyAssignees",
        courseId, id)
    # classroom.courses.announcements.modifyAssignees
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(ModifyAnnouncementAssigneesRequest, "gar_ModifyAnnouncementAssigneesRequest"))

    f(the_body = ModifyAnnouncementAssigneesRequest)

}

#' Get Announcements
#'
#' Returns an announcement.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to access therequested course or announcement, or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course or announcement does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.announcements
#' \item https://www.googleapis.com/auth/classroom.announcements.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.announcements", "https://www.googleapis.com/auth/classroom.announcements.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param id Identifier of the announcement
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_announcements_get <- function(courseId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/announcements/%s",
        courseId, id)
    # classroom.courses.announcements.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Announcements Patch
#'
#' Updates one or more fields of an announcement.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting developer project did not createthe corresponding announcement or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `FAILED_PRECONDITION` if the requested announcement has already beendeleted.
#' * `NOT_FOUND` if the requested course or announcement does not exist
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.announcements
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.announcements")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Announcement The \link{gc_announcement} object to pass to this method
#' @param courseId Identifier of the course
#' @param id Identifier of the announcement
#' @param updateMask Mask that identifies which fields on the announcement to update
#' @importFrom googleAuthR gar_api_generator
#' @family Announcement functions
#' @export
gc_courses_announcements_patch <- function(Announcement, courseId, id, updateMask = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/announcements/%s",
        courseId, id)
    # classroom.courses.announcements.patch
    pars = list(updateMask = updateMask)
    f <- googleAuthR::gar_api_generator(url, "PATCH", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    stopifnot(inherits(Announcement, "gar_Announcement"))

    f(the_body = Announcement)

}

#' Deletes an announcement
#'
#' This request must be made by the Developer Console project of the [OAuth client ID](https://support.google.com/cloud/answer/6158849) used to
#' create the corresponding announcement item.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting developer project did not createthe corresponding announcement, if the requesting user is not permitted to delete the requested course or for access errors.
#' * `FAILED_PRECONDITION` if the requested announcement has already beendeleted.
#' * `NOT_FOUND` if no course exists with the requested ID.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.announcements
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.announcements")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param id Identifier of the announcement to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_announcements_delete <- function(courseId, id) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/announcements/%s",
        courseId, id)
    # classroom.courses.announcements.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' Announcements List
#'
#' Returns a list of announcements that the requester is permitted to view. Course students may only
#' view `PUBLISHED` announcements. Course teachersand domain administrators may view all announcements.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to accessthe requested course or for access errors.
#' * `INVALID_ARGUMENT` if the request is malformed.
#' * `NOT_FOUND` if the requested course does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.announcements
#' \item https://www.googleapis.com/auth/classroom.announcements.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.announcements", "https://www.googleapis.com/auth/classroom.announcements.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param announcementStates Restriction on the `state` of announcements returned
#' @param orderBy Optional sort ordering for results
#' @param pageToken nextPageToken
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_announcements_list <- function(courseId, announcementStates = NULL, orderBy = NULL,
    pageToken = NULL, pageSize = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/announcements",
        courseId)
    # classroom.courses.announcements.list
    pars = list(announcementStates = announcementStates, orderBy = orderBy, pageToken = pageToken,
        pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Deletes a student of a course
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to deletestudents of this course or for access errors.
#' * `NOT_FOUND` if no student of this course has the requested ID or if thecourse does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.rosters
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.rosters")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param userId Identifier of the student to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_students_delete <- function(courseId, userId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/students/%s",
        courseId, userId)
    # classroom.courses.students.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' Get Students
#'
#' Returns a student of a course.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to viewstudents of this course or for access errors.
#' * `NOT_FOUND` if no student of this course has the requested ID or if thecourse does not exist.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.profile.emails
#' \item https://www.googleapis.com/auth/classroom.profile.photos
#' \item https://www.googleapis.com/auth/classroom.rosters
#' \item https://www.googleapis.com/auth/classroom.rosters.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.profile.emails", "https://www.googleapis.com/auth/classroom.profile.photos", "https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param userId Identifier of the student to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_students_get <- function(courseId, userId) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/students/%s",
        courseId, userId)
    # classroom.courses.students.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Students List
#'
#' Returns a list of students of this course that the requester is permitted to view.
#'
#' This method returns the following error codes:
#' * `NOT_FOUND` if the course does not exist.
#' * `PERMISSION_DENIED` for access errors.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.profile.emails
#' \item https://www.googleapis.com/auth/classroom.profile.photos
#' \item https://www.googleapis.com/auth/classroom.rosters
#' \item https://www.googleapis.com/auth/classroom.rosters.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.profile.emails", "https://www.googleapis.com/auth/classroom.profile.photos", "https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param courseId Identifier of the course
#' @param pageToken nextPageToken
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_courses_students_list <- function(courseId, pageToken = NULL, pageSize = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/students", courseId)
    # classroom.courses.students.list
    pars = list(pageToken = pageToken, pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Create Students
#'
#' Adds a user as a student of a course.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to createstudents in this course or for access errors.
#' * `NOT_FOUND` if the requested course ID does not exist.
#' * `FAILED_PRECONDITION` if the requested user's account is disabled,for the following request errors:
#'     - CourseMemberLimitReached
#'     - CourseNotModifiable
#'     - UserGroupsMembershipLimitReached
#' * `ALREADY_EXISTS` if the user is already a student or teacher in thecourse.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.profile.emails
#' \item https://www.googleapis.com/auth/classroom.profile.photos
#' \item https://www.googleapis.com/auth/classroom.rosters
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.profile.emails", "https://www.googleapis.com/auth/classroom.profile.photos", "https://www.googleapis.com/auth/classroom.rosters")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Student The \link{gc_student} object to pass to this method
#' @param courseId Identifier of the course to create the student in
#' @param enrollmentCode Enrollment code of the course to create the student in
#' @importFrom googleAuthR gar_api_generator
#' @family Student functions
#' @export
gc_courses_students_create <- function(Student, courseId, enrollmentCode = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/courses/%s/students", courseId)
    # classroom.courses.students.create
    pars = list(enrollmentCode = enrollmentCode)
    f <- googleAuthR::gar_api_generator(url, "POST", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    stopifnot(inherits(Student, "gar_Student"))

    f(the_body = Student)

}

#' Get Profile
#'
#' Returns a user profile
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the requesting user is not permitted to accessthis user profile, if no profile exists with the requested ID, or foraccess errors.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.profile.emails
#' \item https://www.googleapis.com/auth/classroom.profile.photos
#' \item https://www.googleapis.com/auth/classroom.rosters
#' \item https://www.googleapis.com/auth/classroom.rosters.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.profile.emails", "https://www.googleapis.com/auth/classroom.profile.photos", "https://www.googleapis.com/auth/classroom.rosters", "https://www.googleapis.com/auth/classroom.rosters.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param userId Identifier of the profile to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_user_profiles_get <- function(userId) {
    url <- sprintf("https://classroom.googleapis.com/v1/userProfiles/%s", userId)
    # classroom.userProfiles.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Guardian Invitations List
#'
#' Returns a list of guardian invitations that the requesting user ispermitted to view, filtered by the
#' parameters provided.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if a `student_id` is specified, and the requesting  user is not permitted to view guardian invitations for that student, if  `'-'` is specified as the `student_id` and the user is not a domain  administrator, if guardians are not enabled for the domain in question,  or for other access errors.
#' * `INVALID_ARGUMENT` if a `student_id` is specified, but its format cannot  be recognized (it is not an email address, nor a `student_id` from the  API, nor the literal string `me`). May also be returned if an invalid  `page_token` or `state` is provided.
#' * `NOT_FOUND` if a `student_id` is specified, and its format can be  recognized, but Classroom has no record of that student.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.guardianlinks.students
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.guardianlinks.students", "https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param studentId The ID of the student whose guardian invitations are to be returned
#' @param pageToken nextPageToken
#' @param invitedEmailAddress If specified, only results with the specified `invited_email_address`
#' @param states If specified, only results with the specified `state` values are
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_user_profiles_guardian_invitations_list <- function(studentId, pageToken = NULL, invitedEmailAddress = NULL,
    states = NULL, pageSize = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/userProfiles/%s/guardianInvitations",
        studentId)
    # classroom.userProfiles.guardianInvitations.list
    pars = list(pageToken = pageToken, invitedEmailAddress = invitedEmailAddress,
        states = states, pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Get Guardian Invitations
#'
#' Returns a specific guardian invitation.
#'
#' This method returns the following error codes:
#'
#' * `PERMISSION_DENIED` if the requesting user is not permitted to view  guardian invitations for the student identified by the `student_id`, if  guardians are not enabled for the domain in question, or for other  access errors.
#' * `INVALID_ARGUMENT` if a `student_id` is specified, but its format cannot  be recognized (it is not an email address, nor a `student_id` from the  API, nor the literal string `me`).
#' * `NOT_FOUND` if Classroom cannot find any record of the given student or  `invitation_id`. May also be returned if the student exists, but the  requesting user does not have access to see that student.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.guardianlinks.students
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.guardianlinks.students", "https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param studentId The ID of the student whose guardian invitation is being requested
#' @param invitationId The `id` field of the `GuardianInvitation` being requested
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_user_profiles_guardian_invitations_get <- function(studentId, invitationId) {
    url <- sprintf("https://classroom.googleapis.com/v1/userProfiles/%s/guardianInvitations/%s",
        studentId, invitationId)
    # classroom.userProfiles.guardianInvitations.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Modifies a guardian invitation
#'
#' Currently, the only valid modification is to change the `state` from`PENDING` to `COMPLETE`. This has
#' the effect of withdrawing the invitation.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the current user does not have permission to  manage guardians, if guardians are not enabled for the domain in question  or for other access errors.
#' * `FAILED_PRECONDITION` if the guardian link is not in the `PENDING` state.
#' * `INVALID_ARGUMENT` if the format of the student ID provided  cannot be recognized (it is not an email address, nor a `user_id` from  this API), or if the passed `GuardianInvitation` has a `state` other than  `COMPLETE`, or if it modifies fields other than `state`.
#' * `NOT_FOUND` if the student ID provided is a valid student ID, but  Classroom has no record of that student, or if the `id` field does not  refer to a guardian invitation known to Classroom.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.guardianlinks.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.guardianlinks.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param GuardianInvitation The \link{gc_guardian_invitation} object to pass to this method
#' @param studentId The ID of the student whose guardian invitation is to be modified
#' @param invitationId The `id` field of the `GuardianInvitation` to be modified
#' @param updateMask Mask that identifies which fields on the course to update
#' @importFrom googleAuthR gar_api_generator
#' @family GuardianInvitation functions
#' @export
gc_user_profiles_guardian_invitations_patch <- function(GuardianInvitation, studentId,
    invitationId, updateMask = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/userProfiles/%s/guardianInvitations/%s",
        studentId, invitationId)
    # classroom.userProfiles.guardianInvitations.patch
    pars = list(updateMask = updateMask)
    f <- googleAuthR::gar_api_generator(url, "PATCH", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    stopifnot(inherits(GuardianInvitation, "gar_GuardianInvitation"))

    f(the_body = GuardianInvitation)

}

#' Create Guardian Invitations
#'
#' Creates a guardian invitation, and sends an email to the guardian askingthem to confirm that they are the
#' student's guardian. Once the guardian accepts the invitation, their `state` will change to`COMPLETED` and
#' they will start receiving guardian notifications. A`Guardian` resource will also be created to represent
#' the active guardian. The request object must have the `student_id` and`invited_email_address` fields set.
#' Failing to set these fields, orsetting any other fields in the request, will result in an error.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if the current user does not have permission to  manage guardians, if the guardian in question has already rejected  too many requests for that student, if guardians are not enabled for the  domain in question, or for other access errors.
#' * `RESOURCE_EXHAUSTED` if the student or guardian has exceeded the guardian  link limit.
#' * `INVALID_ARGUMENT` if the guardian email address is not valid (for  example, if it is too long), or if the format of the student ID provided  cannot be recognized (it is not an email address, nor a `user_id` from  this API). This error will also be returned if read-only fields are set,  or if the `state` field is set to to a value other than `PENDING`.
#' * `NOT_FOUND` if the student ID provided is a valid student ID, but  Classroom has no record of that student.
#' * `ALREADY_EXISTS` if there is already a pending guardian invitation for  the student and `invited_email_address` provided, or if the provided  `invited_email_address` matches the Google account of an existing  `Guardian` for this user.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.guardianlinks.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.guardianlinks.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param GuardianInvitation The \link{gc_guardian_invitation} object to pass to this method
#' @param studentId ID of the student (in standard format)
#' @importFrom googleAuthR gar_api_generator
#' @family GuardianInvitation functions
#' @export
gc_user_profiles_guardian_invitations_create <- function(GuardianInvitation, studentId) {
    url <- sprintf("https://classroom.googleapis.com/v1/userProfiles/%s/guardianInvitations",
        studentId)
    # classroom.userProfiles.guardianInvitations.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(GuardianInvitation, "gar_GuardianInvitation"))

    f(the_body = GuardianInvitation)

}

#' Guardians List
#'
#' Returns a list of guardians that the requesting user is permitted toview, restricted to those that match the
#' request. To list guardians for any student that the requesting user may view guardians for, use the literal
#' character `-` for the student ID.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if a `student_id` is specified, and the requesting  user is not permitted to view guardian information for that student, if  `'-'` is specified as the `student_id` and the user is not a domain  administrator, if guardians are not enabled for the domain in question,  if the `invited_email_address` filter is set by a user who is not a  domain administrator, or for other access errors.
#' * `INVALID_ARGUMENT` if a `student_id` is specified, but its format cannot  be recognized (it is not an email address, nor a `student_id` from the  API, nor the literal string `me`). May also be returned if an invalid  `page_token` is provided.
#' * `NOT_FOUND` if a `student_id` is specified, and its format can be  recognized, but Classroom has no record of that student.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.guardianlinks.me.readonly
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.students
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.guardianlinks.me.readonly", "https://www.googleapis.com/auth/classroom.guardianlinks.students", "https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param studentId Filter results by the student who the guardian is linked to
#' @param pageToken nextPageToken
#' @param invitedEmailAddress Filter results by the email address that the original invitation was sent
#' @param pageSize Maximum number of items to return
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_user_profiles_guardians_list <- function(studentId, pageToken = NULL, invitedEmailAddress = NULL,
    pageSize = NULL) {
    url <- sprintf("https://classroom.googleapis.com/v1/userProfiles/%s/guardians",
        studentId)
    # classroom.userProfiles.guardians.list
    pars = list(pageToken = pageToken, invitedEmailAddress = invitedEmailAddress,
        pageSize = pageSize)
    f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars),
        data_parse_function = function(x) x)
    f()

}

#' Get Guardians
#'
#' Returns a specific guardian.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if no user that matches the provided `student_id`  is visible to the requesting user, if the requesting user is not  permitted to view guardian information for the student identified by the  `student_id`, if guardians are not enabled for the domain in question,  or for other access errors.
#' * `INVALID_ARGUMENT` if a `student_id` is specified, but its format cannot  be recognized (it is not an email address, nor a `student_id` from the  API, nor the literal string `me`).
#' * `NOT_FOUND` if the requesting user is permitted to view guardians for  the requested `student_id`, but no `Guardian` record exists for that  student that matches the provided `guardian_id`.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.guardianlinks.me.readonly
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.students
#' \item https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.guardianlinks.me.readonly", "https://www.googleapis.com/auth/classroom.guardianlinks.students", "https://www.googleapis.com/auth/classroom.guardianlinks.students.readonly")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param studentId The student whose guardian is being requested
#' @param guardianId The `id` field from a `Guardian`
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_user_profiles_guardians_get <- function(studentId, guardianId) {
    url <- sprintf("https://classroom.googleapis.com/v1/userProfiles/%s/guardians/%s",
        studentId, guardianId)
    # classroom.userProfiles.guardians.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()

}

#' Deletes a guardian
#'
#' The guardian will no longer receive guardian notifications and the guardianwill no longer be accessible
#' via the API.
#'
#' This method returns the following error codes:
#' * `PERMISSION_DENIED` if no user that matches the provided `student_id`  is visible to the requesting user, if the requesting user is not  permitted to manage guardians for the student identified by the  `student_id`, if guardians are not enabled for the domain in question,  or for other access errors.
#' * `INVALID_ARGUMENT` if a `student_id` is specified, but its format cannot  be recognized (it is not an email address, nor a `student_id` from the  API).
#' * `NOT_FOUND` if the requesting user is permitted to modify guardians for  the requested `student_id`, but no `Guardian` record exists for that  student with the provided `guardian_id`.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.guardianlinks.students
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.guardianlinks.students")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param studentId The student whose guardian is to be deleted
#' @param guardianId The `id` field from a `Guardian`
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_user_profiles_guardians_delete <- function(studentId, guardianId) {
    url <- sprintf("https://classroom.googleapis.com/v1/userProfiles/%s/guardians/%s",
        studentId, guardianId)
    # classroom.userProfiles.guardians.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()

}

#' create Registrations
#'
#' Creates a `Registration`, causing Classroom to start sending notificationsfrom the provided `feed` to the
#' destination provided in `cloudPubSubTopic`.Returns the created `Registration`. Currently, this will be the
#' same asthe argument, but with server-assigned fields such as `expiry_time` and`id` filled in. Note that any
#' value specified for the `expiry_time` or `id` fields will beignored. While Classroom may validate the
#' `cloudPubSubTopic` and return errors on abest effort basis, it is the caller's responsibility to ensure that
#' itexists and that Classroom has permission to publish to it.
#'
#' This method may return the following error codes:
#' * `PERMISSION_DENIED` if:
#'     - the authenticated user does not have permission to receive notifications from the requested field; or
#'     - the credential provided does not include the appropriate scope for the requested feed.
#'     - another access error is encountered.
#' - `INVALID_ARGUMENT` if:
#'     - no `cloudPubsubTopic` is specified, or the specified `cloudPubsubTopic` is not valid; or
#'     - no `feed` is specified, or the specified `feed` is not valid.* `NOT_FOUND` if:
#'     - the specified `feed` cannot be located, or the requesting user does not have permission to determine whether or not it exists; or
#'     - the specified `cloudPubsubTopic` cannot be located, or Classroom has not been granted permission to publish to it.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.push-notifications
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.push-notifications")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param Registration The \link{gc_registration} object to pass to this method
#' @importFrom googleAuthR gar_api_generator
#' @family Registration functions
#' @export
gc_registrations_create <- function(Registration) {
    url <- "https://classroom.googleapis.com/v1/registrations"
    # classroom.registrations.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(Registration, "gar_Registration"))

    f(the_body = Registration)

}


#' Delete Registrations
#'
#' Deletes a `Registration`, causing Classroom to stop sending notificationsfor that `Registration`.
#'
#'
#'
#' @seealso \href{https://developers.google.com/classroom/}{Google Documentation}
#'
#' @details
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/classroom.push-notifications
#' }
#'
#' Set \code{options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/classroom.push-notifications")}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details.
#'
#' @param registrationId The `registration_id` of the `Registration` to be deleted
#' @importFrom googleAuthR gar_api_generator
#' @export
gc_registrations_delete <- function(registrationId) {
    url <- sprintf("https://classroom.googleapis.com/v1/registrations/%s", registrationId)
    # classroom.registrations.delete

    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()
}



