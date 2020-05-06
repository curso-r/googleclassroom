#' Google Classroom API Objects
#' Manages classes, rosters, and invitations in Google Classroom.

#' ListTeachersResponse Object
#'
#' @details
#'
#' Response when listing teachers.
#'
#' @param teachers Teachers who match the list request
#' @param nextPageToken Token identifying the next page of results to return
#'
#' @return ListTeachersResponse object
#'
#' @family ListTeachersResponse functions
#' @export
gc_list_teachers_response <- function(teachers = NULL, nextPageToken = NULL) {
    structure(list(teachers = teachers, nextPageToken = nextPageToken), class = c("gar_ListTeachersResponse",
        "list"))
}

#' Link Object
#'
#' @details
#'
#' URL item.
#'
#' @param title Title of the target of the URL
#' @param thumbnailUrl URL of a thumbnail image of the target URL
#' @param url URL to link to
#'
#' @return Link object
#'
#' @family Link functions
#' @export
gc_link <- function(title = NULL, thumbnailUrl = NULL, url = NULL) {
    structure(list(title = title, thumbnailUrl = thumbnailUrl, url = url), class = c("gar_Link",
        "list"))
}

#' Date Object
#'
#' @details
#'
#' Represents a whole or partial calendar date, e.g. a birthday. The time of dayand time zone are either specified elsewhere or are not significant. The dateis relative to the Proleptic Gregorian Calendar. This can represent:* A full date, with non-zero year, month and day values* A month and day value, with a zero year, e.g. an anniversary* A year on its own, with zero month and day values* A year and month value, with a zero day, e.g. a credit card expiration dateRelated types are google.type.TimeOfDay and `google.protobuf.Timestamp`.
#'
#' @param year Year of date
#' @param day Day of month
#' @param month Month of year
#'
#' @return Date object
#'
#' @family Date functions
#' @export
gc_date <- function(year = NULL, day = NULL, month = NULL) {
    structure(list(year = year, day = day, month = month), class = c("gar_Date",
        "list"))
}

#' Name Object
#'
#' @details
#'
#' Details of the user's name.
#'
#' @param givenName The user's first name
#' @param familyName The user's last name
#' @param fullName The user's full name formed by concatenating the first and last name
#'
#' @return Name object
#'
#' @family Name functions
#' @export
gc_name <- function(givenName = NULL, familyName = NULL, fullName = NULL) {
    structure(list(givenName = givenName, familyName = familyName, fullName = fullName),
        class = c("gar_Name", "list"))
}

#' Assignment Object
#'
#' @details
#'
#' Additional details for assignments.
#'
#' @param studentWorkFolder Drive folder where attachments from student submissions are placed
#'
#' @return Assignment object
#'
#' @family Assignment functions
#' @export
gc_assignment <- function(studentWorkFolder = NULL) {
    structure(list(studentWorkFolder = studentWorkFolder), class = c("gar_Assignment",
        "list"))
}

#' SharedDriveFile Object
#'
#' @details
#'
#' Drive file that is used as material for course work.
#'
#' @param driveFile Drive file details
#' @param shareMode Mechanism by which students access the Drive item
#'
#' @return SharedDriveFile object
#'
#' @family SharedDriveFile functions
#' @export
gc_shared_drive_file <- function(driveFile = NULL, shareMode = NULL) {
    structure(list(driveFile = driveFile, shareMode = shareMode), class = c("gar_SharedDriveFile",
        "list"))
}

#' Empty Object
#'
#' @details
#'
#' A generic empty message that you can re-use to avoid defining duplicatedempty messages in your APIs. A typical example is to use it as the requestor the response type of an API method. For instance:    service Foo {      rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty);    }The JSON representation for `Empty` is empty JSON object `{}`.
#'
#'
#'
#' @return Empty object
#'
#' @family Empty functions
#' @export
gc_empty <- function() {
    list()
}

#' GlobalPermission Object
#'
#' @details
#'
#' Global user permission description.
#'
#' @param permission Permission value
#'
#' @return GlobalPermission object
#'
#' @family GlobalPermission functions
#' @export
gc_global_permission <- function(permission = NULL) {
    structure(list(permission = permission), class = c("gar_GlobalPermission", "list"))
}

#' ModifyAnnouncementAssigneesRequest Object
#'
#' @details
#'
#' Request to modify assignee mode and options of an announcement.
#'
#' @param modifyIndividualStudentsOptions Set which students can view or cannot view the announcement
#' @param assigneeMode Mode of the announcement describing whether it is accessible by all
#'
#' @return ModifyAnnouncementAssigneesRequest object
#'
#' @family ModifyAnnouncementAssigneesRequest functions
#' @export
gc_modify_announcement_assignees_request <- function(modifyIndividualStudentsOptions = NULL,
    assigneeMode = NULL) {
    structure(list(modifyIndividualStudentsOptions = modifyIndividualStudentsOptions,
        assigneeMode = assigneeMode), class = c("gar_ModifyAnnouncementAssigneesRequest",
        "list"))
}

#' Teacher Object
#'
#' @details
#'
#' Teacher of a course.
#'
#' @param profile Global user information for the teacher
#' @param userId Identifier of the user
#' @param courseId Identifier of the course
#'
#' @return Teacher object
#'
#' @family Teacher functions
#' @export
gc_teacher <- function(profile = NULL, userId = NULL, courseId = NULL) {
    structure(list(profile = profile, userId = userId, courseId = courseId), class = c("gar_Teacher",
        "list"))
}

#' GradeHistory Object
#'
#' @details
#'
#' The history of each grade on this submission.
#'
#' @param actorUserId The teacher who made the grade change
#' @param gradeTimestamp When the grade of the submission was changed
#' @param gradeChangeType The type of grade change at this time in the submission grade history
#' @param maxPoints The denominator of the grade at this time in the submission grade
#' @param pointsEarned The numerator of the grade at this time in the submission grade history
#'
#' @return GradeHistory object
#'
#' @family GradeHistory functions
#' @export
gc_grade_history <- function(actorUserId = NULL, gradeTimestamp = NULL, gradeChangeType = NULL,
    maxPoints = NULL, pointsEarned = NULL) {
    structure(list(actorUserId = actorUserId, gradeTimestamp = gradeTimestamp, gradeChangeType = gradeChangeType,
        maxPoints = maxPoints, pointsEarned = pointsEarned), class = c("gar_GradeHistory",
        "list"))
}

#' AssignmentSubmission Object
#'
#' @details
#'
#' Student work for an assignment.
#'
#' @param attachments Attachments added by the student
#'
#' @return AssignmentSubmission object
#'
#' @family AssignmentSubmission functions
#' @export
gc_assignment_submission <- function(attachments = NULL) {
    structure(list(attachments = attachments), class = c("gar_AssignmentSubmission",
        "list"))
}

#' Material Object
#'
#' @details
#'
#' Material attached to course work.When creating attachments, setting the `form` field is not supported.
#'
#' @param youtubeVideo YouTube video material
#' @param driveFile Google Drive file material
#' @param form Google Forms material
#' @param link Link material
#'
#' @return Material object
#'
#' @family Material functions
#' @export
gc_material <- function(youtubeVideo = NULL, driveFile = NULL, form = NULL, link = NULL) {
    structure(list(youtubeVideo = youtubeVideo, driveFile = driveFile, form = form,
        link = link), class = c("gar_Material", "list"))
}

#' Feed Object
#'
#' @details
#'
#' A class of notifications that an application can register to receive.For example: 'all roster changes for a domain'.
#'
#' @param courseRosterChangesInfo Information about a `Feed` with a `feed_type` of `COURSE_ROSTER_CHANGES`
#' @param courseWorkChangesInfo Information about a `Feed` with a `feed_type` of `COURSE_WORK_CHANGES`
#' @param feedType The type of feed
#'
#' @return Feed object
#'
#' @family Feed functions
#' @export
gc_feed <- function(courseRosterChangesInfo = NULL, courseWorkChangesInfo = NULL, feedType = NULL) {
    structure(list(courseRosterChangesInfo = courseRosterChangesInfo, courseWorkChangesInfo = courseWorkChangesInfo,
        feedType = feedType), class = c("gar_Feed", "list"))
}

#' Student Object
#'
#' @details
#'
#' Student in a course.
#'
#' @param userId Identifier of the user
#' @param courseId Identifier of the course
#' @param profile Global user information for the student
#' @param studentWorkFolder Information about a Drive Folder for this student's work in this course
#'
#' @return Student object
#'
#' @family Student functions
#' @export
gc_student <- function(userId = NULL, courseId = NULL, profile = NULL, studentWorkFolder = NULL) {
    structure(list(userId = userId, courseId = courseId, profile = profile, studentWorkFolder = studentWorkFolder),
        class = c("gar_Student", "list"))
}

#' Invitation Object
#'
#' @details
#'
#' An invitation to join a course.
#'
#' @param id Identifier assigned by Classroom
#' @param role Role to invite the user to have
#' @param userId Identifier of the invited user
#' @param courseId Identifier of the course to invite the user to
#'
#' @return Invitation object
#'
#' @family Invitation functions
#' @export
gc_invitation <- function(id = NULL, role = NULL, userId = NULL, courseId = NULL) {
    structure(list(id = id, role = role, userId = userId, courseId = courseId), class = c("gar_Invitation",
        "list"))
}

#' TurnInStudentSubmissionRequest Object
#'
#' @details
#'
#' Request to turn in a student submission.
#'
#'
#'
#' @return TurnInStudentSubmissionRequest object
#'
#' @family TurnInStudentSubmissionRequest functions
#' @export
gc_turn_in_student_submission_request <- function() {
    list()
}

#' ListCourseWorkResponse Object
#'
#' @details
#'
#' Response when listing course work.
#'
#' @param nextPageToken Token identifying the next page of results to return
#' @param courseWork Course work items that match the request
#'
#' @return ListCourseWorkResponse object
#'
#' @family ListCourseWorkResponse functions
#' @export
gc_list_course_work_response <- function(nextPageToken = NULL, courseWork = NULL) {
    structure(list(nextPageToken = nextPageToken, courseWork = courseWork), class = c("gar_ListCourseWorkResponse",
        "list"))
}

#' Attachment Object
#'
#' @details
#'
#' Attachment added to student assignment work.When creating attachments, setting the `form` field is not supported.
#'
#' @param driveFile Google Drive file attachment
#' @param youTubeVideo Youtube video attachment
#' @param form Google Forms attachment
#' @param link Link attachment
#'
#' @return Attachment object
#'
#' @family Attachment functions
#' @export
gc_attachment <- function(driveFile = NULL, youTubeVideo = NULL, form = NULL, link = NULL) {
    structure(list(driveFile = driveFile, youTubeVideo = youTubeVideo, form = form,
        link = link), class = c("gar_Attachment", "list"))
}

#' ListTopicResponse Object
#'
#' @details
#'
#' Response when listing topics.
#'
#' @param nextPageToken Token identifying the next page of results to return
#' @param topic Topic items that match the request
#'
#' @return ListTopicResponse object
#'
#' @family ListTopicResponse functions
#' @export
gc_lst_topic_response <- function(nextPageToken = NULL, topic = NULL) {
    structure(list(nextPageToken = nextPageToken, topic = topic), class = c("gar_ListTopicResponse",
        "list"))
}

#' ListAnnouncementsResponse Object
#'
#' @details
#'
#' Response when listing course work.
#'
#' @param announcements Announcement items that match the request
#' @param nextPageToken Token identifying the next page of results to return
#'
#' @return ListAnnouncementsResponse object
#'
#' @family ListAnnouncementsResponse functions
#' @export
gc_list_announcements_response <- function(announcements = NULL, nextPageToken = NULL) {
    structure(list(announcements = announcements, nextPageToken = nextPageToken),
        class = c("gar_ListAnnouncementsResponse", "list"))
}

#' TimeOfDay Object
#'
#' @details
#'
#' Represents a time of day. The date and time zone are either not significantor are specified elsewhere. An API may choose to allow leap seconds. Relatedtypes are google.type.Date and `google.protobuf.Timestamp`.
#'
#' @param seconds Seconds of minutes of the time
#' @param minutes Minutes of hour of day
#' @param hours Hours of day in 24 hour format
#' @param nanos Fractions of seconds in nanoseconds
#'
#' @return TimeOfDay object
#'
#' @family TimeOfDay functions
#' @export
gc_time_of_day <- function(seconds = NULL, minutes = NULL, hours = NULL, nanos = NULL) {
    structure(list(seconds = seconds, minutes = minutes, hours = hours, nanos = nanos),
        class = c("gar_TimeOfDay", "list"))
}

#' ListCoursesResponse Object
#'
#' @details
#'
#' Response when listing courses.
#'
#' @param nextPageToken Token identifying the next page of results to return
#' @param courses Courses that match the list request
#'
#' @return ListCoursesResponse object
#'
#' @family ListCoursesResponse functions
#' @export
gc_list_courses_response <- function(nextPageToken = NULL, courses = NULL) {
    structure(list(nextPageToken = nextPageToken, courses = courses), class = c("gar_ListCoursesResponse",
        "list"))
}

#' Form Object
#'
#' @details
#'
#' Google Forms item.
#'
#' @param title Title of the Form
#' @param thumbnailUrl URL of a thumbnail image of the Form
#' @param responseUrl URL of the form responses document
#' @param formUrl URL of the form
#'
#' @return Form object
#'
#' @family Form functions
#' @export
gc_form <- function(title = NULL, thumbnailUrl = NULL, responseUrl = NULL, formUrl = NULL) {
    structure(list(title = title, thumbnailUrl = thumbnailUrl, responseUrl = responseUrl,
        formUrl = formUrl), class = c("gar_Form", "list"))
}

#' ModifyCourseWorkAssigneesRequest Object
#'
#' @details
#'
#' Request to modify assignee mode and options of a coursework.
#'
#' @param modifyIndividualStudentsOptions Set which students are assigned or not assigned to the coursework
#' @param assigneeMode Mode of the coursework describing whether it will be assigned to all
#'
#' @return ModifyCourseWorkAssigneesRequest object
#'
#' @family ModifyCourseWorkAssigneesRequest functions
#' @export
gc_modify_course_work_assignees_request <- function(modifyIndividualStudentsOptions = NULL,
    assigneeMode = NULL) {
    structure(list(modifyIndividualStudentsOptions = modifyIndividualStudentsOptions,
        assigneeMode = assigneeMode), class = c("gar_ModifyCourseWorkAssigneesRequest",
        "list"))
}

#' ListGuardiansResponse Object
#'
#' @details
#'
#' Response when listing guardians.
#'
#' @param nextPageToken Token identifying the next page of results to return
#' @param guardians Guardians on this page of results that met the criteria specified in
#'
#' @return ListGuardiansResponse object
#'
#' @family ListGuardiansResponse functions
#' @export
gc_list_guardians_response <- function(nextPageToken = NULL, guardians = NULL) {
    structure(list(nextPageToken = nextPageToken, guardians = guardians), class = c("gar_ListGuardiansResponse",
        "list"))
}

#' CourseAlias Object
#'
#' @details
#'
#' Alternative identifier for a course.An alias uniquely identifies a course. It must be unique within one of thefollowing scopes:* domain: A domain-scoped alias is visible to all users within the aliascreator's domain and can be created only by a domain admin. A domain-scopedalias is often used when a course has an identifier external to Classroom.* project: A project-scoped alias is visible to any request from anapplication using the Developer Console project ID that created the aliasand can be created by any project. A project-scoped alias is often used whenan application has alternative identifiers. A random value can also be usedto avoid duplicate courses in the event of transmission failures, as retryinga request will return `ALREADY_EXISTS` if a previous one has succeeded.
#'
#' @param alias Alias string
#'
#' @return CourseAlias object
#'
#' @family CourseAlias functions
#' @export
gc_course_alias <- function(alias = NULL) {
    structure(list(alias = alias), class = c("gar_CourseAlias", "list"))
}

#' ListGuardianInvitationsResponse Object
#'
#' @details
#'
#' Response when listing guardian invitations.
#'
#' @param guardianInvitations Guardian invitations that matched the list request
#' @param nextPageToken Token identifying the next page of results to return
#'
#' @return ListGuardianInvitationsResponse object
#'
#' @family ListGuardianInvitationsResponse functions
#' @export
gc_list_guardian_invitations_response <- function(guardianInvitations = NULL, nextPageToken = NULL) {
    structure(list(guardianInvitations = guardianInvitations, nextPageToken = nextPageToken),
        class = c("gar_ListGuardianInvitationsResponse", "list"))
}

#' ListCourseAliasesResponse Object
#'
#' @details
#'
#' Response when listing course aliases.
#'
#' @param nextPageToken Token identifying the next page of results to return
#' @param aliases The course aliases
#'
#' @return ListCourseAliasesResponse object
#'
#' @family ListCourseAliasesResponse functions
#' @export
gc_list_course_aliases_response <- function(nextPageToken = NULL, aliases = NULL) {
    structure(list(nextPageToken = nextPageToken, aliases = aliases), class = c("gar_ListCourseAliasesResponse",
        "list"))
}

#' IndividualStudentsOptions Object
#'
#' @details
#'
#' Assignee details about a coursework/announcement.This field is set if and only if `assigneeMode` is `INDIVIDUAL_STUDENTS`.
#'
#' @param studentIds Identifiers for the students that have access to the
#'
#' @return IndividualStudentsOptions object
#'
#' @family IndividualStudentsOptions functions
#' @export
gc_individual_students_options <- function(studentIds = NULL) {
    structure(list(studentIds = studentIds), class = c("gar_IndividualStudentsOptions",
        "list"))
}

#' MultipleChoiceSubmission Object
#'
#' @details
#'
#' Student work for a multiple-choice question.
#'
#' @param answer Student's select choice
#'
#' @return MultipleChoiceSubmission object
#'
#' @family MultipleChoiceSubmission functions
#' @export
gc_multiple_choice_submission <- function(answer = NULL) {
    structure(list(answer = answer), class = c("gar_MultipleChoiceSubmission", "list"))
}

#' Registration Object
#'
#' @details
#'
#' An instruction to Classroom to send notifications from the `feed` to theprovided destination.
#'
#' @param feed Specification for the class of notifications that Classroom should deliver
#' @param registrationId A server-generated unique identifier for this `Registration`
#' @param cloudPubsubTopic The Cloud Pub/Sub topic that notifications are to be sent to
#' @param expiryTime The time until which the `Registration` is effective
#'
#' @return Registration object
#'
#' @family Registration functions
#' @export
gc_registration <- function(feed = NULL, registrationId = NULL, cloudPubsubTopic = NULL,
    expiryTime = NULL) {
    structure(list(feed = feed, registrationId = registrationId, cloudPubsubTopic = cloudPubsubTopic,
        expiryTime = expiryTime), class = c("gar_Registration", "list"))
}

#' CourseMaterial Object
#'
#' @details
#'
#' A material attached to a course as part of a material set.
#'
#' @param link Link atatchment
#' @param driveFile Google Drive file attachment
#' @param youTubeVideo Youtube video attachment
#' @param form Google Forms attachment
#'
#' @return CourseMaterial object
#'
#' @family CourseMaterial functions
#' @export
gc_course_material <- function(link = NULL, driveFile = NULL, youTubeVideo = NULL, form = NULL) {
    structure(list(link = link, driveFile = driveFile, youTubeVideo = youTubeVideo,
        form = form), class = c("gar_CourseMaterial", "list"))
}

#' MultipleChoiceQuestion Object
#'
#' @details
#'
#' Additional details for multiple-choice questions.
#'
#' @param choices Possible choices
#'
#' @return MultipleChoiceQuestion object
#'
#' @family MultipleChoiceQuestion functions
#' @export
gc_multiple_choice_question <- function(choices = NULL) {
    structure(list(choices = choices), class = c("gar_MultipleChoiceQuestion", "list"))
}

#' Course Object
#'
#' See \href{https://developers.google.com/classroom/reference/rest/v1/courses#resource:-course}{Google Documentation} for details
#'
#' @details
#'
#' A Course in Classroom.
#'
#' @param updateTime Time of the most recent update to this course
#' @param calendarId The Calendar ID for a calendar that all course members can see, to which
#' @param alternateLink Absolute link to this course in the Classroom web UI
#' @param guardiansEnabled Whether or not guardian notifications are enabled for this course
#' @param ownerId The identifier of the owner of a course
#' @param courseState State of the course
#' @param description Optional description
#' @param teacherGroupEmail The email address of a Google group containing all teachers of the course
#' @param creationTime Creation time of the course
#' @param teacherFolder Information about a Drive Folder that is shared with all teachers of the
#' @param name Name of the course
#' @param section Section of the course
#' @param id Identifier for this course assigned by Classroom
#' @param room Optional room location
#' @param courseGroupEmail The email address of a Google group containing all members of the course
#' @param enrollmentCode Enrollment code to use when joining this course
#' @param courseMaterialSets Sets of materials that appear on the 'about' page of this course
#' @param descriptionHeading Optional heading for the description
#'
#' @return Course object
#'
#' @family Course functions
#' @export
gc_course <- function(id = NULL, name = NULL, section = NULL, descriptionHeading = NULL,
                      ownerId = NULL, creationTime = NULL, updateTime = NULL,
                      enrollmentCode = NULL, courseState = NULL, alternateLink = NULL,
                      teacherGroupEmail = NULL, courseGroupEmail = NULL, teacherFolder = NULL,
                      guardiansEnabled = NULL, calendarId = NULL, room = NULL,  description = NULL,
                      courseMaterialSets = NULL) {
    structure(list(updateTime = updateTime, calendarId = calendarId, alternateLink = alternateLink,
        guardiansEnabled = guardiansEnabled, ownerId = ownerId, courseState = courseState,
        description = description, teacherGroupEmail = teacherGroupEmail, creationTime = creationTime,
        teacherFolder = teacherFolder, name = name, section = section, id = id, room = room,
        courseGroupEmail = courseGroupEmail, enrollmentCode = enrollmentCode, courseMaterialSets = courseMaterialSets,
        descriptionHeading = descriptionHeading), class = c("gar_Course", "list"))
}

#' DriveFile Object
#'
#' @details
#'
#' Representation of a Google Drive file.
#'
#' @param thumbnailUrl URL of a thumbnail image of the Drive item
#' @param id Drive API resource ID
#' @param title Title of the Drive item
#' @param alternateLink URL that can be used to access the Drive item
#'
#' @return DriveFile object
#'
#' @family DriveFile functions
#' @export
gc_drive_file <- function(thumbnailUrl = NULL, id = NULL, title = NULL, alternateLink = NULL) {
    structure(list(thumbnailUrl = thumbnailUrl, id = id, title = title, alternateLink = alternateLink),
        class = c("gar_DriveFile", "list"))
}

#' ReturnStudentSubmissionRequest Object
#'
#' @details
#'
#' Request to return a student submission.
#'
#'
#'
#' @return ReturnStudentSubmissionRequest object
#'
#' @family ReturnStudentSubmissionRequest functions
#' @export
gc_return_student_submission_request <- function() {
    list()
}

#' ReclaimStudentSubmissionRequest Object
#'
#' @details
#'
#' Request to reclaim a student submission.
#'
#'
#'
#' @return ReclaimStudentSubmissionRequest object
#'
#' @family ReclaimStudentSubmissionRequest functions
#' @export
gc_reclaim_student_submission_request <- function() {
    list()
}

#' CourseRosterChangesInfo Object
#'
#' @details
#'
#' Information about a `Feed` with a `feed_type` of `COURSE_ROSTER_CHANGES`.
#'
#' @param courseId The `course_id` of the course to subscribe to roster changes for
#'
#' @return CourseRosterChangesInfo object
#'
#' @family CourseRosterChangesInfo functions
#' @export
gc_course_roster_changes_info <- function(courseId = NULL) {
    structure(list(courseId = courseId), class = c("gar_CourseRosterChangesInfo",
        "list"))
}

#' Topic Object
#'
#' @details
#'
#' Topic created by a teacher for the course
#'
#' @param courseId Identifier of the course
#' @param updateTime The time the topic was last updated by the system
#' @param name The name of the topic, generated by the user
#' @param topicId Unique identifier for the topic
#'
#' @return Topic object
#'
#' @family Topic functions
#' @export
gc_topic <- function(courseId = NULL, updateTime = NULL, name = NULL, topicId = NULL) {
    structure(list(courseId = courseId, updateTime = updateTime, name = name, topicId = topicId),
        class = c("gar_Topic", "list"))
}

#' CourseWork Object
#'
#' @details
#'
#' Course work created by a teacher for students of the course.
#'
#' @param materials Additional materials
#' @param associatedWithDeveloper Whether this course work item is associated with the Developer Console
#' @param updateTime Timestamp of the most recent change to this course work
#' @param alternateLink Absolute link to this course work in the Classroom web UI
#' @param assigneeMode Assignee mode of the coursework
#' @param maxPoints Maximum grade for this course work
#' @param workType Type of this course work
#' @param assignment Assignment details
#' @param multipleChoiceQuestion Multiple choice question details
#' @param description Optional description of this course work
#' @param scheduledTime Optional timestamp when this course work is scheduled to be published
#' @param creationTime Timestamp when this course work was created
#' @param creatorUserId Identifier for the user that created the coursework
#' @param individualStudentsOptions Identifiers of students with access to the coursework
#' @param dueDate Optional date, in UTC, that submissions for this course work are due
#' @param submissionModificationMode Setting to determine when students are allowed to modify submissions
#' @param state Status of this course work
#' @param courseId Identifier of the course
#' @param id Classroom-assigned identifier of this course work, unique per course
#' @param dueTime Optional time of day, in UTC, that submissions for this course work
#' @param title Title of this course work
#' @param topicId Identifier for the topic that this coursework is associated with
#'
#' @return CourseWork object
#'
#' @family CourseWork functions
#' @export
gc_course_work <- function(materials = NULL, associatedWithDeveloper = NULL, updateTime = NULL,
    alternateLink = NULL, assigneeMode = NULL, maxPoints = NULL, workType = NULL,
    assignment = NULL, multipleChoiceQuestion = NULL, description = NULL, scheduledTime = NULL,
    creationTime = NULL, creatorUserId = NULL, individualStudentsOptions = NULL,
    dueDate = NULL, submissionModificationMode = NULL, state = NULL, courseId = NULL,
    id = NULL, dueTime = NULL, title = NULL, topicId = NULL) {
    structure(list(materials = materials, associatedWithDeveloper = associatedWithDeveloper,
        updateTime = updateTime, alternateLink = alternateLink, assigneeMode = assigneeMode,
        maxPoints = maxPoints, workType = workType, assignment = assignment, multipleChoiceQuestion = multipleChoiceQuestion,
        description = description, scheduledTime = scheduledTime, creationTime = creationTime,
        creatorUserId = creatorUserId, individualStudentsOptions = individualStudentsOptions,
        dueDate = dueDate, submissionModificationMode = submissionModificationMode,
        state = state, courseId = courseId, id = id, dueTime = dueTime, title = title,
        topicId = topicId), class = c("gar_CourseWork", "list"))
}

#' Guardian Object
#'
#' @details
#'
#' Association between a student and a guardian of that student. The guardianmay receive information about the student's course work.
#'
#' @param guardianId Identifier for the guardian
#' @param invitedEmailAddress The email address to which the initial guardian invitation was sent
#' @param guardianProfile User profile for the guardian
#' @param studentId Identifier for the student to whom the guardian relationship applies
#'
#' @return Guardian object
#'
#' @family Guardian functions
#' @export
gc_guardian <- function(guardianId = NULL, invitedEmailAddress = NULL, guardianProfile = NULL,
    studentId = NULL) {
    structure(list(guardianId = guardianId, invitedEmailAddress = invitedEmailAddress,
        guardianProfile = guardianProfile, studentId = studentId), class = c("gar_Guardian",
        "list"))
}

#' ListStudentsResponse Object
#'
#' @details
#'
#' Response when listing students.
#'
#' @param students Students who match the list request
#' @param nextPageToken Token identifying the next page of results to return
#'
#' @return ListStudentsResponse object
#'
#' @family ListStudentsResponse functions
#' @export
gc_list_students_response <- function(students = NULL, nextPageToken = NULL) {
    structure(list(students = students, nextPageToken = nextPageToken), class = c("gar_ListStudentsResponse",
        "list"))
}

#' UserProfile Object
#'
#' @details
#'
#' Global information for a user.
#'
#' @param emailAddress Email address of the user
#' @param photoUrl URL of user's profile photo
#' @param permissions Global permissions of the user
#' @param name Name of the user
#' @param id Identifier of the user
#' @param verifiedTeacher Represents whether a G Suite for Education user's domain administrator has
#'
#' @return UserProfile object
#'
#' @family UserProfile functions
#' @export
gc_user_profile <- function(emailAddress = NULL, photoUrl = NULL, permissions = NULL,
    name = NULL, id = NULL, verifiedTeacher = NULL) {
    structure(list(emailAddress = emailAddress, photoUrl = photoUrl, permissions = permissions,
        name = name, id = id, verifiedTeacher = verifiedTeacher), class = c("gar_UserProfile",
        "list"))
}

#' Announcement Object
#'
#' @details
#'
#' Announcement created by a teacher for students of the course
#'
#' @param scheduledTime Optional timestamp when this announcement is scheduled to be published
#' @param creationTime Timestamp when this announcement was created
#' @param creatorUserId Identifier for the user that created the announcement
#' @param individualStudentsOptions Identifiers of students with access to the announcement
#' @param state Status of this announcement
#' @param text Description of this announcement
#' @param courseId Identifier of the course
#' @param id Classroom-assigned identifier of this announcement, unique per course
#' @param materials Additional materials
#' @param updateTime Timestamp of the most recent change to this announcement
#' @param assigneeMode Assignee mode of the announcement
#' @param alternateLink Absolute link to this announcement in the Classroom web UI
#'
#' @return Announcement object
#'
#' @family Announcement functions
#' @export
gc_announcement <- function(scheduledTime = NULL, creationTime = NULL, creatorUserId = NULL,
    individualStudentsOptions = NULL, state = NULL, text = NULL, courseId = NULL,
    id = NULL, materials = NULL, updateTime = NULL, assigneeMode = NULL, alternateLink = NULL) {
    structure(list(scheduledTime = scheduledTime, creationTime = creationTime, creatorUserId = creatorUserId,
        individualStudentsOptions = individualStudentsOptions, state = state, text = text,
        courseId = courseId, id = id, materials = materials, updateTime = updateTime,
        assigneeMode = assigneeMode, alternateLink = alternateLink), class = c("gar_Announcement",
        "list"))
}

#' CourseWorkChangesInfo Object
#'
#' @details
#'
#' Information about a `Feed` with a `feed_type` of `COURSE_WORK_CHANGES`.
#'
#' @param courseId The `course_id` of the course to subscribe to work changes for
#'
#' @return CourseWorkChangesInfo object
#'
#' @family CourseWorkChangesInfo functions
#' @export
gc_course_work_changes_info <- function(courseId = NULL) {
    structure(list(courseId = courseId), class = c("gar_CourseWorkChangesInfo", "list"))
}

#' ModifyIndividualStudentsOptions Object
#'
#' @details
#'
#' Contains fields to add or remove students from a course work or announcementwhere the `assigneeMode` is set to `INDIVIDUAL_STUDENTS`.
#'
#' @param addStudentIds IDs of students to be added as having access to this
#' @param removeStudentIds IDs of students to be removed from having access to this
#'
#' @return ModifyIndividualStudentsOptions object
#'
#' @family ModifyIndividualStudentsOptions functions
#' @export
gc_modify_individual_students_options <- function(addStudentIds = NULL, removeStudentIds = NULL) {
    structure(list(addStudentIds = addStudentIds, removeStudentIds = removeStudentIds),
        class = c("gar_ModifyIndividualStudentsOptions", "list"))
}

#' DriveFolder Object
#'
#' @details
#'
#' Representation of a Google Drive folder.
#'
#' @param title Title of the Drive folder
#' @param alternateLink URL that can be used to access the Drive folder
#' @param id Drive API resource ID
#'
#' @return DriveFolder object
#'
#' @family DriveFolder functions
#' @export
gc_drive_folder <- function(title = NULL, alternateLink = NULL, id = NULL) {
    structure(list(title = title, alternateLink = alternateLink, id = id), class = c("gar_DriveFolder",
        "list"))
}

#' SubmissionHistory Object
#'
#' @details
#'
#' The history of the submission. This currently includes state and gradehistories.
#'
#' @param stateHistory The state history information of the submission, if present
#' @param gradeHistory The grade history information of the submission, if present
#'
#' @return SubmissionHistory object
#'
#' @family SubmissionHistory functions
#' @export
gc_submission_history <- function(stateHistory = NULL, gradeHistory = NULL) {
    structure(list(stateHistory = stateHistory, gradeHistory = gradeHistory), class = c("gar_SubmissionHistory",
        "list"))
}

#' ShortAnswerSubmission Object
#'
#' @details
#'
#' Student work for a short answer question.
#'
#' @param answer Student response to a short-answer question
#'
#' @return ShortAnswerSubmission object
#'
#' @family ShortAnswerSubmission functions
#' @export
gc_short_answer_submission <- function(answer = NULL) {
    structure(list(answer = answer), class = c("gar_ShortAnswerSubmission", "list"))
}

#' StudentSubmission Object
#'
#' @details
#'
#' Student submission for course work.StudentSubmission items are generated when a CourseWork item is created.StudentSubmissions that have never been accessed (i.e. with `state` = NEW)may not have a creation time or update time.
#'
#' @param multipleChoiceSubmission Submission content when course_work_type is MULTIPLE_CHOICE_QUESTION
#' @param assignmentSubmission Submission content when course_work_type is ASSIGNMENT
#' @param associatedWithDeveloper Whether this student submission is associated with the Developer Console
#' @param shortAnswerSubmission Submission content when course_work_type is SHORT_ANSWER_QUESTION
#' @param updateTime Last update time of this submission
#' @param alternateLink Absolute link to the submission in the Classroom web UI
#' @param draftGrade Optional pending grade
#' @param late Whether this submission is late
#' @param courseWorkType Type of course work this submission is for
#' @param creationTime Creation time of this submission
#' @param state State of this submission
#' @param userId Identifier for the student that owns this submission
#' @param courseWorkId Identifier for the course work this corresponds to
#' @param courseId Identifier of the course
#' @param id Classroom-assigned Identifier for the student submission
#' @param submissionHistory The history of the submission (includes state and grade histories)
#' @param assignedGrade Optional grade
#'
#' @return StudentSubmission object
#'
#' @family StudentSubmission functions
#' @export
gc_student_submission <- function(multipleChoiceSubmission = NULL, assignmentSubmission = NULL,
    associatedWithDeveloper = NULL, shortAnswerSubmission = NULL, updateTime = NULL,
    alternateLink = NULL, draftGrade = NULL, late = NULL, courseWorkType = NULL,
    creationTime = NULL, state = NULL, userId = NULL, courseWorkId = NULL, courseId = NULL,
    id = NULL, submissionHistory = NULL, assignedGrade = NULL) {
    structure(list(multipleChoiceSubmission = multipleChoiceSubmission, assignmentSubmission = assignmentSubmission,
        associatedWithDeveloper = associatedWithDeveloper, shortAnswerSubmission = shortAnswerSubmission,
        updateTime = updateTime, alternateLink = alternateLink, draftGrade = draftGrade,
        late = late, courseWorkType = courseWorkType, creationTime = creationTime,
        state = state, userId = userId, courseWorkId = courseWorkId, courseId = courseId,
        id = id, submissionHistory = submissionHistory, assignedGrade = assignedGrade),
        class = c("gar_StudentSubmission", "list"))
}

#' ListStudentSubmissionsResponse Object
#'
#' @details
#'
#' Response when listing student submissions.
#'
#' @param studentSubmissions Student work that matches the request
#' @param nextPageToken Token identifying the next page of results to return
#'
#' @return ListStudentSubmissionsResponse object
#'
#' @family ListStudentSubmissionsResponse functions
#' @export
gc_list_student_submissions_response <- function(studentSubmissions = NULL, nextPageToken = NULL) {
    structure(list(studentSubmissions = studentSubmissions, nextPageToken = nextPageToken),
        class = c("gar_ListStudentSubmissionsResponse", "list"))
}

#' ModifyAttachmentsRequest Object
#'
#' @details
#'
#' Request to modify the attachments of a student submission.
#'
#' @param addAttachments Attachments to add
#'
#' @return ModifyAttachmentsRequest object
#'
#' @family ModifyAttachmentsRequest functions
#' @export
gc_modify_attachments_request <- function(addAttachments = NULL) {
    structure(list(addAttachments = addAttachments), class = c("gar_ModifyAttachmentsRequest",
        "list"))
}

#' YouTubeVideo Object
#'
#' @details
#'
#' YouTube video item.
#'
#' @param id YouTube API resource ID
#' @param title Title of the YouTube video
#' @param alternateLink URL that can be used to view the YouTube video
#' @param thumbnailUrl URL of a thumbnail image of the YouTube video
#'
#' @return YouTubeVideo object
#'
#' @family YouTubeVideo functions
#' @export
gc_youtube_video <- function(id = NULL, title = NULL, alternateLink = NULL, thumbnailUrl = NULL) {
    structure(list(id = id, title = title, alternateLink = alternateLink, thumbnailUrl = thumbnailUrl),
        class = c("gar_YouTubeVideo", "list"))
}

#' ListInvitationsResponse Object
#'
#' @details
#'
#' Response when listing invitations.
#'
#' @param nextPageToken Token identifying the next page of results to return
#' @param invitations Invitations that match the list request
#'
#' @return ListInvitationsResponse object
#'
#' @family ListInvitationsResponse functions
#' @export
gc_list_invitations_response <- function(nextPageToken = NULL, invitations = NULL) {
    structure(list(nextPageToken = nextPageToken, invitations = invitations), class = c("gar_ListInvitationsResponse",
        "list"))
}

#' CloudPubsubTopic Object
#'
#' @details
#'
#' A reference to a Cloud Pub/Sub topic.To register for notifications, the owner of the topic must grant`classroom-notifications@system.gserviceaccount.com` the `projects.topics.publish` permission.
#'
#' @param topicName The `name` field of a Cloud Pub/Sub
#'
#' @return CloudPubsubTopic object
#'
#' @family CloudPubsubTopic functions
#' @export
gc_cloud_pubsub_topic <- function(topicName = NULL) {
    structure(list(topicName = topicName), class = c("gar_CloudPubsubTopic", "list"))
}

#' GuardianInvitation Object
#'
#' @details
#'
#' An invitation to become the guardian of a specified user, sent to a specifiedemail address.
#'
#' @param studentId ID of the student (in standard format)
#' @param state The state that this invitation is in
#' @param invitedEmailAddress Email address that the invitation was sent to
#' @param creationTime The time that this invitation was created
#' @param invitationId Unique identifier for this invitation
#'
#' @return GuardianInvitation object
#'
#' @family GuardianInvitation functions
#' @export
gc_guardian_invitation <- function(studentId = NULL, state = NULL, invitedEmailAddress = NULL,
    creationTime = NULL, invitationId = NULL) {
    structure(list(studentId = studentId, state = state, invitedEmailAddress = invitedEmailAddress,
        creationTime = creationTime, invitationId = invitationId), class = c("gar_GuardianInvitation",
        "list"))
}

#' StateHistory Object
#'
#' @details
#'
#' The history of each state this submission has been in.
#'
#' @param state The workflow pipeline stage
#' @param stateTimestamp When the submission entered this state
#' @param actorUserId The teacher or student who made the change
#'
#' @return StateHistory object
#'
#' @family StateHistory functions
#' @export
gc_state_history <- function(state = NULL, stateTimestamp = NULL, actorUserId = NULL) {
    structure(list(state = state, stateTimestamp = stateTimestamp, actorUserId = actorUserId),
        class = c("gar_StateHistory", "list"))
}


#' CourseMaterialSet Object
#'
#' @details
#'
#' A set of materials that appears on the 'About' page of the course.These materials might include a syllabus, schedule, or other backgroundinformation relating to the course as a whole.
#'
#' @param title Title for this set
#' @param materials Materials attached to this set
#'
#' @return CourseMaterialSet object
#'
#' @family CourseMaterialSet functions
#' @export
gc_course_material_set <- function(title = NULL, materials = NULL) {
    structure(list(title = title, materials = materials), class = c("gar_CourseMaterialSet",
        "list"))
}

