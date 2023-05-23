open Blog
open Profile

(** A state is a representation of the current values of fields in the
    blog application. The state keeps track of the current user, the
    current blog, a list of profiles in the blog, and an exhaustive list
    of sign-ups in the blog. The various components of a state can be
    modified and accessed by numerous functions. *)

exception Invalid_id
(** exception [Invalid_id] is raised when an invalid post ID is entered. *)

exception Invalid_login
(** exception [Invalid_login] is raised when invalid login information
    is entered. *)

exception Invalid_profile
(** exception [Invalid_profile] is raised when an invalid profile is
    entered. *)

exception Already_following
(** exception [Already_following] is raised when a user attempts to
    follow a profile that they have already followed. *)

exception Self_signup
(** exception [Self_signup] is raised when a user attempts to sign up
    for their own post.*)

exception Reached_max
(** exception [Reached_max] is raised when a post in the blog has
    already reached its maximum number of signups and another signup is
    attempted to be added.*)

exception Already_signed_up
(** exception [Already_signed_up] is raised when a user attempts to sign
    up for a post that they have already signed up for.*)

exception Not_allowed
(** exception [Not_allowed] is raised when a a user attempts to delete a
    post of another user. *)

exception Cannot_Follow_Yourself
(** exception [Cannot_Follow_yourself] is raised when a user attempts to
    follow themselves. *)

exception Already_Liked
(** exception [Already_Liked] is raised when a user attempts to like a
    post that they have already liked. *)

exception Not_Following
(**exception [Not_Following] is raised when a user attempts to unfollow
   someone they don't alraedy follow. *)

exception Not_liked
(** exception [Not_liked] is raised when a user attempts to unlike
    someone they don't already follow. *)

type t = {
  current_user : profile;
  blog : blog;
  profiles : profile list;
  sign_ups : (profile * post) list;
}
(** Type [t] representing the state of the blog.*)

val init_state : Profile.profile -> Blog.blog -> t
(** [init_state] initializes the state of the blog given a current user
    profile and an existing blog.*)

val get_blog : t -> blog
(** [get_blog] gets the blog of the current state.*)

val get_usernames : t -> string list
(** [get_usernames] gets a list of the usernames in the current state *)

val add_post : post -> t -> t
(** [add_post] adds a blog post to the current state*)

val delete_post : int -> t -> t
(** [delete_post] deletes a post referenced by a post ID number from the
    state. Raises Not_allowed if the post is not owned by the current
    user. *)

val add_profile : t -> profile -> t
(** [add_profile] adds a profile to the current state. *)

val like_post : (t -> bool -> 'a) -> int -> t -> 'a
(** [like_post] likes a post referenced by a post ID number in the
    state. Raises Already_liked if the current user has already liked
    the post of interest. *)

val already_signed_up : int -> t -> bool
(** [already_signed_up] returns true if the current user has already
    signed up for a slot in a post and false otherwise.*)

val sign_up : int -> t -> t
(** [sign_up] allows the user to sign up for a post referenced by the
    post ID number. Raises [Reached_max] if the post has already reached
    its capacity. Raises [Already_signed_up] if the currrent user has
    already signed up for that post. Raises [Invalid_id] if the given
    post ID number does not exist in the current state of the blog. *)

val sort_by_sport : t -> t
(** [sort_by_sport] sorts the current blog in alphabetical order by
    sport name.*)

val sort_by_author : t -> t
(** [sort_by_author] sorts the current blog in alphabetical order by
    user name.*)

val sort_by_location : t -> t
(** [sort_by_location] sorts the current blog in alphabetical order by
    location name.*)

val get_current_user : t -> profile
(** [get_current_user] gets the current user *)

val sort_by_time : t -> t
(** [sort_by_time] sorts the current blog in increasing time order.*)

val change_user : t -> profile -> t
(** [change_user] changes the current user of the state to the given
    profile of interest. *)

val profiles : t -> profile list
(** [profiles state] is a list of profiles currently registered in
    [state]. *)

val log_in : t -> string -> string -> t
(** [log_in] allows a different user to log-in to the blog by changing
    the current user in the state. *)

val follow : (t -> bool -> 'a) -> string list -> t -> 'a
(** [follow] allows the current user to follow other users. Raises
    [Cannot_follow_yourself] if the current user attempts to follow
    their own profile. Raises [Invalid_profile] if the profile that is
    attempted to be followed is not valid. Raises [Already_following] if
    the current user tries to follow a profile that they have already
    followed. *)

val get_profile_from_user : t -> string -> profile
(** [get_profile_from_user] gets a profile of interest from the state
    given a username. Raises Invalid_profile if the profile of interest
    does not exist. *)

val invalid_input_for_like :
  ('a -> bool -> unit) -> 'a -> string -> unit
(** [invalid_input_for_like] checks whether the input for like is valid. *)

val show : profile -> ('a -> bool -> 'b) -> 'a -> 'b
(** [show] prints the number of followers and number of following counts
    for a given profile. *)

val get_profile : t -> profile
(** [get_profile] gets the current user profile *)

val comment : string -> int -> t -> t
(** [comment] adds the given comment to a post referenced by the given
    post ID number in the state. Raises [Invalid_id] if the post ID
    number does not exist. *)

val string_to_int : ('a -> bool -> int) -> string -> 'a -> int
(** [string_to_int] converts the user inputted ID string to an integer.
    If an invalid ID is entered, the user is prompted to enter a valid
    ID.*)

val already_following : profile -> string -> bool
(** [already_following] checks to see if [profile] is in the list of
    followers given by a profile whose username is the string. Returns
    false if not following otherwise true. *)

val unfollow : (t -> bool -> 'a) -> string -> t -> 'a
(** [unfollow edit_blog followee st] removes the current user as a
    follow of [followee].[st] and true are passed as arguments to
    [edit_blog] function which continues the blog with the updated
    state. *)

val unlike_post : (t -> bool -> 'a) -> int -> t -> 'a
(** [unlike edit_blog post_id st] removes the current user's like on the
    post which has id [post_id]. [st] and true are passed as arguments
    to [edit_blog] function which continues the blog with the updated
    state. *)

val show_liked_list : t -> (t -> bool -> 'b) -> int -> 'b
(**[show_liked_list] shows the usernames of of the likes on a post with
   state [t]. It uses [edit_blog] to contnue with the blog with the
   updated state and the [id] of the post*)
