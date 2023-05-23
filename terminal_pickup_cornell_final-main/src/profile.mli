(** A profile tracks information about users of the blog application,
    including their login details, their posts, their list of followers,
    the list of users that they follow profile. *)

type profile = {
  username : string;
  password : string;
  user_posts : int list;
  mutable followers : string list;
  mutable following : string list;
}
(** [profile] is the type which represents a user of the blog. *)

val get_user : profile -> string
(** [get_user profile] is the username under [profile]. *)

val get_followers : profile -> string list
(** [get_followers profile] is the list of users who follow the user
    with [profile]. *)

val get_profile_inputs : string list -> string * string
(** [get_profile_inputs st] is a tuple with a username that is not
    already in [st] and a password of an individual user. *)

val get_following : profile -> string list
(** [get_following profile] is the list of users that the user with
    [profile] is following. *)

val get_first_profile_inputs : unit -> string * string
(** [get_first_profile_inputs] gets the username and password
    information from the first user. *)

val create_profile : string * string -> profile
(** [create_profile tuple] creates a profile with inputted username and
    password given by [tuple]. *)

val create_first_profile : string * string -> profile
(** [create_first_profile tuple] creates a profile with inputted
    username and password given by [tuple], with user_posts attribute
    being set to 0.*)

val follower_count : profile -> int
(** [follower_count profile] is the number of followers of the user with
    [profile]. *)

val following_count : profile -> int
(** [following_count profile] is the number of users that the user with
    [profile] follows. *)

val print_profile : profile -> unit
(** [print_profile] prints formatted profile information. *)
