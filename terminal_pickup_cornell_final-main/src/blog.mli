open Profile
open Str

(** A blog is a list of posts that can be added, slightly modified, and
    accessed through commands provided by the blog application. *)

type time = {
  day : string;
  month : string;
  year : string;
  hour : string;
  minute : string;
}
(**[time] is the type representing the time and date. Hour and minute
   are in miliary time. *)

type post = {
  author : string;
  sport : string;
  location : string;
  time : time;
  mutable likes : int;
  mutable likes_list : profile list;
  id : int;
  signups : int;
  max_signups : int;
  signup_list : profile list;
  comments : int;
  comment_list : (profile * string) list;
}
(** [post] is the type representing an individual blog post and all
    associated attributes. *)

exception Invalid_id
(** exception [Invalid_id] is raised when an invalid post ID is entered. *)

exception Invalid_time
(** exception [Invalid_time] is raised when an invalid time is entered. *)

exception Invalid_cap
(** exception [Invalid_cap] is raised when an invalid number of players
    capacity is used. *)

exception Invalid_sport
(** exception [Invalid_sport] is raised when an invalid sport name is
    entered. *)

exception Invalid_location
(** exception [Invalid_location] is raised when an invalid location for
    a given sport is entered. *)

exception Invalid_date
(** exception [Invalid] is raised when an invalid date is entered. *)

exception Invalid_comment_number
(** exception [Invalid] is raised when an invalid comment number is
    entered to be shown. *)

exception No_comments
(** exception [Invalid] is raised when an there are no comments to be
    shown. *)

exception Post_Does_Not_Exist
(** exception [Post_Does_Not_Exist] is raised when the post of the given
    id does not exist. *)

exception Single_or_all
(** exception [Single_or_all] is raised when the user inputs a command
    other than 'single' or 'all' when asked how they want to see the
    comments of a post. *)

exception Empty_comment
(** exception [Empty_comment] is raised when the user does not comment
    anything which would otherwise result in an empty comment. *)

type blog = post list
(** [blog] is a type representing a blog, which is simply a list of
    posts. *)

val get_likes_list : post -> profile list
(** [get_likes_list] returns the list of likes on a post.*)

val get_inputs : 'a -> string list
(** [get_inputs] gets user inputs and populates them into the proper
    fields of a post. *)

val get_location : string -> string
(** [get_location] gets the user inputted location. If location is
    invalid, then Invalid_location is raised.*)

val get_last_id : blog -> int
(** [get_last_id] returns the id of the last post in a blog. *)

val create_blog : string list -> profile -> blog
(** [create_blog] creates a blog with a single post from a list of user
    inputted attributes and a profile . *)

val create_post : string list -> blog -> profile -> post
(** [create_post] creates a post by populating the record attributes
    with a list of user inputted attributes and a profile, then assigns
    the id by getting the last id of the inputted blog.*)

val see_blog : blog -> unit
(** [see_blog] prints a formatted version of the entire blog.*)

val ask_id : blog -> int
(** [ask_id] asks the user to enter a post ID and reads in the input.
    Raises Invald_id if the id does not exist or if an integer is not
    entered.*)

val ask_comment : 'a -> string
(** [ask_comment] asks the user to enter a comment and reads in the
    input.*)

val ask_comment_number : post -> int
(** [ask_comment_number] asks the user to enter a comment number and
    reads in the input. Raises Invalid_comment_number if the given
    comment number does not exist.*)

val get_comment : post -> int -> profile * string
(** [get_comment] gets the comment corresponding to the given comment
    number for the given post. Raises Invalid_comment_number if the
    given comment number does not exist.*)

val display_comment : int -> profile * string -> unit
(** [display_comment] prints formatted inforamtion about a user and a
    comment. *)

val print : blog -> unit
(** [print] prints an unformatted version of an entire blog.*)

val print_formatted : post -> unit
(** [print_formatted] prints a formatted version of a singular post.*)

val get_post_from_id : blog -> int -> post
(** [get_post_from_id] returns the post from the blog of the given post
    ID. Raises Post_Does_Not_Exist if the a post of that ID does not
    exist in the blog.*)

val increase_likes : post -> unit
(** [increase_likes] increments the number of likes on a post by one.*)

val check_id_exist : int -> blog -> bool
(** [check_id_exist] checks whether the given id is present in the blog
    .*)

val get_post : int -> blog -> post
(** [get_post] gets the post of the post of the given id in the blog.
    Returns Invalid_id if the id is not present in the blog.*)

val get_author : int -> blog -> string
(** [get_author] gets the author of the post with the given id in the
    blog.*)

val get_likes : int -> blog -> int
(** [get_likes] gets the number of likes of the post with the given id
    in the blog. *)

val reached_max : int -> blog -> bool
(** [reached_max] returns true if the maximum number of signups has been
    reached and false otherwise.*)

val is_valid_post : int -> blog -> bool
(** [is_valid_post] returns true if the a post of that post ID exists in
    the blog.*)

val get_signups : int -> blog -> int
(** [get_signups] gets the signups of a post in the blog corresponding
    to the given ID.*)

val get_max_signups : int -> blog -> int
(** [get_max_signups] gets the maximum number of signups on a post in
    the blog corresponding to the given ID.*)

val get_signup_list : int -> blog -> profile list
(** [get_signup_list] gets the signup list of a post in the blog
    corresponding to the given ID.*)

val get_comments : int -> blog -> (profile * string) list
(** [get_comments] gets the comments on a post in the blog corresponding
    to the given ID..*)

val get_number_of_comments : int -> blog -> int
(** [get_number_of_comments] gets the number of comments on a post in
    the blog corresponding to the given ID. *)

val ask_single_or_all : int -> blog -> string
(** [ask_single_or_all post_id blog] asks the user whether they want to
    query for a single comment or display all comments on the post in
    [blog] with id [post_id]. *)

val display_all_comments : post -> unit
(** [display_all_comments post] prints formatted information about all
    users and their comments on [post]. *)
