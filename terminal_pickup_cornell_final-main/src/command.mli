(** A command is any of a number of possible inputs that allow the user
    to perform a particular action in the blog application. *)

type command =
  | Add
  | Delete
  | Like
  | Signup
  | Quit
  | Logout
  | See
  | Follow
  | Show
  | Sort
  | Search
  | Comment
  | Comments
  | Unfollow
  | Unlike
  | Show_likes
      (** type [command] represents all possible commands that the user
          may enter in order to interact with the blog. *)

exception Malformed
(** exception [Malformed] is raised when an empty or incorrect command
    is parsed. *)

val parse : string -> command
(** [parse] parses the user input into a command. Raises [Malformed] if
    invalid command is entered. *)
