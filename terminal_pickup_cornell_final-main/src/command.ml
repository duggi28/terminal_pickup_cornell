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

exception Malformed

let rec remove_blanks = function
  | [] -> raise Malformed
  | [ h ] -> if h = "" then [] else [ h ]
  | h :: t -> if h = "" then remove_blanks t else h :: remove_blanks t

let parse_verb = function
  | [] -> raise Malformed
  | h :: t ->
      if (h = "Add" || h = "add") && t = [] then Add
      else if (h = "Delete" || h = "delete") && t = [] then Delete
      else if (h = "Like" || h = "like") && t = [] then Like
      else if (h = "Signup" || h = "signup") && t = [] then Signup
      else if (h = "Quit" || h = "quit") && t = [] then Quit
      else if (h = "See" || h = "see") && t = [] then See
      else if (h = "Sort" || h = "sort") && t = [] then Sort
      else if h = "Logout" || h = "logout" then Logout
      else if (h = "Search" || h = "search") && t = [] then Search
      else if (h = "Follow" || h = "follow") && t = [] then Follow
      else if (h = "Show" || h = "show") && t = [] then Show
      else if (h = "Comment" || h = "comment") && t = [] then Comment
      else if (h = "Comments" || h = "comments") && t = [] then Comments
      else if (h = "Unfollow" || h = "unfollow") && t = [] then Unfollow
      else if (h = "Unlike" || h = "unlike") && t = [] then Unlike
      else if (h = "likes" || h = "Likes") && t = [] then Show_likes
      else raise Malformed

let parse str =
  String.split_on_char ' ' str |> remove_blanks |> parse_verb
