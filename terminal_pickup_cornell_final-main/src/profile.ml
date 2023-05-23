type profile = {
  username : string;
  password : string;
  user_posts : int list;
  mutable followers : string list;
  mutable following : string list;
}

let get_user profile = profile.username
let get_followers profile = profile.followers
let get_following profile = profile.following

let create_profile tuple : profile =
  match tuple with
  | u, p ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nProfile created!\n\n";
      {
        username = u;
        password = p;
        followers = [];
        following = [];
        user_posts = [];
      }

let create_first_profile tuple : profile =
  match tuple with
  | u, p ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nProfile created!\n\n";
      {
        username = u;
        password = p;
        followers = [];
        following = [];
        user_posts = [ 0 ];
      }

let rec get_password () =
  print_endline "\nPassword: ";
  print_string "> ";
  match read_line () with
  | (s : string) ->
      if String.length s < 5 then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Password must contain at least 6 characters and a special \
           character\n";
        get_password ())
      else if
        not
          (String.contains s '!' || String.contains s '@'
         || String.contains s '#' || String.contains s '$'
         || String.contains s '%' || String.contains s '^'
         || String.contains s '&' || String.contains s '*'
         || String.contains s '!')
      then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Password must contain at least 6 characters and a special \
           character\n";
        get_password ())
      else s

let rec get_username usernames =
  print_endline "\nUsername: ";
  print_string "> ";
  match read_line () with
  | (a : string) ->
      if List.mem a usernames then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Username already being used";
        get_username usernames)
      else a

let get_profile_inputs st =
  let username = get_username st in
  let rec password = get_password () in
  (username, password)

let get_first_profile_inputs () =
  let username =
    print_endline "\nUsername: ";
    print_string "> ";
    match read_line () with
    | (a : string) -> a
  in
  let rec password = get_password () in
  (username, password)

let follower_count profile = List.length profile.followers
let following_count profile = List.length profile.following

let print_profile profile =
  print_endline
    (profile.username ^ " Followers: "
    ^ string_of_int (follower_count profile)
    ^ " Following: "
    ^ string_of_int (following_count profile))
