open Profile
open Str

type time = {
  day : string;
  month : string;
  year : string;
  hour : string;
  minute : string;
}

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

type blog = post list

exception Invalid_id
exception Invalid_time
exception Invalid_cap
exception Invalid_sport
exception Invalid_location
exception Invalid_date
exception Invalid_comment_number
exception No_comments
exception Post_Does_Not_Exist
exception Single_or_all
exception Empty_comment

let rec is_valid_post id blog =
  match blog with
  | [] -> false
  | h :: t -> if h.id = id then true else is_valid_post id t

let get_likes_list blog = blog.likes_list
let increase_likes post = post.likes <- post.likes + 1

let rec get_post_from_id blog_list id =
  match blog_list with
  | [] -> raise Post_Does_Not_Exist
  | h :: t -> if h.id = id then h else get_post_from_id t id

let get_last_id (blog : blog) =
  if List.length blog = 0 then 0 else List.hd blog |> fun x -> x.id

let ask_comment init =
  print_endline "Please enter comment: ";
  print_string "> ";
  let l = String.trim (read_line ()) in
  if l = "" then raise Empty_comment else l

let ask_comment_number desired_post =
  if desired_post.comments > 0 then (
    print_endline
      ("\nPost has "
      ^ string_of_int desired_post.comments
      ^ " comments. Enter a comment number: ");
    print_string "> ";
    match read_line () with
    | (a : string) -> (
        try int_of_string a with Failure _ -> raise Invalid_id))
  else raise No_comments

let get_comment (desired_post : post) (comment_id : int) :
    profile * string =
  try List.nth (List.rev desired_post.comment_list) comment_id
  with Failure _ | Invalid_argument _ -> raise Invalid_comment_number

let display_comment
    (index : int)
    ((desired_profile, desired_comment) : profile * string) : unit =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("\n("
    ^ string_of_int (index + 1)
    ^ ") USER: "
    ^ get_user desired_profile
    ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("    COMMENT: " ^ desired_comment ^ "\n")

let get_sport () =
  let sport =
    match read_line () with
    | (s : string) -> s
  in
  match sport with
  | "Badminton" | "badminton" -> "Badminton"
  | "Baseball" | "baseball" -> "Baseball"
  | "Basketball" | "basketball" -> "Basketball"
  | "Bowling" | "bowling" -> "Bowling"
  | "Field Hockey" | "Field hockey" | "field hockey" | "field Hockey" ->
      "Field Hockey"
  | "Football" | "football" -> "Football"
  | "Golf" | "golf" -> "Golf"
  | "Hockey" | "hockey" -> "Hockey"
  | "Lacrosse" | "lacrosse" -> "Lacrosse"
  | "Rock Climbing" | "Rock climbing" | "rock Climbing"
  | "rock climbing" ->
      "Rock Climbing"
  | "Soccer" | "soccer" -> "Soccer"
  | "Softball" | "softball" -> "Softball"
  | "Squash" | "squash" -> "Squash"
  | "Swimming" | "swimming" -> "Swimming"
  | "Tennis" | "tennis" -> "Tennis"
  | "Volleyball" | "volleyball" -> "Volleyball"
  | _ -> raise Invalid_sport

let rec get_check_sport () =
  try get_sport ()
  with Invalid_sport ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Please enter a valid sport. Valid sports include: Badminton, \
       Baseball, Basketball, Bowling, Field Hockey, Football,\n";
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Golf, Hockey, Lacrosse, Rock Climbing, Soccer, Softball, \
       Squash, Swimming, Tennis, Volleyball \n";
    print_string ">";
    get_check_sport ()

let get_location sport =
  match sport with
  | "Badminton" ->
      print_endline
        "Possible locations for badminton include: Helen Newman Hall, \
         Noyes Community Recreation Center, Barton Hall";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "helen newman hall" | "helen newman" -> "Helen Newman Hall"
        | "noyes community recreation center" | "noyes"
        | "noyes rec center" ->
            "Noyes Community Recreation Center"
        | "barton hall" | "barton" -> "Barton Hall"
        | _ -> raise Invalid_location
      in
      location
  | "Baseball" ->
      print_endline
        "Possible locations for baseball include: Hoy Field, Ramin \
         Multipurpose Room";
      print_string "> ";

      let location =
        match String.lowercase_ascii (read_line ()) with
        | "hoy field" | "hoy" -> " Hoy Field"
        | "ramin multipurpose room" | "ramin" | "ramin room" ->
            "Ramin Multipurpose Room"
        | _ -> raise Invalid_location
      in
      location
  | "Basketball" ->
      print_endline
        "Possible locations for basketball include: Newman Arena, \
         Helen Newman Hall, West Campus Basketball Courts, North \
         Campus Basketball Courts, Noyes Community Recreation Center, \
         Barton Hall";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "newman arena" | "newman" -> "Newman Arena"
        | "helen newman hall" | "helen newman" -> "Helen Newman Hall"
        | " west campus basketball courts" | "west" | "west campus"
        | "west courts" ->
            "West Campus Basketball Courts"
        | "north campus basketball courts" | "north" | "north campus"
        | "north courts" ->
            "North Campus Basketball Courts"
        | "noyes community recreation center" | "noyes" ->
            " Noyes Community Recreation Center"
        | "barton hall" | "barton" -> "Barton Hall"
        | _ -> raise Invalid_location
      in
      location
  | "Bowling" ->
      print_endline
        "Possible locations for bowling include: Helen Newman Hall";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "helen newman hall" | "helen newman" -> "Helen Newman Hall"
        | _ -> raise Invalid_location
      in
      location
  | "Field Hockey" ->
      print_endline
        "Possible locations for field hockey include: Dodson Field";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "dodson field" | "dodson" | "dodson fields" -> "Dodson Field"
        | _ -> raise Invalid_location
      in
      location
  | "Football" ->
      print_endline
        "Possible locations for football include: Jessup Field, \
         Schoellkopf Field, Ramin Multipurpose Room, Robinson Alumni \
         Fields";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "jessup field" | "jessup fields" | "jessup" -> "Jessup Field"
        | "schoellkopf field" | "schoellkopf fields" | "schoellkopf" ->
            "Schoellkopf Field"
        | "ramin multipurpose room" | "ramin" | "ramin room" ->
            "Ramin Multipurpose Room"
        | "robinson alumni fields" | "robinson" | "alumni fields"
        | "robinson field" | "robinson alumni field" ->
            "Robinson Alumni Fields"
        | _ -> raise Invalid_location
      in
      location
  | "Golf" ->
      print_endline
        "Possible locations for golf include: Robert Trent Jones Golf \
         Course";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "robert trent jones golf course" | "robert trent"
        | "robert trent jones" | "golf course" ->
            "Robert Trent Jones Golf Course"
        | _ -> raise Invalid_location
      in
      location
  | "Hockey" ->
      print_endline "Possible locations for hockey include: Lynah Rink";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "lynah rink" -> "Lynah Rink"
        | _ -> raise Invalid_location
      in
      location
  | "Lacrosse" ->
      print_endline
        "Possible locations for lacrosse include: McGovern Fields, \
         Jessup Field, Schoellkopf Field, Ramin Multipurpose Room, \
         Robinson Alumni Fields";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "mcgovern fields" | "mcgovern field" | "mcgovern" ->
            "McGovern Fields"
        | "jessup field" | "jessup fields" | "jessup" -> "Jessup Field"
        | "schoellkopf field" | "schoellkopf fields" | "schoellkopf" ->
            "Schoellkopf Field"
        | "ramin multipurpose room" | "ramin" ->
            "Ramin Multipurpose Room"
        | "robinson alumni fields" | "robinson" | "alumni fields"
        | "robinson field" | "robinson alumni field" ->
            "Robinson Alumni Fields"
        | _ -> raise Invalid_location
      in
      location
  | "Rock Climbing" ->
      print_endline
        "Possible locations for rock climbing include: Lindseth \
         Climbing Center, Noyes Community Recreation Center";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "lindseth climbing center" | "climbing center" | "lindseth" ->
            "Lindseth Climbing Center"
        | "noyes community recreation center" | "noyes" ->
            "Noyes Community Recreation Center"
        | _ -> raise Invalid_location
      in
      location
  | "Soccer" ->
      print_endline
        "Possible locations for soccer include: McGovern Fields, \
         Jessup Field, Kane Sports Complex, Berman Field, Ramin \
         Multipurpose Room, Robinson Alumni Fields";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "mcgovern fields" | "mcgovern field" | "mcgovern" ->
            "McGovern Fields"
        | "jessup field" | "jessup fields" | "jessup" -> "Jessup Field"
        | "kane sports complex" | "kane" | "kane sport complex" ->
            "Kane Sports Complex"
        | "berman field" | "berman fields" | "berman" -> "Berman Field"
        | "ramin multipurpose room" | "ramin" ->
            "Ramin Multipurpose Room"
        | "robinson alumni fields" | "robinson" | "alumni fields"
        | "robinson field" | "robinson alumni field" ->
            "Robinson Alumni Fields"
        | _ -> raise Invalid_location
      in
      location
  | "Softball" ->
      print_endline
        "Possible locations for softball include: Niemand-Robison \
         Softball Field, Ramin Multipurpose Room";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "niemand-robison softball field" | "softball field"
        | "niemand" | "niemand-robinson"
        | "niemand robinson softball field"
        | "niemand-robison softball fields" ->
            "Niemand-Robison Softball Field"
        | "ramin multipurpose room" | "ramin" | "ramin room" ->
            "Ramin Multipurpose Room"
        | _ -> raise Invalid_location
      in
      location
  | "Squash" ->
      print_endline
        "Possible locations for squash include: Belkin Squash Courts";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "belkin squash courts" | "belkin squash court" | "belkin" ->
            "Belkin Squash Courts"
        | _ -> raise Invalid_location
      in
      location
  | "Swimming" ->
      print_endline
        "Possible locations for swimming include: Helen Newman Hall, \
         Teagle Pool";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "helen newman hall" | "helen newman" -> "Helen Newman Hall"
        | "teagle pool" | "teagle" -> "Teagle Pool"
        | _ -> raise Invalid_location
      in
      location
  | "Tennis" ->
      print_endline
        "Possible locations for tennis include: Reis Tennis Center, \
         Risley Tennis Courts";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "reis tennis center" | "reis" -> "Reis Tennis Center"
        | "risley tennis courts" | "risley" -> "Risley Tennis Courts"
        | _ -> raise Invalid_location
      in
      location
  | "Volleyball" ->
      print_endline
        "Possible locations for volleyball include: Helen Newman Hall, \
         North Sand Courts, Noyes Community Recreation Center, Barton \
         Hall";
      print_string "> ";
      let location =
        match String.lowercase_ascii (read_line ()) with
        | "helen newman hall" | "helen newman" -> "Helen Newman Hall"
        | "north sand courts" | "sand court" | "sand courts"
        | "north courts" | "north sand" ->
            "North Sand Courts"
        | "noyes community recreation center" | "noyes" ->
            "Noyes Community Recreation Center"
        | "barton hall" | "barton" | "Barton Hall" -> "Barton Hall"
        | _ -> raise Invalid_location
      in
      location
  | _ -> raise Invalid_sport

let rec get_check_location sport =
  try get_location sport with
  | Invalid_location ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Please enter a valid location\n";
      get_check_location sport
  | Invalid_sport ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Please enter a valid location\n";
      get_check_location sport

let check_time t =
  let hr = "\\([0-1][0-9]\\|2[0-3]\\):\\([0-5][0-9]\\)" in
  Str.string_match (Str.regexp hr) t 0

let get_time () =
  let t =
    match read_line () with
    | (s : string) -> s
  in
  match check_time t with
  | true -> t
  | false -> raise Invalid_time

let rec get_check_time () =
  try get_time ()
  with Invalid_time ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Please enter a valid time\n";
    print_string ">";
    get_check_time ()

let check_valid_int str =
  try
    let _ = int_of_string str in
    true
  with Failure _ -> false

let is_valid_date (d : string) =
  if String.length d != 10 then false
  else
    let month = String.sub d 0 2 |> check_valid_int in
    let slashe1 = String.get d 2 = '/' in
    let slashe2 = String.get d 5 = '/' in
    let day = String.sub d 3 2 |> check_valid_int in
    let year = String.sub d 6 4 |> check_valid_int in
    month && day && year && slashe1 && slashe2

let check_date d =
  if is_valid_date d = false then false
  else
    let dr = "\\(0[1-9]\\|[12][0-9]\\|3[0-1]\\)" in
    let mr = "\\([0][1-9]\\|1[0-2]\\)" in
    let yr = "\\(2[0-9][0-9][0-9]\\)" in
    let d_list = String.split_on_char '/' d in
    let day = List.nth d_list 1 in
    let num_day_ok =
      match List.nth d_list 0 with
      | "01" -> day <= "31"
      | "02" -> day <= "29"
      | "03" -> day <= "31"
      | "04" -> day <= "30"
      | "05" -> day <= "31"
      | "06" -> day <= "30"
      | "07" -> day <= "31"
      | "08" -> day <= "31"
      | "09" -> day <= "30"
      | "10" -> day <= "31"
      | "11" -> day <= "30"
      | "12" -> day <= "31"
      | _ -> false
    in
    num_day_ok
    && Str.string_match (Str.regexp (mr ^ "/" ^ dr ^ "/" ^ yr)) d 0
    && String.compare (List.nth d_list 2) "2022" >= 0

let get_date () =
  let d =
    match read_line () with
    | (s : string) -> s
  in
  match check_date d with
  | true -> d
  | false -> raise Invalid_date

let rec get_check_date () =
  try get_date ()
  with Invalid_date ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Please enter a valid date in the future\n";
    print_string "> ";
    get_check_date ()

let get_inputs init =
  let sport =
    print_endline "Sport";
    print_string "> ";
    get_check_sport ()
  in
  let location =
    print_endline "\nLocation";
    get_check_location sport
  in
  let date =
    print_endline "\nDate (mm/dd/yyyy)";
    print_string "> ";
    get_check_date ()
  in

  let time =
    print_endline "Time aa:bb (military)";
    print_string "> ";
    get_check_time ()
  in

  let max_signups =
    print_endline "\nHow many other players do you need?";
    let rec get_signups () =
      print_string "> ";
      match read_line () with
      | (s : string) -> (
          try
            let x = int_of_string s in
            if x < 0 then (
              ANSITerminal.print_string [ ANSITerminal.red ]
                "\nPlease enter a valid number\n";
              get_signups ())
            else s
          with Failure x ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nPlease enter a valid number\n";
            get_signups ())
    in

    get_signups ()
  in
  [ sport; location; date; time; max_signups ]

let create_time date time =
  let d =
    try String.split_on_char '/' date
    with Failure x -> raise Invalid_time
  in
  let t =
    try String.split_on_char ':' time
    with Failure x -> raise Invalid_time
  in
  try
    {
      day = List.nth d 1;
      month = List.nth d 0;
      year = List.nth d 2;
      hour = List.nth t 0;
      minute = List.nth t 1;
    }
  with Failure x -> raise Invalid_time

let create_blog inputs profile : blog =
  [
    {
      author = profile.username;
      sport = List.nth inputs 0;
      location = List.nth inputs 1;
      time = create_time (List.nth inputs 2) (List.nth inputs 3);
      likes = 0;
      likes_list = [];
      id = 0;
      signups = 0;
      max_signups = int_of_string (List.nth inputs 4);
      signup_list = [];
      comments = 0;
      comment_list = [];
    };
  ]

let create_post inputs blog profile : post =
  {
    author = profile.username;
    sport = List.nth inputs 0;
    location = List.nth inputs 1;
    time = create_time (List.nth inputs 2) (List.nth inputs 3);
    likes = 0;
    likes_list = [];
    id = get_last_id blog + 1;
    signups = 0;
    max_signups = int_of_string (List.nth inputs 4);
    signup_list = [];
    comments = 0;
    comment_list = [];
  }

let check_id_exist id (blog : blog) =
  let ids = List.map (fun x -> x.id) blog in
  List.mem id ids

let get_post id blog =
  if check_id_exist id blog then
    List.filter (fun x -> x.id = id) blog |> List.hd
  else raise Invalid_id

let ask_id blog =
  print_endline "\nPlease enter post Id ";
  print_string "> ";
  match read_line () with
  | (a : string) ->
      let post =
        try int_of_string a with Failure x -> raise Invalid_id
      in
      if check_id_exist post blog then post else raise Invalid_id

let get_author id blog =
  let post = get_post id blog in
  post.author

let get_likes id blog =
  let post = get_post id blog in
  post.likes

let get_signups id blog =
  let post = get_post id blog in
  post.signups

let get_max_signups id blog =
  let post = get_post id blog in
  post.max_signups

let get_signup_list id blog =
  let post = get_post id blog in
  post.signup_list

let get_comments id blog =
  let post = get_post id blog in
  post.comment_list

let get_number_of_comments id blog =
  let post = get_post id blog in
  post.comments

let rec print blog =
  match blog with
  | [] -> print_endline ""
  | h :: t ->
      print_endline
        ("\n" ^ h.author ^ " likes to play " ^ h.sport
       ^ ". Like count: " ^ string_of_int h.likes ^ "\n");
      print t

let string_of_time (time : time) : string =
  time.month ^ "/" ^ time.day ^ "/" ^ time.year ^ " at " ^ time.hour
  ^ ":" ^ time.minute

let rec print_formatted post =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("__________________________________________\n\n" ^ "|POST ID: "
   ^ string_of_int post.id ^ "\n|NAME: " ^ post.author ^ "\n|SPORT: "
   ^ post.sport ^ "\n|LOCATION: " ^ post.location ^ "\n|TIME: "
    ^ string_of_time post.time
    ^ "\n|# LIKES: "
    ^ string_of_int post.likes
    ^ "\n|# SIGNUPS: "
    ^ string_of_int post.signups
    ^ "/"
    ^ string_of_int post.max_signups
    ^ "\n|# COMMENTS: "
    ^ string_of_int post.comments
    ^ "\n__________________________________________\n\n")

let rec see_blog (blog : blog) =
  if List.length blog = 0 then
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Blog is empty. Would you like to add a post?\n"
  else List.iter (fun post -> print_formatted post) blog

let reached_max id (blog : blog) =
  let post = get_post id blog in
  post.signups = post.max_signups

let ask_single_or_all id (blog : blog) : string =
  let number_comments = get_number_of_comments id blog in
  if number_comments = 0 then raise No_comments
  else (
    print_endline
      ("Post has "
      ^ string_of_int number_comments
      ^ (if number_comments = 1 then " comment." else " comments.")
      ^ " Would you like to see a single comment or all comments?");
    print_string "> ";
    read_line ())

let display_all_comments (post : post) : unit =
  let comments_list = List.rev post.comment_list in
  List.iteri display_comment comments_list
