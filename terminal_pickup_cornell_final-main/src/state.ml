open Blog
open Profile

type t = {
  current_user : profile;
  blog : blog;
  profiles : profile list;
  sign_ups : (profile * post) list;
}

exception Invalid_id
exception Invalid_login
exception Invalid_profile
exception Already_following
exception Self_signup
exception Reached_max
exception Already_signed_up
exception Not_allowed
exception Cannot_Follow_Yourself
exception Already_Liked
exception Not_Following
exception Not_liked

let init_state user blog =
  { current_user = user; blog; profiles = [ user ]; sign_ups = [] }

let add_profile st profile =
  { st with profiles = profile :: st.profiles }

let get_profile t = t.current_user
let change_user st profile = { st with current_user = profile }
let profiles st = st.profiles
let get_blog st = st.blog
let get_usernames st = List.map (fun p -> p.username) st.profiles

let add_post (post : post) (st : t) : t =
  {
    st with
    blog = post :: st.blog;
    current_user =
      {
        st.current_user with
        user_posts =
          (get_last_id st.blog + 1) :: st.current_user.user_posts;
      };
  }

let find_username_password st username password =
  let user =
    st |> profiles
    |> List.filter (fun x ->
           x.username = username && x.password = password)
  in
  if List.length user = 0 then raise Invalid_login else List.hd user

let log_in st username password =
  try
    {
      st with
      current_user = find_username_password st username password;
    }
  with Invalid_login -> raise Invalid_login

let delete_post id st =
  if not (check_id_exist id st.blog) then raise Invalid_id
  else if List.mem id st.current_user.user_posts then
    let updated_user =
      {
        st.current_user with
        user_posts =
          List.filter (fun x -> x <> id) st.current_user.user_posts;
      }
    in
    {
      st with
      blog = List.filter (fun x -> x.id <> id) st.blog;
      current_user = updated_user;
      profiles =
        List.map
          (fun x -> if x = st.current_user then updated_user else x)
          st.profiles;
    }
  else raise Not_allowed

let get_current_user st = st.current_user

let add_to_likes_list blog st =
  blog.likes_list <- get_current_user st :: blog.likes_list

let invalid_input_for_like f st str =
  try
    let _ = int_of_string str in
    print_endline ""
  with Failure _ ->
    ANSITerminal.print_string [ ANSITerminal.red ] "Invalid ID\n";
    f st true

let remove_follower_from_list follower lst =
  List.filter (fun x -> x <> follower) lst

let like_post f id (st : t) =
  if is_valid_post id st.blog = false then raise Invalid_id
  else
    let post = get_post_from_id st.blog id in
    if List.mem st.current_user (get_likes_list post) then
      raise Already_Liked
    else increase_likes post;
    add_to_likes_list (get_post id st.blog) st;
    f st true

let unlike_post f id st =
  if is_valid_post id st.blog = false then raise Invalid_id
  else
    let post = get_post_from_id st.blog id in
    if List.mem st.current_user (get_likes_list post) = false then
      raise Not_liked
    else post.likes <- post.likes - 1;
    let lst1 =
      remove_follower_from_list st.current_user post.likes_list
    in
    post.likes_list <- lst1;
    f st true

let already_signed_up (id : int) (st : t) : bool =
  List.mem st.current_user (get_signup_list id st.blog)

let sign_up id (st : t) : t =
  if check_id_exist id st.blog then
    if get_user st.current_user = get_author id st.blog then
      raise Self_signup
    else if reached_max id st.blog then raise Reached_max
    else if already_signed_up id st then raise Already_signed_up
    else
      let rest = List.filter (fun x -> x.id <> id) st.blog in
      let post = List.hd (List.filter (fun x -> x.id = id) st.blog) in
      { post with signups = get_signups id st.blog + 1 }
      |> fun updated_post ->
      {
        st with
        blog =
          {
            updated_post with
            signup_list = st.current_user :: get_signup_list id st.blog;
          }
          :: rest;
        sign_ups = (st.current_user, get_post id st.blog) :: st.sign_ups;
      }
  else raise Invalid_id

let sort_by_sport st =
  {
    st with
    blog = List.sort (fun x y -> String.compare x.sport y.sport) st.blog;
  }

let sort_by_author st =
  {
    st with
    blog =
      List.sort (fun x y -> String.compare x.author y.author) st.blog;
  }

let sort_by_location st =
  {
    st with
    blog =
      List.sort
        (fun x y -> String.compare x.location y.location)
        st.blog;
  }

let sort_by_time st =
  {
    st with
    blog =
      List.sort
        (fun (x : post) (y : post) ->
          let xt = x.time in
          let yt = y.time in
          if int_of_string xt.year > int_of_string yt.year then 1
          else if int_of_string xt.year < int_of_string yt.year then -1
          else if int_of_string xt.month > int_of_string yt.month then 1
          else if int_of_string xt.month < int_of_string yt.month then
            -1
          else if int_of_string xt.day > int_of_string yt.day then 1
          else if int_of_string xt.day < int_of_string yt.day then -1
          else if int_of_string xt.hour > int_of_string yt.hour then 1
          else if int_of_string xt.hour < int_of_string yt.hour then -1
          else if int_of_string xt.minute > int_of_string yt.minute then
            1
          else if int_of_string xt.minute < int_of_string yt.minute then
            -1
          else 0)
        st.blog;
  }

let rec is_valid_profile profile t = List.mem profile t.profiles

let already_following profile1 profile2 =
  List.mem profile2 (get_following profile1)

let increase_follower profile followee =
  profile.following <- profile.followers @ [ followee.username ];
  followee.followers <- followee.followers @ [ profile.username ]

let rec prof_helper profiles user =
  match profiles with
  | [] -> raise Invalid_profile
  | h :: t -> if h.username = user then h else prof_helper t user

let get_profile_from_user t user = prof_helper t.profiles user

let get_followee =
  let followee = read_line () in
  followee

let rec user_already str lst =
  match lst with
  | [] -> false
  | h :: t -> if h.username = str then true else user_already str t

let follow f followee t =
  if List.length followee != 1 then raise Invalid_profile
  else
    let followee_profile = get_profile_from_user t (List.hd followee) in
    if followee_profile = t.current_user then
      raise Cannot_Follow_Yourself
    else if is_valid_profile followee_profile t = false then
      raise Invalid_profile
    else if already_following t.current_user (List.hd followee) = true
    then raise Already_following
    else increase_follower t.current_user followee_profile;
    print_endline "Successfully followed";
    print_profile t.current_user;
    f t true

let rec show_things profile_lst =
  match profile_lst with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> h ^ ", " ^ show_things t

let show t f st =
  print_endline
    ("Number of Followers: "
    ^ string_of_int (List.length t.followers)
    ^ "\n" ^ show_things t.followers ^ "\n");
  print_endline
    ("Number of Following: "
    ^ string_of_int (List.length t.following)
    ^ "\n" ^ show_things t.following);
  f st true

let comment (content : string) (id : int) (st : t) : t =
  if check_id_exist id st.blog then
    let comment_pair = (st.current_user, content) in
    let rest = List.filter (fun x -> x.id <> id) st.blog in
    let post = List.hd (List.filter (fun x -> x.id = id) st.blog) in
    {
      post with
      comment_list = comment_pair :: get_comments id st.blog;
      comments = get_number_of_comments id st.blog + 1;
    }
    |> fun updated_post -> { st with blog = updated_post :: rest }
  else raise Invalid_id

let string_to_int f str st =
  try int_of_string str
  with Failure str ->
    print_endline "Enter a valid id";
    f st true

let rec get_profile_lst_from_follower_lst (lst : string list) x =
  match lst with
  | [] -> []
  | h :: t ->
      get_profile_from_user x h :: get_profile_lst_from_follower_lst t x

let rec prof_list_to_string_lst lst =
  match lst with
  | [] -> []
  | h :: t -> h.username :: prof_list_to_string_lst t

let unfollow f followee t =
  let user = t.current_user in
  if List.mem followee user.following = false then raise Not_Following
  else if is_valid_profile (get_profile_from_user t followee) t = false
  then raise Invalid_profile
  else
    let lst1 = remove_follower_from_list followee user.following in
    let lst2 =
      remove_follower_from_list t.current_user
        (get_profile_lst_from_follower_lst
           (get_profile_from_user t followee).followers t)
    in
    user.following <- lst1;
    (get_profile_from_user t followee).followers <-
      prof_list_to_string_lst lst2;
    f t true

let show_liked_list t f id =
  if is_valid_post id t.blog = false then raise Invalid_id
  else
    let post = get_post_from_id t.blog id in
    if List.length post.likes_list = 0 then
      ANSITerminal.print_string
        [ ANSITerminal.magenta ]
        "No likes so far\n"
    else
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("Likes: " ^ "\n"
        ^ show_things (post.likes_list |> prof_list_to_string_lst)
        ^ "\n");
    f t true
