open Game
open Blog
open Command
open Profile
open State

let rec edit_blog st logged_in =
  match logged_in with
  | false ->
      let rec home_page st =
        print_endline
          "\nWould you like to log in, create new profile, or Quit?";
        print_string "> ";
        let l = read_line () in
        match String.trim l with
        | "Quit" -> exit 0
        | "log in" | "Log in" | "login" | "Login" -> (
            print_endline "\nUsername: ";
            print_string "> ";
            let username =
              match read_line () with
              | s -> s
            in
            print_endline "\nPassword: ";
            print_string "> ";
            let password =
              match read_line () with
              | s -> s
            in
            try
              let s = log_in st username password in
              ANSITerminal.print_string [ ANSITerminal.green ]
                "Login Successful! \n";
              edit_blog s true
            with Invalid_login ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Username or password not found. \n\n";
              home_page st)
        | "create new profile" ->
            let user =
              create_profile (get_profile_inputs (get_usernames st))
            in
            user |> fun x ->
            print_endline "You are now logged in.";
            add_profile st x |> fun x ->
            change_user x user |> fun x -> edit_blog x true
        | x ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "must type either 'log in' or 'create new profile' \n";
            home_page st
      in
      home_page st
  | true -> (
      print_endline
        "\n\
         Possible commands: [See], [Add], [Delete], [Like], [Sort], \
         [Show](following/follower information), [Logout],";
      print_endline
        "[Quit], [Follow], [Signup], [Search], [Comment], [Comments], \
         [Unfollow], [Unlike], [Likes].\n";
      print_string "> ";
      match parse (read_line ()) with
      | exception Malformed ->
          ANSITerminal.print_string [ ANSITerminal.red ] "What?\n";
          edit_blog st true
      | See ->
          see_blog (get_blog st);
          edit_blog st true
      | Logout ->
          print_endline "logged out \n";
          edit_blog st false
      | Add -> (
          try
            print_endline "";
            get_inputs st |> fun x ->
            create_post x (get_blog st) (get_profile st) |> fun x ->
            add_post x st |> fun x -> edit_blog x true
          with
          | Invalid_time ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Please enter a valid time\n";
              edit_blog st true
          | Invalid_cap ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Please enter a valid number of players\n";
              edit_blog st true)
      | Show_likes -> (
          print_endline "\nEnter the post id";
          print_string "> ";
          let id = read_line () in
          invalid_input_for_like edit_blog st id;
          let it = int_of_string id in
          try show_liked_list st edit_blog it
          with Invalid_id ->
            print_endline "";
            edit_blog st true)
      | Delete -> (
          try
            ask_id (get_blog st) |> fun x ->
            delete_post x st |> fun x -> edit_blog x true
          with
          | Blog.Invalid_id ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Please enter a valid ID\n";
              edit_blog st true
          | State.Not_allowed ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You can only delete your own post\n";
              edit_blog st true)
      | Unfollow -> (
          print_endline
            "\nEnter the username of the user you want to unfollow: ";
          print_string "> ";
          let profile = read_line () in
          try unfollow edit_blog profile st with
          | Invalid_profile ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Please enter a valid profile\n";
              edit_blog st true
          | Not_Following ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You are not following them in the first place\n";
              edit_blog st true)
      | Unlike -> (
          print_endline "\nEnter the post ID:";
          print_string "> ";
          let id = read_line () in
          invalid_input_for_like edit_blog st id;
          let it = int_of_string id in
          try unlike_post edit_blog it st with
          | Invalid_id ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Please enter valid ID\n";
              edit_blog st true
          | Not_liked ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You are already not liking it\n";
              edit_blog st true)
      | Like -> (
          print_endline "\nEnter the post ID:";
          print_string "> ";
          let id = read_line () in
          invalid_input_for_like edit_blog st id;
          let it = int_of_string id in
          try like_post edit_blog it st with
          | Invalid_id ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Please enter valid ID\n";
              edit_blog st true
          | Already_Liked ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You already liked the post\n";
              edit_blog st true)
      | Signup -> (
          try
            let signup_post = ask_id (get_blog st) in
            let new_state = sign_up signup_post st in
            ANSITerminal.print_string [ ANSITerminal.green ]
              "You have signed up for this post!\n";
            edit_blog new_state true
          with
          | Blog.Invalid_id ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Please enter a valid ID\n";
              edit_blog st true
          | State.Self_signup ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Cannot sign up for own post\n";
              edit_blog st true
          | State.Reached_max ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "This post has reached the maximum number of sign-ups\n";
              edit_blog st true
          | State.Already_signed_up ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You have already signed up for this post\n";
              edit_blog st true)
      | Quit -> exit 0
      | Follow -> (
          print_endline "\nEnter the username of who you want to follow";
          print_string "> ";
          let profile = read_line () in
          try follow edit_blog [ profile ] st with
          | Invalid_profile ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You have entered an invalid profile\n";
              edit_blog st true
          | Already_following ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You are already following them\n";
              edit_blog st true
          | Cannot_Follow_Yourself ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You cannot follow yourself\n";
              edit_blog st true)
      | Show -> show (st |> get_profile) edit_blog st
      | Sort -> (
          print_endline
            "\nDo you want to sort by Author, Location, Sport, or Time?";
          print_string "> ";
          let element = read_line () in
          match element with
          | "Sport" | "sport" ->
              let st = sort_by_sport st in
              see_blog (get_blog st);
              edit_blog st true
          | "Author" | "author" ->
              let st = sort_by_author st in
              see_blog (get_blog st);
              edit_blog st true
          | "Location" | "location" ->
              let st = sort_by_sport st in
              see_blog (get_blog st);
              edit_blog st true
          | "Time" | "time" ->
              let st = sort_by_time st in
              see_blog (get_blog st);
              edit_blog st true
          | _ ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "You cannot sort by that attribute.\n";
              edit_blog st true)
      | Search ->
          print_endline "\nPlease enter post ID:";
          print_string "> ";
          let rec helper () =
            try
              let input = read_line () in
              let id = int_of_string (String.concat "" [ input ]) in
              print_formatted (get_post id (get_blog st));
              edit_blog st true
            with
            | Blog.Invalid_id ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  "Please enter a valid ID\n";
                helper ()
            | Failure exn ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  "Please enter a valid ID\n";
                helper ()
          in
          helper ()
      | Comment ->
          let rec helper () =
            try
              let comment_post = ask_id (get_blog st) in
              let content = ask_comment (get_blog st) in
              let new_state = comment content comment_post st in
              ANSITerminal.print_string [ ANSITerminal.green ]
                ("\nYou have commented on post "
                ^ string_of_int comment_post
                ^ "!\n");
              edit_blog new_state true
            with
            | Blog.Invalid_id ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  "Please enter a valid ID.\n";
                helper ()
            | Blog.Empty_comment ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  "Please enter a nonempty comment.\n";
                helper ()
          in

          helper ()
      | Comments ->
          let rec helper () =
            try
              let post_id = ask_id (get_blog st) in
              let number_comments =
                get_number_of_comments post_id st.blog
              in
              let single_or_all =
                String.lowercase_ascii
                  (ask_single_or_all post_id st.blog)
              in
              let rec find x lst =
                match lst with
                | [] -> raise (Failure "Not Found")
                | h :: t -> if x = h then 0 else 1 + find x t
              in
              match single_or_all with
              | "single" | "single comment" ->
                  let rec helper1 () =
                    try
                      let desired_post =
                        get_post post_id (get_blog st)
                      in
                      let comment_id =
                        ask_comment_number desired_post - 1
                      in
                      let desired_comment =
                        get_comment desired_post comment_id
                      in
                      display_comment
                        (find desired_comment
                           (List.rev (get_comments post_id st.blog)))
                        desired_comment;
                      edit_blog st true
                    with
                    | Blog.Invalid_id ->
                        ANSITerminal.print_string [ ANSITerminal.red ]
                          "Please enter a valid ID\n";
                        helper1 ()
                    | Blog.Invalid_comment_number ->
                        ANSITerminal.print_string [ ANSITerminal.red ]
                          ("Post has "
                          ^ string_of_int number_comments
                          ^ (if number_comments = 1 then " comment."
                            else " comments.")
                          ^ " Please enter a valid comment number.\n");
                        helper1 ()
                  in
                  helper1 ()
              | "all" | "all comments" ->
                  let rec helper2 () =
                    try
                      let desired_post =
                        get_post post_id (get_blog st)
                      in
                      display_all_comments desired_post;
                      edit_blog st true
                    with Blog.Invalid_id ->
                      ANSITerminal.print_string [ ANSITerminal.red ]
                        "Please enter a valid ID\n";
                      helper2 ()
                  in
                  helper2 ()
              | _ ->
                  let s =
                    "Not a valid command. Command must be 'single' or \
                     'all'.\n"
                  in
                  ANSITerminal.print_string [ ANSITerminal.red ] s;
                  helper ()
            with
            | Blog.Invalid_id ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  "Please enter a valid ID\n";
                helper ()
            | Blog.No_comments ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  "Post has no comments.\n";
                edit_blog st true
          in
          helper ())

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to the Pickup Cornell blog!\n";
  print_endline
    "Your the first here, would you like to create a new profile ?\n";
  print_string "[y/n] ";
  match read_line () with
  | "y" -> (
      let init_user =
        create_first_profile (get_first_profile_inputs ())
      in
      try
        get_inputs 1 |> fun x ->
        create_blog x init_user |> fun x ->
        edit_blog (init_state init_user x) true
      with
      | Invalid_time ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Please enter a valid time\n";
          main ()
      | Invalid_cap ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Please enter a valid number of players\n";
          main ())
  | "n" ->
      print_endline "Okay, goodbye! \n";
      exit 0
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nSorry, I didn't understand that. Try again.\n";
      main ()

(* Execute the game engine. *)
let () = main ()
