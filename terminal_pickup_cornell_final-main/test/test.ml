open OUnit2
open Game.Blog
open Game.Command
open Game.State
open Game.Profile

(** TESTING PLAN:

    We created a test suite that tested functions in all modules
    individually. We mostly focused on the functions that were the most
    vital to ensuring that our application ran appropriately from a
    user's perspective. Almost all getter functions were tested by
    OUnit. Functions with a read_line() function were playtested because
    they required user input. Most of the error messages that would be
    raised depending on the currently logged-in user were playtested.
    However, the log-in function itself was tested by OUnit.

    Our glass-box testing approach tested all possible scenarios of all
    the main commands that the user could input at start-up, when logged
    in, and when logged out. For example, the signup function, which was
    playtested, has 5 possible outputs: an error message for signing up
    for user's own post, an error message for exceeding maximum
    sign-ups, an error message for having already signed up for a post,
    an error message for not selecting a valid post in the blog, and a
    successful signup. Possible outputs for other commands were outlined
    in a similar manner and then tested by either OUnit or playtesting.

    In our test file, we created functions which created OUnit tests for
    individual function and we created several values for blogs, posts,
    and profiles, which covered the most vital scenarios and were used
    as either inputs for consumers or expected outputs for producers.

    Modules that were tested by OUnit include:

    - blog.ml
    - command.ml
    - profile.ml
    - state.ml

    Our plan demonstrates the correctness of the system by making sure
    that individual functions were correct, as outlined above, on a
    small scale and testing modules based on specific scenarios in which
    they become relevant for large-scale testing. For example, state.ml
    would only be of interest when we want to test updates to the values
    of an individual post or current user's profile. This way, smaller
    issues could be pinpointed and fixed more quickly while larger
    painpoints affecting the user could be fixed by analyzing their
    effects on the overall blog experience. *)

(** [create_blog_test name inputs profile expected] creates an OUnit
    test called [name] that ensures the equality of [expected] and
    [create_blog inputs profile]. *)
let create_blog_test
    (name : string)
    (inputs : string list)
    (profile : profile)
    (expected : blog) =
  name >:: fun _ -> assert_equal expected (create_blog inputs profile)

(** [create_post_test name inputs blog profile expected] creates an
    OUnit test called [name] that ensures the equality of [expected] and
    [create_post inputs blog profile]. *)
let create_post_test
    (name : string)
    (inputs : string list)
    (blog : blog)
    (profile : profile)
    (expected : post) =
  name >:: fun _ ->
  assert_equal expected (create_post inputs blog profile)

(** [parse_test name verb expected] creates an OUnit test called [name]
    that ensures the equality of [expected] and [parse verb]. *)
let parse_test (name : string) (verb : string) (expected : command) =
  name >:: fun _ -> assert_equal expected (parse verb)

(** [create_profile_test name tuple expected] creates an OUnit test
    called [name] that ensures the equality of [expected] and
    [create_profile tuple]. *)
let create_profile_test
    (name : string)
    (tuple : string * string)
    (expected : profile) =
  name >:: fun _ -> assert_equal expected (create_profile tuple)

(** [get_from_blog_test] creates an OUnit test called [name] that
    ensures the equality of [expected] and [<get_function> id blog].
    "get" functions include get_author, get_likes, get_signups, etc. *)
let get_from_blog_test
    (name : string)
    f
    (id : int)
    (blog : blog)
    expected =
  name >:: fun _ -> assert_equal expected (f id blog)

(** [get_from_blog_invalid_test] creates an OUnit test called [name]
    that ensures that the Invalid_id exception is raised for an invalid
    post id number in the function call [<get_function> id blog]. *)
let get_from_blog_invalid_test
    (name : string)
    f
    (id : int)
    (blog : blog) =
  name >:: fun _ ->
  assert_raises Game.Blog.Invalid_id (fun () -> f id blog)

(** [get_last_id_test] creates an OUnit test called [name] that ensures
    the equality of [expected] and [get_last_id blog].*)
let get_last_id_test (name : string) (blog : blog) (expected : int) =
  name >:: fun _ -> assert_equal expected (get_last_id blog)

(** [get_comment_test] creates an OUnit test called [name] that ensures
    the equality of [expected] and [get_comment  post id].*)
let get_comment_test
    (name : string)
    (post : post)
    (id : int)
    (expected : profile * string) =
  name >:: fun _ -> assert_equal expected (get_comment post id)

(** [get_post_from_id_test] creates an OUnit test called [name] that
    ensures the equality of [expected] and [get_post_from_id blog id].*)
let get_post_from_id_test
    (name : string)
    (blog : blog)
    (id : int)
    (expected : post) =
  name >:: fun _ -> assert_equal expected (get_post_from_id blog id)

(** [get_post_from_id_invalid_test] creates an OUnit test called [name]
    that ensures that the Post_Does_Not_Exist exception is raised for an
    invalid post id number in the function call
    [get_post_from_id blog id]. *)
let get_post_from_id_invalid_test
    (name : string)
    (blog : blog)
    (id : int) =
  name >:: fun _ ->
  assert_raises Post_Does_Not_Exist (fun () -> get_post_from_id blog id)

(** [to_string_blog] turns a blog into a string of post IDs*)
let rec to_string_blog blog =
  match blog with
  | [] -> ""
  | h :: t -> string_of_int h.id ^ to_string_blog t

(** [sorted_blog_test] creates an OUnit test called [name] that ensures
    the equality of [expected] and [get_blog (<sort fxn> state)].*)
let sorted_blog_test
    (name : string)
    f
    (st : Game.State.t)
    (expected : string) =
  print_string (to_string_blog (get_blog (f st)));
  name >:: fun _ ->
  assert_equal expected (to_string_blog (get_blog (f st)))

(** [ask_comment_test] creates an OUnit test called [name] that ensures
    the equality of [expected] and [ask_comment init].*)

(** Tests already_following helper function *)
let already_following_test name profile prof_name output =
  name >:: fun _ ->
  assert_equal output (already_following profile prof_name)

let follow_test name f followee t output =
  name >:: fun _ -> assert_equal output (follow f followee t)

(** [follower_count_test name profile expected] creates an OUnit test
    called [name] that ensures the equality of [expected] and
    [follower_count profile]. *)
let follower_count_test
    (name : string)
    (profile : profile)
    (expected : int) =
  name >:: fun _ -> assert_equal expected (follower_count profile)

(** [following_count_test name profile expected] creates an OUnit test
    called [name] that ensures the equality of [expected] and
    [following_count profile]. *)
let following_count_test
    (name : string)
    (profile : profile)
    (expected : int) =
  name >:: fun _ -> assert_equal expected (following_count profile)

(** [init_state_test name user blog expected] created an OUnit test
    called [name] that ensures the equality of [expected] and
    [init_state user blog]. *)
let init_state_test
    (name : string)
    (user : profile)
    (blog : blog)
    (expected : t) =
  name >:: fun _ -> assert_equal expected (init_state user blog)

(** [add_profile_test name state profile expected] creates an OUnit test
    called [name] that ensures the equality of [expected] and
    [add_profile state profile]. *)
let add_profile_test
    (name : string)
    (st : t)
    (profile : profile)
    (expected : t) =
  name >:: fun _ -> assert_equal expected (add_profile st profile)

(** [get_profile_test name state expected] creates an OUnit test called
    [name] that ensures the equality of [expected] and
    [get_profile state]. *)
let get_profile_test (name : string) (st : t) (expected : profile) =
  name >:: fun _ -> assert_equal expected (get_profile st)

(** [change_user_test name state new_user expected] creates an OUnit
    test called [name] which ensures the equality of [expected] and
    [change_user state new_profile]. *)
let change_user_test
    (name : string)
    (st : t)
    (new_user : profile)
    (expected : t) =
  name >:: fun _ -> assert_equal expected (change_user st new_user)

(** [profiles_test name state expected] creates an OUnit test called
    [name] that ensures the equality of [expected] and [profiles st]. *)
let profiles_test (name : string) (st : t) (expected : profile list) =
  name >:: fun _ -> assert_equal expected (profiles st)

(** *)
let add_post_test (name : string) (post : post) (st : t) (expected : t)
    =
  name >:: fun _ -> assert_equal expected (add_post post st)

(** *)
let log_in_test
    (name : string)
    (st : t)
    (username : string)
    (password : string)
    (expected : t) =
  name >:: fun _ -> assert_equal expected (log_in st username password)

let delete_post_test
    (name : string)
    (post_id : int)
    (st : t)
    (expected : t) =
  name >:: fun _ -> assert_equal expected (delete_post post_id st)

let inputs1 =
  [ "Badminton"; "Helen Newman Hall"; "04/20/2022"; "12:30"; "3" ]

let post1 =
  {
    author = "Rithvik Duggireddy";
    sport = "Badminton";
    location = "Helen Newman Hall";
    time =
      {
        day = "20";
        month = "04";
        year = "2022";
        hour = "12";
        minute = "30";
      };
    likes = 0;
    likes_list = [];
    id = 0;
    signups = 0;
    max_signups = 3;
    signup_list = [];
    comments = 0;
    comment_list = [];
  }

let profile1_before_post : profile =
  {
    user_posts = [];
    username = "Rithvik Duggireddy";
    password = "easy123!";
    followers = [];
    following = [];
  }

let profile1 : profile =
  { profile1_before_post with user_posts = [ post1.id ] }

let blog1 : blog = [ post1 ]

let state1 =
  {
    current_user = profile1;
    blog = blog1;
    profiles = [ profile1 ];
    sign_ups = [];
  }

let profile1_empty = { profile1 with user_posts = [] }

let state1_empty =
  {
    state1 with
    current_user = profile1_empty;
    blog = [];
    profiles = [ profile1_empty ];
  }

let post1_new = { post1 with sport = "Basketball"; id = 1 }
let blog1_new = [ post1_new; post1 ]

let profile1_new =
  { profile1 with user_posts = [ post1_new.id; post1.id ] }

let state1_new =
  {
    current_user = profile1_new;
    blog = blog1_new;
    profiles = [ profile1_new ];
    sign_ups = [];
  }

let inputs2 : string list =
  [ "Soccer"; "Jessup Field"; "11/19/2023"; "16:00"; "4" ]

let profile2 : profile =
  {
    user_posts = [];
    username = "Jonathan Akar";
    password = "hard456#";
    followers = [];
    following = [];
  }

let state2 = { state1 with profiles = profile2 :: state1.profiles }
let changed_state2 = { state2 with current_user = profile2 }

let post2 : post =
  {
    author = "Jonathan Akar";
    sport = "Soccer";
    location = "Jessup Field";
    time =
      {
        day = "19";
        month = "11";
        year = "2023";
        hour = "16";
        minute = "00";
      };
    likes = 0;
    likes_list = [];
    id = 1;
    signups = 0;
    max_signups = 4;
    signup_list = [];
    comments = 0;
    comment_list = [];
  }

let profile2_after_post2 : profile =
  { profile2 with user_posts = [ post2.id ] }

let profile4 : profile =
  {
    user_posts = [];
    username = "Eric Zhong";
    password = "zhong234$";
    followers = [];
    following = [];
  }

let extra_state2 =
  { state2 with profiles = profile4 :: state2.profiles }

let profiles_extra_state2 = [ profile4; profile2; profile1 ]

let profile4_w_followers : profile =
  {
    user_posts = [];
    username = "Eric Zhong";
    password = "zhong234@";
    followers = [ "Rithvik Duggireddy"; "Jonathan Akar" ];
    following = [];
  }

let profile4_following : profile =
  {
    user_posts = [];
    username = "Eric Zhong";
    password = "zhong234@";
    followers = [];
    following = [ "Rithvik Duggireddy"; "Jonathan Akar" ];
  }

let post_no_likes =
  {
    author = "Eric Zhong";
    sport = "Volleyball";
    location = "Helen Newman Hall";
    time =
      {
        day = "02";
        month = "11";
        year = "2030";
        hour = "12";
        minute = "30";
      };
    likes = 0;
    likes_list = [];
    id = 3;
    signups = 0;
    max_signups = 3;
    signup_list = [];
    comments = 0;
    comment_list = [];
  }

let post_liked =
  {
    author = "Eric Zhong";
    sport = "Volleyball";
    location = "Helen Newman Hall";
    time =
      {
        day = "02";
        month = "11";
        year = "2030";
        hour = "12";
        minute = "30";
      };
    likes = 2;
    likes_list = [ profile1; profile2 ];
    id = 3;
    signups = 0;
    max_signups = 3;
    signup_list = [];
    comments = 0;
    comment_list = [];
  }

let blog2 : blog = [ post1; post2 ]

let post3 : post =
  {
    author = "Olivia McGoldrick";
    sport = "Volleyball";
    location = "Helen Newman Hall";
    time =
      {
        day = "10";
        month = "05";
        year = "2022";
        hour = "10";
        minute = "30";
      };
    likes = 0;
    likes_list = [];
    id = 2;
    signups = 0;
    max_signups = 1;
    signup_list = [];
    comments = 0;
    comment_list = [];
  }

let blog3 =
  increase_likes post3;
  [ post1; post2; post3 ]

let state3 =
  let st = comment "Test comment 1" 0 (init_state profile1 blog3) in
  sign_up 2 st

let blog3_update_state = get_blog state3
let state4 = init_state profile1 blog3
let uppercase_verbs : string list = [ "Add"; "Delete" ]
let lowercase_verbs : string list = [ "add"; "delete" ]
let user1, pass1 = ("Rithvik Duggireddy", "easy123!")

let state5 =
  {
    state2 with
    blog = post3 :: state2.blog;
    current_user =
      {
        state2.current_user with
        user_posts =
          (get_last_id state2.blog + 1)
          :: state2.current_user.user_posts;
      };
  }

let state6 : t =
  {
    current_user = profile2_after_post2;
    blog = [ post2; post1 ];
    profiles = [ profile2_after_post2; profile1 ];
    sign_ups = [];
  }

let state6_after_delete : t =
  {
    current_user = profile2;
    blog = [ post1 ];
    profiles = [ profile2; profile1 ];
    sign_ups = [];
  }

let blog_tests =
  [
    create_blog_test "testing the creation of a blog with sample inputs"
      inputs1 profile1 blog1;
    create_post_test
      "testing the creation of a post in an existing blog with sample \
       inputs"
      inputs2 blog1 profile2 post2;
    get_from_blog_test "get author from blog, expected Rithvik "
      get_author 0 blog2 "Rithvik Duggireddy";
    get_from_blog_test "get author from blog, expected Jon" get_author 1
      blog2 "Jonathan Akar";
    get_from_blog_invalid_test
      "get author from blog, expected Invalid_id" get_author 5 blog2;
    get_from_blog_test "get max signups from blog, expected 3 "
      get_max_signups 0 blog2 3;
    get_from_blog_test "get max signups from blog, expected 4"
      get_max_signups 1 blog2 4;
    get_from_blog_invalid_test
      "get max signups from blog, expected Invalid_id" get_max_signups 5
      blog2;
    get_from_blog_test "get signups from blog, expected 0 " get_signups
      0 blog2 0;
    get_from_blog_invalid_test
      "get signups from blog, expected Invalid_id" get_signups 5 blog2;
    get_from_blog_test "get likes from blog, expected 0 " get_likes 0
      blog2 0;
    get_from_blog_invalid_test
      "get likes from blog, expected Invalid_id" get_likes 5 blog2;
    get_from_blog_test "get signup list from blog, expected [] "
      get_signup_list 0 blog2 [];
    get_from_blog_invalid_test
      "get signup list from blog, expected Invalid_id" get_signup_list 5
      blog2;
    get_from_blog_test "get comments from blog, expected [] "
      get_comments 0 blog2 [];
    get_from_blog_invalid_test
      "get comments from blog, expected Invalid_id" get_comments 5 blog2;
    get_from_blog_test "get number of comments from blog, expected 0 "
      get_number_of_comments 0 blog2 0;
    get_from_blog_invalid_test
      "get number of comments from blog, expected Invalid_id"
      get_number_of_comments 5 blog2;
    get_from_blog_test "get number of comments from blog, expected 0 "
      get_number_of_comments 0 blog2 0;
    get_from_blog_invalid_test
      "get number of comments from blog, expected Invalid_id"
      get_signups 5 blog2;
    get_from_blog_test
      "check whether post from blog has reached max, expected false "
      reached_max 0 blog2 false;
    get_from_blog_invalid_test
      "check whether post from blog has reached max, expected \
       Invalid_id"
      reached_max 5 blog2;
    get_from_blog_test
      "check whether post from blog is valid, expected true "
      is_valid_post 0 blog2 true;
    get_from_blog_test "get likes from blog, expected 1 " get_likes 2
      blog3 1;
    get_last_id_test "get last id test, expected 0" blog1 0;
    get_from_blog_test
      "get comments from blog, expected [ (profile1, Test comment 1) ]"
      get_comments 0 blog3_update_state
      [ (profile1, "Test comment 1") ];
    get_from_blog_test "get signups from blog, expected 2 " get_signups
      2 blog3_update_state 1;
    get_from_blog_test "get signup list from blog, expected [profile1] "
      get_signup_list 2 blog3_update_state [ profile1 ];
    get_from_blog_test "get number of comments from blog, expected 0 "
      get_number_of_comments 0 blog3_update_state 1;
    get_post_from_id_test "testing get_post_from_id, expecting post1"
      blog3 0 post1;
    get_post_from_id_invalid_test
      "testing get_post_from_id, expecting Invalid" blog3_update_state 5;
  ]

let command_tests =
  [
    parse_test "testing the parsing of a capitalized verb"
      (List.hd uppercase_verbs ^ "  ")
      Add;
    parse_test "testing the parsing of a capitalized verb (2)"
      (List.nth uppercase_verbs 1)
      Delete;
    parse_test "testing the parsing of a lowercase verb"
      ("  " ^ List.hd lowercase_verbs ^ "    ")
      Add;
    parse_test "testing the parsing of a lowercase verb (2)"
      (List.nth lowercase_verbs 1 ^ "          ")
      Delete;
    ( "testing that Malformed is raised for invalid commands"
    >:: fun _ -> assert_raises Malformed (fun () -> parse "A dd") );
  ]

let state_tests =
  [
    sorted_blog_test "test sort by author, expect 120" sort_by_author
      state4 "120";
    sorted_blog_test "test sort by location, expect 120"
      sort_by_location state4 "021";
    sorted_blog_test "test sort by time, expect 120" sort_by_time state4
      "021";
    sorted_blog_test "test sort by sport, expect 120" sort_by_sport
      state4 "012";
    already_following_test
      "Rithvick does not follow Eric so return false" profile1
      "Eric Zhong" false;
    already_following_test "Eric already follows Rithvik so return true"
      profile4_following "Rithvik Duggireddy" true;
    already_following_test "Invalid profile should return false"
      profile2 "John Jay" false;
    init_state_test "testing the initialization of blog state" profile1
      blog1 state1;
    add_profile_test "test adding a profile to initialized state" state1
      profile2 state2;
    get_profile_test
      "testing access to current user's profile in initialized state"
      state1 profile1;
    get_profile_test
      "testing access to current user's profile in updated state" state2
      profile1;
    change_user_test "testing the change of users" state2 profile2
      changed_state2;
    profiles_test "testing access to list of profiles in given state"
      extra_state2 profiles_extra_state2;
    add_post_test
      "testing the user can add a post in a given state (also tests \
       get_last_id)"
      post3 state2 state5;
    log_in_test
      "testing logging in with a different profile (also tests \
       find_username_password)"
      changed_state2 profile1.username profile1.password state2;
    delete_post_test
      "testing the deletion of a current user's post in a state with \
       multiple posts, multiple users"
      post2.id state6 state6_after_delete;
    delete_post_test
      "testing the deletion of a current user's post in a state with \
       multiple posts, 1 user"
      post1_new.id state1_new state1;
    delete_post_test
      "testing the deletion of a current user's post in a state with a \
       single post, 1 user"
      post1.id state1 state1_empty;
    ( "testing you can't delete another user's post" >:: fun _ ->
      assert_raises Not_allowed (fun () -> delete_post post1.id state6)
    );
  ]

let profile_tests =
  [
    create_profile_test
      "testing the creation of a profile with sample username and \
       password"
      (user1, pass1) profile1_before_post;
    follower_count_test "testing that 2 followers is returned"
      profile4_w_followers 2;
    follower_count_test "testing that 0 followers is returned"
      profile4_following 0;
    following_count_test "testing that 2 following is returned"
      profile4_following 2;
    following_count_test "testing that 0 following is returned"
      profile4_w_followers 0;
  ]

let suite =
  "pickup cornell test suite"
  >::: List.flatten
         [ blog_tests; command_tests; state_tests; profile_tests ]

let _ = run_test_tt_main suite