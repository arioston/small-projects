open Core

let calculate_probability sinMatch max_number_of_simulations =
  Float.round (Float.of_int sinMatch) /. (Float.of_int max_number_of_simulations) *. 100.

let%expect_test _ =
  print_string (string_of_float (calculate_probability 100 100));
  [%expect {| 100. |}]

let%expect_test _ =
  print_string (string_of_float (calculate_probability 1 100));
  [%expect {| 1. |}]

let%expect_test _ =
  print_string (string_of_float (calculate_probability 0 100));
  [%expect {| 0. |}]

let get_random_day () =
  let randomInt = Random.int 365 in
  Date.add_days (Date.create_exn ~y:2001 ~m:Jan ~d:1) randomInt

let%expect_test _ =
  print_string (Date.to_string (get_random_day ()));
  [%expect {| 2001-04-30 |}]

let take_gen n gen_fun =
  let rec aux n acc =
    if n <= 0 then List.rev acc
    else aux (n - 1) (gen_fun () :: acc)
  in 
  aux n []

let%expect_test _ =
  print_string (Date.to_string (List.hd_exn (take_gen 1 get_random_day)));
  [%expect {| 2001-04-30 |}]

(*Returns a list of number random date objects for birthdays.*)
let get_birthday numberOfBirthdays = take_gen numberOfBirthdays get_random_day

let%expect_test _ =
  print_string (string_of_int (List.length (get_birthday 100)));
  [%expect {| 100 |}]

let%expect_test _ =
  print_string (string_of_int (List.length (get_birthday 0)));
  [%expect {| 0 |}]

(* Returns true if two dates are the same month and day (ignoring year). *)

(* Returns the date object of a birthday that occurs more than once in the birthdays list. *)
let get_match birthdays = List.find_a_dup birthdays ~compare:Date.compare

let%expect_test _ =
  let birthdays = [Date.create_exn ~y:2001 ~m:Jan ~d:1; Date.create_exn ~y:2001 ~m:Jan ~d:1] in
  print_string (Date.to_string (Option.value (get_match birthdays) ~default:Date.unix_epoch) );
  [%expect {| 2001-01-01 |}]

let%expect_test _ =
  let birthdays = [Date.create_exn ~y:2001 ~m:Jan ~d:1; Date.create_exn ~y:2002 ~m:Jan ~d:1] in
  print_string (Date.to_string (Option.value (get_match birthdays) ~default:Date.unix_epoch));
  [%expect {| 1970-01-01 |}]

let string_of_date date = 
  let month = Month.to_string (Date.month date) in
  let day = Date.day date in
  month ^ " " ^ (Int.to_string day)
  
let%expect_test _ =
  let date = Date.create_exn ~y:2001 ~m:Jan ~d:1 in
  print_string (string_of_date date);
  [%expect {| Jan 1 |}]