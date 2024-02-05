let translate word count bitMapChar =
  match bitMapChar with
  | '*' -> word.[count mod (String.length word)]
  | _ -> bitMapChar


let%expect_test _ = 
  let word = "hello" in
  let bitMap = "*****" in
  let result = String.mapi (translate word) bitMap in
  print_endline result;
  [%expect {| hello |}]

let%expect_test _ =
  let word = "    " in 
  let bitMap = "****" in
  let result = String.mapi (translate word) bitMap in
  print_endline result;
  [%expect {|     |}]

let%expect_test _ =
  let word = "Hello" in
  let bitMap = "....." in
  let result = String.mapi (translate word) bitMap in
  print_endline result;
  [%expect {| ..... |}]