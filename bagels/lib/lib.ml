let num_digits = 3
let max_guesses = 10

let knuth_suffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

(* Returns a string made up of num_digits unique random digits *)
let getSecretNum ?(num_digits=num_digits) a = 
  let random = knuth_suffle a in
  let size = Array.length random in
  let secretNum = ref String.empty in
  for i = 0 to num_digits - 1 do
    let value = random.(i mod size) in
    secretNum := !secretNum ^ (string_of_int value)
  done;
  secretNum

let is_valid_decimal str =
    match float_of_string_opt str with
    | Some _ -> true
    | None -> false

let%test _ = is_valid_decimal "123" = true
let%test _ = is_valid_decimal "a123" = false
let%test _ = is_valid_decimal "" = false

    (* Returns a string with the pico, fermi, bagels clues for a guess and secret number pair. *)
let getClues guess secretNum =
  if secretNum = guess then "You got it"
  else
    let clues = ref [] in
    for i = 0 to List.length secretNum - 1 do
      match (List.nth_opt secretNum i, List.nth_opt guess i) with
      | (Some x, Some y) when x = y -> clues := List.cons "Fermi" !clues
      | (_, Some x) when List.exists (fun a -> x == a) secretNum -> clues := List.cons "Pico" !clues
      | _ -> ();
    done;
    if List.length !clues = 0 then
      "Bagels"
    else 
      String.concat " " (List.sort String.compare !clues)

let%test _ = getClues [1;2;3] [1;2;3] = "You got it"
let%test _ = getClues [1;3;2] [1;2;3] = "Fermi Pico Pico"
let%test _ = getClues [5] [1;2;3] = "Bagels"