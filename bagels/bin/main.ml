open Bagels.Lib

let () = print_endline ("
  Bagels, a deductive logic game.
  
  I am thinking of a  " ^ string_of_int max_guesses ^ "-digit number with no repeated digits.
  Try to guess what it is. Here are some clues:
  When I say:     That means:
    Pico         One digit is correct but in the wrong position.
    Fermi        One digit is correct and in the right position.
    Bagels       No digit is correct.

  For example, if the secret number was 248 and your guess was 843, the
  clues would be Fermi Pico.");
  while true do
    let secretNum = getSecretNum([|1;2;3;4;5;6;7;8;9;0|]) in
    print_endline "I have thought up a number.";
    print_endline (" You have " ^ string_of_int max_guesses ^ " guesses to get it.");
    
    let num_guesses = ref 1 in
    let end_of_game = ref false in
    while (!num_guesses <= max_guesses) && not !end_of_game do
      let guess = ref String.empty in
      
      while (String.length !guess <= num_digits || not (is_valid_decimal !guess)) && not !end_of_game  do
        print_endline ("Guess #" ^ string_of_int !num_guesses ^ ": ");
        guess := read_line ();

        let clues = getClues (List.of_seq (String.to_seq !guess)) (List.of_seq (String.to_seq !secretNum)) in
        print_endline clues;
        num_guesses := !num_guesses + 1;
        
        match () with
          | _ when guess = secretNum -> end_of_game := true
          | _ when !num_guesses > max_guesses -> print_endline ("You ran out of guesses. The answer was " ^ !secretNum); end_of_game := true
          | _ -> ()
      done;
    done;

    print_endline "Do you want to play again? (yes or no)";
    if read_line () <> "yes" then
      exit 0
    else
      print_endline "Hooray! Let's play again."
  done;
    print_endline "Thanks for playing!";
