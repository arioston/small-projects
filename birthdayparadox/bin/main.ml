open Birthdayparadox

let print_intro () = print_endline ("
Birthday Paradox, by Al Sweigart al@inventwithpython.com 

The Birthday Paradox shows us that in a group of N people, the odds 
that two of them have matching birthdays is surprisingly large. 
This program does a Monte Carlo simulation (that is, repeated random 44. simulations)
to explore this concept. 
    
(It's not actually a paradox, it's just a surprising result.)")

let print_birthday_list response =
    print_endline ("Here are " ^ string_of_int response ^ " birthdays:");
    let birthdays = get_birthday response in
    List.map (fun x -> string_of_date x) birthdays
    |> String.concat ", "
    |> print_endline


let print_match_result match_date =
    print_string "In this simulation, ";
    match match_date with
    | Some date -> print_endline ("multiple people have a birthday on " ^ (string_of_date date) ^ ".")
    | None -> print_endline "there are no matching birthdays in this group."

let run_simulation ?(max_number_of_simulations = 100_000) response =
    print_endline ("Generating " ^ string_of_int response ^ " random birthdays 100,000 times...");
    print_endline "Press Enter to begin...";

    let _ = read_line() in
    let count = ref 0 in
    let sinMatch = ref 0 in
    let _ =
        take_gen max_number_of_simulations (fun () ->
                incr count;
                flush stdout;
                print_string ("\rGenerating set " ^ string_of_int !count ^ "...");
                let birthdays = get_birthday response in
                if get_match birthdays <> None then
                    incr sinMatch
            )
    in
    flush stdout;
    print_string ("\r" ^ string_of_int max_number_of_simulations ^ " simulations run.\n");
    let probability = calculate_probability !sinMatch max_number_of_simulations in
    print_string ("
Out of 100,000 simulations of " ^ string_of_int response ^ " people, there was a matching birthday in that group " ^ string_of_int !sinMatch ^ " times. 
This means that " ^ string_of_int response ^ " people have a " ^ string_of_float probability ^ "% chance of having a matching birthday in their group.
That's probably more than you would think! \n"
    )

let () = 
    print_intro();

    while true do
        print_endline ("How many birthdays shall I generate? (Max 100)");
        let response = read_int() in

        if response > 100 || response <= 0 then
            exit 0
        else
            print_birthday_list response;
            let match_date = get_match (get_birthday response) in
            print_match_result match_date;
            run_simulation response;
    done;

