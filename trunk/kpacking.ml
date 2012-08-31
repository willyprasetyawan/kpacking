open Printf;;

(* cli arguments *)
let opt_pop_size = ref 50;;
let opt_crossover = ref 0.95;;
let opt_mutation = ref 999.0;; (* not really, we don't know yet how many object
                                  are in the problem file *)
let opt_max_iterations = ref 100;;
let opt_file = ref "";;
let opt_first_generation_style = ref 1;;
let opt_solution = ref 0.0;;
let verbose = ref false;;

let usage = "usage: " ^ Sys.argv.(0) ^ " [-v] [-p population_size] \
          [-c crossover_chance] [-m mutation_chance] [-i iterations] -f file";;
 
let speclist = [
  ("-v", Arg.Unit (fun () -> verbose := true), ": verbose output");
  ("-p", Arg.Int (fun p -> opt_pop_size := p), ": population size \
                                                                [default 50]");
  ("-c", Arg.Float (fun c -> opt_crossover := c), ": chance of crossover \
                                                              [default 0.95]");
  ("-m", Arg.Float (fun m -> opt_mutation := m), ": chance of mutation \
                                                   [default 1/(num objects)]");
  ("-i", Arg.Int (fun i -> opt_max_iterations := i), ": iterations \
                                                               [default 100]");
  ("-g", Arg.Int (fun g -> opt_first_generation_style := g), ": first \
    generation style [0, 1 or 2. Default 1 for random genes (0 = zero filled, \
                                    2 = other style random gene generation)]");
  ("-s", Arg.Float (fun s -> opt_solution := s), ": optimal known solution \
                 (the program will exits if found before the last iteration)");
  ("-f", Arg.String (fun f -> opt_file := f), ": file with problem data");
]
 
(* read cli arguments *)
let () =
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;;

(* problem parameters, loaded from file *)
type obj = {value: float; weight: float};;
type problem_parameters = {stock: obj list; max_weight: float};;

(* load problem from file *)

(* explode a string to a list of words *)
let rec explode sep str =
  try
    let i = String.index str sep in
      String.sub str 0 i :: explode sep (String.sub str (i+1) 
                                                     (String.length str -i -1))
  with Not_found -> [str];;

(* load a file as a list of string, skip lines beginning with # *)
let open_file file =
  let f = open_in file in
  let output = ref [] in
  try
    while true do
      let l = input_line f in
      if (l.[0] != '#') then  output := l :: !output
    done; []
  with End_of_file -> close_in f;
  List.rev !output;;

let lines = open_file !opt_file;;

(* try to interpret it *)
(* first line, weight capacity *)
let max_weight = float_of_string (List.hd lines);;
    
(* from object line to object *)
let new_object l =
  let o = explode ' ' l in
  {value = float_of_string (List.hd o);
   weight = float_of_string (List.nth o 1)};;

let stock = List.map new_object (List.tl lines);;
let problem = {stock; max_weight};;


(* if mutation chance is not set, by default we take 1/(num objects) *)
if (!opt_mutation = 999.0) 
  then opt_mutation := (1.0 /. float_of_int (List.length problem.stock));;

(* algorithm parameters definition, from cli or defaults *)
type algorithm_parameters = {pop_size: int;
                             p_crossover: float;
                             p_mutation: float;
                             max_iterations: int;
                             first_population_style: int;
                             solution: float};;
let params = {pop_size = !opt_pop_size;
              p_crossover = !opt_crossover; 
              p_mutation = !opt_mutation;
              max_iterations = !opt_max_iterations;
              first_population_style = !opt_first_generation_style;
              solution = !opt_solution};;
let () = if (!verbose) then printf "population size = %d\
                                    chance of crossover = %f\n\
                                    chance of mutation = %f\n\
                                    iterations = %d\n\
                                    first generation style = %d\n\n%!"
                                    params.pop_size params.p_crossover 
                                    params.p_mutation params.max_iterations 
                                    params.first_population_style;


(* population of solutions *)
(* solution type with dna and computed fitness *)
type solution = {dna : int array ; fitness : float};;
(* an example of solution *)
(*let foo = {dna = [| 0; 1 |] ; fitness = 0.0};;*)

(* output record *)
type output = {best_dna: int array;
               best_fitness: float;
               mean_fitness: float;
               worst_fitness: float};;


(* compute fitness from dna *)
let compute_fitness dna stock =
  (* get_value return a value from an index of an obj in stock
     and the number of times it get picked *)
  let get_value obj_index n =
    ((List.nth stock obj_index).value *. (float_of_int n)) in
  Array.fold_left (+.) 0. (Array.mapi get_value dna);;

(* compute weight of a solution *)
let weight solution =
  (* get_weight return a weight from an index of an obj in stock 
     and the number of times it get picked *)
  let get_weight obj_index n =
    ((List.nth problem.stock obj_index).weight *. (float_of_int n)) in
  Array.fold_left (+.) 0. (Array.mapi get_weight solution.dna);;

(* check if a solution is valid (weight<=max_weight) *)
let is_valid solution problem =
  if weight solution > problem.max_weight then false else true;;


(* generate a new random solution for the stock of objects
   note: very unoptimized for problems with low knapsack capacity,
         in that case very few solutions from random_solution are valid *)
let random_solution stock =
  (* generate a boolean dna of lenght i*)
  let rec dna_gen = function
      0 -> []
    | i -> Random.int 2 :: dna_gen (i-1) in
  (* pick dna as an Array and compute fitness *)
  let dna = Array.of_list (dna_gen (List.length stock)) in
  let fitness = compute_fitness dna stock in
  {dna; fitness};;

(* generate a new solution with a zero-filled dna *)
let zerofilled_solution stock =
  let dna = Array.make (List.length stock) 0 in
  let fitness = 0.0 in
  {dna; fitness};;

(* a different generation for a random solution, 
   build only valid solutions iteratively *)
let random_valid_solution stock =
  (* initialize dna to all 0 *)
  let dna = Array.make (List.length stock) 0 in
  (* modify a random place in dna till dna length or an invalid dna *)
  let rec add_random new_dna = function
      0 -> new_dna
    | i -> let old_dna = (Array.copy new_dna) in
           let () = Array.set new_dna (Random.int (Array.length new_dna))
                                                              (Random.int 2) in
           let fitness = 0.0 in
           if (is_valid {dna = new_dna; fitness} problem)
             then add_random new_dna (i -1)
             else old_dna in
  let dna = add_random dna (Array.length dna) in
  let fitness = compute_fitness dna stock in
  {dna; fitness};;


(* point mutation *)
let mutate solution p_mutation =
  let length = Array.length solution.dna in
  let rec inner dna = function
      0 -> dna
    | i -> let new_dna = if (p_mutation >= Random.float 1.)
                         then Array.concat [(Array.sub solution.dna 0 i);
                              (Array.make 1 (Random.int 2));
                              (Array.sub solution.dna (i+1) (length -i -1))]
                         else dna in
                         inner new_dna (i-1) in
  let dna = inner solution.dna (length - 1) in
  let fitness = compute_fitness dna problem.stock in
  {dna; fitness};;

(* one-point crossover, one child is returned
   note: only valid for parents with length at last 2 *)
let crossover parent other_parent =
  let length = Array.length parent.dna in
  let i = (Random.int (length -1) +1) in (* index _after_ which to crossover *)
  let dna_son = Array.concat [(Array.sub parent.dna 0 i); 
                              (Array.sub other_parent.dna i (length -i))] in
  let fitness_son = compute_fitness dna_son problem.stock in
  {dna = dna_son; fitness = fitness_son};;

(* how to compare solutions *)
let sol_compare a b =
  (* _inverse_ order, best fitness first *)
  compare b.fitness a.fitness;;

(* example of sort by fitness *)
(*List.sort (sol_compare) population;;*)

(* compute the mean fitness of a population of solutions *)
let mean population =
  let get_fitness sol = sol.fitness in
  let sum = List.fold_left (+.) 0. (List.map get_fitness population) in
  sum /. (float_of_int (List.length population));;

(* binary tournament selection
   "two men enter, one man leaves." *)
let thunderdome population =
  let bound = List.length population in
  (* select two _differents_ int between 0 (inclusive) and bound (esclusive)
     note: bound _at last_ = 2 is assumed *)
  let i, j = Random.int bound, Random.int bound in
  let rec must_be_differents i j =
    if j = i then must_be_differents i (Random.int bound) in
  (* selection *)
  let a, b = List.nth population i, List.nth population j in
  if a.fitness >= b.fitness then a else b;;


(* select first generation strategy by option *)
exception Fail of string;;
let first_generation_solution = 
  match params.first_population_style with
      0 -> zerofilled_solution
    | 1 -> random_solution
    | 2 -> random_valid_solution
    | i -> raise (Fail "wrong argument; option `-g' expects 0, 1 or 2");;

(* generate first population, invalid solutions are dropped *)
let rec sixth_day generator = function
    0 -> []
  | i -> let rec sol candidate =
           if (is_valid candidate problem) 
             then (if (!verbose) then printf "candidates: %4d\r%!" i; candidate)
             else sol (generator problem.stock) in
           sol (generator problem.stock) :: sixth_day generator (i-1);;

(* example: generate initial population *)
(*let population = sixth_day params.pop_size;;*)


(* print selected output data about a cicle *)
let print_output record =
  let dna, best, mean, worst = record.best_dna, record.best_fitness, 
                                   record.mean_fitness, record.worst_fitness in
  printf "%s,%f,%f,%f\n" (String.concat "" (Array.to_list (Array.map 
                                         string_of_int dna))) best mean worst;;

(* return the best solution and the iteration when it was found from a run
   as a tuple from out (note: out has inverted generation) *)
let rec select_best i = function
    []   -> invalid_arg "empty list"
  | [h]  -> {dna = h.best_dna; fitness = h.best_fitness}, i +1
  | h::t -> let best = select_best (i +1) t in
            let now = {dna = h.best_dna; fitness = h.best_fitness}, i in
            if (fst now).fitness > (fst best).fitness then now else best;;

(* print a solution *)
let print_solution sol =
  printf "dna: %s, fitness: %f" (String.concat "" (Array.to_list (Array.map 
                                         string_of_int sol.dna))) sol.fitness;;


(* main optimization flow *)
let search pdata pparams =

  (* initial population *)
  let population = sixth_day first_generation_solution pparams.pop_size in
    
  (* reproduction of children implementation *)
  let rec reproduce old_generation = function
      0 -> []
    | i -> let p1, p2 = 
                      thunderdome old_generation, thunderdome old_generation in
           (* crossover step *)
           let child = if (pparams.p_crossover >= Random.float 1.) 
             then crossover p1 p2 else p1 in
           (* mutation step *)
           let child = mutate child pparams.p_mutation in
           (* is the new solution a valid solution for the problem? *)
           let child = if (is_valid child problem)
             then child::reproduce old_generation (i -1)
             else reproduce old_generation i in
           child in

  (* cicle until the stop condition is reached *) 
  let rec cicle population best_fitness = function
    (* stop condition *)
      0 -> [] 
    | i -> (* reproduce a new generation *)
           let population = reproduce population pparams.pop_size in
           (* return a record of output with the best solution, and the best,
              mean, and worst fitness of the cicle, then cicle again
              note: may be worth to take beast and worst with a single scan
                    of the list *)
           let sorted = List.sort (sol_compare) population in
           let best = List.hd sorted in
           let new_best_fitness = if best_fitness > best.fitness
                              then best_fitness
                              else best.fitness in            
           (* debug and status output on stdout *)
           if (!verbose) then printf "generations to go: %6d, \
                                   best fitness: %8.2f\r%!" i new_best_fitness;
           let worst = List.nth sorted ((List.length population) -1) in
           let meanf = mean population in

           (* exit if optimal solution is found or start next iteration *)
           if (pparams.solution!=0.0 && new_best_fitness=pparams.solution)
              then {best_dna = best.dna; best_fitness = best.fitness; 
                    mean_fitness = meanf; worst_fitness = worst.fitness} :: []
              else {best_dna = best.dna; best_fitness = best.fitness; 
                    mean_fitness = meanf; worst_fitness = worst.fitness} :: 
                    cicle population new_best_fitness (i -1) in

  (* starting the optimization *)
  if (!verbose) then printf "generations to go: %6d\r%!" pparams.max_iterations;
  cicle population 0. pparams.max_iterations;;



(* initialize Random with seed from /dev/urandom *)
Random.self_init ();;

(* start search *)
let out = search problem params;;

(* output *)
(*print_endline "\nResults";;*)
printf "\nPartial results\n";;
printf "#Best candidate, Best candidate fitness, Mean fitness, Worst \
                                                         candidate fitness\n";;
List.iter print_output out;;

(* note: out is in reverse order *)
let best_out = select_best 0 out;;
printf "\nBest overall solution\n";;
printf "iteration: %d, " (snd best_out);;
print_solution (fst best_out);
printf "\n";;

