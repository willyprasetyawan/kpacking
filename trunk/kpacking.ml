
(* problem parameters, TODO load from file *)
type obj = {value: float; weight: float};;
type problem_parameters = {stock: obj list; max_weight: float};;
let example_stock = [{value = 0.5; weight=3.};{value = 1.; weight=2.};{value = 1.5; weight=2.}];;
let problem = {stock = example_stock; max_weight = 3.};;

(* algorithm parameters definition, TODO load from file or cl *)
type algorithm_parameters = {pop_size: int; p_crossover: float; p_mutation: float};;
let params = {pop_size = 20; p_crossover = 0.95; p_mutation = 0.2}

(* population of solutions *)
(* solution type with dna and computed fitness *)
type solution = {dna : int array ; fitness : float};;
(* an example of solution *)
(*let foo = {dna = [| 0; 1 |] ; fitness = 0.0};;*)


(* compute fitness *)
let compute_fitness dna stock =
    (* get_value return a value from an index of an obj in stock and the number of times it get picked *)
    let get_value obj_index n =
        ((List.nth stock obj_index).value *. (float_of_int n)) in
    Array.fold_left (+.) 0. (Array.mapi get_value dna);;

(* generate a new random gene for the stock of objects *)
let random_solution stock =
    (* generate a boolean dna of lenght i, note: not tail recursive *)
    let rec dna_gen = function
        0 -> []
      | i -> Random.int 2 :: dna_gen (i-1) in
    (* pick dna as an Array and compute fitness *)
    let dna = Array.of_list (dna_gen (List.length stock)) in
    let fitness = compute_fitness dna stock in
    {dna; fitness};;

(* check if a solution is valid (weight<=max_weight) *)
let is_valid solution problem =
    (* get_weight return a weight from an index of an obj in stock and the number of times it get picked *)
    let get_weight obj_index n =
        ((List.nth problem.stock obj_index).weight *. (float_of_int n)) in
    let solution_weight = Array.fold_left (+.) 0. (Array.mapi get_weight solution.dna) in
    if solution_weight > problem.max_weight then false
                                            else true;;
(* point mutation *)
let mutate solution =
    let length = Array.length solution.dna in
    let i = Random.int length in (* index of the element to mutate *)
    let dna = Array.concat [(Array.sub solution.dna 0 i); (Array.make 1 (Random.int 2)); (Array.sub solution.dna (i+1) (length -i -1))] in
    let fitness = compute_fitness dna problem.stock in
    {dna; fitness};;

(* one-point crossover, children returned as a tuple
   note: only valid for parents with length at last 2 *)
let crossover parent other_parent =
    let length = Array.length parent.dna in
    let i = (Random.int (length -1) +1) in (* index of the element _after_ which we'll do the crossover *)
    let dna_son = Array.concat [(Array.sub parent.dna 0 i); (Array.sub other_parent.dna i (length -i))] in
    let dna_daughter = Array.concat [(Array.sub other_parent.dna 0 i); (Array.sub parent.dna i (length -i))] in
    let fitness_son = compute_fitness dna_son problem.stock in
    let fitness_daughter = compute_fitness dna_daughter problem.stock in
    {dna = dna_son; fitness = fitness_son}, {dna = dna_daughter; fitness = fitness_daughter};;

(* generate first population, invalid solutions are dropped *)
let rec sixth_day = function
        0 -> []
      | i -> let rec sol candidate =
                 if (is_valid candidate problem) then candidate
                 else sol (random_solution problem.stock) in
             sol (random_solution problem.stock) :: sixth_day (i-1);;

(* note: after mutation, crossover etc. check for solution validity *)

(* initialize Random with seed from /dev/urandom *)
Random.self_init;;

(* example: generate a new random solution *)
(*random_solution problem.stock;;*)

(* example: generate initial population *)
let population = sixth_day params.pop_size;;
let a = List.nth population 0;;
let b = List.nth population 1;;

print_string "Hello world!\n";;
