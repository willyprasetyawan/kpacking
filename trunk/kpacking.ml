(* initialize Random with seed from /dev/urandom *)
Random.self_init;;

open Printf;;

(* problem parameters, TODO load from file *)
type obj = {value: float; weight: float};;
type problem_parameters = {stock: obj list; max_weight: float};;
let example_stock = [{value = 92.0; weight=23.0};
                     {value = 57.0; weight=31.0};
                     {value = 49.0; weight=29.0};
                     {value = 68.0; weight=44.0};
                     {value = 60.0; weight=53.0};
                     {value = 43.0; weight=38.0};
                     {value = 67.0; weight=63.0};
                     {value = 84.0; weight=85.0};
                     {value = 87.0; weight=89.0};
                     {value = 72.0; weight=82.0}];;
let problem = {stock = example_stock; max_weight = 165.0};;

(* algorithm parameters definition, TODO load from file or cl *)
type algorithm_parameters = {pop_size: int; p_crossover: float; p_mutation: float; max_iterations : int};;
let params = {pop_size = 50; p_crossover = 0.95; p_mutation = 0.2; max_iterations = 100}

(* population of solutions *)
(* solution type with dna and computed fitness *)
type solution = {dna : int array ; fitness : float};;
(* an example of solution *)
(*let foo = {dna = [| 0; 1 |] ; fitness = 0.0};;*)

(* output record *)
type output = {best_dna: int array; best_fitness: float; mean_fitness: float; worst_fitness: float};;

(* compute fitness
   note: as value of the solution *)
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

(* compute weight of a solution *)
let weight solution =
    (* get_weight return a weight from an index of an obj in stock and the number of times it get picked *)
    let get_weight obj_index n =
        ((List.nth problem.stock obj_index).weight *. (float_of_int n)) in
    Array.fold_left (+.) 0. (Array.mapi get_weight solution.dna);;

(* check if a solution is valid (weight<=max_weight) *)
let is_valid solution problem =
    if weight solution > problem.max_weight then false
                                            else true;;
(* point mutation *)
let mutate solution =
    let length = Array.length solution.dna in
    let i = Random.int length in (* index of the element to mutate *)
    let dna = Array.concat [(Array.sub solution.dna 0 i); (Array.make 1 (Random.int 2)); (Array.sub solution.dna (i+1) (length -i -1))] in
    let fitness = compute_fitness dna problem.stock in
    {dna; fitness};;

(* one-point crossover, one child is returned
   note: only valid for parents with length at last 2 *)
let crossover parent other_parent =
    let length = Array.length parent.dna in
    let i = (Random.int (length -1) +1) in (* index of the element _after_ which we'll do the crossover *)
    let dna_son = Array.concat [(Array.sub parent.dna 0 i); (Array.sub other_parent.dna i (length -i))] in
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
    let get_fitness sol =
        sol.fitness in
(*    let () = List.map get_fitness population;;*)
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
    if a.fitness >= b.fitness then a
                              else b;;

(* n-way tournament selection *)
(*
let tournament population n =
	let rec inner = function 
		0 -> []
		| i -> (List.nth population (Random.int (List.length population)))::inner (i-1) in
	List.hd(List.sort (cmp_genes) (inner n));;
*)

(* generate first population, invalid solutions are dropped
   note: very unoptimized for problems with low knapsack capacity,
         in that case only solutions vith a few non zero are valid *)
let rec sixth_day = function
        0 -> []
      | i -> let rec sol candidate =
                 if (is_valid candidate problem) then candidate
                 else sol (random_solution problem.stock) in
             sol (random_solution problem.stock) :: sixth_day (i-1);;

(* example: generate initial population *)
(*let population = sixth_day params.pop_size;;*)



(* print selected output data about a cicle *)
let print_output record =
    (*fprintf stdout "Best candidate fitness = %f, worst %f \n" record.best_fitness record.worst_fitness;;*)
    let dna, best, mean, worst = record.best_dna, record.best_fitness, record.mean_fitness, record.worst_fitness in
    printf "%s,%f,%f,%f\n" (String.concat "" (Array.to_list (Array.map string_of_int dna))) best mean worst;;

(* return the best solution from a run *)
let rec select_best = function
      []   -> invalid_arg "empty list"
    | [h]  -> {dna = h.best_dna; fitness = h.best_fitness}
    | h::t -> let best = select_best t in 
              if h.best_fitness < best.fitness then {dna = h.best_dna; fitness = h.best_fitness}
                                                    else best;;

(* print a solution *)
let print_solution sol =
    printf "dna: %s, fitness: %f" (String.concat "" (Array.to_list (Array.map string_of_int sol.dna))) sol.fitness;;



(* main optimization flow *)
let search pdata pparams =

    (* initial population *)
    let population = sixth_day pparams.pop_size in
    
    (* reproduction of children implementation *)
    let rec reproduce = function
          0 -> []
        | i -> let p1, p2 = thunderdome population, thunderdome population in
               (* crossover step *)
               let child = if (pparams.p_crossover >= Random.float 1.) then crossover p1 p2
                                                                       else p1 in
               (* mutation step *)
               let child = if (pparams.p_mutation >= Random.float 1.) then mutate child
                                                                      else child in
               (* is the new solution a valid solution for the problem? *)
               let child = if (is_valid child problem) then child::reproduce (i -1)
                                                       else reproduce i in
               child in

    (* cicle until the stop condition is reached *) 
    let rec cicle population = function
        (* stop condition *)
          0 -> [] 
        | i -> (* reproduce a new generation *)
               (* debug and status output on stdout *)
               (*let () = print_string "\r" in*) (* return to the beginning of the line *)
               let () = print_string "generations to go: " in
               let () = print_int i in
               let () = print_string "\n" in
               let population = reproduce pparams.pop_size in
               (* return a record of output with the best solution, and the best, mean, and worst fitness of the cicle,
                  then cicle again *)
               let best = List.nth (List.sort (sol_compare) population) 0 in
               let worst = List.nth (List.sort (sol_compare) population) ((List.length population) -1) in
               let meanf = mean population in
               {best_dna = best.dna; best_fitness = best.fitness; mean_fitness = meanf; worst_fitness = worst.fitness} :: cicle population (i -1) in

    (* starting the optimization *)
    cicle population pparams.max_iterations;;


(* start search *)
let out = search problem params;;

(* output *)
(*print_endline "\nResults";;*)
printf "\nPartial results\n";;
printf "#Best candidate, Best candidate fitness, Mean fitness, Worst candidate fitness\n";;
List.iter print_output out;;

printf "\nBest overall solution\n";;
print_solution (select_best out);;
printf "\n";;





