(* initialize Random with seed from /dev/urandom *)
Random.self_init;;

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
let params = {pop_size = 100; p_crossover = 0.95; p_mutation = 0.2; max_iterations = 100}

(* population of solutions *)
(* solution type with dna and computed fitness *)
type solution = {dna : int array ; fitness : float};;
(* an example of solution *)
(*let foo = {dna = [| 0; 1 |] ; fitness = 0.0};;*)


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

(* generate first population, invalid solutions are dropped *)
let rec sixth_day = function
        0 -> []
      | i -> let rec sol candidate =
                 if (is_valid candidate problem) then candidate
                 else sol (random_solution problem.stock) in
             sol (random_solution problem.stock) :: sixth_day (i-1);;

(* print selected data about a population *)
let print population =
    Print.fprintf stdout "Best candidate fitness = %f" (List.nth (List.sort (sol_compare) population) 1).fitness;;

(* example: generate initial population *)
(*let population = sixth_day params.pop_size;;*)

(* example of sort by fitness *)
(*List.sort (sol_compare) population;;

print_string "Hello world!\n";;*)


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
               let population = reproduce pparams.pop_size in
               (* return the best solution found, best, average and worst fitness of the cicle and then cicle again *)
               let best = List.nth (List.sort (sol_compare) population) 0 in
               [best] :: cicle population (i -1) in

    (* optimization *)
    cicle population pparams.max_iterations;;

(* start *)
search problem params;;
