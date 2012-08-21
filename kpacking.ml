
(* object type with value ancd weight *)
type obj = {value : float; weight : float};;

(* a stock of objects to pack *)
let stock = [{value = 0.5; weight=3.0};{value = 1.0; weight=2.0}];; 

(* gene type with dna and fitness *)
type gene = {fitness : float; dna : int array};;

(* an example of solution *)
let foo = {fitness = 0.0; dna = [| 0; 1 |]};;

(* compute fitness *)
let compute_fitness dna =
    
    (* funzione che ritorna un valore dall'indice dell'oggetto nello stock e dal numero di volte che viene preso *)
    let get_value obj_index n =
        ((List.nth stock obj_index).value *. (float_of_int n)) in

    Array.fold_left (+.) 0. (Array.mapi get_value dna);;

	
(* generate a new random gene with dna of lenght i and relative computed fitness *)
let random_gene length =

    (* generate a boolean dna of lenght i, note: not tail recursive *)
    let rec dna_gen = function
       0 -> []
     | i -> Random.int 2 :: dna_gen (i-1) in

    (* pick dna as an Array and compute fitness *)
    let dna = Array.of_list (dna_gen length) in
    let fitness = compute_fitness dna in

    {fitness; dna};;


