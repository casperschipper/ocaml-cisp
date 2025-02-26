(* Utility function to compute greatest common divisor (GCD) *)
let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

(* Type definition for a fraction *)
type fraction = int * int

(* Function to simplify a fraction *)
let simplify (num, den) =
  if den = 0 then failwith "Denominator cannot be zero"
  else
    let g = gcd num den in
    let num = num / g in
    let den = den / g in
    if den < 0 then (-num, -den) else (num, den)

(* Function to add two fractions *)
let add (n1, d1) (n2, d2) = simplify ((n1 * d2) + (n2 * d1), d1 * d2)

(* Function to multiply two fractions *)
let multiply (n1, d1) (n2, d2) = simplify (n1 * n2, d1 * d2)

(* Function to divide two fractions *)
let divide (n1, d1) (n2, d2) =
  if n2 = 0 then failwith "Cannot divide by zero"
  else simplify (n1 * d2, d1 * n2)

(* Function to convert a fraction to string for display *)
let string_of_fraction (num, den) = string_of_int num ^ "/" ^ string_of_int den

(* Example usage *)
let () =
  let f1 = (1, 2) in
  let f2 = (3, 4) in
  Printf.printf "Addition: %s\n" (string_of_fraction (add f1 f2)) ;
  Printf.printf "Multiplication: %s\n" (string_of_fraction (multiply f1 f2)) ;
  Printf.printf "Division: %s\n" (string_of_fraction (divide f1 f2))
