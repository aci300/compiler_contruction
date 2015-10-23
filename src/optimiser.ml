(* An optimiser  *) 


(* Checks to see if an expression contains only maths *)


let rec isMath (exp : expression) = match exp with
  | Plus (n, m) -> true && isMath n && isMath m
  | Times (n, m) -> true && isMath n && isMath m
  | Minus (n, m) -> true && isMathsOnly n && isMathsOnly m
  | Div (n, m) -> true && isMath n && isMath m
  | Mod (n, m) -> true && isMath n && isMath m
							       
  | _ -> false
