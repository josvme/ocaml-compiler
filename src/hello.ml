let rec gcd a b = 
  if a = b then 
    a 
  else if a > b then 
    gcd (a-b) b
  else 
    gcd a (b-a)
;;

let successors n edges = 
  let matching (s,_) = s = n in (* You should consider it as let equal a b = (a = b), where (a=b) gives true/false *)
    List.map snd (List.filter matching edges)
;;

let edges = [
("a", "b"); ("a", "c");
("a", "d"); ("b", "e");
("c", "f"); ("d", "e");
("e", "f"); ("e", "g") ]

let rec dfs edges visited = function 
  [] -> List.rev visited
  | n :: nodes -> 
    if List.mem n visited then 
      dfs edges visited nodes
    else 
      dfs edges (n::visited) ((successors n edges) @ nodes)
;;

print_endline "Starting GCD";
Printf.printf "%d" (gcd 100 50);