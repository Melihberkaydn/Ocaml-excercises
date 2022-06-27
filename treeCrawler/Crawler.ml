
type tree = Empty 
          | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop


type path =
| Top
| Lft of path * int * tree
| Rght of tree * int * path

(*Crawls the given binary search tree according to the command list *)
let crawl cmds tree = 
let go_up = function
| _, Top -> invalid_arg "go_up"
| l, Lft (p, x, r) | r, Rght (l, x, p) ->
Node (x, l, r), p 
in
let go_left = function
| Empty, _-> invalid_arg "go_left"
| Node (x, l, r), p -> l, Lft (p, x, r)
in
let go_right = function
| Empty,_ -> invalid_arg "go_right"
| Node (x, l, r), p -> r, Rght (l, x, p) 
in
let rec giveInitTree (currentTree,path) = match path with Top -> currentTree
| _ -> giveInitTree (go_up (currentTree, path))
in
let rec crawler commands (currentTree, path) stack = match commands with [] -> giveInitTree(currentTree, path)
| x::xs ->  match x with Right -> crawler xs (go_right (currentTree,path)) stack
| Left -> crawler xs (go_left (currentTree,path)) stack
| Up -> crawler xs (go_up (currentTree,path))stack
| Delete -> crawler xs (Empty,path) stack
| New(k) -> crawler xs (Node (k, Empty, Empty),path) stack
| Push -> let stack = [currentTree]@stack in crawler xs (currentTree, path) stack
| Pop -> let firstElement = List.hd stack in let stack = List.tl stack in crawler xs (firstElement, path) stack
in crawler cmds (tree, Top) []

