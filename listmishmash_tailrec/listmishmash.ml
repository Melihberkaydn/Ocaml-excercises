(*
Tail recursive implementation of the list mishmash
Details of the exercise can be found at readme
*)
let  interleave3 l1 l2 l3 = let rec reverseList acc = function [] -> acc
| x::xs -> reverseList (x::acc) xs in
  let rec interleave_tailrec acc l1 l2 l3 = match l1, l2, l3 with 
| [], [], [] -> acc
| l1h::l1r, [], [] -> (interleave_tailrec ((l1h::acc)) l1r l2 l3) 
| [], l2h::l2r, [] -> (interleave_tailrec ((l2h::acc)) l1 l2r l3) 
| [], [], l3h::l3r -> (interleave_tailrec ((l3h::acc)) l1 l2 l3r) 
| l1h::l1r, l2h::l2r, [] -> (interleave_tailrec ((l2h::l1h::acc)) l1r l2r l3) 
| l1h::l1r, [], l3h::l3r -> (interleave_tailrec ((l3h::l1h::acc)) l1r l2 l3r) 
| [], l2h::l2r, l3h::l3r -> (interleave_tailrec ((l3h::l2h::acc)) l1 l2r l3r) 
| l1h::l1r, l2h::l2r, l3h::l3r -> (interleave_tailrec ((l3h::l2h::l1h::acc)) l1r l2r l3r) in
reverseList [] (interleave_tailrec [] l1 l2 l3)
