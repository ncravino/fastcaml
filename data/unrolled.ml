(*Unrolled Linked List Implementation in OCaml*)
module Unrolled = struct
  type 'a elem = E of 'a | Nil
  type  'a list = N of 'a elem array * int * 'a list | Empty 

  let empty = Empty 

  let block_size = 8

  let rec add ?(tail=Empty) list e  = 
    match list with
      |Empty -> 
          let arr = Array.make block_size Nil in
          let () = arr.(block_size-1) <- E e in
            N (arr,1,tail)
      |N (a,idx,next) when idx < block_size ->
          let () = a.(block_size-idx-1)<- E e in
            N(a,idx+1,next)
      |l ->
          add ~tail:l Empty e


  let fold l ~f ~init =
    let rec fold_a acc a i =
      if (i<block_size) then
        match a.(i) with
          |E x -> fold_a (f acc x) a (i+1)
          |Nil -> acc
      else
        acc
    in
    let rec fold_aux l acc = 
      match l with 
        |Empty -> acc
        |N (a,idx,next) ->
            let acc = fold_a acc a (block_size - idx) in
              fold_aux next acc
    in
      fold_aux l init

  let iter l ~f =
    let rec iter_a a i =
      if (i<block_size) then
        match a.(i) with
          |E x -> let () = f x in iter_a  a (i+1)
          |Nil -> ()
      else
        ()
    in
    let rec iter_aux l = 
      match l with 
        |Empty -> ()
        |N (a,idx,next) ->
            let () = iter_a  a (block_size - idx) in
              iter_aux next 
    in
      iter_aux l 



  let rev l = 
    fold l ~f:(fun a c -> add a c) ~init:empty


  let filter l ~f = fold l 
                      ~f:(fun a c -> if(f c) then add a c else a)
                      ~init:empty


  let remove_all l e = fold l 
                         ~f:(fun a c -> if(c = e) then a else add a c)
                         ~init:empty


  let to_list l = 
    let rl = fold l ~f:(fun a c -> c::a) ~init:[] in
      List.rev rl

end

let () = 
  let x = [1;2;3;4;5;6;7;8;9;10] in
  let r = List.fold_left (fun a i -> Unrolled.add a i) Unrolled.empty x in
  let y = Unrolled.to_list (Unrolled.rev r) in
    assert(x=y) 



