module Vlist = struct
  
  type 'a header = {data : 'a array; offset:int;next : 'a t} and 'a t = L of 'a header | Nil
  
  let empty = Nil
  
  let cons elem list = 
    match list with
    |Nil -> let data = Array.make 8 elem in
      L {data;offset=0;next=Nil}
    |L l when l.offset = 7 ->
	    let data = Array.make 8 elem in
	    let () = data.(0) <- elem in
	    L {data;offset=0;next=list}
    |L l -> 
	    let newdata = Array.copy l.data in
	    let newoffset = l.offset + 1 in
	    let () = newdata.(newoffset) <- elem in
	    L {data=newdata;offset=newoffset;next=l.next}

  let head l = 
    match l with
    |Nil -> None 
    |L l -> Some l.data.(l.offset)

  let tail lst = 
	  match lst with 
	  |Nil -> Nil
	  |L l ->
	    if l.offset = 0 then
		    l.next 
	    else 
	      L {l with offset = l.offset-1}
(*wip*)
  let fold ~f ~init l = 
  	let rec loop acc data next offset =
  		let x= data.(offset) in
  		let res = (f acc x) in
  		let newoffset = offset - 1 in
  		if newoffset < 0 then 
  		match next with 
  		| Nil -> res 
  		| L nxt -> loop acc nxt.data nxt.next 7
  		else
  		loop acc data next newoffset
  	in
  	match l with 
  	|Nil -> init
  	| L lst -> loop init lst.data lst.next lst.offset

  let of_list l =
	List.fold_left (fun acc e -> cons e acc) empty (List.rev l)
  
  let of_rep_elem elem count =
  	let rec loop acc counter =
		if counter > 0 then
			loop (cons elem acc) (counter - 1)
		else
			acc
	in
	loop empty count	  
end
(*simple testing *)
let lll= Vlist.of_list [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18];;
let x = Vlist.(head (tail lll));;
let megalst = Vlist.of_rep_elem 1 10_000_000;;
