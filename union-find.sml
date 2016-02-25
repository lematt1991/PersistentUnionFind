(*
 * This is an efficient persistant implementation of a 
 * union-find data structure.  It uses mutation under the 
 * hood, but maintains a purely functional interface.
 * Currently, it only supports integers.  
 *)
structure UnionFind = 
	struct

	structure A = PersistantArray
	
	type elt = int (*elements of the data structure*)

	(*
	 * the type of the data structure.  `parent` is an array
	 * relating entries to their parent class.  `rank` is the
	 * size of the class
	 *)
	type t = {parent : elt A.array ref, rank : int A.array}

	(*create a disjoint set, each element belongs to its own class*)
	fun create n : t = {
	    parent = ref (A.init(n, fn i => i)),
	    rank = A.init(n, fn i => 0)
	}

	(*find which class an entry belongs do, performing path compression along the way*)
	fun find({parent, rank}, i) =
	    let fun lp(parent, j) =
		   let val x = A.get(parent, j)
		   in
		       if x = j
		       then (parent, x)  (*found the parent*)
		       else
			   let val (p, k) = lp(parent, x)  (*get the parent*)
			   in
			       if k = x
			       then (p, k)  (*don't set this if we just going to update it to the same thing*)
			       else (A.set(p, j, k), k)  (*path compression*)
			   end 
		   end
		val (p, j) = lp(!parent, i)
		val _ = parent := p
	    in j end

	(* Union two classes together.  The larger rank class becomes the 
	 * representative
	 *)
	fun union(s as {parent, rank}, i1, i2) =
	    let val p1 = find(s, i1)
		val p2 = find(s, i2)
	    in if p1 = p2
	       then s
	       else
		   let val r1 = A.get(rank, p1)
		       val r2 = A.get(rank, p2)
		   in if r1 > r2
		      then {parent=ref (A.set(!parent, p2, p1)), rank=rank}
		      else
			  if r2 > r1
			  then {parent=ref (A.set(!parent, p1, p2)), rank=rank}
			  else {parent=ref (A.set(!parent, p1, p2)), rank=A.set(rank, r2, A.get(rank, r1) + 1)}
		   end
	    end
		
	fun test1() =
	    let val s0 = create 10
		val _ = print(String.concat["s0: 3 -> ", Int.toString (find(s0, 3)), "\n"])
		val s1 = union(s0, 1, 4)	      
		val _ = print(String.concat["s1: 1 -> ", Int.toString (find(s1, 1)), "\n"])
		val _ = print(String.concat["s1: 4 -> ", Int.toString (find(s1, 4)), "\n"])
		val _ = print(String.concat["s0: 1 -> ", Int.toString (find(s0, 1)), "\n"])
		val _ = print(String.concat["s0: 4 -> ", Int.toString (find(s0, 4)), "\n"])
	    in () end

		
	end

val _ = UnionFind.test1()
