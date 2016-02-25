(*
 * This is a naive implementation of a peresistant union-find
 * data structure.  We use a red black tree map as the underlying
 * implementation.  Entries get mapped to their parent, and the 
 * representative member of the class is found by following the 
 * chain of parents, until no parent is found.  This has the downside
 * of not being able to support path compression and that lookups/unions
 * take log time.  
 *)

functor NaiveUnionFind(
    Arg : ORD_KEY
) :
	sig
	    type t
	    val create : Arg.ord_key -> t
	    val find : t -> Arg.ord_key -> Arg.ord_key
	    val union : t -> Arg.ord_key -> Arg.ord_key -> t
	end = struct
	
	structure M = RedBlackMapFn(Arg)
				   
	type t = Arg.ord_key M.map
		     
	fun create n = M.empty
			   
	fun find s i =
	    case M.find(s, i)
	     of NONE => i
	      | SOME i' => find s i'
				
	fun union s i1 i2 =
	    let val s1 = find s i1
		val s2 = find s i2
	    in M.insert(s, s1, s2) end
		
		
	end
	    


