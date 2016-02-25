
signature PersistantArraySig =
sig
    type 'a array
    val init : int * (int -> 'a) -> 'a array
    val get : 'a array * int -> 'a
    val set : 'a array * int * 'a -> 'a array
    val printArr : 'a array * ('a -> string) -> unit
end

structure PersistantArray  =
struct

datatype 'a array = A of 'a data ref
     and 'a data = Arr of 'a Array.array
		 | Diff of int * 'a * 'a array

fun init(n, f) = A (ref (Arr (Array.tabulate(n, f))))

(* reverse the indirections from this Diff node (if any) to the 
 * array, swapping Diff values with the array as necessary.  This 
 * way if we access an old version of the array, we only have to 
 * pay for all the indirections once.
 *)
fun reroot(A arr) =
    case !arr
     of Arr _ => ()
      | Diff(i, v, x as A arr') => (
	  reroot x;
	  case !arr'  (*swap the Diff node in `arr` with the Arr node in `arr'`*)
	   of Arr data =>
	      let val old = Array.sub(data, i)
		  val _ = Array.update(data, i, v)
		  val _ = arr' := Diff(i, old, A arr)
		  val _ = arr := Arr data
	      in () end
	      )
fun get(x as A arr, i) = (
    reroot x;
    case !arr
     of Arr arr => Array.sub(arr, i)
      | Diff(i', v, next) => raise Fail "Impossible!"
)

fun set(x as A arr, i, v) = (
    reroot x;
    case !arr
     of Diff(i', v', n) => raise Fail "Impossible!"
      | Arr data =>
	let val old = Array.sub(data, i)
	    val _ = Array.update(data, i, v)
	    val new = A(ref(Arr data))
	    val _ = arr := Diff(i, old, new)
	in new end
)

fun printArr(A arr, toString) =
    case !arr
     of Diff(i, v, n) => (print (String.concat["Diff(", Int.toString i, ", ", toString v, ") -> "]);
			  printArr(n, toString))
      | Arr arr =>
	let val _ = print "["
	    val _ = Array.app (fn i => print ((toString i) ^ ", ")) arr
	    val _ = print "]\n"
      in () end


(*Tests*)

fun t1() =
    let val a0 = init(7, fn _ => 0)
	val a1 = set(a0, 1, 7)
	val a2 = set(a1, 2, 8)
	val a3 = set(a1, 2, 9)
	val _ = print "a3: "
	val _ = printArr(a3, Int.toString)
	val _ = print "a2: "			      
	val _ = printArr(a2, Int.toString)
	val _ = print "a1: "	
	val _ = printArr(a1, Int.toString)
	val _ = print "a0: "	
	val _ = printArr(a0, Int.toString)
    in () end


fun t2() = 
    let val a0 = init(7, fn _ => 0)
	val a1 = set(a0, 1, 7)
	val a2 = set(a1, 2, 8)
	val a3 = set(a1, 2, 9)
	val _ = reroot a1
	val _ = print "a3: "
	val _ = printArr(a3, Int.toString)
	val _ = print "a2: "			      
	val _ = printArr(a2, Int.toString)
	val _ = print "a1: "	
	val _ = printArr(a1, Int.toString)
	val _ = print "a0: "	
	val _ = printArr(a0, Int.toString)
    in () end
	
end

