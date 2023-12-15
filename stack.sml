signature STACK =
sig

    type 'a stack
    exception EmptyStack
    exception Error of string
    val create: 'a stack
    val push : 'a -> 'a stack -> 'a stack
    val pop : 'a stack -> 'a stack
    val merge : 'a stack -> 'a stack -> 'a stack
    val top : 'a stack -> 'a
    val empty: 'a stack -> bool
    val poptop : 'a stack -> ('a * 'a stack) option
    val nth : 'a stack -> int -> 'a
    val drop : 'a stack  ->  int -> 'a stack
    val depth : 'a stack -> int
    val app : ('a -> unit) -> 'a stack -> unit
    val map : ('a -> 'b) -> 'a stack -> 'b stack
    val mapPartial : ('a -> 'b option) -> 'a stack -> 'b stack
    val find : ('a -> bool) -> 'a stack -> 'a option
    val filter : ('a -> bool) -> 'a stack -> 'a stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
    val exists : ('a -> bool) -> 'a stack -> bool
    val all : ('a -> bool) -> 'a stack -> bool
    val list2stack : 'a list -> 'a stack (* Convert a list into a stack *)
    val stack2list: 'a stack -> 'a list (* Convert a stack into a list *)
    val toString: ('a -> string) -> 'a stack -> string

end

structure FunStack : STACK =
struct
    exception EmptyStack
    exception Error of string
    type 'a stack = 'a list
    val create: 'a stack = []
    fun push x s = (x::s)

    fun split (h::t) = (h,t)
        | split [] = raise EmptyStack
    fun pop s = #2(split s)
    fun top s = #1(split s)
    fun empty [] = true
        | empty (h::t) = false
    fun poptop [] = NONE
        | poptop (h::t) = SOME (h,t)
    fun nth s n = List.nth(s,n)
    fun drop s n = List.drop(s,n)
    fun depth l = List.length l
    fun app f l = List.app f l
    fun map f s = List.map f s
    fun mapPartial f s = List.mapPartial f s
    fun find f s = List.find f s
    fun filter f s = List.filter f s
    fun foldl f s = List.foldl f s
    fun foldr f s = List.foldr f s
    fun exists f s = List.exists f s
    fun all f s = List.all f s
    fun stack2list s : ('a stack) = s : ('a list)
    fun list2stack s : 'a list = s : 'a stack
    fun merge a b = List.concat [a,b]
    fun phew f [] = ""
        | phew f (h::t) = let val a =  f h ^ ", " ^ phew f t in  a  end
    fun toString f s = let val a = phew f s in "[ "^a ^" ]" end
end