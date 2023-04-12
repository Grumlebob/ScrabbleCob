module MultiSet

   type MultiSet<'T when 'T: comparison> = MultiSet of Map<'T, uint32>

   val empty : MultiSet<'a>
   
   val isEmpty : MultiSet<'T> -> bool

   val size : MultiSet<'T> -> uint32

   val contains : 'T -> MultiSet<'T> -> bool
    
   val numItems : 'T -> MultiSet<'T> -> uint32

   val add : 'T -> uint32 -> MultiSet<'T> -> MultiSet<'T>

   val addSingle : 'T -> MultiSet<'T> -> MultiSet<'T>

   val remove : 'T -> uint32 -> MultiSet<'T> -> MultiSet<'T>

   val removeSingle : 'T -> MultiSet<'T> -> MultiSet<'T>

   val fold : ('a -> 'T -> uint32 -> 'a) -> 'a -> MultiSet<'T> -> 'a

   val foldBack : ('T -> uint32 -> 'a -> 'a) -> MultiSet<'T> -> 'a -> 'a
    
   val ofList : 'T list -> MultiSet<'T>

   val toList : MultiSet<'T> -> 'T list

   val map : ('A -> 'B) -> MultiSet<'A> -> MultiSet<'B>

   val union : MultiSet<'T> -> MultiSet<'T> -> MultiSet<'T>

   val sum : MultiSet<'T> -> MultiSet<'T> -> MultiSet<'T>

   val subtract : MultiSet<'T> -> MultiSet<'T> -> MultiSet<'T>
                
   val intersection : MultiSet<'T> -> MultiSet<'T> -> MultiSet<'T>




