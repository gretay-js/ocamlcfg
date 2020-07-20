
type 'a t


val empty : 'a t
val push : 'a t -> 'a -> 'a t
val pop : 'a t -> ('a * 'a t) option
