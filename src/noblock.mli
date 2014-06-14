include Monad.Monad

val read_line  : Unix.file_descr -> string m
val write_line : Unix.file_descr -> string -> unit m
