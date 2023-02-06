(* Objects may be labeled so that they can be referred to by other objects. A labeled object is called an indirect object.*)

type obj =
  | Bool of bool
  | Integer of int
  | Real of float
  | String of string
  | Name of string
  | Array of obj list
  | Dictionary of (obj * obj) list
  | Stream

type indirectObj = int * int * obj
type header = Zero | One | Two | Three | Four | Five | Six | Seven

let evalh h =
  match h with
  | Seven -> "%PDF-1.7"
  | Zero -> "%PDF-1.0"
  | One -> "%PDF-1.1"
  | Two -> "%PDF-1.2"
  | Three -> "%PDF-1.3"
  | Four -> "%PDF-1.4"
  | Five -> "%PDF-1.5"
  | Six -> "%PDF-1.6"

let binaryHeader = "129129129129"
