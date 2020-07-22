
type 'a t = 'a list * 'a list

let empty = ([], [])

let push (front, back) x = (front, x :: back)

let pop = function
  | (x :: front, back) -> Some (x, (front, back))
  | ([], back) ->
    match List.rev back with
    | [] -> None
    | x :: back' -> Some (x, (back', []))

let length (front, back) = List.length front + List.length back
