
module S = struct
  module Spill = struct
    module T = struct
      type t =
        { block: Label.t
        ; inst: int
        ; stack_location: int
        }

      let compare a b =
        let cmp = Label.compare a.block b.block in
        if cmp <> 0 then cmp
        else
          if a.inst = b.inst then
            a.stack_location - b.stack_location
          else
            a.inst - b.inst
    end

    include T
    module Set = Set.Make(T)
  end

  module Dom = struct
    type t = Label.t
    module Set = Label.Set
  end
end
