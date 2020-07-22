
module S = struct
  module Spill = struct
    module T = struct
      type t =
        { block: Label.t
        ; inst: int
        ; stack_slot: int
        }

      let compare = Stdlib.compare
    end

    include T
    module Set = Set.Make(T)
  end

  module Dom = struct
    type t = Label.t
    module Set = Label.Set
  end

  module Path = struct
    module T = struct
      type t = Label.t list
      let compare = Stdlib.compare
    end

    include T
    module Set = Set.Make(T)
  end
end
