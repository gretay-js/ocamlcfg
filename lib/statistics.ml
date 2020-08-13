
module StringMap = Map.Make(String)

let counters = ref StringMap.empty

let record ~file f =
  let old_counters = !counters in
  counters := StringMap.empty;
  Misc.try_finally f
    ~always:(fun () ->
      if not (StringMap.is_empty !counters) then begin
        let oc = open_out file in
        let fmt = Format.formatter_of_out_channel oc in
        StringMap.iter
          (fun group key_val ->
            StringMap.iter
              (fun key v -> Format.fprintf fmt "%s.%s: %d\n" group key v)
              key_val)
          !counters;
        close_out oc;
        counters := old_counters
      end)

let set ~group ~key v =
  counters := StringMap.update group (function
    | None -> Some (StringMap.singleton key v)
    | Some map -> Some (StringMap.update key (fun _ -> Some v) map)) !counters

let inc ~group ~key =
  counters := StringMap.update group (function
    | None -> Some (StringMap.singleton key 1)
    | Some map -> Some (StringMap.update key (function
      | None -> Some 1
      | Some v -> Some (v + 1)) map)) !counters
