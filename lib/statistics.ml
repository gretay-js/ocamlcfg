
module StringMap = Map.Make(String)

let counters = ref StringMap.empty

let record ~file f =
  let old_counters = !counters in
  counters := StringMap.empty;
  Misc.try_finally f
    ~always:(fun () ->
      let oc = open_out file in
      let fmt = Format.formatter_of_out_channel oc in
      let pp_sep fmt () = Format.fprintf fmt "\n" in
      Format.fprintf fmt "(";
      Format.pp_print_list ~pp_sep (fun fmt (group, key_val) ->
        Format.fprintf fmt "(%s (" group;
        Format.pp_print_list ~pp_sep (fun fmt (key, v) ->
          Format.fprintf fmt "(%s %d)" key v) fmt (StringMap.bindings key_val);
        Format.fprintf fmt ")") fmt (StringMap.bindings !counters);
      Format.fprintf fmt ")";
      close_out oc;
      counters := old_counters)

let inc ~group ~key =
  counters := StringMap.update group (function
    | None -> Some (StringMap.singleton key 1)
    | Some map -> Some (StringMap.update key (function
      | None -> Some 1
      | Some v -> Some (v + 1)) map)) !counters
