(* This program simply runs a register allocator on the all the functions saved
   in a .cmir-cfg-regalloc file. *)

module List = ListLabels
module DLL = Oxcaml_utils.Doubly_linked_list

let fatal : ('a, Format.formatter, unit, unit, unit, _) format6 -> 'a =
 fun fmt ->
  Format.kfprintf
    (fun _ -> exit 1)
    Format.err_formatter
    ("*** error: " ^^ fmt ^^ "\n%!")

let num_processed_files = ref 0

let num_processed_functions = ref 0

type register_allocator =
  | GI
  | IRC
  | LS

let register_allocators = [GI; IRC; LS]

let string_of_register_allocator = function
  | GI -> "gi"
  | IRC -> "irc"
  | LS -> "ls"

type strategy =
  | Allocator of register_allocator
  | Default
  | Custom

let stategies =
  ["default", Default; "custom", Custom]
  @ List.map register_allocators ~f:(fun ra ->
        string_of_register_allocator ra, Allocator ra)

type config =
  { strategy : strategy;
    validation : bool;
    linscan_threshold : int;
    debug_output : bool;
    csv_output : bool;
    print_summary : bool;
    paths : string list
  }

let is_entry_function_name name = String.ends_with ~suffix:"__entry" name

external time_include_children : bool -> float
  = "caml_sys_time_include_children"

let cpu_time () = time_include_children false

type in_stats =
  { is_entry_function : bool;
    num_regs : int;
    num_blocks : int;
    num_instrs : int;
    num_destruction_points : int;
    num_high_pressure_points : int
  }

let dummy_in_stats =
  { is_entry_function = false;
    num_regs = -1;
    num_blocks = -1;
    num_instrs = -1;
    num_destruction_points = -1;
    num_high_pressure_points = -1
  }

type out_stats =
  { spills : int;
    reloads : int;
    spill_cost : int;
    reload_cost : int
  }

let row_header =
  String.concat ";"
    [ "allocator";
      "function_name";
      "duration";
      "rounds";
      "in_is_entry_function";
      "in_num_regs";
      "in_num_blocks";
      "in_num_instrs";
      "in_num_destruction_points";
      "in_num_high_pressure_points";
      "out_spills";
      "out_reloads";
      "out_spill_cost";
      "out_reload_cost" ]

let print_row ~allocator ~function_name ~duration ~rounds
    ~in_stats:
      { is_entry_function;
        num_regs;
        num_blocks;
        num_instrs;
        num_destruction_points;
        num_high_pressure_points
      } ~out_stats:{ spills; reloads; spill_cost; reload_cost } =
  Printf.printf "%s;%s;%g;%d;%s;%d;%d;%d;%d;%d;%d;%d;%d;%d\n%!" allocator
    function_name duration rounds
    (if is_entry_function then "True" else "False")
    num_regs num_blocks num_instrs num_destruction_points
    num_high_pressure_points spills reloads spill_cost reload_cost

let collect_in_stats (cfg_with_infos : Cfg_with_infos.t)
    (relocatable_regs : Reg.t list) =
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let liveness = Cfg_with_infos.liveness cfg_with_infos in
  let accumulate_regsets_per_class (tbl : Reg.Set.t ref Reg_class.Tbl.t)
      (regs : Reg.Set.t) =
    Reg.Set.iter
      (fun (reg : Reg.t) ->
        let reg_class = Reg_class.of_machtype reg.typ in
        let regset_ref = Reg_class.Tbl.find tbl reg_class in
        regset_ref := Reg.Set.add reg !regset_ref)
      regs
  in
  let is_high_pressure_point (type a) (instr : a Cfg.instruction) =
    let regsets_per_class =
      Reg_class.Tbl.init ~f:(fun _reg_class -> ref Reg.Set.empty)
    in
    let live = InstructionId.Tbl.find liveness instr.id in
    accumulate_regsets_per_class regsets_per_class live.across;
    let res = ref false in
    Reg_class.Tbl.iter regsets_per_class ~f:(fun reg_class regset ->
        let is_high_pressure =
          Reg.Set.cardinal !regset > Reg_class.num_available_registers reg_class
        in
        res := is_high_pressure || !res);
    !res
  in
  let count_high_pressure_points instrs =
    DLL.fold_left instrs ~init:0 ~f:(fun acc instr ->
        if is_high_pressure_point instr then succ acc else acc)
  in
  let is_entry_function = is_entry_function_name cfg.fun_name in
  let num_regs = List.length relocatable_regs in
  let num_blocks = Label.Tbl.length cfg.blocks in
  let num_instrs, num_destruction_points, num_high_pressure_points =
    Cfg.fold_blocks (Cfg_with_layout.cfg cfg_with_layout) ~init:(0, 0, 0)
      ~f:(fun
           _label
           block
           (num_instrs, num_destruction_points, num_high_pressure_points)
         ->
        let num_instrs = num_instrs + DLL.length block.body + 1 in
        let is_destruction_point =
          Proc.is_destruction_point
            ~more_destruction_points:
              (Lazy.force Regalloc_split_utils.split_more_destruction_points)
            block.terminator.desc
        in
        let num_destruction_points =
          if is_destruction_point
          then succ num_destruction_points
          else num_destruction_points
        in
        let num_high_pressure_points =
          num_high_pressure_points + count_high_pressure_points block.body
        in
        let num_high_pressure_points =
          if (not is_destruction_point)
             && is_high_pressure_point block.terminator
          then succ num_high_pressure_points
          else num_high_pressure_points
        in
        num_instrs, num_destruction_points, num_high_pressure_points)
  in
  (* note: liveness is invalidated here to be sure it is counted as part of
     regalloc duration. *)
  Cfg_with_infos.invalidate_liveness cfg_with_infos;
  { is_entry_function;
    num_regs;
    num_blocks;
    num_instrs;
    num_destruction_points;
    num_high_pressure_points
  }

let safe_pow10 n =
  let res = ref 1 in
  let idx = ref 0 in
  while !idx < n && Misc.no_overflow_mul 10 !res do
    idx := !idx + 1;
    res := !res * 10
  done;
  !res

let collect_out_stats (cfg_with_infos : Cfg_with_infos.t) =
  let loop_depths = (Cfg_with_infos.loop_infos cfg_with_infos).loop_depths in
  Cfg_with_infos.fold_blocks cfg_with_infos
    ~init:{ spills = 0; reloads = 0; spill_cost = 0; reload_cost = 0 }
    ~f:(fun (label : Label.t) (block : Cfg.basic_block) (acc : out_stats) ->
      let cost =
        match Label.Map.find_opt label loop_depths with
        | Some depth -> safe_pow10 depth
        | None -> 1
      in
      let update_cost curr =
        (* note: keep in sync with the compiler heuristics *)
        if Misc.no_overflow_add curr cost then curr + cost else max_int
      in
      DLL.fold_left block.body ~init:acc
        ~f:(fun (acc : out_stats) (instr : Cfg.basic Cfg.instruction) ->
          match[@ocaml.warning "-4"] instr.desc with
          | Op Spill ->
            { acc with
              spills = succ acc.spills;
              spill_cost = update_cost acc.spill_cost
            }
          | Op Reload ->
            { acc with
              reloads = succ acc.reloads;
              reload_cost = update_cost acc.reload_cost
            }
          | _ -> acc))

(* note: this is unfortunately a reimplementation of
   `Asmgen.should_use_linscan`, which cannot be used because we have lost bit of
   information / are using different types here. *)
let should_use_linscan config cfg_with_layout =
  is_entry_function_name (Cfg_with_layout.cfg cfg_with_layout).fun_name
  || List.compare_length_with
       (Reg.all_relocatable_regs ())
       ~len:config.linscan_threshold
     > 0

let select_allocator config cfg_with_layout =
  match config.strategy with
  | Allocator allocator -> allocator
  | Default -> if should_use_linscan config cfg_with_layout then LS else IRC
  | Custom -> fatal "not implemented"

let process_function (config : config) (cfg_with_layout : Cfg_with_layout.t)
    (cmm_label : Label.t) (reg_stamp : int) (relocatable_regs : Reg.t list) =
  incr num_processed_functions;
  if config.debug_output
  then
    Printf.eprintf "  processing function %S...\n%!"
      (Cfg_with_layout.cfg cfg_with_layout).fun_name;
  let cfg_with_infos = Cfg_with_infos.make cfg_with_layout in
  let in_stats =
    if config.csv_output
    then collect_in_stats cfg_with_infos relocatable_regs
    else dummy_in_stats
  in
  Cmm.reset ();
  Cmm.set_label cmm_label;
  Reg.For_testing.set_state ~stamp:reg_stamp ~relocatable_regs;
  let cfg_description =
    match config.validation with
    | false -> None
    | true -> Some (Regalloc_validate.Description.create cfg_with_layout)
  in
  let start_time = cpu_time () in
  let allocator = select_allocator config cfg_with_layout in
  let (_, rounds_ref) : Cfg_with_infos.t * int ref =
    match allocator with
    | GI -> Regalloc_gi.run cfg_with_infos, Regalloc_gi.For_testing.rounds
    | IRC -> Regalloc_irc.run cfg_with_infos, Regalloc_irc.For_testing.rounds
    | LS -> Regalloc_ls.run cfg_with_infos, Regalloc_ls.For_testing.rounds
  in
  let end_time = cpu_time () in
  (match cfg_description with
  | None -> ()
  | Some cfg_description ->
    let (_ : Cfg_with_layout.t) =
      Regalloc_validate.run cfg_description cfg_with_layout
    in
    ());
  let duration = end_time -. start_time in
  if config.csv_output
  then begin
    let out_stats = collect_out_stats cfg_with_infos in
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    print_row
      ~allocator:(string_of_register_allocator allocator)
      ~function_name:cfg.fun_name ~duration ~rounds:!rounds_ref ~in_stats
      ~out_stats
  end;
  if config.debug_output
  then Printf.eprintf "  register allocation took %gs...\n%!" duration;
  ()

let process_file (file : string) (config : config) =
  if config.debug_output then Printf.eprintf "processing file %S...\n%!" file;
  incr num_processed_files;
  let unit_info, _digest = Cfg_format.restore file in
  List.iter unit_info.items ~f:(fun (item : Cfg_format.cfg_item_info) ->
      begin
        match item with
        | Cfg _ -> ()
        | Data _ -> ()
        | Cfg_before_regalloc
            { cfg_with_layout_and_relocatable_regs; cmm_label; reg_stamp } ->
          let cfg_with_layout, relocatable_regs =
            cfg_with_layout_and_relocatable_regs
          in
          process_function config cfg_with_layout cmm_label reg_stamp
            relocatable_regs
      end)

let is_regalloc_file (file : string) =
  Filename.check_suffix file ".cmir-cfg-regalloc"

let collect_files (paths : string list) =
  let files = ref [] in
  let add_file file = if is_regalloc_file file then files := file :: !files in
  let rec collect path =
    if not (Sys.file_exists path)
    then begin
      fatal "%S does not exist" path
    end
    else if Sys.is_directory path
    then begin
      Array.iter
        (fun elem ->
          let elem_path = Filename.concat path elem in
          collect elem_path)
        (Sys.readdir path)
    end
    else add_file path
  in
  List.iter paths ~f:collect;
  List.sort ~cmp:String.compare !files

let parse_command_line () =
  let strategy = ref None in
  let set_strategy str =
    match List.assoc_opt str stategies with
    | None -> assert false
    | Some strat -> strategy := Some strat
  in
  let validate = ref false in
  let linscan_threshold = ref !Oxcaml_flags.regalloc_linscan_threshold in
  let csv_output = ref false in
  let debug_output = ref false in
  let print_summary = ref false in
  let paths = ref [] in
  let args : (Arg.key * Arg.spec * Arg.doc) list =
    [ ( "-regalloc",
        Arg.Symbol (List.map stategies ~f:fst, set_strategy),
        "  Choose register allocator" );
      ( "-param",
        Arg.String
          (fun s ->
            Oxcaml_flags.regalloc_params := s :: !Oxcaml_flags.regalloc_params),
        " Pass a parameter to the register allocator" );
      ( "-linscan-threshold",
        Arg.Set_int linscan_threshold,
        "Select linscan if the number of registers is above the threshold" );
      "-validate", Arg.Set validate, "Enable validation";
      "-csv-output", Arg.Set csv_output, "Enable CSV output";
      "-debug-output", Arg.Set debug_output, "Enable debug output";
      "-summary", Arg.Set print_summary, "Print summary" ]
  in
  let anonymous path = paths := path :: !paths in
  Arg.parse args anonymous "run register allocation on .cmir-cfg-regalloc files";
  match !strategy with
  | None -> fatal "register allocator was not set (use -regalloc)"
  | Some strategy ->
    { strategy;
      validation = !validate;
      linscan_threshold = !linscan_threshold;
      csv_output = !csv_output;
      debug_output = !debug_output;
      print_summary = !print_summary;
      paths = !paths
    }

let () =
  let config = parse_command_line () in
  if config.csv_output
  then begin
    Printf.printf "%s\n%!" row_header
  end;
  let files = collect_files config.paths in
  List.iter files ~f:(fun file -> process_file file config);
  if config.print_summary
  then begin
    Printf.printf "processed %d files(s) / %d function(s)\n%!"
      !num_processed_files !num_processed_functions
  end
