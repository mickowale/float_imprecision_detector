module D = Domain
module F = Format

type phase = Widen | Narrow
module Make (Memory : D.MEMORY_DOMAIN) = struct
  module Semantics = Semantics.Make (Memory)
  module Table = Domain.Table (Semantics.Memory)
  module Memory = Semantics.Memory
  

  
  let collect_memories lbl r_list = 
    r_list |> 
    List.fold_left
    (fun acc (n_lbl, mem) -> 
      if n_lbl = lbl then mem :: acc else acc) []

  let find_incoming_memories_bb blk next_label llctx table = 
    let rec get_previous acc lbl = 
      let transferred = 
          Semantics.transfer_label llctx lbl (Table.find lbl table) in
      let new_acc = List.append acc (collect_memories next_label transferred) in 
      match lbl with 
      | Llvm.At_end _ -> new_acc
      | Llvm.Before instr -> get_previous new_acc (Llvm.instr_succ instr) in
    get_previous [] (Llvm.instr_begin blk) 

  let find_incoming_memories lbl llctx table = 
    let parent_func =
      match lbl with 
      | Llvm.At_end bb -> Llvm.block_parent bb
      | Llvm.Before instr -> instr |> Llvm.instr_parent |> Llvm.block_parent in
    Llvm.fold_left_blocks 
      (fun acc bb -> List.append acc (find_incoming_memories_bb bb lbl llctx table)) 
      [] parent_func

  let rec iter_with_phase llctx phase worklist table acc = 
    match worklist with 
    | [] -> table, acc
    | hd :: tl -> 
      let old_mem = Table.find hd table in 
      let mem_list = find_incoming_memories hd llctx table in
      let new_mem = List.fold_left (fun acc mem -> Memory.join acc mem) Memory.bottom mem_list in 
      match phase with 
      | Widen -> 
        if (Memory.order new_mem old_mem) then 
          iter_with_phase llctx phase tl table acc
        else 
          let new_mem = Memory.widen old_mem new_mem in
          let new_table = Table.add hd new_mem table in 
          let lbls = 
              Semantics.transfer_label llctx hd new_mem
              |> List.map fst in
          iter_with_phase llctx phase tl new_table (List.append acc lbls)
      | Narrow -> 
        if (Memory.order old_mem new_mem) then 
          iter_with_phase llctx phase tl table acc
        else 
          let new_mem = Memory.narrow old_mem new_mem in
          (* let _ = Printf.printf " After narrowing"; Memory.pp F.std_formatter new_mem; print_endline "" in *)
          let new_table = Table.add hd new_mem table in 
          let lbls = 
              Semantics.transfer_label llctx hd new_mem
              |> List.map fst in
          iter_with_phase llctx phase tl new_table (List.append acc lbls)
          
  let rec run llctx phase worklist table = 
    if D.Worklist.cardinal worklist = 0 then table
    else 
      let new_table, new_worklist =
         iter_with_phase llctx phase (D.Worklist.elements worklist) table [] in
      run llctx phase (D.Worklist.of_list new_worklist) new_table

  let check_instr llctx instr memory =
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Call when Utils.is_print instr ->
        let arg = Llvm.operand instr 0 in
        let v = Semantics.eval arg memory in
        F.printf "%s @@ %s : %a\n" (Utils.string_of_lhs arg)
          (Utils.string_of_location llctx instr)
          Memory.Value.pp v
    | Llvm.Opcode.SDiv | Llvm.Opcode.UDiv ->
        let e = Llvm.operand instr 1 in
        let v = Semantics.eval e memory in
        let zero = Memory.Value.of_int 0 in
        if Memory.Value.order zero v then
          let location = Utils.string_of_location llctx instr in
          let exp = Utils.string_of_exp e in
          F.printf "Potential Division-by-zero @@ %s, %s = %a\n" location exp
            Memory.Value.pp v
        else ()
    | _ -> ()


  let checkFPValues llctx memory threshold = 
    
    let floatChecker llctx instr = 
      match Llvm.instr_opcode instr with
      | Llvm.Opcode.FMul | Llvm.Opcode.FDiv | Llvm.Opcode.FSub | Llvm.Opcode.FAdd -> 
          let value = Semantics.eval instr memory in
          let diff = Memory.Value.value_diff value in 
          let location = Utils.string_of_location llctx instr in 
          let exp = Utils.string_of_exp instr in 
          if diff > threshold then 
            F.printf "FP Imprecision @@ %s, %s = %a, diff = %f \n" location exp
            Memory.Value.pp value diff
          else ()
      | _ -> () in
    
    let check_instrs e =  
      match Llvm.classify_value e with 
      | Llvm.ValueKind.Instruction _ when 
        Llvm.classify_type (Llvm.type_of e) = Llvm.TypeKind.Float-> 
        floatChecker llctx e 
      | _ -> () in
    Memory.iter (fun e _ -> check_instrs e) memory
    
  let copy_memory mem accMem = 
    Memory.fold (fun k a accMem -> Memory.add k a accMem) mem accMem

  let check llctx threshold table =
    let totalMemory = 
      Table.fold
        (fun label memory accMemory ->
          match label with
          | Llvm.Before instr -> 
            let _ = check_instr llctx instr memory in
            copy_memory memory accMemory
          | _ -> accMemory)
        table Memory.bottom in
    checkFPValues llctx totalMemory threshold
end
