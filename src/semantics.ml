module D = Domain
module F = Format

module type S = sig
  module Memory : D.MEMORY_DOMAIN

  module Value : D.NUMERICAL_DOMAIN with type t = Memory.Value.t

  val eval : Llvm.llvalue -> Memory.t -> Value.t

  val filter : Llvm.llvalue -> bool -> Memory.t -> Memory.t

  val transfer_label :
    Llvm.llcontext -> D.Label.t -> Memory.t -> (D.Label.t * Memory.t) list

  val transfer :
    Llvm.llcontext -> Llvm.llvalue -> Memory.t -> (D.Label.t * Memory.t) list
end

module Make (Memory : D.MEMORY_DOMAIN) : S = struct
  exception Unsupported

  module Memory = Memory
  module Value = Memory.Value
  
  let eval e mem =
    let kind = Llvm.classify_value e in
    match kind with
    | Llvm.ValueKind.ConstantInt -> 
      (match Llvm.int64_of_const e with 
      | Some n -> n |> Int64.to_int |> Value.of_int 
      | None -> failwith "The constant was not an int constant")
    | Llvm.ValueKind.ConstantFP -> 
      (match Llvm.float_of_const e with 
      | Some f -> f |> Value.of_float
      | None -> failwith "The constant was not a float constant")
    | Llvm.ValueKind.Instruction _ -> Memory.find e mem
    | _ -> raise Unsupported

  let filter cond truth memory = 
    let pred = 
      (match Llvm.icmp_predicate cond with 
      | Some p -> 
        let p = if truth then p else Utils.neg_ipred p in 
        Value.Icmp p
      | None -> 
        (match Llvm.fcmp_predicate cond with 
        | Some p ->
          let p = if truth then p else Utils.neg_fpred p in 
          Value.Fcmp p
        | None -> failwith "non conditional ins in filter")) in 
    let opr1 = Llvm.operand cond 0 in
    let opr2 = Llvm.operand cond 1 in 
    let var, const = 
      match Llvm.classify_value opr1, Llvm.classify_value opr2 with 
      | Llvm.ValueKind.Instruction _, Llvm.ValueKind.ConstantInt 
      | Llvm.ValueKind.Argument, Llvm.ValueKind.ConstantInt
      | _ -> opr1, opr2 in
    let var_value = eval var memory in 
    let cons_value = eval const memory in 
    let new_var_value = Value.filter pred var_value cons_value in
    Memory.add var new_var_value memory

  let transfer llctx instr memory =
    let next = Llvm.instr_succ instr in
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Ret -> [ (next, memory) ]
    | Llvm.Opcode.Call when Utils.is_llvm_intrinsic instr -> [ (next, memory) ]
    | Llvm.Opcode.Call when Utils.is_print instr -> [ (next, memory) ]
    | Llvm.Opcode.Call when Utils.is_input instr ->
      let mem = (Memory.add instr Value.top memory) in
      [(next, mem)]
    | Llvm.Opcode.PHI -> 
      let first_non_phi = Utils.get_next_nonphi instr in 
      let fnp_value = 
        Llvm.incoming instr 
        |> List.map fst
        |> List.map (fun e -> eval e memory)
        |> List.fold_left (fun v acc -> Value.join acc v) Value.bottom in
      let new_fnp_mem = Memory.add instr fnp_value memory in 
      (try [(first_non_phi, new_fnp_mem); (Utils.get_next_phi instr , memory)]
      with Not_found -> [(first_non_phi, new_fnp_mem)])
    | Llvm.Opcode.Add | Llvm.Opcode.FAdd -> 
      let opr1 = Llvm.operand instr 0 in 
      let opr2 = Llvm.operand instr 1 in 
      let res = Value.add (eval opr1 memory) (eval opr2 memory) in 
      let mem = Memory.add instr res memory in
      [(next, mem)]
    | Llvm.Opcode.Sub | Llvm.Opcode.FSub -> 
      let opr1 = Llvm.operand instr 0 in 
      let opr2 = Llvm.operand instr 1 in 
      let res = Value.sub (eval opr1 memory) (eval opr2 memory) in 
      let mem = Memory.add instr res memory in
      [(next, mem)]
    | Llvm.Opcode.Mul | Llvm.Opcode.FMul ->
      let opr1 = Llvm.operand instr 0 in 
      let opr2 = Llvm.operand instr 1 in 
      let opr1 = eval opr1 memory in 
      let opr2 = eval opr2 memory in 
      let res = Value.mul opr1 opr2 in 
      (* F.printf "@%s\n" (Utils.string_of_exp instr);
      F.printf "Instr: %a * %a -> %a \n" Memory.Value.pp opr1 Memory.Value.pp opr2
        Memory.Value.pp res;  *)
      let mem = Memory.add instr res memory in
      [(next, mem)]
    | Llvm.Opcode.SDiv | Llvm.Opcode.UDiv | Llvm.Opcode.FDiv -> 
      let opr1 = Llvm.operand instr 0 in 
      let opr2 = Llvm.operand instr 1 in 
      let opr1 = eval opr1 memory in 
      let opr2 = eval opr2 memory in 
      let res = Value.div opr1 opr2 in 
      let mem = Memory.add instr res memory in
      [(next, mem)]
    | Llvm.Opcode.Br -> (
        match Llvm.get_branch instr with
        | Some (`Conditional (cond, b1, b2)) -> 
          [(Llvm.instr_begin b1, filter cond true memory);
          (Llvm.instr_begin b2, filter cond false memory)]
        | Some (`Unconditional b) -> [(Llvm.instr_begin b, memory)]
        | _ -> raise Unsupported )
    | Llvm.Opcode.ICmp -> 
      let pred = 
        (match Llvm.icmp_predicate instr with 
        | Some p -> p 
        | _ -> raise Unsupported) in 
      let opr1 = Llvm.operand instr 0 in
      let opr2 = Llvm.operand instr 1 in 
      let res = Value.cmp (Icmp pred) (eval opr1 memory) (eval opr2 memory) in 
      let mem = Memory.add instr res memory in 
      [(next, mem)]   
    | Llvm.Opcode.FCmp -> 
      let pred = 
        (match Llvm.fcmp_predicate instr with 
        | Some p -> p 
        | _ -> raise Unsupported) in 
      let opr1 = Llvm.operand instr 0 in
      let opr2 = Llvm.operand instr 1 in 
      let res = Value.cmp (Fcmp pred) (eval opr1 memory) (eval opr2 memory) in 
      let mem = Memory.add instr res memory in 
      [(next, mem)] 
    | Llvm.Opcode.FPToUI | Llvm.Opcode.FPToSI -> 
      let opr = Llvm.operand instr 0 in 
      let res = Value.ftoi (eval opr memory) in 
      let mem = Memory.add instr res memory in 
      [(next, mem)]
    | Llvm.Opcode.UIToFP | Llvm.Opcode.SIToFP -> 
      let opr = Llvm.operand instr 0 in 
      let res = Value.itof (eval opr memory) in 
      let mem = Memory.add instr res memory in 
      [(next, mem)]
    | Llvm.Opcode.FPTrunc
    | Llvm.Opcode.FPExt -> 
      let opr = Llvm.operand instr 0 in
      let res = eval opr memory in 
      let mem = Memory.add instr res memory in 
      [(next,mem)]
    | _ -> raise Unsupported

  let transfer_label llctx label memory =
    match label with
    | Llvm.At_end _ -> []
    | Llvm.Before instr -> 
      try transfer llctx instr memory with 
      | c-> 
        let _ = print_endline (Utils.string_of_exp instr) in 
        raise c
end
