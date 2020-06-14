module IntervalMemory = Domain.Memory (Domain.Interval)
module IntervalAnalysis = Analysis.Make (IntervalMemory)

let usage = "Usage: analyzer [threshold float] [ LLVM IR file ]"

let main argv =
  if Array.length argv <> 3 then (
    prerr_endline "analyzer: You must specify one float value and one LLVM IR file";
    prerr_endline usage;
    exit 1 );
  let llctx = Llvm.create_context () in
  let llmem = Llvm.MemoryBuffer.of_file argv.(2) in
  let llm = Llvm_irreader.parse_ir llctx llmem in
  let worklist = Domain.Worklist.init llm in
  let threshold = 
    try Float.of_string (argv.(1)) with 
    | _ -> 
      prerr_endline ("'" ^ argv.(1) ^ "'"^ " is not a float");
      prerr_endline usage;
      exit 1; in 
  IntervalAnalysis.Table.empty
  |> IntervalAnalysis.run llctx Analysis.Widen worklist
  |> IntervalAnalysis.run llctx Analysis.Narrow worklist
  |> IntervalAnalysis.check llctx threshold


let _ = main Sys.argv
(* let _ = experiments () *)
