module F = Format

module type SET = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

module type CPO = sig
  include SET

  val bottom : t

  val order : t -> t -> bool

  val join : t -> t -> t

  val widen : t -> t -> t

  val narrow : t -> t -> t
end

module type NUMERICAL_DOMAIN = sig
  include CPO

  type comp = Icmp of Llvm.Icmp.t | Fcmp of Llvm.Fcmp.t

  val top : t

  val of_int : int -> t

  val of_float: float -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val ftoi: t -> t

  val itof: t -> t

  val cmp : comp -> t -> t -> t

  val filter : comp -> t -> t -> t

  val value_diff: t -> float
end

module Variable : SET with type t = Llvm.llvalue = struct
  type t = Llvm.llvalue

  let compare = compare

  let pp fmt v = Utils.string_of_lhs v |> Format.fprintf fmt "%s"
end

module type MEMORY_DOMAIN = sig
  include CPO

  module Value : NUMERICAL_DOMAIN

  val add : Variable.t -> Value.t -> t -> t

  val fold: (Variable.t -> Value.t -> 'a -> 'a) -> t -> 'a -> 'a

  val iter: (Variable.t -> Value.t -> unit) -> t -> unit

  val find : Variable.t -> t -> Value.t
end

module Label = struct
  type t = (Llvm.llbasicblock, Llvm.llvalue) Llvm.llpos

  let compare = compare

  let pp fmt = function
    | Llvm.Before instr ->
        Utils.string_of_instr instr |> Format.fprintf fmt "%s"
    | Llvm.At_end _ -> Format.fprintf fmt "At_end"
end

module type INTERVAL = sig
  type elt = Int of int | PInf | MInf | Float of float
  type f32Interval = elt * elt
  type f64Interval = elt * elt
  type t = Bot | IntInterval of elt * elt | FInterval of f32Interval * f64Interval


  include NUMERICAL_DOMAIN with type t := t
end

module Interval : INTERVAL = struct
  type elt = Int of int | PInf | MInf | Float of float

  type f32Interval = elt * elt

  type f64Interval = elt * elt

  type t = Bot | IntInterval of elt * elt | FInterval of f32Interval * f64Interval

  type comp = Icmp of Llvm.Icmp.t | Fcmp of Llvm.Fcmp.t

  exception Impossible

  (* This is the chosen precision limit. The tool cannot be more precise than this *)
  let prec_unit = 0.000001

  let _ = Mpfr.set_default_prec 23

  let r = Utils.get_round_mode ()

  let f32add n1 n2 = 
    let n1, n2 = Mpfrf.of_float n1 r, Mpfrf.of_float n2 r in
    Mpfrf.add n1 n2 r |> Mpfrf.to_float
  let f32sub n1 n2 = 
    let n1, n2 = Mpfrf.of_float n1 r, Mpfrf.of_float n2 r in
    Mpfrf.sub n1 n2 r |> Mpfrf.to_float
  let f32mul n1 n2 = 
    let n1, n2 = Mpfrf.of_float n1 r, Mpfrf.of_float n2 r in
    Mpfrf.mul n1 n2 r |> Mpfrf.to_float
  let f32div n1 n2 = 
    let n1, n2 = Mpfrf.of_float n1 r, Mpfrf.of_float n2 r in
    Mpfrf.div n1 n2 r |> Mpfrf.to_float
  let f32ofInt n1 = 
    Mpfrf.of_int n1 r |> Mpfrf.to_float

  let pp_elt fmt = function
    | Int i -> Format.fprintf fmt "%d" i
    | Float f -> Format.fprintf fmt "%f" f
    | PInf -> Format.fprintf fmt "+oo"
    | MInf -> Format.fprintf fmt "-oo"

  let pp fmt = function
    | Bot -> Format.fprintf fmt "Bot"
    | IntInterval (l, u) -> Format.fprintf fmt "[%a, %a]" pp_elt l pp_elt u
    | FInterval ((a, b), (a', b')) -> 
      Format.fprintf fmt "(32[%a, %a], 64[%a, %a])" pp_elt a pp_elt b pp_elt a' pp_elt b'
  let compare = compare

  let bottom = Bot

  let top = IntInterval (MInf, PInf)
  let ftop = FInterval ((MInf, PInf), (MInf, PInf))
  let zero = IntInterval (Int 0, Int 0)
  let fzero = FInterval ((Float 0.0, Float 0.0), (Float 0.0, Float 0.0))

  let get_max a b = 
    match a, b with
    | Int x, Int y -> if x >= y then a else b
    | Float x, Float y -> if x >= y then a else b
    | _, PInf | PInf, _ -> PInf
    | a, MInf -> a
    | MInf, b -> b 
    | _ -> 
      print_endline "Inside get_max";
      raise Impossible
  
  let get_min a b = 
    match a, b with
    | Int x, Int y -> if x <= y then a else b
    | Float x, Float y -> if x <= y then a else b
    | _, MInf | MInf, _ -> MInf
    | a, PInf -> a
    | PInf, b -> b 
    | _ -> 
    print_endline "Inside get_min";
    raise Impossible

  let elt_add (x: elt) (y: elt) f32 = 
    (match x, y with 
    | Int x, Int y -> Int (x + y)
    | Float x, Float y -> 
      if f32 then Float (f32add x y)
      else Float (x +. y)
    | MInf, _ | _ , MInf -> MInf 
    | PInf, _ | _, PInf -> PInf
    | _ -> 
    print_endline "Inside elt_add";
    raise Impossible)

  let elt_sub (x: elt) (y: elt) f32 = 
    (match x, y with 
    | Int x, Int y -> Int (x - y)
    | Float x, Float y -> 
      if f32 then Float (f32sub x y)
      else Float (x -. y)
    | MInf, _  | _, PInf -> MInf 
    | _ , MInf | PInf, _ -> PInf
    | _ -> 
    print_endline "Inside elt_sub";
    raise Impossible)

  let of_int i = IntInterval (Int i, Int i)
  let of_float f = FInterval ((Float f, Float f), (Float f, Float f))

  let order x y = 
    let answer = (
      match (x, y) with 
      | IntInterval (a,b), IntInterval (c,d) ->
        get_min a c = c && get_max b d = d
      | FInterval (_, (a', b')), FInterval(_, (c', d')) ->
        get_min a' c' = c' && get_max b' d' = d'
      | Bot, _ -> true
      | _, Bot -> false
      | _ -> false ) in 
    (* let _ = F.printf "order %a: %a = %b\n" pp x 
    pp y 
    answer in *)
    answer

  let value_diff = function
  | FInterval ((Float a, Float b), (Float a', Float b')) -> 
    let lower_diff = a -. a' |> Float.abs in 
    let higher_diff = b -. b' |> Float.abs in 
    Float.max lower_diff higher_diff
  | _ -> 1000000.0
  
  let add x y = 
    match x, y with 
    | IntInterval (a, b), IntInterval (c, d) ->
      IntInterval (elt_add a c false, elt_add b d false)
    | FInterval ((a, b), (a', b')), FInterval ((c, d), (c', d')) -> 
      let f32 = (elt_add a c true), (elt_add b d) true in 
      let f64 = (elt_add a' c' false), (elt_add b' d') false in 
      FInterval (f32, f64)
    | Bot, _ | _, Bot -> Bot
    | _ -> 
    print_endline "Inside value add";
    raise Impossible

  let sub x y = 
    match x, y with 
    | IntInterval (a, b), IntInterval (c, d) ->
      IntInterval (elt_sub a d false , elt_sub b c false)
    | FInterval ((a, b), (a', b')), FInterval ((c, d), (c', d')) -> 
      let f32 = (elt_sub a c true), (elt_sub b d true) in 
      let f64 = (elt_sub a' c' false), (elt_sub b' d' false) in 
      FInterval (f32, f64)
    | Bot, _ | _, Bot -> Bot
    | _ -> 
    print_endline "Inside value sub";
    raise Impossible

  let mul x y =
    let elt_mul (x: elt) (y: elt) f32 = 
      (match x, y with 
      | Int x, Int y -> Int (x * y)
      | Float x, Float y -> 
        if f32 then Float (f32mul x y)
        else Float (x *. y)
      | MInf, Int x -> if x >= 0 then MInf else PInf
      | MInf, Float x -> if x >= 0.0 then MInf else PInf
      | Int x, MInf -> if x >= 0 then MInf else PInf
      | Float x, MInf -> if x >= 0.0 then MInf else PInf
      | MInf, PInf | PInf, MInf -> MInf 
      | PInf, Int x -> if x >= 0 then PInf else MInf
      | PInf, Float x -> if x >= 0.0 then PInf else MInf
      | Int x, PInf -> if x >= 0 then PInf else MInf
      | Float x, PInf -> if x >= 0.0 then PInf else MInf
      | PInf, _ | MInf, MInf -> PInf
      | _ -> 
        print_endline "Inside elt mul";
        raise Impossible) in 
    match x, y with 
    | IntInterval (a, b), IntInterval (c, d) ->
      let w = elt_mul a c false in
      let x = elt_mul a d false in
      let y = elt_mul b c false in 
      let z = elt_mul b d false in 
      let min = get_min (get_min (get_min w x) y) z in 
      let max = get_max (get_max (get_max w x) y) z in 
      IntInterval (min, max)
    | FInterval ((a, b), (a', b')), FInterval ((c, d), (c', d')) -> 
      let w = elt_mul a c true in
      let x = elt_mul a d true in
      let y = elt_mul b c true in 
      let z = elt_mul b d true in 
      let w' = elt_mul a' c' false in
      let x' = elt_mul a' d' false in
      let y' = elt_mul b' c' false in 
      let z' = elt_mul b' d' false in 
      let min = get_min (get_min (get_min w x) y) z in 
      let max = get_max (get_max (get_max w x) y) z in 
      let min' = get_min (get_min (get_min w' x') y') z' in 
      let max' = get_max (get_max (get_max w' x') y') z' in 
      FInterval ((min, max), (min', max'))
    | Bot, _ | _, Bot -> Bot
    | _ -> 
      print_endline "Inside value mul";
      raise Impossible

  let div x y = 
    let elt_div (x: elt) (y: elt) f32 = 
      (match x, y with 
      | Int x, Int y -> 
        if y <> 0 then Int (x / y)
        else if x >= 0 then PInf
        else MInf
      | Float x, Float y -> 
        if y <> 0.0 then (if f32 then Float (f32div x y) else Float (x /. y))
        else if x >= 0.0 then PInf
        else MInf
      | MInf, Int x -> if x >= 0 then MInf else PInf
      | MInf, Float x -> if x >= 0.0 then MInf else PInf
      | PInf, Int x -> if x < 0 then MInf else PInf
      | PInf, Float x -> if x < 0.0 then MInf else PInf
      | PInf, MInf -> MInf
      | PInf, PInf | MInf, MInf -> PInf
      | _, PInf | _, MInf -> Int 0
      | _ -> 
        print_endline "Inside elt_div";
        raise Impossible) in 
    
    if x = Bot || y = Bot then bottom 
    else if order zero y then top 
    else if order fzero y then ftop 
    else 
      match x, y with 
      | IntInterval (a, b), IntInterval (c, d) ->
        let w = elt_div a c false in
        let x = elt_div a d false in
        let y = elt_div b c false in 
        let z = elt_div b d false in 
        let min = get_min (get_min (get_min w x) y) z in 
        let max = get_max (get_max (get_max w x) y) z in 
        IntInterval (min, max)
      | FInterval ((a, b), (a', b')), FInterval ((c, d), (c', d')) -> 
        let w = elt_div a c true in
        let x = elt_div a d true in
        let y = elt_div b c true in 
        let z = elt_div b d true in 
        let w' = elt_div a' c' false in
        let x' = elt_div a' d' false in
        let y' = elt_div b' c' false in 
        let z' = elt_div b' d' false in 
        let min = get_min (get_min (get_min w x) y) z in 
        let max = get_max (get_max (get_max w x) y) z in 
        let min' = get_min (get_min (get_min w' x') y') z' in 
        let max' = get_max (get_max (get_max w' x') y') z' in 
        FInterval ((min, max), (min', max'))
      | Bot, _ | _, Bot -> Bot 
      | _ -> 
        print_endline "Inside value div";
        raise Impossible

  let elt_ftoi = function 
    | Float a -> Int (Float.to_int a)
    | Int _ -> failwith "Casting to int from a non float expression"
    | c -> c

  let elt_itof f32 = function
    | Int i -> if f32 then Float (f32ofInt i) else Float (Float.of_int i)
    | Float _ -> failwith "Casting to float from a non float expression"
    | c -> c


  let ftoi = function 
    | FInterval (_, (a',b')) -> 
      IntInterval (elt_ftoi a', elt_ftoi b')
    | Bot -> Bot
    | _ -> failwith "Casting to int from a non float expression"
  
  let itof = function 
    | IntInterval (a, b) -> 
      let f32 = elt_itof true a, elt_itof true b in 
      let f64 = elt_itof false a, elt_itof false b in 
      FInterval(f32, f64)
    | Bot -> Bot
    | _ -> failwith "Casting to float from a non int expression"

  let join x y = 
    match x, y with 
    | IntInterval (a, b), IntInterval (c, d) ->
      IntInterval (get_min a c, get_max b d)
    | FInterval ((a, b), (a', b')), FInterval ((c, d), (c', d')) -> 
      let f32 = get_min a c, get_max b d in 
      let f64 = get_min a' c', get_max b' d' in 
      FInterval (f32, f64)
    | Bot, a -> a
    | a, Bot -> a
    | _ -> 
    print_endline "Join";
    raise Impossible

  let widen x y =
    match x, y with 
    | IntInterval (a, b), IntInterval (c ,d) -> 
      let min = if get_min a c = c && a <> c then MInf else a in 
      let max = if get_max b d = d && b <> d then PInf else b in 
      IntInterval (min, max)
    | FInterval ((a, b), (a', b')), FInterval ((c, d), (c', d')) -> 
      let min = if get_min a c = c && a <> c then MInf else a in 
      let max = if get_max b d = d && b <> d then PInf else b in 
      let min' = if get_min a' c' = c' && a' <> c' then MInf else a' in 
      let max' = if get_max b' d' = d' && b' <> d' then PInf else b' in 
      FInterval ((min, max), (min', max'))
    | Bot, a -> a
    | a, Bot -> a
    | _ -> 
    print_endline "Inside widen";
    raise Impossible

  let narrow x y = 
    match x, y with 
    | IntInterval (a, b), IntInterval (c, d) -> 
      let min = if a = MInf then c else a in 
      let max = if b = PInf then d else b in 
      IntInterval (min, max)
    | FInterval ((a, b), (a', b')), FInterval ((c, d), (c', d')) -> 
      let min = if a = MInf then c else a in 
      let max = if b = PInf then d else b in 
      let min' = if a' = MInf then c' else a' in 
      let max' = if b' = PInf then d' else b' in 
      FInterval ((min, max), (min', max'))
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | _ -> 
    print_endline "Inside narrow";
    raise Impossible

  let are_disjoint v1 v2 = 
    (match v1, v2 with 
    | IntInterval (a, b), IntInterval (c, d) -> 
      (get_max b c = c && b <> c) || (get_max a d = a && a <> d)
    | FInterval (_, (a', b')), FInterval (_, (c', d')) -> 
      (get_max b' c' = c' && b' <> c') || (get_max a' d' = a' && a' <> d')
    | _ -> true )

  let are_equal v1 v2 = 
    (match v1, v2 with 
    | IntInterval (Int a, Int b), IntInterval (Int c, Int d) -> 
      a = b  && c = d && a = c
    | FInterval (_, (Float a', Float b')), FInterval (_, (Float c', Float d')) -> 
      a' = b'  && c' = d' && a' = c'
    | _ -> false ) 

  let rec cmp pred x y =
    let truev = IntInterval (Int 1, Int 1) in 
    let falsev = IntInterval (Int 0, Int 0) in
    let bothv = IntInterval (Int 0, Int 1) in

    let get_sample = function
    | IntInterval (a, _) -> a
    | FInterval (_, (a', _)) -> a'
    | _ -> failwith "This shoud never happen" in 
    match pred with 
    | Icmp Llvm.Icmp.Eq | Fcmp Llvm.Fcmp.Oeq | Fcmp Llvm.Fcmp.Ueq->
      if are_disjoint x y then falsev
      else if are_equal x y then truev
      else bothv
    | Icmp Llvm.Icmp.Ne | Fcmp Llvm.Fcmp.One | Fcmp Llvm.Fcmp.Une -> 
      if are_disjoint x y then truev
      else if are_equal x y then falsev
      else bothv
    | Icmp Llvm.Icmp.Ugt | Icmp Llvm.Icmp.Sgt | Fcmp Llvm.Fcmp.Ogt | Fcmp Llvm.Fcmp.Ugt -> 
      if are_disjoint x y then 
        ( match x, y with 
         | Bot, _ | _, Bot -> Bot
         | _ -> 
         let a = get_sample x in 
         let b = get_sample y in 
         if (get_max a b = a) then truev
         else falsev )
      else if are_equal x y then falsev
      else bothv
    | Icmp Llvm.Icmp.Uge | Icmp Llvm.Icmp.Sge | Fcmp Llvm.Fcmp.Oge | Fcmp Llvm.Fcmp.Uge-> 
      if are_disjoint x y then 
        ( match x, y with 
         | Bot, _ | _, Bot -> Bot
         | _ -> 
         let a = get_sample x in 
         let b = get_sample y in 
         if (get_max a b = a) then truev
         else falsev )
      else if are_equal x y then truev
      else bothv
    | Icmp Llvm.Icmp.Ult | Icmp Llvm.Icmp.Slt -> cmp (Icmp Llvm.Icmp.Uge) y x
    | Fcmp Llvm.Fcmp.Olt | Fcmp Llvm.Fcmp.Ult -> cmp (Fcmp Llvm.Fcmp.Oge) y x
    | Icmp Llvm.Icmp.Ule | Icmp Llvm.Icmp.Sle -> cmp (Icmp Llvm.Icmp.Ugt) y x
    | Fcmp Llvm.Fcmp.Ole | Fcmp Llvm.Fcmp.Ule -> cmp (Fcmp Llvm.Fcmp.Ogt) y x
    | _ -> 
      print_endline "Inside cmp";
      raise Impossible


  let filter pred v1 v2 = 
    let truev = IntInterval (Int 1, Int 1) in 
    let falsev = IntInterval (Int 0, Int 0) in

    let get_intersection v1 v2 = 
      (match v1, v2 with 
      | Bot, _ | _, Bot -> Bot
      | IntInterval (a, b), IntInterval (c, d) -> 
        IntInterval (get_max a c, get_min b d)
      | FInterval ((a, b), (a', b')), FInterval ((c, d), (c', d')) -> 
        let f32 = (get_max a c, get_min b d) in 
        let f64 = (get_max a' c', get_min b' d') in 
        FInterval (f32, f64)
      | _ -> 
      print_endline "Inside getting intersection";
      raise Impossible) in
    let check_bottom x y = 
      (match x, y with 
      | _, Bot -> Bot
      | _ -> x) in
    let extract_greater eq x y = 
      (match x,y with 
      | IntInterval (a, b), IntInterval (c, _) -> 
        let min = 
          if eq then get_max a c else get_max a (elt_add c (Int 1) false) in 
        IntInterval (min, b)
      | FInterval ((a, b), (a', b')), FInterval ((c, _), (c', _)) -> 
        let f32min = if eq then get_max a c else get_max a (elt_add c (Float prec_unit) true) in
        let f64min = if eq then get_max a' c' else get_max a' (elt_add c' (Float prec_unit) false) in
        FInterval ((f32min, b), (f64min, b'))
      | Bot, _ | _, Bot -> Bot
      | _ -> 
      print_endline "Inside extracting greater ";
      raise Impossible ) in
    let extract_less eq x y = 
      (match x,y with 
      | IntInterval (a, b), IntInterval (_, d) -> 
        let max = 
          if eq then get_min b d else get_min b (elt_sub d (Int 1) false) in 
        IntInterval (a, max)
      | FInterval ((a, b), (a', b')), FInterval ((_, d), (_, d')) -> 
        let f32max = if eq then get_min b d else get_min b (elt_sub d (Float prec_unit) true) in
        let f64max = if eq then get_min b' d' else get_min b' (elt_sub d' (Float prec_unit) false) in
        FInterval ((a, f32max), (a', f64max))
      | Bot, _ | _, Bot -> Bot
      | _ -> 
      print_endline "Inside extracting less";
      raise Impossible ) in   
    match pred with 
    | Icmp Llvm.Icmp.Eq | Fcmp Llvm.Fcmp.Oeq | Fcmp Llvm.Fcmp.Ueq ->
      if are_disjoint v1 v2 then Bot
      else if order v1 v2 then v1
      else get_intersection v1 v2
    | Icmp Llvm.Icmp.Ne | Fcmp Llvm.Fcmp.One | Fcmp Llvm.Fcmp.Une-> 
      if are_disjoint v1 v2 then v1
      else if are_equal v1 v2 then Bot
      else check_bottom v1 v2
    | Icmp Llvm.Icmp.Ugt | Icmp Llvm.Icmp.Sgt -> 
      if cmp (Icmp Llvm.Icmp.Ugt) v1 v2 = truev then v1
      else if cmp (Icmp Llvm.Icmp.Ugt) v1 v2 = falsev then Bot
      else extract_greater false v1 v2
    | Fcmp Llvm.Fcmp.Ogt | Fcmp Llvm.Fcmp.Ugt -> 
      if cmp (Fcmp Llvm.Fcmp.Ogt) v1 v2 = truev then v1
      else if cmp (Fcmp Llvm.Fcmp.Ogt) v1 v2 = falsev then Bot
      else extract_greater false v1 v2
    | Icmp Llvm.Icmp.Uge | Icmp Llvm.Icmp.Sge -> 
      if cmp (Icmp Llvm.Icmp.Uge) v1 v2 = truev then v1
      else if cmp (Icmp Llvm.Icmp.Uge) v1 v2 = falsev then Bot
      else extract_greater true v1 v2
    | Fcmp Llvm.Fcmp.Oge | Fcmp Llvm.Fcmp.Uge -> 
      if cmp (Fcmp Llvm.Fcmp.Oge ) v1 v2 = truev then v1
      else if cmp (Fcmp Llvm.Fcmp.Oge ) v1 v2 = falsev then Bot
      else extract_greater true v1 v2
    | Icmp Llvm.Icmp.Ult | Icmp Llvm.Icmp.Slt ->
      if cmp (Icmp Llvm.Icmp.Ult) v1 v2 = truev then v1
      else if cmp (Icmp Llvm.Icmp.Ult) v1 v2 = falsev then Bot
      else extract_less false v1 v2
    | Fcmp Llvm.Fcmp.Olt | Fcmp Llvm.Fcmp.Ult ->
      if cmp (Fcmp Llvm.Fcmp.Olt) v1 v2 = truev then v1
      else if cmp (Fcmp Llvm.Fcmp.Olt) v1 v2 = falsev then Bot
      else extract_less false v1 v2
    | Icmp Llvm.Icmp.Ule | Icmp Llvm.Icmp.Sle ->
      if cmp (Icmp Llvm.Icmp.Ule) v1 v2 = truev then v1
      else if cmp (Icmp Llvm.Icmp.Ule) v1 v2 = falsev then Bot
      else extract_less true v1 v2
    | Fcmp Llvm.Fcmp.Ole | Fcmp Llvm.Fcmp.Ule ->
      if cmp (Fcmp Llvm.Fcmp.Ole) v1 v2 = truev then v1
      else if cmp (Fcmp Llvm.Fcmp.Ole) v1 v2 = falsev then Bot
      else extract_less true v1 v2
    | Fcmp Llvm.Fcmp.True -> truev 
    | Fcmp Llvm.Fcmp.False -> falsev
    | _ -> 
    print_endline "Inside filtering";
    raise Impossible

end

module Memory (Value : NUMERICAL_DOMAIN) :
  MEMORY_DOMAIN with type Value.t = Value.t = struct
  module M = Map.Make (Variable)
  module Value = Value

  type t = Value.t M.t

  let bottom = M.empty

  let add = M.add

  let fold = M.fold

  let iter = M.iter 

  let compare = compare

  let find x m = try M.find x m with Not_found -> Value.bottom

  let order m1 m2 = 
    M.for_all (fun l v -> 
      match M.find_opt l m2 with
      | None -> false 
      | Some v2 -> Value.order v v2) m1

  let join m1 m2 = 
    let all_keys = 
      M.fold (fun k _v acc -> k :: acc ) m1 []
      |> List.append (M.fold (fun k _v acc -> k :: acc) m2 []) in
    List.fold_left 
      (fun m k  -> M.add k (Value.join (find k m1) (find k m2)) m ) bottom all_keys

  let widen m1 m2 = 
    let all_keys = 
      M.fold (fun k _v acc -> k :: acc ) m1 []
      |> List.append (M.fold (fun k _v acc -> k :: acc) m2 []) in
    List.fold_left 
      (fun m k  -> M.add k (Value.widen (find k m1) (find k m2)) m ) bottom all_keys

  let narrow m1 m2 = 
    M.mapi (fun l v -> 
      match M.find_opt l m2 with 
      | None -> Value.bottom
      | Some v2 -> Value.narrow v v2) m1

  let pp fmt m =
    M.iter (fun k v -> F.fprintf fmt "%a -> %a\n" Variable.pp k Value.pp v) m
end

module Table (M : MEMORY_DOMAIN) = struct
  include Map.Make (Label)

  let find label tbl = try find label tbl with Not_found -> M.bottom
end

module Worklist = struct
  include Set.Make (Label)

  let rec collect_labels acc pos =
    let acc = add pos acc in
    match pos with
    | Llvm.At_end _ -> acc
    | Llvm.Before instr -> collect_labels acc (Llvm.instr_succ instr)

  let init m =
    Llvm.fold_left_functions
      (fun a func ->
        if Llvm.is_declaration func then a
        else
          Llvm.fold_left_blocks
            (fun a blk ->
              let pos = Llvm.instr_begin blk in
              collect_labels a pos)
            a func)
      empty m
end