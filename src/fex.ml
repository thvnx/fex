(** In the spirit of double-word, triple-word, and quad-word
arithmetics, the arithmetic on floating-point expansions was first
developed by Priest {{: string } 336}, and in a slightly different way
by Shewchuk {{: string } 377}.

If, starting from some set of floating-point inputs, we only perform
exact additions and multiplications, then the values we obtain are
always equal to finite sums of floating-point numbers. Such finite
sums are called expansions. Hence, a natural idea is to try to
manipulate such expansions, for performing calculations that are
either exact, or approximate yet very accurate. *)



(** Accurate computations of sums and products of two numbers. *)
module EFT = struct

  (** Let [a] and [b] be floating-point numbers, and assume that the
  exponent of [a] is larger than or equal to that of [b],
  [fast_tow_sum a b] computes two floating-point numbers [s] and [t]
  that satisfy the following:

  - [s] + [t] = [a] + [b] exactly;
  - [s] is the floating-point number that is closest to [a +. b].

  The [fast_tow_sum] algorithm was introduced by Dekker {{: string }
  108} in 1971 (but already appeared in 1965 as part of a summation
  algorithm due to Kahan {{: string } 201}. *)
  let fast_two_sum a b =
    let s = a +. b in
    let t = b -. (s -. a) in
    (s , t)

  (** Let [a] and [b] be floating-point numbers, [tow_sum a b]
  computes two floating-point numbers [s] and [t] that satisfy the
  following:

  - [s] + [t] = [a] + [b] exactly;
  - [s] is the floating-point number that is closest to [a +. b].

  The [two_sum] algorithm, due to Knuth {{: string } 222} and Moller
  {{: sting } 279}, requires 6 consecutive floating-point operations
  instead of 3 for [fast_two_sum], but does not require a preliminary
  comparison of [a] and [b]. *)
  let two_sum a b =
    let s = a +. b in
    let u = s -. a in
    let t = (a -. (s -. u)) +. (b -. u) in
    (s , t)

  (** Let [a] and [b] be floating-point numbers, [tow_product a b]
  computes two floating-point numbers [s] and [t] that satisfy the
  following:

  - [s] + [t] = [a] * [b] exactly;
  - [s] is the floating-point number that is closest to [a *. b].

  The [two_product] algorithm was discovered by Dekker {{: string }
  108}. *)
  let two_product a b =
    (* Veltkamp splitting *)
    let split a =
      let c = 134217729. *. a in
      let ah = c -. (c -. a) in
      let al = a -. ah in
      (ah , al)
    in
    let s = a *. b in
    let (ah , al) = split a in
    let (bh , bl) = split b in
    let t = (al *. bl) -. (((s -. ah *. bh) -. al *. bh) -. ah *. bl) in
    (s , t)

end;;






class _Expansion expansion_value =
object(self)
  (** Definition 18 (Expansion—adapted from Shewchuk’s definition
[377]). An expansion x is a set of n floating-point numbers x1, x2,
. . . , xn used for representing the real number *)
  val mutable value = expansion_value

  method get_value = value
  method set_value v =
    value <- v

  method to_string =
    let rec ts acc l =
      match l with
      | []      -> acc
      | h :: [] -> acc ^ (Printf.sprintf "%e" h)
      | h :: t  -> ts (acc ^ (Printf.sprintf "%e" h) ^ " & ") t
    in
    ts "" (List.rev value)

  method res_to_string =
    Printf.sprintf "%e" (List.hd (List.rev value))

  method add (x:_Expansion) =
    let r = new _Expansion (self#fast_expansion_sum x#get_value) in
    r#compress;
    r

  method mul (x:_Expansion) =
    let r = new _Expansion (self#expansion_product x#get_value) in
    r#compress;
    r


  (** Algorithm 14.7 (p 505)
   * Shewchuk's Grow-Expansion algorithm [377] Input values: an S-nonoverlapping expansion e of m components and a floating-point number b. Output value: an S-nonoverlapping expansion e of m + 1 com- ponents. We assume that the radix is 2 and that the precision p satisfies p   3. *)
  method grow_expansion x =
    let rec grow acc op1 op2 =
      match op1 with
      | h :: t ->
         let (xx , yy) = EFT.two_sum op2 h in
         grow (acc @ [yy]) t xx
      | []     ->
         acc @ [op2]
    in
    grow [] value x

  method expansion_sum x =
    let rec sum acc op1 op2 =
      match op2 with
      | h :: t ->
         let grow = (op1#grow_expansion h) in
         sum (acc @ [List.hd grow]) (new _Expansion (List.tl grow)) t
      | []     -> acc @ op1#get_value
    in
    let tmp = new _Expansion value in
    sum [] tmp x

  method fast_expansion_sum x =
    let merge a b =
      List.merge (fun x y -> if x >= y then 1 else -1) a b
    in
    let rec fast_sum acc op1 op2 =
      match op1 with
      | h :: t ->
         let xx, yy = EFT.two_sum op2 h in
         fast_sum (acc @ [yy]) t xx
      | []     ->
         acc @ [op2]
    in
    let tmp = merge x value in
    let (xx, yy) = EFT.fast_two_sum (List.nth tmp 1) (List.nth tmp 0) in
    fast_sum [yy] (List.tl (List.tl tmp)) xx

  method scale_expansion x =
    let rec scale acc op1 op2 op3 =
      match op1 with
      | h :: t ->
         let ph, pl = EFT.two_product h op3 in
         let sh, sl = EFT.two_sum op2 pl in
         let fh, fl = EFT.fast_two_sum ph sh in
         scale (acc @ [sl ; fl]) t fh op3
      | []     ->
         acc @ [op2]
    in
    let (xx, yy) = EFT.two_product (List.hd value) x in
    scale [yy] (List.tl value) xx x

  method distillation_sum x =
    let rec create_first_expansions acc lst =
      match lst with
      | e1 :: e2 :: t  ->
         let xx, yy = EFT.two_sum e1 e2 in
         create_first_expansions (acc @ [new _Expansion [xx; yy]]) t
      | e :: []        ->
         acc @ [new _Expansion [e]]
      | []             ->
         acc
    in
    let rec sum acc op =
      match op with
      | h :: t ->
         sum (h#fast_expansion_sum acc) t
      | []     ->
         acc
    in
    let tmp = create_first_expansions [] x in
    let res = sum (List.hd tmp)#get_value (List.tl tmp) in
    res

  method expansion_product x =
    let rec partial_product acc op1 =
      match op1 with
      | h :: t ->
         partial_product (acc @ (self#scale_expansion h)) t
      | []     ->
         acc
    in
    self#distillation_sum (partial_product [] x)

  method compress =
    let rec first_pass acc op1 op2 =
      match op2 with
      | h :: t ->
         let xx, yy = EFT.fast_two_sum op1 h in
         if yy <> 0. then
           first_pass ([xx] @ acc) yy t
         else
           first_pass acc xx t
      | []     ->
         (op1, acc)
    in
    let rec second_pass acc op1 op2 =
      match op1 with
      | h :: t ->
         let xx, yy = EFT.fast_two_sum h op2 in
         if yy <> 0. then
           second_pass (acc @ [yy]) t xx
         else
           second_pass acc t xx
      | []     ->
         acc @ [op2]
    in
    let exp = List.rev value in
    let (q, g) = first_pass [] (List.hd exp) (List.tl exp) in
    let t = second_pass [] g q in
    self#set_value t

end ;;


class adaptativeExpansion expansion_value =
object
  inherit _Expansion expansion_value

end ;;

class fixedLengthExpansion expansion_value =
object
  inherit _Expansion expansion_value

end ;;

let ( +.. ) (a:adaptativeExpansion) (b:adaptativeExpansion) = a#add b ;;
let ( *.. ) (a:adaptativeExpansion) (b:adaptativeExpansion) = a#mul b ;;

let ( ~.. ) (a:float) = new adaptativeExpansion [a];;

let float_to_fex_list l = List.map ( fun i -> ~.. i ) l
let float_to_fex l = ~.. l
