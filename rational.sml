signature BIGINT = 
   sig 
      type bigint = int list * bool
        (* exception bigint_error *)
      (* val add : bigint * bigint * bigint * int -> bigint *)
      val muldigit : int list * int * int list * int -> int list
      val subtractP : int list * int list * int list * int -> int list
      (* val multiply : bigint * bigint * bigint -> bigint *)
      val findfactor : int list * int list * int -> int
      val isGreater : int list * int list -> bool
      val isEqual : int list * int list -> bool
      val add : bigint * bigint -> bigint
      val subtract : bigint * bigint -> bigint
      val multiplyBigint : bigint * bigint -> bigint
      val removeZeros : bigint -> bigint
      val divideP : int list * int list * int list * int list -> int list * int list
      val divide : bigint * bigint -> bigint
      val modulus : bigint * bigint -> bigint
      val gcd : bigint * bigint -> bigint
      val ifEqual : bigint * bigint -> bool
      val ifLess : bigint * bigint -> bool
      val multiply : bigint * bigint -> bigint
      (* val gcdMain : bigint * bigint -> bigint *)
      (* fromstring and toString can also be implemented *)
   end;


structure BigInt:BIGINT = struct

   type bigint = int list * bool

   fun remove_zeros(l : int list) = 
      if List.length(l) = 0 then [0]
      else
         if (hd(l) = 0) then remove_zeros(tl(l))
         else l

   fun addP(num1 : int list, num2 : int list, ans : int list, carry : int) =  (*num1 + num2 kar rhe he*)
      if List.length(num2) = 0 andalso List.length(num1) = 0 then 
         if carry = 0 then ans
         else carry :: ans
      else
         if List.length(num2) = 0 then addP(tl(num1), [], (hd(num1) + carry) mod 10 :: ans, (hd(num1) + carry) div 10)
         else 
            if List.length(num1) = 0 then addP([], tl(num2), (hd(num2) + carry) mod 10 :: ans, (hd(num2) + carry) div 10)
            else addP(tl(num1), tl(num2), (hd(num1) + hd(num2) + carry) mod 10 :: ans, (hd(num1) + hd(num2) + carry) div 10)
   
   fun subtractP(num1 : int list, num2 : int list, ans : int list, carry : int) =  (*num1 - num2 kar rhe he*)
      if List.length(num1) = 0 then remove_zeros(ans)
      else
         if List.length(num2) = 0 then 
            if hd(num1) + carry < 0 then subtractP(tl(num1), [], (hd(num1) + carry + 10)::ans, ~1)
            else subtractP(tl(num1), [], (hd(num1) + carry) :: ans, 0) 
         else
            if hd(num1) - hd(num2) + carry < 0 then subtractP(tl(num1), tl(num2), (hd(num1) - hd(num2) + carry + 10) :: ans , ~1)
            else subtractP(tl(num1), tl(num2), (hd(num1) - hd(num2) + carry) :: ans, 0)
   
   fun isGreater(l1 : int list, l2 : int list) =  (* returns true if l1 > l2 (l1 and l2 seedhe he dono) *)
      if List.length(l1) = List.length(l2) then 
         if List.length(l1) = 0 then false
         else
            if hd(l1) = hd(l2) then isGreater(tl(l1), tl(l2))
            else hd(l1) > hd(l2)
      else List.length(l1) > List.length(l2)

   fun isEqual(l1 : int list, l2 : int list) = 
      if List.length(l1) = List.length(l2) then 
         if List.length(l1) = 0 then true
         else
            if hd(l1) = hd(l2) then isEqual(tl(l1), tl(l2))
            else false
      else List.length(l1) = List.length(l2)

   fun removeZeros(l : bigint) = 
      if List.length(#1 l) = 0 then ([0], true)  (* 0 is considered positive *)
      else
         if (hd(#1 l) = 0) then removeZeros((tl(#1 l), #2 l))
         else l

   fun addBigint(l1 : bigint, l2 : bigint) = 
      if #2 l1 andalso #2 l2 then (addP(rev(#1 l1), rev(#1 l2), [], 0), true) : bigint
      else 
         if not (#2 l1) andalso not (#2 l2) then (addP(rev(#1 l1), rev(#1 l2), [], 0), false) : bigint
         else 
            if #2 l1 andalso not (#2 l2) then
               if isEqual(#1 l1, #1 l2) then ([0], true) : bigint
               else 
                  if isGreater(#1 l1, #1 l2) then (subtractP(rev(#1 l1), rev(#1 l2), [], 0), true) : bigint
                  else (subtractP(rev(#1 l2), rev(#1 l1), [], 0), false) : bigint
            else 
               if isEqual(#1 l1, #1 l2) then ([0], true) : bigint
               else 
                  if isGreater(#1 l1, #1 l2) then (subtractP(rev(#1 l1), rev(#1 l2), [], 0), false) : bigint
                  else (subtractP(rev(#1 l2), rev(#1 l1), [], 0), true) : bigint   

   fun subtractBigint(l1 : bigint, l2 : bigint) =
      if #2 l1 andalso not (#2 l2) then (addP(rev(#1 l1), rev(#1 l2), [], 0), true) : bigint
      else 
         if not (#2 l1) andalso #2 l2 then (addP(rev(#1 l1), rev(#1 l2), [], 0), false) : bigint
         else 
            if #2 l1 andalso #2 l2 then
               if isEqual(#1 l1, #1 l2) then ([0], true) : bigint
               else 
                  if isGreater(#1 l1, #1 l2) then (subtractP(rev(#1 l1), rev(#1 l2), [], 0), true) : bigint
                  else (subtractP(rev(#1 l2), rev(#1 l1), [], 0), false) : bigint
            else
               if isEqual(#1 l1, #1 l2) then ([0], true) : bigint
               else 
                  if isGreater(#1 l1, #1 l2) then (subtractP(rev(#1 l1), rev(#1 l2), [], 0), false) : bigint
                  else (subtractP(rev(#1 l2), rev(#1 l1), [], 0), true) : bigint

   fun compare(l1 : int list, l2 : int list) =  (* returns true if l1 >= l2 (l1 and l2 seedhe he dono) *)
      if (List.length(l1) = 0 andalso List.length(l2) = 0) then true
      else
         if (List.length(l1) = List.length(l2)) then
            if (hd(l1) = hd(l2)) then compare(tl(l1), tl(l2))
            else (hd(l1) > hd(l2))
         else (List.length(l1) > List.length(l2))

   fun muldigit(num : int list, muldig : int, ans : int list, carry : int) =
      if muldig = 0 then [0]
      else
         if List.length(num) = 0 then 
            if carry = 0 then ans
            else carry :: ans
         else muldigit(tl(num), muldig, (hd(num)*muldig + carry) mod 10 :: ans, (hd(num)*muldig + carry) div 10)

   fun multiplyP(num1 : int list, num2 : int list, ans : int list) = 
      if List.length(num1) = 1 then muldigit(num2, hd(num1), [], 0)
      else addP(List.rev(muldigit(num2, hd(num1), [], 0)), 0 :: List.rev(multiplyP(tl(num1), num2, [])), [], 0)

   fun multiplyBigint(int1 : bigint, int2 : bigint) = 
      if (#2 int1 andalso #2 int2) orelse (not (#2 int1) andalso not (#2 int2)) then (multiplyP(rev(#1 int1),rev(#1 int2), []), true)
      else (multiplyP(rev(#1 int1), rev(#1 int2), []), false)

   fun findfactor(dvr : int list, rem : int list, ans : int) = 
      if compare(List.rev(rem), muldigit(dvr, ans, [], 0)) then findfactor(dvr, rem, ans + 1)
      else 
         if List.length(rem) = 0 then 0
         else ans - 1

   fun remove_zeros(l : int list) = 
      if List.length(l) = 0 then [0]
      else
         if (hd(l) = 0) then remove_zeros(tl(l))
         else l

   fun removezero(l : int list) = 
      if List.length(l) = 0 then l
      else
         if (hd(l) = 0) then removezero(tl(l))
         else l

   fun divideP(dvd : int list, dvr : int list, quot : int list, rem : int list) =
      let
         val factor = findfactor(dvr, rem, 0)
         val rem_new = subtractP(rem, List.rev(removezero(muldigit(dvr, factor, [], 0))), [], 0)
      in
         if List.length(dvd) = 0 then (remove_zeros(List.rev(factor::quot)), remove_zeros(rem_new))
         else divideP(tl(dvd), dvr, factor::quot, hd(dvd)::List.rev(removezero(rem_new)))
      end

   fun subtract(l1 : bigint, l2 : bigint) = subtractBigint(removeZeros(l1), removeZeros(l2))
   fun add(l1 : bigint, l2 : bigint) = addBigint(removeZeros(l1), removeZeros(l2))
   fun multiply(l1 : bigint, l2 : bigint) = multiplyBigint(removeZeros(l1), removeZeros(l2))
   fun divide(l1 : bigint, l2 : bigint) = 
      if (#1 l2) = [1] then (#1 l1, not ((not (#2 l1) andalso (#2 l2)) orelse ((#2 l1) andalso not (#2 l2))))
      else (#1 (divideP(#1 (removeZeros(l1)),rev(#1 (removeZeros(l2))),[],[])), not ((not (#2 l1) andalso (#2 l2)) orelse ((#2 l1) andalso not (#2 l2))))
   fun modulus(l1 : bigint, l2 : bigint) = 
      if (#1 l2) = [1] then ([0], true)
      else (#2 (divideP(#1 (removeZeros(l1)),rev(#1 (removeZeros(l2))),[],[])), true)
   
   fun gcdMain(l1 : bigint, l2 : bigint) = 
      if l1 = ([0], true) then l2 
      else gcdMain(modulus(l2, l1), l1)

   fun gcd(l1 : bigint, l2 : bigint) = gcdMain((#1 (removeZeros(l1)), true), (#1 (removeZeros(l2)), true))

   fun ifEqual(l1 : bigint, l2 : bigint) = 
      if (not (#2 l1) andalso (#2 l2)) orelse ((#2 l1) andalso not (#2 l2)) then false
      else isEqual(#1 l1, #1 l2)

   fun ifLess(l1 : bigint, l2 : bigint) = 
      if (not (#2 l1) andalso (#2 l2)) orelse ((#2 l1) andalso not (#2 l2)) then (not (#2 l1) andalso (#2 l2))
      else (((#2 l1) andalso (#2 l2)) andalso isGreater(#1 l2, #1 l1)) orelse isGreater(#1 l1, #1 l2)

end;

signature RATIONAL =
   sig
        type rational = BigInt.bigint * BigInt.bigint
        exception rat_error
        val make_rat: BigInt.bigint * BigInt.bigint -> rational option
        val rat: BigInt.bigint -> rational option
        val reci: BigInt.bigint -> rational option
        val neg: rational -> rational
        val inverse : rational -> rational option
        val equal : rational * rational -> bool (* equality *)
        val less : rational * rational -> bool (* less than *)
        val add : rational * rational -> rational (* addition *)
        val subtract : rational * rational -> rational (* subtraction *)
        val multiply : rational * rational -> rational (* multiplication *)
        val divide : rational * rational -> rational option (* division *)
        val showRat : rational -> string
        val showDecimal : rational -> string
        val fromDecimal : string -> rational
        val toDecimal : rational -> string
end;

functor Rational (BigInt:BIGINT) : RATIONAL
= struct
   type rational = BigInt.bigint * BigInt.bigint
   exception rat_error

   fun make_rat(num : BigInt.bigint, denom : BigInt.bigint) = 
      if BigInt.removeZeros(denom) = ([0], true) then raise rat_error 
         else 
            let
               val p = BigInt.divide(num, BigInt.gcd(num,denom))
               val q = BigInt.divide(denom, BigInt.gcd(num,denom))
            in SOME ((#1 p, not ((not (#2 p) andalso (#2 q)) orelse ((#2 p) andalso not (#2 q)))), (#1 q, true))
            end

   fun rat(num : BigInt.bigint) = make_rat(num, ([1], true))
   fun reci(num : BigInt.bigint) = make_rat(([1], true), num)
   fun neg(num : rational) = valOf(make_rat((#1(#1 num), not (#2(#1 num))), #2 num))
   fun inverse(num : rational) = make_rat(#2 num, #1 num)
   fun equal(rat1 : rational, rat2 : rational) = BigInt.ifEqual(BigInt.multiply(#1 rat1, #2 rat2), BigInt.multiply(#2 rat1, #1 rat2))
   fun less(rat1 : rational, rat2 : rational) = BigInt.ifLess(BigInt.multiply(#1 rat1, #2 rat2), BigInt.multiply(#2 rat1, #1 rat2))
   fun add(rat1 : rational, rat2 : rational) = valOf(make_rat(BigInt.add(BigInt.multiply(#1 rat1, #2 rat2), BigInt.multiply(#2 rat1, #1 rat2)), BigInt.multiply(#2 rat1, #2 rat2)))
   fun subtract(rat1 : rational, rat2 : rational) = valOf(make_rat(BigInt.subtract(BigInt.multiply(#1 rat1, #2 rat2), BigInt.multiply(#2 rat1, #1 rat2)), BigInt.multiply(#2 rat1, #2 rat2)))
   fun multiply(rat1 : rational, rat2 : rational) = valOf(make_rat(BigInt.multiply(#1 rat1, #1 rat2), BigInt.multiply(#2 rat1, #2 rat2)))
   fun divide(rat1 : rational, rat2 : rational) = make_rat(BigInt.multiply(#1 rat1, #2 rat2), BigInt.multiply(#2 rat1, #1 rat2))
   
   fun beforeDecimal(l : char list, ans : int list) =
      if hd(l) = #"." then List.rev(ans)
      else beforeDecimal(tl(l), (Char.ord(hd(l))-48) :: ans)
   
   fun afterDecimal(l : char list, ans : int list, b : bool) = 
      if hd(l) = #"." then afterDecimal(tl(l), [], true)
      else 
         if b then 
            if hd(l) = #"(" then List.rev(ans)
            else afterDecimal(tl(l), (Char.ord(hd(l))-48) :: ans, true)
         else afterDecimal(tl(l), [], false)
   
   fun repeatingPart(l : char list, ans : int list, b : bool) = 
      if hd(l) = #"(" then repeatingPart(tl(l), [], true)
      else 
         if b then 
            if hd(l) = #")" then List.rev(ans)
            else repeatingPart(tl(l), (Char.ord(hd(l))-48) :: ans, true)
         else repeatingPart(tl(l), [], false)

   fun makeTen(n : int, l : int list) = 
      if n = 0 then List.rev(l)
      else makeTen(n-1, 0 :: l)

   fun makeNine(n : int, m : int, l : int list) = 
      if n = 0 andalso m = 0 then List.rev(l)
      else  
        if n = 0 then makeNine(0, m-1, 0 :: l)
        else makeNine(n-1, m, 9 :: l)

   fun toStr(l : int list, ans : char list ) = 
      if List.length(l) = 0 then String.implode(List.rev(ans))
      else toStr(tl(l), Char.chr(48+hd(l))::ans)
   
   fun fromDecimal(decimal : string) = 
      if hd(String.explode(decimal)) = #"~" then
         let 
         val integerPart = beforeDecimal(tl(String.explode(decimal)), [])
         val nonRepeatingPart = afterDecimal(tl(String.explode(decimal)), [], false)
         val recurringPart = repeatingPart(tl(String.explode(decimal)), [], false)
         val ratIntegerPart = make_rat(BigInt.removeZeros(integerPart, true), ([1], true))
         val ratNonRepeatingPart = make_rat(BigInt.removeZeros(nonRepeatingPart, true), (makeTen(List.length(nonRepeatingPart), [1]), true))
         val ratRecurringPart = make_rat(BigInt.removeZeros(recurringPart, true), (makeNine(List.length(recurringPart), List.length(nonRepeatingPart), []), true))
         in
         neg(add(add(valOf(ratIntegerPart), valOf(ratNonRepeatingPart)), valOf(ratRecurringPart)))
         end
      else 
         if hd(String.explode(decimal)) = #"+" then
            let 
            val integerPart = beforeDecimal(tl(String.explode(decimal)), [])
            val nonRepeatingPart = afterDecimal(tl(String.explode(decimal)), [], false)
            val recurringPart = repeatingPart(tl(String.explode(decimal)), [], false)
            val ratIntegerPart = make_rat(BigInt.removeZeros(integerPart, true), ([1], true))
            val ratNonRepeatingPart = make_rat(BigInt.removeZeros(nonRepeatingPart, true), (makeTen(List.length(nonRepeatingPart), [1]), true))
            val ratRecurringPart = make_rat(BigInt.removeZeros(recurringPart, true), (makeNine(List.length(recurringPart), List.length(nonRepeatingPart), []), true))
            in
            add(add(valOf(ratIntegerPart), valOf(ratNonRepeatingPart)), valOf(ratRecurringPart))
            end   
         else 
            let 
            val integerPart = beforeDecimal(String.explode(decimal), [])
            val nonRepeatingPart = afterDecimal(String.explode(decimal), [], false)
            val recurringPart = repeatingPart(String.explode(decimal), [], false)
            val ratIntegerPart = make_rat(BigInt.removeZeros(integerPart, true), ([1], true))
            val ratNonRepeatingPart = make_rat(BigInt.removeZeros(nonRepeatingPart, true), (makeTen(List.length(nonRepeatingPart), [1]), true))
            val ratRecurringPart = make_rat(BigInt.removeZeros(recurringPart, true), (makeNine(List.length(recurringPart), List.length(nonRepeatingPart), []), true))
            in
            add(add(valOf(ratIntegerPart), valOf(ratNonRepeatingPart)), valOf(ratRecurringPart))
            end

   fun toStrg(l : (int*int*BigInt.bigint) list, ans : char list ) = 
      if List.length(l) = 0 then String.implode(List.rev(ans))
      else toStrg(tl(l), Char.chr(48+(#1(hd(l))))::ans)

   fun toStrg1(l : (int*int*BigInt.bigint) list, ans : string ) = 
      if List.length(l) = 0 then ans
      else toStrg1(tl(l), ans^" "^toStr(#1(#3(hd(l))), []))

   fun showRat(rat : rational) = 
      if #2(#1 rat) then toStr(#1(#1 rat), [])^"/"^toStr(#1(#2 rat), []) 
      else "~"^toStr(#1(#1 rat), [])^"/"^toStr(#1(#2 rat), []) 

   fun lookup(x : int, l : (int*int*BigInt.bigint) list, index : int, rem : BigInt.bigint) = 
      if List.length(l) = 0 then ~1
      else 
         if BigInt.isEqual(#1(#3(hd(l))), #1 rem) then index 
         else lookup(x, tl(l), index + 1, rem)

   fun processAfterDecimal(ans : (int*int*BigInt.bigint) list, index : int, recurring : int list, nonRecurring : int list, count : int, b : bool) = 
      if List.length(ans) = 0 then toStr(List.rev(nonRecurring), [])^"("^toStr(List.rev(recurring), [])^")"
      else 
         if b then processAfterDecimal(tl(ans), index, #1(hd(ans)) :: recurring, nonRecurring, count + 1, true)
         else
            if count = index then processAfterDecimal(tl(ans), index, #1(hd(ans)) :: recurring, nonRecurring, count + 1, true)
            else processAfterDecimal(tl(ans), index, recurring, #1(hd(ans)) :: nonRecurring, count + 1, false)

   fun showDecimalHelp(rem : BigInt.bigint, dvr : BigInt.bigint, ans : (int*int*BigInt.bigint) list, index : int) = 
      let 
      val dig = hd(#1(BigInt.divide(BigInt.multiply(rem, ([1,0], true)), dvr)))
      val newRem = BigInt.modulus(BigInt.multiply(rem, ([1,0], true)), dvr)
      in
      if lookup(dig, List.rev(ans), 1, rem) = ~1 then showDecimalHelp(newRem, dvr, (dig,index,rem)::ans, index + 1)
      else processAfterDecimal(List.rev(ans), lookup(dig, List.rev(ans), 1, rem), [], [], 1, false)
      end

   fun showDecimal(rat : rational) = 
      if #2 (#1 rat) then toStr(#1(BigInt.divide(#1 rat, #2 rat)), [])^"."^showDecimalHelp(BigInt.modulus(#1 rat, #2 rat), #2 rat, [], 1)
      else "~"^toStr(#1(BigInt.divide(#1 rat, #2 rat)), [])^"."^showDecimalHelp(BigInt.modulus(#1 rat, #2 rat), #2 rat, [], 1)
   
   fun toDecimal(rat : rational) = showDecimal(rat)
end;

structure Rational = Rational(BigInt);