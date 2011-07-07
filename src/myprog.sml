signature SET = sig
	type ''a myset = ''a list
	val belongs : ''a*(''a list) -> bool
	val makeSet : ''a list -> ''a myset
	val additem : ''a*(''a list) -> ''a myset
	val addlist : (''a list)*(''a myset) -> ''a myset
	val union : (''a myset)*(''a myset) -> ''a myset
	val intersection : (''a myset)*(''a myset) -> ''a myset
	val getlist : ''a myset -> ''a list
	val addToAll : ''a*''a myset myset -> ''a myset myset
	val powerSet : ''a myset -> ''a myset myset
end
structure Set :> SET = struct
	type ''a myset = ''a list
	fun belongs (x,[]) = false
		| belongs (x,y::ys) = if (x=y) then true else belongs(x,ys)
	fun makeSet [] = []
		| makeSet (x::xs) =
			let
				val tmpst = makeSet xs
				fun adder(z) = 
					if (belongs(z,tmpst)) then tmpst
					else z::tmpst
			in adder(x)
			end
	fun additem (x,tmpst) = if (belongs(x,tmpst)) then tmpst else x::tmpst
	fun addlist ([],tmpst) = tmpst
		| addlist (x::xs,tmpst) = additem(x,addlist(xs,tmpst))
	fun union ([],tmpst) = tmpst
		| union (x::xs,tmpst) = additem(x,union(xs,tmpst))
	fun intersection ([],tmpst) = nil
		| intersection (x::xs,tmpst) = if (belongs(x,tmpst)) then x::intersection(xs,tmpst) else intersection(xs,tmpst)
	fun getlist [] = []
		| getlist (x::xs) = x::xs
	fun addToAll (x,[]) = []
		| addToAll (x,y::ys) = additem(x,y)::addToAll(x,ys)
	fun powerSet [] = [[]]
		| powerSet (x::xs) = (powerSet(xs)@addToAll(x,powerSet(xs)))
end
signature NUMBERS = sig
	datatype digits = digit of char
	datatype sign = sign of char
	datatype naturals = nat of digit | nat of naturals*digit
	datatype integers = zero | Positive of naturals | Negative of naturals
	exception notADigit
	exception notANatural
	exception notAInteger
	val toDigit : char -> digits
	val toNatural : string -> naturals
	val toInteger : string -> integers
end
structure Numbers :> NUMBERS = struct
	datatype digits = digit of char
	datatype sign = sign of char
	datatype naturals = nat of digit | nat of naturals*digit
	fun toDigit (#'0') = digit of #'0'
		|toDigit(#'1') = digit of #'1'
		|toDigit(#'2') = digit of #'2'
		|toDigit(#'3') = digit of #'3'
		|toDigit(#'4') = digit of #'4'
		|toDigit(#'5') = digit of #'5'
		|toDigit(#'6') = digit of #'6'
		|toDigit(#'7') = digit of #'7'
		|toDigit(#'8') = digit of #'8'
		|toDigit(#'9') = digit of #'9'
		|toDigit(_) = raise NotADigit
	fun toNatural x = 
		let
			fun charlst(x)= String.explode(x)
			fun toDigitList nil = nil
				| toDigitList(x::xs) = toDigit(x)::toDigitList(xs)
			fun toNat [] = raise notANatural
			| fun toNat((digit of #'0')::xs) = raise notANatural
			| fun toNat(y) = 
				let 
					fun rev [] = []
						| rev(x::xs) = rev(xs) @ [x]
					fun toNatFrmRevDig([x]) = (nat of x)
						| toNatFrmRevDig(x::xs) = nat(toNatFrmRevDig(xs),x)
				in toNatFrmDig(rev(y))
				end
		in toNat(toDigitList(charlst(x)))
		end
		handle notADigit => notANatural
	fun toInteger x =
		let
			fun charlst(x) = String.explode(x)
			fun toIntegerFrmChrs([#'0']) = Zero
			| toIntegerFrmChrs(#'+'::xs) = Positive(toNatural(String.implode(xs)))
			| toIntegerFrmChrs(#'-'::xs) = Negative(toNatural(String.implode(xs)))
			| toIntegerFrmChrs(_) = raise notAInteger
		in toIntegerFrmChrs(charlst(x))
		end
		handle notANatural => notAInteger
end
signature VARIABLE = sig
	datatype VAR = Var of Numbers.naturals*string*string | Var of int*string*string | Var of char*string*string | Var of String
	val toVar : string*string*string -> VAR
end