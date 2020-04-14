structure tigertips =
struct

type unique = unit ref

datatype Mode = RO | RW  

datatype Tipo = TUnit
	| TNil
	| TInt of Mode
	| TString
	| TArray of Tipo * unique
	| TRecord of (string * Tipo * int) list * unique
	| TFunc of Tipo list * Tipo
	| TTipo of string * Tipo option ref

end
