type var = string  (*name of the signal e.g., A B C*)
type class_name = string
type mn = string



type tau = Int | Bool | Void | Class of class_name

type value = INT of int | BOOL of bool | Null | Variable of var

type field = var * (var list)

type expression = 
        | Skip
        | Return of value 
        | Value of value
        | Assign of (var * expression)
        | AssignField of (field * expression)
        | Casting of class_name * value
        | NewObj of class_name * (var list)
        | Call of field * (var list)
        | Declear of tau * var 
        | InstanceOf of value *  class_name
        | Sequence of expression * expression
        | IfElse of value * expression * expression

type term = 
      Num of int
    | Var of string
    | Plus of term * term 
    | Minus of term * term 

type bin_op = GT | LT | EQ | GTEQ | LTEQ

type pi = 
  | True
  | False
  | Atomic of bin_op * term * term
  | And    of pi * pi
  | Or     of pi * pi
  | Imply  of pi * pi
  | Not    of pi

type speration_logic = 
  | Empty
  | FieldPointing of field * value 
  | TypePointing of var * class_name 
  | PointingTo of var * (class_name * value list)
  | Disjoint of speration_logic * speration_logic

type spec = (pi * speration_logic) list
type static = spec 
type dynamic = spec

type mType = Virtual | Overide | Inherit | Origin
type formalArguments = ((tau * var) list) 
type classMeth = mType * tau * mn * formalArguments  * static * dynamic * expression 
type classDefinition = class_name * (class_name option) * formalArguments * (classMeth list) 
type program = classDefinition list 