type var = string  (*name of the signal e.g., A B C*)
type class_name = string
type mn = string

type spec = SPEC
type static = spec 
type dynamic = spec

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

type mType = Virtual | Overide | Inherit | Origin
type formalArguments = ((tau * var) list) 
type classMeth = mType * tau * mn * formalArguments (* static * dynamic*) * expression 
type classDefinition = class_name * (class_name option) * formalArguments * (classMeth list) 
type program = classDefinition list 