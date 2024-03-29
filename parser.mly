%{ open Ast %}
%{ open List %}


%token <string> VAR 
%token <string> UPPERCASEVAR
%token <int> INTE
%token LPAR RPAR  SIMI

%token EOF ENTIL EMPTY DISJ COMMA CONCAT  EQUAL END IN RUN OMEGA CONJ
%token LBRACK RBRACK PLUS MINUS
%token LSPEC RSPEC  COLON  TILDE INFIXOP0
%token CLASS EXTENDS TypeInt TypeBool TypeVoid OVERRIDE VIRTUAL INHERIT
%token TRUE FALSE  NULL RETURN NEW GREATER LESSEQ GreaterEQ
%token OPEN CLOSE REQUIERES ENSURES  IMPLICATION  LESS

%start full_prog 
%type <Ast.program> full_prog


%%
full_prog:
| EOF {[]}
| a = classDef r = full_prog { append [a] r }

extendinng:
| {None}
| EXTENDS  var = UPPERCASEVAR {Some var}

tau:
| TypeInt {Int}
| TypeBool {Bool}
| TypeVoid {Void}
| var = UPPERCASEVAR {Class var}



metyTYPE:
| OVERRIDE {Overide}
| VIRTUAL {Virtual}
| INHERIT {Inherit}

formalA :
| t= tau var=VAR {(t, var)}

formalARGUMENTS: 
| obj = separated_list (COMMA, formalA) {obj}

maybeCOnstructor:
| {None}
| var=VAR {Some var }

expression_shell:
| {Skip}
| expr = expression SIMI obj = expression_shell {
  Sequence (expr, obj)}
  

literal: 
| n = INTE {INT n}
| TRUE {BOOL true}
| FALSE {BOOL false}
| NULL {Null}
| var = VAR {Variable var}

maybeContinue:
| {None}
| CONCAT obj = separated_list(CONCAT, VAR) {Some obj}


expression:
| v = literal {Value v}
| t = tau var = VAR {Declear (t, var)}
| RETURN v = literal {Return v}
| var = VAR access= maybeContinue EQUAL v = expression  {
  match access with 
  | None -> Assign (var, v)
  | Some obj -> AssignField((var, obj), v)
  }
| NEW c = UPPERCASEVAR LPAR realargues = separated_list(COMMA, VAR) RPAR   {NewObj (c, realargues) }
| var = VAR access= maybeContinue LPAR realargues = separated_list(COMMA, VAR) RPAR 
  {match access with 
  |None -> Call ((var, []), realargues) 
  |Some obj -> Call ((var, obj), realargues)}

pure_formula_term:
  | n = INTE { let (i, _) = n in Num (int_of_string i) }
  | v = VAR { Var v }
  | pure_formula_term PLUS pure_formula_term { Plus ($1, $3) }
  | pure_formula_term MINUS pure_formula_term { Minus ($1, $3) }
  | LPAR pure_formula_term RPAR { $2 }

pure_formula:
  | TRUE { True }
  | FALSE { False }
  | a = pure_formula_term LESS b = pure_formula_term { Atomic (LT, a, b) }
  | a = pure_formula_term GREATER b = pure_formula_term { Atomic (GT, a, b) }
  | a = pure_formula_term LESSEQ b = pure_formula_term { Atomic (LTEQ, a, b) }
  | a = pure_formula_term GreaterEQ b = pure_formula_term { Atomic (GTEQ, a, b) }

  | pure_formula CONJ pure_formula { And ($1, $3) }
  | pure_formula DISJ pure_formula { Or ($1, $3) }
  | pure_formula IMPLICATION pure_formula { Imply ($1, $3) }
  | TILDE pure_formula { Not ($2) }

specification: 
| {[]}

objectContent:
| {([], [])}
| t = tau var=VAR SIMI rest = objectContent {
  let (restL, restR) = rest in 
  ((t, var)::restL, restR)
  }
| mtype=metyTYPE t = tau mn= maybeCOnstructor 
  LPAR formargues = formalARGUMENTS RPAR  
  OPEN 
REQUIERES COLON spec_pre = specification
ENSURES COLON 
CLOSE
  LBRACK 
  p = expression_shell
  RBRACK 
  rest = objectContent {
  let (restL, restR) = rest in 
  let var = match mn with 
  | None -> "constructor"
  | Some var -> var 
  in 
  (restL, (mtype, t, var, formargues, spec_pre, [], p):: restR)
  }
| t = tau mn= maybeCOnstructor 
  LPAR formargues = formalARGUMENTS RPAR  
  OPEN 
REQUIERES COLON spec_pre = specification
ENSURES COLON 
CLOSE
  LBRACK 
  p = expression_shell
  RBRACK 
  rest = objectContent {
  let (restL, restR) = rest in 
    let var = match mn with 
  | None -> "constructor"
  | Some var -> var 
  in 
  (restL, (Origin, t, var, formargues, spec_pre, [], p):: restR)
  }



classDef: 
| CLASS var = UPPERCASEVAR ext = extendinng 
LBRACK 
 content = objectContent
RBRACK {let (formalArgs, classMeth) = content in (var, ext, formalArgs, classMeth)}

(*
singleVAR: var = VAR {[var]}

existVar:
| {[]}
| p = singleVAR {p}
| p1 = singleVAR  COMMA  p2 = existVar {append p1 p2 }


es:
| EMPTY { Emp }
| LBRACK signals = existVar RBRACK 
{
  let temp = List.map (fun a -> (One a)) signals in 
  Instance (temp) }
| LPAR r = es RPAR { r }
| a = es CONCAT b = es { Con(a, b) } 
| a = es  DISJ  b=es  {Disj (a, b)}
| LPAR a = es RPAR KLEENE {Kleene a}
| LPAR r = es RPAR n = INTE { Ntimed (r, n) }
| LPAR a = es RPAR POWER OMEGA {Omega a}



entailment:
| lhs = es   ENTIL   rhs = es { (lhs, rhs)}

pRog_aux:
| NOTHING { Nothing }
| PAUSE   { Pause } 
| EMIT s = VAR  {Emit s}
| LOOP p = pRog END  LOOP { Loop p}
| SIGNAL s = VAR IN p = pRog END SIGNAL { Declear (s, p)}
| PRESENT s = VAR THEN p1 = pRog ELSE p2 = pRog END PRESENT { Present (s, p1, p2)}
| TRAP mn = VAR IN p1 = pRog END TRAP {Trap (mn, p1)}
| EXIT mn = VAR  {Exit mn}
(*| EXIT mn = VAR d = INTE  {Exit (mn, d)}*)
| RUN mn = VAR {Run mn}
| ABORT p = pRog  WHEN s = VAR {Suspend (p, s)}

pRog:
| p = pRog_aux {p}
| p1 = pRog SIMI p2 = pRog{ Seq (p1, p2)}
| LPAR p1 = pRog RPAR PAR LPAR p2 = pRog RPAR{ Par (p1, p2)}

(*

*)

specProg: 
| MODULE nm = VAR COLON 
  INPUT ins = existVar SIMI
  OUTPUT outs = existVar SIMI
  LSPEC REQUIRE pre = es ENSURE post = es RSPEC p = pRog 
  END MODULE
  {(nm, ins, outs, pre, post, p)}
| MODULE nm = VAR COLON 
  OUTPUT outs = existVar SIMI
  LSPEC REQUIRE pre = es ENSURE post = es RSPEC p = pRog 
  END MODULE
  {(nm, [], outs, pre, post, p)}

| MODULE nm = VAR COLON 
  INPUT ins = existVar SIMI
  OUTPUT outs = existVar SIMI
  p = pRog 
  END MODULE
  {(nm, ins, outs, Instance [], Kleene (Instance []), p)}

| MODULE nm = VAR COLON 
  OUTPUT outs = existVar SIMI
  p = pRog 
  END MODULE
  {(nm, [], outs, Instance [], Kleene (Instance []), p)}


*)