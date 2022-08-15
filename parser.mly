%{ open Ast %}
%{ open List %}


%token <string> VAR
%token <int> INTE
%token LPAR RPAR  SIMI

%token EOF ENTIL EMPTY DISJ COMMA CONCAT  KLEENE END IN RUN OMEGA
%token THEN ELSE LBRACK RBRACK 
%token LSPEC RSPEC  COLON 
%token CLASS


%start full_prog 
%type <Ast.program> full_prog


%%
full_prog:
| EOF {[]}
| a = classDef r = full_prog { append [a] r }

classDef: 
| CLASS var = VAR LBRACK RBRACK {(var, None, [], [])}

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