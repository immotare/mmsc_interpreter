%{
  open AstNode
%}

%token <int> INT
%token <float> FLOAT
%token <string> STR
%token <string> IDENTIFIER
%token <bool> BOOL

%token LPAREN RPAREN
%token DEFINE LET LAMBDA IF BEGIN
%token PLUS MINUS MUL DIV
%token AND OR GT LT EQ

%start main
%type <AstNode.t> main
%%
main:
  expr { $1 }
;
expr:
  | arith { Arith($1) }
  | atom { Atom($1) }
  | define { Define($1) }
  | lambda { Lambda($1) }
  | pred { Pred($1) }
  | ifs { If($1) }
  | proc_call { ProcCall($1) }
;
atom:
  | INT { Mint($1) }
  | FLOAT { Mfloat($1) }
  | STR { Mstr($1) }
  | IDENTIFIER { Midentifier($1) }
  | BOOL { Mbool($1) }
;
arith:
  | LPAREN PLUS arith_op arith_op RPAREN { Plus($3, $4) }
  | LPAREN MINUS  arith_op arith_op RPAREN {Minus($3, $4) }
  | LPAREN MUL arith_op arith_op RPAREN { Mul($3, $4) }
  | LPAREN DIV arith_op arith_op RPAREN { Div($3, $4) }
;
arith_op:
  | INT { OpInt($1) }
  | FLOAT { OpFloat($1) }
  | IDENTIFIER { OpIdentifier($1) }
  | arith { OpArith($1) }
;
lambda:
  | LPAREN LAMBDA LPAREN params RPAREN body RPAREN { ($4, $6) }
;
define:
  | LPAREN DEFINE LPAREN identifier params RPAREN body RPAREN { Func($4, $5, $7) }
  | LPAREN DEFINE identifier expr RPAREN { Bind($3, $4) }
;
identifier:
  | IDENTIFIER { $1 }
;
params:
  | identifier params { $1::$2 }
  | identifier { [$1] }
;
body:
  | expr body { $1::$2 }
  | expr { [$1] }
;
pred:
  | LPAREN AND expr expr RPAREN { And($3, $4) }
  | LPAREN OR expr expr RPAREN { Or($3, $4) }
  | LPAREN GT expr expr RPAREN { Gt($3, $4) }
  | LPAREN LT expr expr RPAREN { Lt($3, $4) }
  | LPAREN EQ expr expr RPAREN { Eq($3, $4) }
;
ifs:
  | LPAREN IF expr expr expr RPAREN { ($3, $4, $5) }
;
proc_call:
  | LPAREN expr body RPAREN { ($2, $3) }
;