%{
  open AstNode
%}

%token <int> INT
%token <float> FLOAT
%token <string> STR
%token <string> IDENTIFIER

%token LPAREN RPAREN
%token DEFINE LET LAMBDA IF BEGIN
%token PLUS MINUS MUL DIV

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
;
atom:
  | INT { Mint($1) }
  | FLOAT { Mfloat($1) }
  | STR { Mstr($1) }
  | IDENTIFIER { Midentifier($1) }
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
define:
  | LPAREN DEFINE LPAREN identifier params RPAREN body RPAREN { Func($4, $5, $7) }
  | LPAREN DEFINE identifier value RPAREN { Bind($3, $4) }
;
identifier:
  | IDENTIFIER { $1 }
;
params:
  | identifier params { $1::$2 }
  | identifier { [$1] }
;
body:
  | value body { $1::$2 }
  | value { [$1] }
;
value:
  | atom { Atom($1) }
  | arith { Arith($1) }
;
