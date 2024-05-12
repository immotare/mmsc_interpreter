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
  | DEFINE { Keyword(Define) }
  | IF { Keyword(If) }
  | LAMBDA { Keyword(Lambda) }
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
