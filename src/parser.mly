%token EQUAL
%token LPAR RPAR
%token POWER
%token TIMES DIV MOD
%token PLUS MINUS
%token <string>VAR
%token <string * string>FUNC
%token <int>INT
%token <float>FLOAT
%token IMAGINARY
%token EXCLAMATION
%token INTERROGATION
%token EOL
%token EOF

%start <unit> main

%left PLUS MINUS
%left TIMES DIV MOD
%left POWER

%%

main:
| e = eq EOL { e }

eq:
| lhs = hs EQUAL INTERROGATION {
    print_endline (Calc.to_string lhs)
}
| var = VAR EQUAL rhs = hs {
    Variable.add var rhs;
    print_endline (Calc.to_string rhs)
}

hs:
| e = expr { e }

expr:
| t = token { t }
| t1 = expr POWER t2 = expr    { Calc.power t1 t2 }
| t1 = expr TIMES t2 = expr    { Calc.mul t1 t2 }
| t1 = expr DIV   t2 = expr    { Calc.div t1 t2 }
| t1 = expr MOD   t2 = expr    { Calc.modulo t1 t2 }
| t1 = expr PLUS  t2 = expr    { Calc.add t1 t2 }
| t1 = expr MINUS t2 = expr    { Calc.sub t1 t2 }

token:
| t = INT { Calc.Simple (Calc.Basic.Int t) }
| t = FLOAT { Calc.Simple (Calc.Basic.Float t) }
| t = INT IMAGINARY { Calc.Complex (Calc.Basic.Int 0, Calc.Basic.Int t) }
| t = FLOAT IMAGINARY { Calc.Complex (Calc.Basic.Int 0, Calc.Basic.Float t) }
| IMAGINARY { Calc.Complex (Calc.Basic.Int 0, Calc.Basic.Int 1) }
| var = VAR { Variable.get var }
