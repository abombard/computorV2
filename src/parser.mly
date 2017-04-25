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
    print_endline (Computor.Calc.to_string lhs)
}
| var = VAR EQUAL rhs = hs {
    Computor.Variables.add var rhs;
    print_endline (Computor.Calc.to_string rhs)
}

hs:
| e = expr { e }

expr:
| t = token { t }
| t1 = expr POWER t2 = expr    { Computor.Calc.power t1 t2 }
| t1 = expr TIMES t2 = expr    { Computor.Calc.mul t1 t2 }
| t1 = expr DIV   t2 = expr    { Computor.Calc.div t1 t2 }
| t1 = expr MOD   t2 = expr    { Computor.Calc.modulo t1 t2 }
| t1 = expr PLUS  t2 = expr    { Computor.Calc.add t1 t2 }
| t1 = expr MINUS t2 = expr    { Computor.Calc.sub t1 t2 }

token:
| t = INT { Computor.Calc.Simple (Computor.Basic.Int t) }
| t = FLOAT { Computor.Calc.Simple (Computor.Basic.Float t) }
| t = INT IMAGINARY { Computor.Calc.Img (Computor.Basic.Int 0, Computor.Basic.Int t) } 
| t = FLOAT IMAGINARY { Computor.Calc.Img (Computor.Basic.Int 0, Computor.Basic.Float t) } 
| IMAGINARY { Computor.Calc.Img (Computor.Basic.Int 0, Computor.Basic.Int 1) }
| var = VAR { Computor.Variables.get var }
