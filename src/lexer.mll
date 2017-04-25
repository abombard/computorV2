{
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t']                                            { token lexbuf }
| ['\n']                                                { EOL }
| ['-' '+']? ['0'-'9']+ as int                          { INT ( int_of_string int ) }
| ['-' '+']? ['0'-'9']+ '.' ['0'-'9']* as float         { FLOAT ( float_of_string float ) }
| ['-' '+']? 'i'                                        { IMAGINARY }
| '+'                                                   { PLUS }
| '-'                                                   { MINUS }
| '*'                                                   { TIMES }
| '/'                                                   { DIV }
| '%'                                                   { MOD }
| '='                                                   { EQUAL }
| '^'                                                   { POWER }
| '('                                                   { LPAR }
| ')'                                                   { RPAR }
| ['a'-'z' 'A'-'Z']+ as var                             { VAR ( var ) }
| ['a'-'z' 'A'-'Z']+ '(' ['a'-'z' 'A'-'Z']+ ')' as func { FUNC (
    let i = String.index func '(' in
    let f = String.sub func 0 i in
    let v = String.sub func (i+1) (String.length func - (i+1)) in
    (f, v)
)}
| '?'                                                   { INTERROGATION }
| '!'                                                   { EXCLAMATION }
| eof                                                   { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

