#header 
<< #include "charptr.h" >>

<<
#include "charptr.c"

int main() {
   ANTLR(expr(), stdin);
}
>>

#lexclass START
#token NUM "[0-9]+"
#token PLUS "[\+\-]"
#token SPACE "[\ \n]" << zzskip(); >>
#token EOFC "@"

input: (expr EOFC)*;
//expr: NUM (PLUS NUM)*; //Works fine
//expr: expr PLUS expr | NUM; //error: infinite left-recursion to rule expr from rule expr & warning: alts 1 and 2 of the rule itself ambiguous upon { NUM }
//expr: NUM PLUS expr | NUM; //warning: alts 1 and 2 of the rule itself ambiguous upon { NUM } (si entres per un NUM pensara que vols entrar per la primera forma, per tant si no l'acompanyes de PLUS donara error, i hauria d'estar be. Culpa de com converteix a codi C les gramatiques)
//expr: expr PLUS NUM | NUM; //error: infinite left-recursion to rule expr from rule expr & warning: alts 1 and 2 of the rule itself ambiguous upon { NUM } (com abans)
//expr: NUM | NUM PLUS expr; //warning: alts 1 and 2 of the rule itself ambiguous upon { NUM } (mirar codi generat en C: no ha generat la crida recursiva correctament, quan trobi final d'una expressio no continua en bucle retorna i acaba)
expr: NUM (PLUS NUM)*;