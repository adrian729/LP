#header
<<
#include <string>
#include <iostream>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>
#include <map>

map<string,int> keysMap;

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  if (type == NUM) {
    attr->kind = "intconst";
    attr->text = text;
  }
  else if (type == TBOOL or type == FBOOL) {
    attr->kind = "boolean";
    attr->text = text;
  }
  else if (type == PLUS or type == TIMES) {
    attr->kind = "intoperator";
    attr->text = text;
  }
  else if (type == COMP) {
    attr->kind = "booleanoperator";
    attr->kind = text;
  }
  else if (type == ID) {
    attr->kind = "id";
    attr->text = text;
  }
  else {
    attr->kind = text;
    attr->text = "";
  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
 AST *c=a->down;
 for (int i=0; c!=NULL && i<n; i++) c=c->right;
 return c;
} 

/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  //TODO: modificar per a que no pinti el tipus (o que ho faci nomes en alguns casos).
  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

//PLUS "[\+\-]"
//TIMES "[\*\/]"
int operate(AST *a) {
  if (a->text == "+") return operate(child(a,0)) + operate(child(a,1));
  else if (a->text == "-") return operate(child(a,0)) - operate(child(a,1));
  else if (a->text == "*") return operate(child(a,0)) * operate(child(a,1));
  else if (a->text == "/") return operate(child(a,0)) / operate(child(a,1));
  return atoi(a->text.c_str());
}
//COMP "[<>] | [!<>=][=]"
bool compare(AST *a) {
  if (a->text == "<") return evaluate(child(a,0)) < evaluate(child(a,1));
  else if (a->text == ">") return evaluate(child(a,0)) > evaluate(child(a,1));
  else if (a->text == "==") return evaluate(child(a,0)) == evaluate(child(a,1));
  else if (a->text == "!=") return evaluate(child(a,0)) != evaluate(child(a,1));
  else if (a->text == "<=") return evaluate(child(a,0)) <= evaluate(child(a,1));
  else if (a->text == ">=") return evaluate(child(a,0)) >= evaluate(child(a,1));
  else if (a->kind == "boolean" and a->text == "TRUE") return true;
  else return false;
}

int evaluate(AST *a){
  if (a == NULL) return 0;
  else if (a->kind == "intconst") return atoi(a->text.c_str());
  else if (a->kind == "intoperator") return operate(a);
  else if (a->kind == "booleanoperator") return compare(a);
  else return 0;
}

int main() {
  AST *root = NULL;
  ANTLR(expr(&root), stdin);
  ASTPrint(root);
  cout << evaluate(root) << endl;
}
>>

#lexclass START

//Keywords
#token WRITE "write"
#token IF "if"
#token THEN "then"
#token ELSE "else"
#token ENDIF "endif"
#token WHILE "while"
#token ENDWHILE "endwhile"
#token DO "do"

//Operators
#token PLUS "[\+\-]"
#token TIMES "[\*\/]"
#token LP "\("
#token RP "\)"
#token ASIG ":="
#token COMP "[<>] | [!<>=][=]"

#token TBOOL "TRUE"
#token FBOOL "FALSE"
#token NUM "[0-9]+"
#token ID "[a-zA-Z] [a-zA-Z0-9]*"

//WhiteSpace
#token SPACE "[\ \n\t]" << zzskip();>>

//expr: NUM (PLUS^ NUM)* ; //non recursive version - only PLUS
//expr: NUM (PLUS^ expr | ); //recursive version - only PLUS
program: (instruction)* "@";
instruction: expr ;//(WRITE^ expr) | (ID ASIG^ expr);
expr: term (PLUS^ term)*;
term: subexpr (TIMES^ subexpr)*;
subexpr: NUM | LP! expr RP! | ID;