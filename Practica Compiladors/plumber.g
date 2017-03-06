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
// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
    attr->kind = text;
    attr->text = "";
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

/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
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

int main() {
  AST *root = NULL;
  ANTLR(plumber(&root), stdin);
  ASTPrint(root);
}
>>

/** LEXIC */
#lexclass START

// Keywords
#token TUBE "TUBE"
#token CON "CONNECTOR"

#token SPLIT "SPLIT"
#token MERGE "MERGE"
#token LEN "LENGTH"
#token DIAM "DIAMETER"

#token TVEC "TUBEVECTOR"
#token OF "OF"

#token PUSH "PUSH"
#token POP "POP"
#token FULL "FULL"
#token EMPTY "EMPTY"

#token WHILE "WHILE"
#token EWHILE "ENDWHILE"


// Operations
#token ASSIG "="

// Num Ops
#token PLUS "[\+\-]"
#token TIMES "[\*]"

// Bool Ops
#token NOT "NOT"
#token AND "AND"
#token OR "OR"

#token COMP "< | > | =="

#token BOOL "TRUE | FALSE"

// Atoms
#token NUM "[0-9]+"
#token ID "[A-Za-z$\_][A-Za-z0-9$\_]*"

//WhiteSpace
#token SPACE "[\ \n]" << zzskip();>>


/** SYNTAX */
plumber: ops;

ops: (instruction <<#0=createASTlist(_sibling);>>)*;

instruction: 
    assignation // create a new tube or connector, or a vector of tubes or split a tube in another two
    | length    // writes length
    | diameter  // writes diameter
    | wLoop     // while loop
    | vecOp     // POP or PUSH a vector
    ;

assignation:
    ID ASSIG^ (ID | tubeDec | connectorDec | tvecDec)
    | "\("! ID ","! ID "\)"! ASSIG^ splitTube
    ;

splitTube: SPLIT^ (ID | tubeDec) ;

wLoop: WHILE^ "\("! boolOr "\)"! ops ENDWHILE! ;

intW: ID ID <<#0=createASTlist(_sibling);>> ;

vecOp: 
    PUSH^ ID (ID | tubeDec)
    | POP^ ID ID
    ;

vecState: (EMPTY^ | FULL^) "\("! ID "\)"! ;

tubeDec:
    TUBE^ numExpr numExpr
    | mergeExpr
    ;

mergeExpr: MERGE^ (ID | tubeDec) (ID | connectorDec) (ID | tubeDec) ;

connectorDec: CON^ numExpr ;

tvecDec: TVEC^ OF! numExpr ;

length: LEN^ "\("! (ID | tubeDec) "\)"! ;

diameter: DIAM^ "\("! (ID | tubeDec | connectorDec) "\)"! ;

boolOr: boolAnd (OR^ boolAnd)* ;

boolAnd: boolNot (AND^ boolNot)* ;

boolNot: (NOT^ | ) boolAtom ;

boolAtom:
    BOOL
    | boolExpr
    | vecState
    | "\("! boolOr "\)"!
    ;

boolExpr: numExpr (COMP^ numExpr) ;

numExpr: term (PLUS^ term)* ;

term: atom (TIMES^ atom)* ;

atom:
    NUM
    | ID
    | length
    | diameter
    ;


