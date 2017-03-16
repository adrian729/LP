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
#include <vector>


/**DATA STRUCTURES*/

//Tube
typedef struct {
  int length, diameter; // tube length and diameter.
} Tube;

//Connector
typedef struct {
  int diameter; // tube length and diameter.
} Connector;

//TubeVector
typedef struct {
  vector<Tube> tubes; // vector of tubes.
  int size, top; // vector size and next item index.
} TubeVector;


map<string, string> stock; // ID, typeOfObject
map<string, Tube> tubeStock; // ID, Tube
map<string, Connector> connectorStock; // ID, Connector
map<string, TubeVector> tubeVectorStock; // ID, TubeVector


/**METHODS*/

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {

  switch(type) {
    case ID:
      attr->kind = "id";
      attr->text = text;
      break;
    case NUM:
    case PLUS:
    case TIMES:
    case LEN:
    case DIAM:
      attr->kind = "number";
      attr->text = text;
      break;
    case BOOL:
    case NOT:
    case AND:
    case OR:
    case COMP:
    case EMPTY:
    case FULL:
      attr->kind = "boolean";
      attr->text = text;
      break;
    case TUBE:
    case MERGE:
      attr->kind = "tube";
      attr->text = text;
      break;
    case CON:
      attr->kind = "connector";
      attr->text = text;
      break;
    case TVEC:
      attr->kind = "tubeVector";
      attr->text = text;
      break;
    case SPLIT:
      attr->kind = "split";
      attr->text = text;
      break;
    case ASSIG:
      attr->kind = "assignation";
      attr->text = text;
      break;
    case WHILE:
    case PUSH:
    case POP:
      attr->kind = "instruction";
      attr->text = text;
      break;
    default:
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

//Modified ASTPrintIndent to use diferent kind types and produce the same tree.
void ASTPrintIndentNEW(AST *a,string s)
{

  if (a==NULL) return;

  if (a->text != "") cout << a->text;
  else cout << a->kind;
  cout << endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    if(i->text.size() > 0) ASTPrintIndentNEW(i,s+"  |"+string(i->text.size(),' '));
    else ASTPrintIndentNEW(i,s+"  |"+string(i->kind.size(),' '));

    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      if(i->text.size() > 0) ASTPrintIndentNEW(i,s+"   "+string(i->text.size(),' '));
      else ASTPrintIndentNEW(i,s+"   "+string(i->kind.size(),' '));
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

//Modified ASTPrint to use diferent kind types and produce the same tree.
void ASTPrintNEW(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndentNEW(a,"");
    a=a->right;
  }
}

Tube tubeDec(AST *a){  
  Tube t;
  if(a->kind == "id"){
    map<string,string>::iterator stockIt = stock.find(a->text);
    if(stockIt != stock.end()){
      t = tubeStock.find(a->text)->second;
    }
  }
  return t;
}

int evalNumberInst(AST *a){
  if(a->text == "LENGTH") {
    return tubeDec(child(a, 0)).length;
  }
  else if(a->text == "DIAMETER") {

  }
  return -1;
}

void executePlumber(AST *a) {
  int inst = 0;
  while(child(a, inst)) {
    /*CHI:*/
    cout << child(a, inst)->kind << " " << child(a, inst)->text << endl;
    /*ENDCHI*/
    if(child(a, inst)->kind == "number") {
      cout << evalNumberInst(child(a, inst)) << endl;
    }
    ++inst;
  }
}

int main() {
  AST *root = NULL;
  ANTLR(plumber(&root), stdin);
  /*CHI:
  ASTPrint(root);
  cout << endl;
  ENDCHI*/
  ASTPrintNEW(root);
  executePlumber(root);
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
#token ENDWHILE "ENDWHILE"


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
#token SPACE "[\ \t\n]" << zzskip();>>


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

// Instructions
assignation:
    ID ASSIG^ (ID | tubeDec | connectorDec | tvecDec | mergeExpr)
    | "\("! ID ","! ID "\)"! ASSIG^ splitTube
    ;

length: LEN^ "\("! ID "\)"! ;

diameter: DIAM^ "\("! ID "\)"! ;

wLoop: WHILE^ "\("! boolOr "\)"! ops ENDWHILE! ;

vecOp: 
    PUSH^ ID ID
    | POP^ ID ID
    ;

// functions
tubeDec: TUBE^ numExpr numExpr ;

connectorDec: CON^ numExpr ;

tvecDec: TVEC^ OF! numExpr ;

mergeExpr: MERGE^ (ID | mergeExpr) ID (ID | mergeExpr) ;

splitTube: SPLIT^ ID ;

vecState: (EMPTY^ | FULL^) "\("! ID "\)"! ;

// Expresions
boolOr: boolAnd (OR^ boolAnd)* ;

boolAnd: boolNot (AND^ boolNot)* ;

boolNot: (NOT^ | ) boolAtom ;

boolAtom:
    BOOL
    | vecState
    | boolExpr
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



