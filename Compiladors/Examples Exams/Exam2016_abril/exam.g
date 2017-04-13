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
  ANTLR(program(&root), stdin);
  ASTPrint(root);
}
>>

#lexclass START

#token GRID      "Grid"
#token PLACE     "PLACE"
#token AT        "AT"
#token MOVE      "MOVE"
#token DIRECTION "NORTH | SOUTH | EAST | WEST"
#token FITS      "FITS"
#token HEIGHT    "HEIGHT"
#token WHILE     "WHILE"
#token DEF       "DEF"
#token ENDEF     "ENDEF"
#token COMP      "[<>]"
#token AND       "AND"
#token ASSIG     "="  
#token ID        "[A-Za-z_][A-Za-z0-9_]*"
#token NUM       "[0-9]+"

#token SPACE "[\ \n\t]" << zzskip();>>

program: (grid conj_insts conj_defs) <<#0=createASTlist(_sibling);>>;

grid: GRID^ NUM NUM ;
conj_insts: (instruction <<#0=createASTlist(_sibling);>>)*;
conj_defs: (function_def <<#0=createASTlist(_sibling);>>)*;


function_def: DEF^ ID conj_insts ;


instruction:
  move_inst
  | wLoop
  | ID (ASSIG! PLACE^ pair_num_list AT! (pair_num_list | ID) | )
  ;


move_inst: MOVE^ ID DIRECTION NUM ;

wLoop: WHILE^ "\("! boolAnd "\)"! "\["! conj_insts "\]"! ;


pair_num_list: ("\("! NUM ","! NUM "\)"!) <<#0=createASTlist(_sibling);>>;


boolAnd: boolExpr (AND^ boolExpr)* ;
boolExpr: fitsExpr | numExpr (COMP^ numExpr)* ;
numExpr: HEIGHT^ "\("! ID "\)"! | NUM ;
fitsExpr: FITS^ "\("! ID ","! trio_num_list "\)"! ;

trio_num_list: (NUM ","! NUM ","! NUM) <<#0=createASTlist(_sibling);>>;


