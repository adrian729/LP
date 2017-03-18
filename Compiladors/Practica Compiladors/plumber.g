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


#include <exception>
/**EXCEPTIONS**/
class NoIdException : public exception {
  string err_msg;
  public:
    NoIdException(const string& id)
      : err_msg(string("WRONG (SEMANTICALLY): ") + id + string(" DOES NOT EXIST."))
    {}

    virtual const char* what() const throw() {
      return err_msg.c_str();
    }
};

class WrongTypeException : public exception {
  string err_msg;
  public:
    WrongTypeException(const string& id, const string& type)
      : err_msg(string("WRONG (SEMANTICALLY): ") + id + string(" IS NOT A ") + type + string("."))
    {}

    virtual const char* what() const throw() {
      return err_msg.c_str();
    }
};


/**DATA STRUCTURES**/

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


// map table to handle or ID's of existing objects
map<string, string> stock; // ID, typeOfObject

// Tubes
map<string, Tube> tubeStock; // ID, Tube
// Connectors
map<string, Connector> connectorStock; // ID, Connector
// Vectors of Tubes
map<string, TubeVector> tubeVectorStock; // ID, TubeVector





/**AST FUNCTIONS**/

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



/**EXECUTION FUNCTIONS**/

/**Forward declaration (for possible circular calls)**/

Connector evaluateConnectorExpression(AST*);
Tube evaluateTubeExpression(AST*);
int diameter(string id);
int evaluateNumberExpression(AST*);
bool evaluateBooleanExpression(AST*);
bool evaluateAssignationExpression(AST*);
bool evaluateInstructionExpression(AST*);
void executePlumber(AST*);

/**Implementation**/

/**
 * Returns the connector described by the expression.
 * represented by the parameter id.
 * @param a The AST of the expression.
 * @return The connector described by the expression.
 */
Connector evaluateConnectorExpression(AST *a) {
  Connector c;
  if(a->kind == "connector") {
    /*CHI:*/
    cout << "connector" << endl;
    /*ENDCHI*/
    c.diameter = evaluateNumberExpression(child(a, 0));
  }
  else if(a->kind == "id"){
    /*CHI:*/
    cout << "id->connector" << endl;
    /*ENDCHI*/
    // Iterator to the id on stock map
    map<string,string>::iterator stockIt = stock.find(a->text);
    // If ID exists and it is a tube.
    if(stockIt != stock.end() && stockIt->second == "connector"){
      c = connectorStock.find(a->text)->second;
    }
  }
  return c;
}

/**
 * Returns the tube described by the expression.
 * represented by the parameter id.
 * @param a The AST of the expression.
 * @return The tube described by the expression.
 */
Tube evaluateTubeExpression(AST *a) {
  Tube t;
  if(a->kind == "tube"){
    if(a->text == "MERGE") {
      /*CHI:*/
      cout << "merge" << endl;
      /*ENDCHI*/
      Tube t1 = evaluateTubeExpression(child(a, 0));
      Connector c = evaluateConnectorExpression(child(a, 1));
      Tube t2 = evaluateTubeExpression(child(a, 2));
    }
    else if(a->text == "TUBE") {
      /*CHI:*/
      cout << "tube" << endl;
      /*ENDCHI*/
      t.length = evaluateNumberExpression(child(a, 0));
      t.diameter = evaluateNumberExpression(child(a, 1));
    }
  }
  else if(a->kind == "id") {
      /*CHI:*/
      cout << "id->tube" << endl;
      /*ENDCHI*/
      // Iterator to the id on stock map
      map<string,string>::iterator stockIt = stock.find(a->text);
      // If ID doesn't exists, throw an excpeption.
      if(stockIt == stock.end()) {
        /*CHI:*/
        cout << "ERR: ID " << a->text << " NOT FOUND." << endl;
        /*ENDCHI*/
        throw NoIdException(a->text);
      }
      // If it is not a tube throw an exception.
      if(stockIt->second != "tube") {
        /*CHI:*/
        cout << "ERR: " << a->text << " IS NOT A TUBE." << endl;
        /*ENDCHI*/
        throw WrongTypeException(a->text, "TUBE");
      }
      t = tubeStock.find(a->text)->second;
  }
  return t;
}

/**
 * Returns de diameter of the tube or connector
 * represented by the parameter id.
 * @param a The id of the tube or connector.
 * @return The diameter of the tube or connector.
 */
int diameter(string id) {
  map<string,string>::iterator stockIt = stock.find(id);
  if(stockIt != stock.end()) {
    // tube
    if(stockIt->second == "tube") {
      return (tubeStock.find(id)->second).diameter;
    }
    // connector
    else if(stockIt->second == "connector") {
      return (connectorStock.find(id)->second).diameter;
    }
  }
  else {
    
  }
  return -1;
}

/**
 * Evaluates the expression represented in the AST a.
 * It may be a number expression.
 * It must return an integer.
 * @param a The AST of the expression.
 * @return The value of the expression.
 */
int evaluateNumberExpression(AST *a){
  if(a->text == "LENGTH") {
    return evaluateTubeExpression(child(a, 0)).length;
  }
  else if(a->text == "DIAMETER") {    
    if(child(a,0)->kind == "id"){
      return diameter(child(a,0)->text);
    }
  }
  else if(a->text == "+") {
    return evaluateNumberExpression(child(a, 0)) + evaluateNumberExpression(child(a, 1));
  }
  else if(a->text == "-") {
    return evaluateNumberExpression(child(a, 0)) - evaluateNumberExpression(child(a, 1));
  }
  else if(a->text == "*") {
    return evaluateNumberExpression(child(a, 0)) * evaluateNumberExpression(child(a, 1));
  }

  // Else it must be a number, return it.
  return stoi(a->text);
}

/**
 * Evaluates the expression represented in the AST a.
 * It may be a boolean expression.
 * It must return a boolean.
 * @param a The AST of the expression.
 * @return The value of the expression.
 */
bool evaluateBooleanExpression(AST *a){
  //TODO: FULL, EMPTY
  if(a->text == "FULL") {
    /*CHI:*/
    cout << "full" << endl;
    /*ENDCHI*/

  }
  else if(a->text == "EMPTY") {
    /*CHI:*/
    cout << "empty" << endl;
    /*ENDCHI*/
  }
  else if(a->text == "<") {
    /*CHI:*/
    cout << "<" << endl;
    /*ENDCHI*/
    return evaluateNumberExpression(child(a, 0)) < evaluateNumberExpression(child(a, 1));
  }
  else if(a->text == ">") {
    /*CHI:*/
    cout << ">" << endl;
    /*ENDCHI*/
    return evaluateNumberExpression(child(a, 0)) > evaluateNumberExpression(child(a, 1));
  }
  else if(a->text == "==") {
    /*CHI:*/
    cout << "==" << endl;
    /*ENDCHI*/
    return evaluateNumberExpression(child(a, 0)) == evaluateNumberExpression(child(a, 1));
  }
  else if(a->text == "OR") {
    /*CHI:*/
    cout << "OR" << endl;
    /*ENDCHI*/
    return evaluateBooleanExpression(child(a, 0)) || evaluateBooleanExpression(child(a, 1));
  }
  else if(a->text == "AND") {
    /*CHI:*/
    cout << "AND" << endl;
    /*ENDCHI*/
    return evaluateBooleanExpression(child(a, 0)) && evaluateBooleanExpression(child(a, 1));
  }
  else if(a->text == "NOT") {
    /*CHI:*/
    cout << "NOT" << endl;
    /*ENDCHI*/
    return !evaluateBooleanExpression(child(a, 0));
  }
  else if(a->text == "TRUE") {
    return true;
  }
  /* a->text == FALSE equals default => don't need it */

  return false;
}

/**
 * Evaluates the expression represented in the AST a.
 * It must be an assignation.
 * @param a The AST of the expression.
 * @return Boolean true if everything OK, false if something have gone wrong.
 */
bool evaluateAssignationExpression(AST *a){
  /*CHI:*/
  cout << "Assign" << endl;
  /*ENDCHI*/

  

  return false;
}

/**
 * Evaluates the expression represented in the AST a.
 * It must be an instruction.
 * @param a The AST of the instruction.
 * @return Boolean true if everything OK, false if something have gone wrong.
 */
bool evaluateInstructionExpression(AST *a){
  if(a->text == "WHILE") {
    /*CHI:*/
    cout << "while" << endl;
    /*ENDCHI*/
    //TODO: bucle (acabar primer exp. booleanes, o podran so)
    if(evaluateBooleanExpression(child(a, 0))) {
      /*CHI:*/
      cout << "Condition true" << endl;
      /*ENDCHI*/
    }
    else {
      /*CHI:*/
      cout << "Condition false" << endl;
      /*ENDCHI*/      
    }
  }
  else if(a->text == "PUSH") {
    /*CHI:*/
    cout << "push" << endl;
    /*ENDCHI*/
  }
  else if(a->text == "POP") {
    /*CHI:*/
    cout << "pop" << endl;
    /*ENDCHI*/
  }

  return false;
}

/**
 * Evaluates the list of instructions represented in the AST a.
 * @param a The AST of the instruction.
 */
void executePlumber(AST *a) {
  int inst = 0;
  while(child(a, inst)) {
    try {
      /*CHI:*/
      cout << child(a, inst)->kind << " " << child(a, inst)->text << endl;
      /*ENDCHI*/
      string chKind = child(a, inst)->kind;
      if(chKind == "number") {
        /*CHI:*/
        cout << evaluateNumberExpression(child(a, inst)) << endl;
        /*ENDCHI*/
      }
      else if(chKind == "assignation"){
        evaluateAssignationExpression(child(a, inst));
      }
      else if(chKind == "instruction"){
        evaluateInstructionExpression(child(a, inst));
      }
    } catch(NoIdException& nie) {
      cerr << nie.what() << endl;
    } catch(WrongTypeException& wte) {
      cerr << wte.what() << endl;
    } catch(...) { // It catches any non handled exception.
      cerr << "Unhandled exception occurred." << endl;
    }
    ++inst;
  }
}

int main() {
  AST *root = NULL;
  ANTLR(plumber(&root), stdin);
  /*CHI:*/
  ASTPrint(root);
  cout << endl;
  /*ENDCHI*/
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
