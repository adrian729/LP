/*
 * A n t l r  T r a n s l a t i o n  H e a d e r
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 *
 *   antlr -gt plumber.g
 *
 */

#define ANTLR_VERSION	13333
#include "pcctscfg.h"
#include "pccts_stdio.h"

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
#define GENAST

#include "ast.h"

#define zzSET_SIZE 4
#include "antlr.h"
#include "tokens.h"
#include "dlgdef.h"
#include "mode.h"

/* MR23 In order to remove calls to PURIFY use the antlr -nopurify option */

#ifndef PCCTS_PURIFY
#define PCCTS_PURIFY(r,s) memset((char *) &(r),'\0',(s));
#endif

#include "ast.c"
zzASTgvars

ANTLR_INFO

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

class WrongNumberException : public exception {
  string err_msg;
  public:
  WrongNumberException(const int& size)
  : err_msg(string("ERROR: THE NUMBER IS ") + to_string(size) + string(" AND SHOULD BE A NATURAL NUMBER."))
  {}
  
    virtual const char* what() const throw() {
    return err_msg.c_str();
  }
};

class IndexOutOfBoundsException : public exception {
  string err_msg;
  public:
  IndexOutOfBoundsException(const int& index, const int& size)
  : err_msg(string("ERROR: INDEX OUT OF BOUNDS - INDEX ") + to_string(index) + string(" OF ") + to_string(size) + string("."))
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
  int top; // vector size and next item index.
} TubeVector;

const string TubeType = "tube";
const string ConnectorType = "connector";
const string TubeVectorType = "tubeVector";

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
pair<Tube, Tube> evaluateSplitExpression(string);
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
// Diameter should be a natural number.
if(c.diameter < 0) {
throw WrongNumberException(c.diameter);
}

  }
else if(a->kind == "id"){
/*CHI:*/
cout << "id->connector " << a->text << endl;
/*ENDCHI*/
// Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(a->text);
// If ID exists and it is a tube.
if(stockIt != stock.end() && stockIt->second == ConnectorType){
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
// TODO: MERGE!
Tube t;
if(a->kind == "tube"){
if(a->text == "MERGE") {
/*CHI:*/
cout << "   merge " << endl;
/*ENDCHI*/
Tube t1 = evaluateTubeExpression(child(a, 0));
Connector c = evaluateConnectorExpression(child(a, 1));
Tube t2 = evaluateTubeExpression(child(a, 2));
/*CHI:*/
cout << "   merged " << child(a, 0)->text << " " << child(a, 1)->text << " " << child(a, 2)->text << endl;
/*ENDCHI*/

      // TODO: A REAL MEGE!!!!
t.length = 12;
t.diameter = 4;
}
else if(a->text == "TUBE") {
/*CHI:*/
cout << "   tube" << endl;
/*ENDCHI*/
t.length = evaluateNumberExpression(child(a, 0));
// Length should be a natural number.
if(t.length < 0) {
throw WrongNumberException(t.length);
}
t.diameter = evaluateNumberExpression(child(a, 1));
// Diameter should be a natural number.
if(t.diameter < 0) {
throw WrongNumberException(t.diameter);
}
}
}
else if(a->kind == "id") {
/*CHI:*/
cout << "   id->tube " << a->text << endl;
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
if(stockIt->second != TubeType) {
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
// If ID doesn't exists, throw an excpeption.
if(stockIt == stock.end()) {
/*CHI:*/
cout << "ERR: ID " << id << " NOT FOUND." << endl;
/*ENDCHI*/
throw NoIdException(id);
}
// tube
if(stockIt->second == TubeType) {
return (tubeStock.find(id)->second).diameter;
}
// connector
else if(stockIt->second == ConnectorType) {
return (connectorStock.find(id)->second).diameter;
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
if(a->text == "FULL") {
/*CHI:*/
cout << "   full" << endl;
/*ENDCHI*/
// It should be an id.
if(child(a, 0)->kind != "id") {
/*CHI:*/
cout << "ERR: " << child(a, 0)->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(child(a, 0)->text, "VALID ID");
}

    // Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(child(a, 0)->text);
// If ID doesn't exists, throw an excpeption.
if(stockIt == stock.end()) {
/*CHI:*/
cout << "ERR: ID " << child(a, 0)->text << " NOT FOUND." << endl;
/*ENDCHI*/
throw NoIdException(child(a, 0)->text);
}
// It should point to a tube vector.
if(stock[child(a, 0)->text] != TubeVectorType) {
/*CHI:*/
cout << "ERR: " << child(a, 0)->text << " IS NOT A TUBE VECTOR." << endl;
/*ENDCHI*/
throw WrongTypeException(child(a, 0)->text, "TUBE VECTOR");
}

    /*CHI:*/
bool full = tubeVectorStock[child(a, 0)->text].top == tubeVectorStock[child(a, 0)->text].tubes.size();
cout << "     " << child(a, 0)->text << " " << stock[child(a, 0)->text] << " full " << full << endl;
/*ENDCHI*/
return tubeVectorStock[child(a, 0)->text].top == tubeVectorStock[child(a, 0)->text].tubes.size();
}
else if(a->text == "EMPTY") {
/*CHI:*/
cout << "   empty" << endl;
/*ENDCHI*/
// It should be an id.
if(child(a, 0)->kind != "id") {
/*CHI:*/
cout << "ERR: " << child(a, 0)->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(child(a, 0)->text, "VALID ID");
}

    // Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(child(a, 0)->text);
// If ID doesn't exists, throw an excpeption.
if(stockIt == stock.end()) {
/*CHI:*/
cout << "ERR: ID " << child(a, 0)->text << " NOT FOUND." << endl;
/*ENDCHI*/
throw NoIdException(child(a, 0)->text);
}
// It should point to a tube vector.
if(stock[child(a, 0)->text] != TubeVectorType) {
/*CHI:*/
cout << "ERR: " << child(a, 0)->text << " IS NOT A TUBE VECTOR." << endl;
/*ENDCHI*/
throw WrongTypeException(child(a, 0)->text, "TUBE VECTOR");
}

    /*CHI:*/
bool empty = tubeVectorStock[child(a, 0)->text].top == 0;
cout << "     " << child(a, 0)->text << " " << stock[child(a, 0)->text] << " empty " << empty << endl;
/*ENDCHI*/
return tubeVectorStock[child(a, 0)->text].top == 0;
}
else if(a->text == "<") {
/*CHI:*/
cout << "   <" << endl;
/*ENDCHI*/
return evaluateNumberExpression(child(a, 0)) < evaluateNumberExpression(child(a, 1));
}
else if(a->text == ">") {
/*CHI:*/
cout << "   >" << endl;
/*ENDCHI*/
return evaluateNumberExpression(child(a, 0)) > evaluateNumberExpression(child(a, 1));
}
else if(a->text == "==") {
/*CHI:*/
cout << "   ==" << endl;
/*ENDCHI*/
return evaluateNumberExpression(child(a, 0)) == evaluateNumberExpression(child(a, 1));
}
else if(a->text == "OR") {
/*CHI:*/
cout << "   OR" << endl;
/*ENDCHI*/
return evaluateBooleanExpression(child(a, 0)) || evaluateBooleanExpression(child(a, 1));
}
else if(a->text == "AND") {
/*CHI:*/
cout << "   AND" << endl;
/*ENDCHI*/
return evaluateBooleanExpression(child(a, 0)) && evaluateBooleanExpression(child(a, 1));
}
else if(a->text == "NOT") {
/*CHI:*/
cout << "   NOT" << endl;
/*ENDCHI*/
return !evaluateBooleanExpression(child(a, 0));
}
else if(a->text == "TRUE") {
return true;
}
/* a->text == FALSE equals default => don't need it */

  return false;
}

pair<Tube, Tube> evaluateSplitExpression(string id) {
// Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(id);
// If ID doesn't exists, throw an excpeption.
if(stockIt == stock.end()) {
/*CHI:*/
cout << "ERR: ID " << id << " NOT FOUND." << endl;
/*ENDCHI*/
throw NoIdException(id);
}
// If it is not a tube throw an exception.
if(stockIt->second != TubeType) {
/*CHI:*/
cout << "ERR: " << id << " IS NOT A TUBE." << endl;
/*ENDCHI*/
throw WrongTypeException(id, "TUBE");
}

  Tube t, t1, t2;
t = tubeStock.find(id)->second;
t1.length = t.length / 2;
// Length should be a natural.
if(t1.length < 0) {
throw WrongNumberException(t1.length);
}
t1.diameter = t.diameter;
t2.length = t.length / 2 + t.length % 2; // If odd t length, t2 larger than t1.
// Length should be a natural.
if(t2.length < 0) {
throw WrongNumberException(t2.length);
}
t2.diameter = t.diameter;

  return pair<Tube, Tube> (t1, t2);
}

/**
* Evaluates the expression represented in the AST a.
* It must be an assignation.
* @param a The AST of the expression.
* @return Boolean true if everything OK, false if something have gone wrong.
*/
bool evaluateAssignationExpression(AST *a){

  /*CHI:*/
cout << " Assign" << endl;
/*ENDCHI*/

  AST *child0 = child(a, 0); // must be an ID
// If it is not an id, throw exception.
if(child0->kind != "id") {
/*CHI:*/
cout << "ERR: " << child0->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(child0->text, "VALID ID");
}

  if(child(a, 2) && child(a, 2)->kind == "split") { // If child 2 exists and it is split
/*CHI:*/
cout << " Split" << endl;
/*ENDCHI*/

    AST *child1 = child(a, 1); // must be an ID
// If it is not an id, throw exception.
if(child1->kind != "id") {
/*CHI:*/
cout << "ERR: " << child1->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(child1->text, "VALID ID");
}
AST *tubeToSplit = child(child(a, 2), 0);
// If it is not an id, throw exception.
if(tubeToSplit->kind != "id") {
/*CHI:*/
cout << "ERR: " << tubeToSplit->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(tubeToSplit->text, "VALID ID");
}
/*CHI:*/
cout << "   Spliting " << tubeToSplit->text << endl;
/*ENDCHI*/

    // Create new tubes.
pair<Tube, Tube> splitedTube = evaluateSplitExpression(tubeToSplit->text);
// Erase objects of IDs if replaced.
// Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(child0->text);
// If ID already exists (and it's not a Tube), erase the object!
if(stockIt != stock.end()) {
if(stock[child0->text] == ConnectorType) {
connectorStock.erase(child0->text);
}
else if(stock[child0->text] == TubeVectorType) {
tubeVectorStock.erase(child0->text);
}
}
// Iterator to the id on stock map
stockIt = stock.find(child1->text);
// If ID already exists (and it's not a Tube), erase the object!
if(stockIt != stock.end()) {
if(stock[child1->text] == ConnectorType) {
connectorStock.erase(child1->text);
}
else if(stock[child1->text] == TubeVectorType) {
tubeVectorStock.erase(child1->text);
}
}
// Create new tubes
stock[child0->text] = TubeType;
stock[child1->text] = TubeType;
tubeStock[child0->text] = splitedTube.first;
tubeStock[child1->text] = splitedTube.second;
// Destroy old tube from stock and tubeStock.
stock.erase(tubeToSplit->text);
tubeStock.erase(tubeToSplit->text);
/*CHI:*/
cout << " Split made " << child0->text << endl;
/*ENDCHI*/
/*CHI:*/
cout << " Split made " << child1->text << endl;
/*ENDCHI*/

    return true;
}
else if(child(a, 1)->kind == "tubeVector") {
/*CHI:*/
cout << " TubeVector" << endl;
/*ENDCHI*/

    int size = evaluateNumberExpression(child(child(a, 1), 0));
// Size should be a natural number.
if(size < 0) {
throw WrongNumberException(size);
}

    /*CHI:*/
cout << " Size " << size << endl;
/*ENDCHI*/
// Erase objects of IDs if replaced.
// Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(child0->text);
// If ID already exists (and it's not a Tube), erase the object!
if(stockIt != stock.end()) {
if(stock[child0->text] == TubeType) {
tubeStock.erase(child0->text);
}
else if(stock[child0->text] == ConnectorType) {
connectorStock.erase(child0->text);
}
}
// Create tubeVector.
TubeVector tvec;
tvec.tubes = vector<Tube>(size);
tvec.top = 0;
stock[child0->text] = TubeVectorType;
tubeVectorStock[child0->text] = tvec;
/*CHI:*/
cout << "   TVEC->" << child0->text << endl;
/*ENDCHI*/

    return true;
}
else if(child(a, 1)->kind == "connector") {
/*CHI:*/
cout << " Connector" << endl;
/*ENDCHI*/

    int diam = evaluateNumberExpression(child(child(a, 1), 0));
// Diameter should be a natural number.
if(diam < 0) {
throw WrongNumberException(diam);
}

    // Erase objects of IDs if replaced.
// Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(child0->text);
// If ID already exists (and it's not a Tube), erase the object!
if(stockIt != stock.end()) {
if(stock[child0->text] == TubeType) {
tubeStock.erase(child0->text);
}
else if(stock[child0->text] == TubeVectorType) {
tubeVectorStock.erase(child0->text);
}
}
Connector con;
con.diameter = diam;
stock[child0->text] = ConnectorType;
connectorStock[child0->text] = con;
/*CHI:*/
cout << "   CON->" << child0->text << endl;
/*ENDCHI*/

    return true;
}
else if(child(a, 1)->kind == "tube") {
/*CHI:*/
cout << " Tube" << endl;
/*ENDCHI*/

    Tube t = evaluateTubeExpression(child(a, 1));
// Erase objects of IDs if replaced.
// Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(child0->text);
// If ID already exists (and it's not a Tube), erase the object!
if(stockIt != stock.end()) {
if(stock[child0->text] == ConnectorType) {
connectorStock.erase(child0->text);
}
else if(stock[child0->text] == TubeVectorType) {
tubeVectorStock.erase(child0->text);
}
}
stock[child0->text] = TubeType;
tubeStock[child0->text] = t;
/*CHI:*/
cout << "   TB->" << child0->text << endl;
/*ENDCHI*/

    return true;
}
else if(child(a, 1)->kind == "id") {
/*CHI:*/
cout << " Id" << endl;
/*ENDCHI*/
// Iterator to the id on stock map
map<string,string>::iterator stockIt = stock.find(child(a, 1)->text);
// If ID doesn't exists, throw an excpeption.
if(stockIt == stock.end()) {
/*CHI:*/
cout << "ERR: ID " << child(a, 1)->text << " NOT FOUND." << endl;
/*ENDCHI*/
throw NoIdException(child(a, 1)->text);
}

    // Erase objects of IDs if replaced.
// Iterator to the id on stock map.
stockIt = stock.find(child0->text);
// If ID already exists, erase the object!
if(stockIt != stock.end()) {
if(stock[child0->text] == TubeType) {
tubeStock.erase(child0->text);
}
else if(stock[child0->text] == ConnectorType) {
connectorStock.erase(child0->text);
}
else if(stock[child0->text] == TubeVectorType) {
tubeVectorStock.erase(child0->text);
}
}

    stock[child0->text] = stock[child(a, 1)->text];
if(stock[child0->text] == TubeType) {
tubeStock[child0->text] = tubeStock[child(a, 1)->text];
/*CHI:*/
cout << "   TB->" << child0->text << endl;
/*ENDCHI*/
}
else if(stock[child0->text] == ConnectorType) {
connectorStock[child0->text] = connectorStock[child(a, 1)->text];
/*CHI:*/
cout << "   CON->" << child0->text << endl;
/*ENDCHI*/
}
else if(stock[child0->text] == TubeVectorType) {
tubeVectorStock[child0->text] = tubeVectorStock[child(a, 1)->text];
/*CHI:*/
cout << "   TVEC->" << child0->text << endl;
/*ENDCHI*/
}

    return true;
}

  return false;
}

/**
* Evaluates the expression represented in the AST a.
* It must be an instruction.
* @param a The AST of the instruction.
* @return Boolean true if everything OK, false if something have gone wrong.
*/
bool evaluateInstructionExpression(AST *a){
//TODO: WHILE, PUSH, POP
if(a->text == "WHILE") {
/*CHI:*/
cout << " while" << endl;
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

    return true;
}
else if(a->text == "PUSH") {
/*CHI:*/
cout << " push" << endl;
/*ENDCHI*/
AST *child0 = child(a, 0);
// If it is not an id, throw exception.
if(child0->kind != "id") {
/*CHI:*/
cout << "ERR: " << child0->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(child0->text, "VALID ID");
}
// Iterator to the child0->text on stock map
map<string,string>::iterator stockIt = stock.find(child0->text);
// If ID doesn't exists, throw an excpeption.
if(stockIt == stock.end()) {
/*CHI:*/
cout << "ERR: ID " << child0->text << " NOT FOUND." << endl;
/*ENDCHI*/
throw NoIdException(child0->text);
}
// If it is not a tube vector throw an exception.
if(stockIt->second != TubeVectorType) {
/*CHI:*/
cout << "ERR: " << child0->text << " IS NOT A TUBE VECTOR." << endl;
/*ENDCHI*/
throw WrongTypeException(child0->text, "TUBE VECTOR");
}
/*CHI:*/
cout << "   B-TVEC " << child0->text << " SIZE " << tubeVectorStock[child0->text].tubes.size() << " TOP " << tubeVectorStock[child0->text].top << endl;
/*ENDCHI*/

    AST *child1 = child(a, 1);
// If it is not an id, throw exception.
if(child1->kind != "id") {
/*CHI:*/
cout << "ERR: " << child1->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(child1->text, "VALID ID");
}
// Iterator to the child1->text on stock map
stockIt = stock.find(child1->text);
// If ID doesn't exists, throw an excpeption.
if(stockIt == stock.end()) {
/*CHI:*/
cout << "ERR: ID " << child1->text << " NOT FOUND." << endl;
/*ENDCHI*/
throw NoIdException(child1->text);
}
// If it is not a tube throw an exception.
if(stockIt->second != TubeType) {
/*CHI:*/
cout << "ERR: " << child1->text << " IS NOT A TUBE." << endl;
/*ENDCHI*/
throw WrongTypeException(child1->text, "TUBE");
}

    TubeVector tv = tubeVectorStock[child0->text];
if(tv.top >= tv.tubes.size()) {
/*CHI:*/
cout << "ERR: index " << tv.top << " SIZE " << tv.tubes.size() << "." << endl;
/*ENDCHI*/
throw IndexOutOfBoundsException(tv.top, tv.tubes.size());
}
Tube t = tubeStock[child1->text];
(tv.tubes)[tv.top] = t;
++(tv.top);
tubeVectorStock[child0->text] = tv;
// Erase the tube put on the vector.
stock.erase(child1->text);
tubeStock.erase(child1->text);
/*CHI:*/
cout << "   A-TVEC " << child0->text << " SIZE " << tubeVectorStock[child0->text].tubes.size() << " TOP " << tubeVectorStock[child0->text].top << endl;
/*ENDCHI*/

    return true;
}
else if(a->text == "POP") {
/*CHI:*/
cout << " pop" << endl;
/*ENDCHI*/

    AST *child0 = child(a, 0);
// If it is not an id, throw exception.
if(child0->kind != "id") {
/*CHI:*/
cout << "ERR: " << child0->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(child0->text, "VALID ID");
}
// Iterator to the child0->text on stock map
map<string,string>::iterator stockIt = stock.find(child0->text);
// If ID doesn't exists, throw an excpeption.
if(stockIt == stock.end()) {
/*CHI:*/
cout << "ERR: ID " << child0->text << " NOT FOUND." << endl;
/*ENDCHI*/
throw NoIdException(child0->text);
}
// If it is not a tube vector throw an exception.
if(stockIt->second != TubeVectorType) {
/*CHI:*/
cout << "ERR: " << child0->text << " IS NOT A TUBE VECTOR." << endl;
/*ENDCHI*/
throw WrongTypeException(child0->text, "TUBE VECTOR");
}
/*CHI:*/
cout << "   B-TVEC " << child0->text << " SIZE " << tubeVectorStock[child0->text].tubes.size() << " TOP " << tubeVectorStock[child0->text].top << endl;
/*ENDCHI*/

    AST *child1 = child(a, 1);
// If it is not an id, throw exception.
if(child1->kind != "id") {
/*CHI:*/
cout << "ERR: " << child1->text << " IS NOT A VALID ID." << endl;
/*ENDCHI*/
throw WrongTypeException(child1->text, "VALID ID");
}

    TubeVector tv = tubeVectorStock[child0->text];
// If the vector is empty.
if(tv.top <= 0) {
/*CHI:*/
cout << "ERR: index " << (tv.top - 1) << " SIZE " << tv.tubes.size() << "." << endl;
/*ENDCHI*/
throw IndexOutOfBoundsException(tv.top, tv.tubes.size());
}

    // Erase objects of IDs if already exists.
// Iterator to the id on stock map.
stockIt = stock.find(child1->text);
// If ID already exists (and it's not a Tube), erase the object!
if(stockIt != stock.end()) {
if(stock[child1->text] == ConnectorType) {
connectorStock.erase(child1->text);
}
else if(stock[child1->text] == TubeVectorType) {
tubeVectorStock.erase(child1->text);
}
}

    --(tv.top);
Tube t = tv.tubes[tv.top];

    stock[child1->text] = TubeType;
tubeStock[child1->text] = t;
tubeVectorStock[child0->text] = tv;

    /*CHI:*/
cout << "   A-TVEC " << child0->text << " SIZE " << tubeVectorStock[child0->text].tubes.size() << " TOP " << tubeVectorStock[child0->text].top << endl;
/*ENDCHI*/

    return true;
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
cout << nie.what() << endl;
} catch(WrongTypeException& wte) {
cout << wte.what() << endl;
} catch(IndexOutOfBoundsException& ioobe) {
cout << ioobe.what() << endl;
} catch(WrongNumberException& wse){
cout << wse.what() << endl;
} 
/*UEXCEPT:
catch(...) { // It catches any non handled exception.
cout << "Unhandled exception occurred." << endl;
}
/*UEXCEPTEND;*/
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

  

void
#ifdef __USE_PROTOS
plumber(AST**_root)
#else
plumber(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  ops(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x1);
  }
}

void
#ifdef __USE_PROTOS
ops(AST**_root)
#else
ops(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd1[LA(1)]&0x2) ) {
      instruction(zzSTR); zzlink(_root, &_sibling, &_tail);
      (*_root)=createASTlist(_sibling);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x4);
  }
}

void
#ifdef __USE_PROTOS
instruction(AST**_root)
#else
instruction(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (setwd1[LA(1)]&0x8) ) {
    assignation(zzSTR); zzlink(_root, &_sibling, &_tail);
  }
  else {
    if ( (LA(1)==LEN) ) {
      length(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {
      if ( (LA(1)==DIAM) ) {
        diameter(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {
        if ( (LA(1)==WHILE) ) {
          wLoop(zzSTR); zzlink(_root, &_sibling, &_tail);
        }
        else {
          if ( (setwd1[LA(1)]&0x10) ) {
            vecOp(zzSTR); zzlink(_root, &_sibling, &_tail);
          }
          else {zzFAIL(1,zzerr1,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
        }
      }
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x20);
  }
}

void
#ifdef __USE_PROTOS
assignation(AST**_root)
#else
assignation(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==ID) ) {
    zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    zzmatch(ASSIG); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    {
      zzBLOCK(zztasp2);
      zzMake0;
      {
      if ( (LA(1)==ID) ) {
        zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
      }
      else {
        if ( (LA(1)==TUBE) ) {
          tubeDec(zzSTR); zzlink(_root, &_sibling, &_tail);
        }
        else {
          if ( (LA(1)==CON) ) {
            connectorDec(zzSTR); zzlink(_root, &_sibling, &_tail);
          }
          else {
            if ( (LA(1)==TVEC) ) {
              tvecDec(zzSTR); zzlink(_root, &_sibling, &_tail);
            }
            else {
              if ( (LA(1)==MERGE) ) {
                mergeExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
              }
              else {zzFAIL(1,zzerr2,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
            }
          }
        }
      }
      zzEXIT(zztasp2);
      }
    }
  }
  else {
    if ( (LA(1)==27) ) {
      zzmatch(27);  zzCONSUME;
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
      zzmatch(28);  zzCONSUME;
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
      zzmatch(29);  zzCONSUME;
      zzmatch(ASSIG); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      splitTube(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {zzFAIL(1,zzerr3,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x40);
  }
}

void
#ifdef __USE_PROTOS
length(AST**_root)
#else
length(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(LEN); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(27);  zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(29);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x80);
  }
}

void
#ifdef __USE_PROTOS
diameter(AST**_root)
#else
diameter(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(DIAM); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(27);  zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(29);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x1);
  }
}

void
#ifdef __USE_PROTOS
wLoop(AST**_root)
#else
wLoop(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(WHILE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(27);  zzCONSUME;
  boolOr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(29);  zzCONSUME;
  ops(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(ENDWHILE);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x2);
  }
}

void
#ifdef __USE_PROTOS
vecOp(AST**_root)
#else
vecOp(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==PUSH) ) {
    zzmatch(PUSH); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  }
  else {
    if ( (LA(1)==POP) ) {
      zzmatch(POP); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {zzFAIL(1,zzerr4,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x4);
  }
}

void
#ifdef __USE_PROTOS
tubeDec(AST**_root)
#else
tubeDec(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(TUBE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  numExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  numExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x8);
  }
}

void
#ifdef __USE_PROTOS
connectorDec(AST**_root)
#else
connectorDec(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(CON); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  numExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x10);
  }
}

void
#ifdef __USE_PROTOS
tvecDec(AST**_root)
#else
tvecDec(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(TVEC); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(OF);  zzCONSUME;
  numExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x20);
  }
}

void
#ifdef __USE_PROTOS
mergeExpr(AST**_root)
#else
mergeExpr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(MERGE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==ID) ) {
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==MERGE) ) {
        mergeExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {zzFAIL(1,zzerr5,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==ID) ) {
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==MERGE) ) {
        mergeExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {zzFAIL(1,zzerr6,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x40);
  }
}

void
#ifdef __USE_PROTOS
splitTube(AST**_root)
#else
splitTube(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(SPLIT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x80);
  }
}

void
#ifdef __USE_PROTOS
vecState(AST**_root)
#else
vecState(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==EMPTY) ) {
      zzmatch(EMPTY); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==FULL) ) {
        zzmatch(FULL); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      }
      else {zzFAIL(1,zzerr7,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  zzmatch(27);  zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(29);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x1);
  }
}

void
#ifdef __USE_PROTOS
boolOr(AST**_root)
#else
boolOr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  boolAnd(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==OR) ) {
      zzmatch(OR); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      boolAnd(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x2);
  }
}

void
#ifdef __USE_PROTOS
boolAnd(AST**_root)
#else
boolAnd(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  boolNot(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==AND) ) {
      zzmatch(AND); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      boolNot(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x4);
  }
}

void
#ifdef __USE_PROTOS
boolNot(AST**_root)
#else
boolNot(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==NOT) ) {
      zzmatch(NOT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (setwd3[LA(1)]&0x8) ) {
      }
      else {zzFAIL(1,zzerr8,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  boolAtom(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x10);
  }
}

void
#ifdef __USE_PROTOS
boolAtom(AST**_root)
#else
boolAtom(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==BOOL) ) {
    zzmatch(BOOL); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  }
  else {
    if ( (setwd3[LA(1)]&0x20) ) {
      vecState(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {
      if ( (setwd3[LA(1)]&0x40) ) {
        boolExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {
        if ( (LA(1)==27) ) {
          zzmatch(27);  zzCONSUME;
          boolOr(zzSTR); zzlink(_root, &_sibling, &_tail);
          zzmatch(29);  zzCONSUME;
        }
        else {zzFAIL(1,zzerr9,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
      }
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x80);
  }
}

void
#ifdef __USE_PROTOS
boolExpr(AST**_root)
#else
boolExpr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  numExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    zzmatch(COMP); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    numExpr(zzSTR); zzlink(_root, &_sibling, &_tail);
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd4, 0x1);
  }
}

void
#ifdef __USE_PROTOS
numExpr(AST**_root)
#else
numExpr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  term(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==PLUS) ) {
      zzmatch(PLUS); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      term(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd4, 0x2);
  }
}

void
#ifdef __USE_PROTOS
term(AST**_root)
#else
term(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  atom(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==TIMES) ) {
      zzmatch(TIMES); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      atom(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd4, 0x4);
  }
}

void
#ifdef __USE_PROTOS
atom(AST**_root)
#else
atom(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==NUM) ) {
    zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  }
  else {
    if ( (LA(1)==ID) ) {
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==LEN) ) {
        length(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {
        if ( (LA(1)==DIAM) ) {
          diameter(zzSTR); zzlink(_root, &_sibling, &_tail);
        }
        else {zzFAIL(1,zzerr10,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
      }
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd4, 0x8);
  }
}
