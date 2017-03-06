#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: plumber.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define TUBE 2
#define CON 3
#define SPLIT 4
#define MERGE 5
#define LEN 6
#define DIAM 7
#define TVEC 8
#define OF 9
#define PUSH 10
#define POP 11
#define FULL 12
#define EMPTY 13
#define WHILE 14
#define EWHILE 15
#define ASSIG 16
#define PLUS 17
#define TIMES 18
#define NOT 19
#define AND 20
#define OR 21
#define COMP 22
#define BOOL 23
#define NUM 24
#define ID 25
#define SPACE 26
#define ENDWHILE 30

#ifdef __USE_PROTOS
void plumber(AST**_root);
#else
extern void plumber();
#endif

#ifdef __USE_PROTOS
void ops(AST**_root);
#else
extern void ops();
#endif

#ifdef __USE_PROTOS
void instruction(AST**_root);
#else
extern void instruction();
#endif

#ifdef __USE_PROTOS
void assignation(AST**_root);
#else
extern void assignation();
#endif

#ifdef __USE_PROTOS
void splitTube(AST**_root);
#else
extern void splitTube();
#endif

#ifdef __USE_PROTOS
void wLoop(AST**_root);
#else
extern void wLoop();
#endif

#ifdef __USE_PROTOS
void intW(AST**_root);
#else
extern void intW();
#endif

#ifdef __USE_PROTOS
void vecOp(AST**_root);
#else
extern void vecOp();
#endif

#ifdef __USE_PROTOS
void vecState(AST**_root);
#else
extern void vecState();
#endif

#ifdef __USE_PROTOS
void tubeDec(AST**_root);
#else
extern void tubeDec();
#endif

#ifdef __USE_PROTOS
void mergeExpr(AST**_root);
#else
extern void mergeExpr();
#endif

#ifdef __USE_PROTOS
void connectorDec(AST**_root);
#else
extern void connectorDec();
#endif

#ifdef __USE_PROTOS
void tvecDec(AST**_root);
#else
extern void tvecDec();
#endif

#ifdef __USE_PROTOS
void length(AST**_root);
#else
extern void length();
#endif

#ifdef __USE_PROTOS
void diameter(AST**_root);
#else
extern void diameter();
#endif

#ifdef __USE_PROTOS
void boolOr(AST**_root);
#else
extern void boolOr();
#endif

#ifdef __USE_PROTOS
void boolAnd(AST**_root);
#else
extern void boolAnd();
#endif

#ifdef __USE_PROTOS
void boolNot(AST**_root);
#else
extern void boolNot();
#endif

#ifdef __USE_PROTOS
void boolAtom(AST**_root);
#else
extern void boolAtom();
#endif

#ifdef __USE_PROTOS
void boolExpr(AST**_root);
#else
extern void boolExpr();
#endif

#ifdef __USE_PROTOS
void numExpr(AST**_root);
#else
extern void numExpr();
#endif

#ifdef __USE_PROTOS
void term(AST**_root);
#else
extern void term();
#endif

#ifdef __USE_PROTOS
void atom(AST**_root);
#else
extern void atom();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType zzerr3[];
extern SetWordType setwd1[];
extern SetWordType zzerr4[];
extern SetWordType zzerr5[];
extern SetWordType zzerr6[];
extern SetWordType zzerr7[];
extern SetWordType zzerr8[];
extern SetWordType setwd2[];
extern SetWordType zzerr9[];
extern SetWordType zzerr10[];
extern SetWordType zzerr11[];
extern SetWordType zzerr12[];
extern SetWordType zzerr13[];
extern SetWordType setwd3[];
extern SetWordType zzerr14[];
extern SetWordType zzerr15[];
extern SetWordType setwd4[];
extern SetWordType zzerr16[];
extern SetWordType setwd5[];
