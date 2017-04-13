#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: exam.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define GRID 2
#define PLACE 3
#define AT 4
#define MOVE 5
#define DIRECTION 6
#define FITS 7
#define HEIGHT 8
#define WHILE 9
#define DEF 10
#define ENDEF 11
#define COMP 12
#define AND 13
#define ASSIG 14
#define ID 15
#define NUM 16
#define SPACE 17

#ifdef __USE_PROTOS
void program(AST**_root);
#else
extern void program();
#endif

#ifdef __USE_PROTOS
void grid(AST**_root);
#else
extern void grid();
#endif

#ifdef __USE_PROTOS
void conj_insts(AST**_root);
#else
extern void conj_insts();
#endif

#ifdef __USE_PROTOS
void conj_defs(AST**_root);
#else
extern void conj_defs();
#endif

#ifdef __USE_PROTOS
void function_def(AST**_root);
#else
extern void function_def();
#endif

#ifdef __USE_PROTOS
void instruction(AST**_root);
#else
extern void instruction();
#endif

#ifdef __USE_PROTOS
void move_inst(AST**_root);
#else
extern void move_inst();
#endif

#ifdef __USE_PROTOS
void wLoop(AST**_root);
#else
extern void wLoop();
#endif

#ifdef __USE_PROTOS
void pair_num_list(AST**_root);
#else
extern void pair_num_list();
#endif

#ifdef __USE_PROTOS
void boolAnd(AST**_root);
#else
extern void boolAnd();
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
void fitsExpr(AST**_root);
#else
extern void fitsExpr();
#endif

#ifdef __USE_PROTOS
void trio_num_list(AST**_root);
#else
extern void trio_num_list();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType zzerr3[];
extern SetWordType setwd1[];
extern SetWordType zzerr4[];
extern SetWordType zzerr5[];
extern SetWordType setwd2[];
extern SetWordType setwd3[];
