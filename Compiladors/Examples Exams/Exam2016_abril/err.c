/*
 * A n t l r  S e t s / E r r o r  F i l e  H e a d e r
 *
 * Generated from: exam.g
 *
 * Terence Parr, Russell Quong, Will Cohen, and Hank Dietz: 1989-2001
 * Parr Research Corporation
 * with Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
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
#define zzSET_SIZE 4
#include "antlr.h"
#include "ast.h"
#include "tokens.h"
#include "dlgdef.h"
#include "err.h"

ANTLRChar *zztokens[23]={
	/* 00 */	"Invalid",
	/* 01 */	"@",
	/* 02 */	"GRID",
	/* 03 */	"PLACE",
	/* 04 */	"AT",
	/* 05 */	"MOVE",
	/* 06 */	"DIRECTION",
	/* 07 */	"FITS",
	/* 08 */	"HEIGHT",
	/* 09 */	"WHILE",
	/* 10 */	"DEF",
	/* 11 */	"ENDEF",
	/* 12 */	"COMP",
	/* 13 */	"AND",
	/* 14 */	"ASSIG",
	/* 15 */	"ID",
	/* 16 */	"NUM",
	/* 17 */	"SPACE",
	/* 18 */	"\\(",
	/* 19 */	"\\)",
	/* 20 */	"\\[",
	/* 21 */	"\\]",
	/* 22 */	","
};
SetWordType zzerr1[4] = {0x0,0x80,0x4,0x0};
SetWordType zzerr2[4] = {0x22,0xc6,0x20,0x0};
SetWordType zzerr3[4] = {0x20,0x82,0x0,0x0};
SetWordType setwd1[23] = {0x0,0xfb,0x0,0x0,0x0,0xc6,0x0,
	0x0,0x0,0xc6,0xea,0x0,0x0,0x0,0x0,
	0xc6,0x0,0x0,0x0,0x0,0x0,0xc8,0x0};
SetWordType zzerr4[4] = {0x80,0x1,0x1,0x0};
SetWordType zzerr5[4] = {0x0,0x1,0x1,0x0};
SetWordType setwd2[23] = {0x0,0x7,0x0,0x0,0x4,0x7,0x0,
	0x0,0x10,0x7,0x7,0x0,0x40,0xe0,0x0,
	0x7,0x10,0x0,0x0,0xe8,0x0,0x7,0x0};
SetWordType setwd3[23] = {0x0,0x0,0x0,0x0,0x0,0x0,0x0,
	0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,
	0x0,0x0,0x0,0x0,0x1,0x0,0x0,0x0};
