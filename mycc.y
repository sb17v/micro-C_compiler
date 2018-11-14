/* TO BE COMPLETED */

%{

#include "lex.yy.h"
#include "global.h"

#define MAXFUN 100
#define MAXFLD 100

static struct ClassFile cf;
static Type return_type;
static Type func_desc;
static Type func_name;
static Backpatchlist return_list;
/* stacks of symbol tables and offsets, depth is just 2 in C (global/local) */
static Table *tblptr[2];
static int offset[2];

/* stack pointers (index into tblptr[] and offset[]) */
static int tblsp = -1;
static int offsp = -1;

/* stack operations */
#define top_tblptr	(tblptr[tblsp])
#define top_offset	(offset[offsp])
#define push_tblptr(t)	(tblptr[++tblsp] = t)
#define push_offset(n)	(offset[++offsp] = n)
#define pop_tblptr	(tblsp--)
#define pop_offset	(offsp--)

/* flag to indicate we are compiling main's body (to differentiate 'return') */
static int is_in_main = 0;

%}

/* declare YYSTYPE attribute types of tokens and nonterminals */
%union
{ Symbol *sym;  /* token value yylval.sym is the symbol table entry of an ID */
  unsigned num; /* token value yylval.num is the value of an int constant */
  float flt;    /* token value yylval.flt is the value of a float constant */
  char *str;    /* token value yylval.str is the value of a string constant */
  unsigned loc; /* location of instruction to backpatch */
  Type typ;	/* type descriptor */
  Expr exp;
}

/* declare ID token and its attribute type */
%token <sym> ID

/* Declare INT tokens (8 bit, 16 bit, 32 bit) and their attribute type 'num' */
%token <num> INT8 INT16 INT32

/* Declare FLT token for literal floats */
%token <flt> FLT

/* Declare STR token for literal strings */
%token <str> STR

/* declare tokens for keywords */
/* Note: install_id() returns Symbol* for keywords and identifiers */
%token <sym> BREAK CHAR DO ELSE FLOAT FOR IF INT MAIN RETURN VOID WHILE

/* declare operator tokens */
%right '=' PA NA TA DA MA AA XA OA LA RA
%left OR
%left AN
%left '|'
%left '^'
%left '&'
%left EQ NE LE '<' GE '>'
%left LS RS
%left '+' '-'
%left '*' '/' '%'
%right '!' '~'
%left PP NN 
%left '.' AR

/* Declare attribute types for marker nonterminals, such as K L M and N */
/* TODO: TO BE COMPLETED WITH ADDITIONAL NONMARKERS AS NECESSARY */
%type <loc> K L M N P

%type <typ> type list args

%type <num> ptr

%type <exp> expr

%type <sym> ftype head

%%

prog	: Mprog exts	{ addwidth(top_tblptr, top_offset);
			  pop_tblptr;
			  pop_offset;
			}
	;

Mprog	: /* empty */	{ push_tblptr(mktable(NULL));
			  push_offset(0);
			}
	;

exts	: exts func
	| exts decl
	| /* empty */
	;

func	: MAIN '(' ')' Mmain block
			{ // need a temporary table pointer
			  Table *table;
			  // the type of main is a JVM type descriptor
			  Type type = mkfun("[Ljava/lang/String;", "V");
              backpatchlist(return_list, pc);
			  // emit the epilogue part of main()
			  emit3(getstatic, constant_pool_add_Fieldref(&cf, "java/lang/System", "out", "Ljava/io/PrintStream;"));
			  emit(iload_2);
			  emit3(invokevirtual, constant_pool_add_Methodref(&cf, "java/io/PrintStream", "println", "(I)V"));
			  emit(return_);
			  // method has public access and is static
			  cf.methods[cf.method_count].access = (enum access_flags)(ACC_PUBLIC | ACC_STATIC);
			  // method name is "main"
			  cf.methods[cf.method_count].name = "main";
			  // method descriptor of "void main(String[] arg)"
			  cf.methods[cf.method_count].descriptor = type;
			  // local variables
			  cf.methods[cf.method_count].max_locals = top_offset;
			  // max operand stack size of this method
			  cf.methods[cf.method_count].max_stack = 100;
			  // length of bytecode is in the emitter's pc variable
			  cf.methods[cf.method_count].code_length = pc;
			  // must copy code to make it persistent
			  cf.methods[cf.method_count].code = copy_code();
			  if (!cf.methods[cf.method_count].code)
				error("Out of memory");
			  // advance to next method to store in method array
			  cf.method_count++;
			  if (cf.method_count > MAXFUN)
			  	error("Max number of functions exceeded");
			  // add width information to table
			  addwidth(top_tblptr, top_offset);
			  // need this table of locals for enterproc
			  table = top_tblptr;
			  // exit the local scope by popping
			  pop_tblptr;
			  pop_offset;
			  // enter the function in the global table
			  enterproc(top_tblptr, $1, type, table);
			}
	| head block
			{ /* TASK 3: TO BE COMPLETED */
            //Type func_desc = mkfun($3, return_type);

            Table *table;
            // the type of main is a JVM type descriptor
            cf.methods[cf.method_count].access = (enum access_flags)(ACC_PUBLIC | ACC_STATIC);
            cf.methods[cf.method_count].name = func_name;
            cf.methods[cf.method_count].descriptor = func_desc;
            cf.methods[cf.method_count].max_locals = top_offset;
            cf.methods[cf.method_count].max_stack = 100;
            cf.methods[cf.method_count].code_length = pc;                
            cf.methods[cf.method_count].code = copy_code();
            if (!cf.methods[cf.method_count].code)
            error("Out of memory");
            cf.method_count++;
            if (cf.method_count > MAXFUN)
            error("Max number of functions exceeded");

            addwidth(top_tblptr, top_offset);
            table = top_tblptr;
            pop_tblptr;
            pop_offset;
            enterproc(top_tblptr, $1, func_desc, table);
			}
	;

head : ftype Margs args ')'
            {
                func_desc = mkfun($3, return_type);

                Table *func_table = top_tblptr;
                addwidth(top_tblptr, top_offset);
                pop_tblptr;
                enterproc(top_tblptr, $1, func_desc, NULL);
                push_tblptr(func_table);
                
                func_name = $1->lexptr;
                $$ = $1;
            }
ftype : type ptr ID '('
            {
                return_type = $1;
                $$ = $3;
            }

Mmain	:		{ int label1, label2;
			  Table *table;
			  // create new table for local scope of main()
			  table = mktable(top_tblptr);
			  // push it to create new scope
			  push_tblptr(table);
			  // for main(), we must start with offset 3 in the local variables of the frame
			  push_offset(3);
			  // init code block to store stmts
			  init_code();
			  // emit the prologue part of main()
			  emit(aload_0);
			  emit(arraylength);
			  emit2(newarray, T_INT);
			  emit(astore_1);
			  emit(iconst_0);
			  emit(istore_2);
			  label1 = pc;
			  emit(iload_2);
			  emit(aload_0);
			  emit(arraylength);
			  label2 = pc;
			  emit3(if_icmpge, PAD);
			  emit(aload_1);
			  emit(iload_2);
			  emit(aload_0);
			  emit(iload_2);
			  emit(aaload);
			  emit3(invokestatic, constant_pool_add_Methodref(&cf, "java/lang/Integer", "parseInt", "(Ljava/lang/String;)I"));
			  emit(iastore);
			  emit32(iinc, 2, 1);
			  emit3(goto_, label1 - pc);
			  backpatch(label2, pc - label2);
			  // global flag to indicate we're in main()
			  is_in_main = 1;
			}
	;

Margs	:		{ /* TASK 3: TO BE COMPLETED */
			    Table *table;
                table = mktable(top_tblptr);
                push_tblptr(table);
                push_offset(0);
               
                init_code();
                is_in_main = 0;
			}
	;

block	: '{' decls stmts '}'
	;

decls	: decls decl
	| /* empty */
	;

decl	: list ';'
	;

type	: VOID		{ $$ = mkvoid(); }
	| INT		{ $$ = mkint(); }
	| FLOAT		{ $$ = mkfloat(); }
	| CHAR		{ $$ = mkchar(); }
	;

args	: args ',' type ptr ID
			{ if ($4 && ischar($3))
				enter(top_tblptr, $5, mkstr(), top_offset++);
			  else
				enter(top_tblptr, $5, $3, top_offset++);
			  $$ = mkpair($1, $3);
			}
	| type ptr ID	{ if ($2 && ischar($1))
				enter(top_tblptr, $3, mkstr(), top_offset++);
			  else
				enter(top_tblptr, $3, $1, top_offset++);
			  $$ = $1;
			}
	;

list	: list ',' ptr ID
			{ /* TASK 1 and 4: TO BE COMPLETED */
			  /* $1 is the type */
			  /* $3 == 1 means pointer type for ID, e.g. char* so use mkstr() */
              if (top_tblptr->level == 0){
                //Global
                if ($3 && ischar($1)){
                    cf.fields[cf.field_count].access = ACC_STATIC;
                    cf.fields[cf.field_count].name = $4->lexptr;
                    cf.fields[cf.field_count].descriptor = mkstr();
                    cf.field_count++;
                    enter(top_tblptr, $4, mkstr(), constant_pool_add_Fieldref(&cf, cf.name, $4->lexptr, mkstr()));
                }else{
                    cf.fields[cf.field_count].access = ACC_STATIC;
                    cf.fields[cf.field_count].name = $4->lexptr;
                    cf.fields[cf.field_count].descriptor = mkstr();
                    cf.field_count++;
                    enter(top_tblptr, $4, $1, constant_pool_add_Fieldref(&cf, cf.name, $4->lexptr, $1));
                }
              } else if(top_tblptr->level > 0){
                //Local
                if ($3 && ischar($1)){
                    enter(top_tblptr, $4, mkstr(), top_offset++);
                }else{
                    enter(top_tblptr, $4, $1, top_offset++);
                }
              }
			  $$ = $1;
			}
	| type ptr ID	{ /* TASK 1 and 4: TO BE COMPLETED */
			  /* $2 == 1 means pointer type for ID, e.g. char* so use mkstr() */
              if (top_tblptr->level == 0){
                //Global
                if ($2 && ischar($1)){
                    cf.fields[cf.field_count].access = ACC_STATIC;
                    cf.fields[cf.field_count].name = $3->lexptr;
                    cf.fields[cf.field_count].descriptor = mkstr();
                    cf.field_count++;
                    enter(top_tblptr, $3, mkstr(), constant_pool_add_Fieldref(&cf, cf.name, $3->lexptr, mkstr()));
                }else{
                    cf.fields[cf.field_count].access = ACC_STATIC;
                    cf.fields[cf.field_count].name = $3->lexptr;
                    cf.fields[cf.field_count].descriptor = $1;
                    cf.field_count++;
                    enter(top_tblptr, $3, $1, constant_pool_add_Fieldref(&cf, cf.name, $3->lexptr, $1));
                }
              } else if(top_tblptr->level > 0){
                //Local
                if ($2 && ischar($1)){
                    enter(top_tblptr, $3, mkstr(), top_offset++);
                }else{
                    enter(top_tblptr, $3, $1, top_offset++);
                }
              }
			  $$ = $1;
			}
	;

ptr	: /* empty */	{ $$ = 0; }
	| '*'		{ $$ = 1; }
	;

stmts   : stmts stmt
        | /* empty */
        ;

/* TASK 1: TO BE COMPLETED: */
stmt    : ';'
        | expr ';'      { emit(pop); }
        | IF '(' expr ')' M stmt
                        { 
                            if($3.truelist.last_entry > 0 && $3.falselist.last_entry > 0)
                            {
                                backpatchlist($3.truelist, $5+3);
                                $3.falselist.backpatch_array[$3.falselist.last_entry++] = $5;
                            backpatchlist($3.falselist, pc);
                            }
                        }
        | IF '(' expr ')' M stmt ELSE N  L stmt
                        { 
                            if($3.truelist.last_entry > 0 && $3.falselist.last_entry > 0)
                            {
                                backpatchlist($3.truelist, $5+3);
                                $3.falselist.backpatch_array[$3.falselist.last_entry++] = $5;
                            backpatchlist($3.falselist, $9);
                            backpatch($8, pc-$8); 
                            }
                        }
        | WHILE '(' L expr ')' M stmt N
                        { 
                            if($4.truelist.last_entry > 0 && $4.falselist.last_entry > 0)
                            {
                            backpatch($8, $3-$8); 
                                backpatchlist($4.truelist, $6+3);
                                $4.falselist.backpatch_array[$4.falselist.last_entry++] = $6;
                                backpatchlist($4.falselist, $8+3);
                            }
                        }
        | DO L stmt WHILE '(' expr ')' K ';'
                        { 
                            if($6.truelist.last_entry > 0 && $6.falselist.last_entry > 0)
                            {
                                $6.falselist.backpatch_array[$6.falselist.last_entry++] = $8;
                            backpatchlist($6.truelist, $2);
                                backpatchlist($6.falselist, $8+3);
                            }
                        }
        | FOR '(' expr P ';' L expr ';' L expr P N ')' L stmt
                        { 
                            if($7.truelist.last_entry > 0 && $7.falselist.last_entry > 0)
                            {
                            emit3(goto_, $9-pc);
                            backpatchlist($7.truelist, $14);
                            backpatchlist($7.falselist, pc);
                            backpatch($12, $6-$12);
                            }
                         }
        | RETURN expr ';'
                        { 
                            if (is_in_main){
                                emit(istore_2);
                                return_list.backpatch_array[(return_list.last_entry)++] = pc;
                                emit3(goto_, 0); /* TO BE COMPLETED */
                            }
                            else if (isint(return_type) && isint($2.type)){
                                emit(ireturn);
                            }
                            else if (isfloat(return_type) && isfloat($2.type)){
                                emit(freturn);
                            }
                            else if (isstr(return_type) && isstr($2.type)){
                                emit(areturn);
                            }
                            else{
                                error("Type Error for 'return'");
                            }
                                // error("return int/float not implemented");
                        }
	| BREAK ';'	{ 
                            /* BREAK is optional to implement (see Pr3) */
                            emit3(goto_, 0);
			}
        | '{' stmts '}'
        | error ';'     { yyerrok; }
        ;

exprs	: exprs ',' expr
	| expr
	;

/* TASK 1: TO BE COMPLETED (use pr3 code, then work on assign operators): */
expr    : ID   '=' expr { 
                            emit(dup);
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            }
                            if (getlevel(top_tblptr, $1) == 0){
                                emit3(putstatic, place);
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(istore, place); 
                                } else if (isfloat(type)){
                                    emit2(fstore, place); 
                                } else if (isstr(type) && isstr($3.type)){
                                    emit2(astore, place);
                                }
                            }
                            $$.type = type;
                        }
        | ID   PA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for += %d", isstr(type));
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(iadd);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(iadd);
                                    emit(dup);
                                    emit2(istore, place); 
                                } else if(isfloat(type)) {
                                    emit2(fload, place);
                                    emit(fadd);
                                    emit(dup);
                                    emit2(fstore, place); 
                                } else {
                                    error("Type Error for +=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   NA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for !=");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(swap);
                                emit(isub);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(swap);
                                    emit(isub);
                                    emit(dup);
                                    emit2(istore, place);
                                } else if (isfloat(type)){
                                    emit2(iload, place);
                                    emit(swap);
                                    emit(isub);
                                    emit(dup);
                                    emit2(istore, place);
                                } else {
                                    error("Type Error for !=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   TA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for *=");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(imul);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(imul);
                                    emit(dup);
                                    emit2(istore, place);
                                } else if (isfloat(type)){
                                    emit2(fload, place);
                                    emit(fmul);
                                    emit(dup);
                                    emit2(fstore, place);
                                } else {
                                    error("Type Error for *=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   DA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for /=");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(swap);
                                emit(idiv);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(swap);
                                    emit(idiv);
                                    emit(dup);
                                    emit2(istore, place); 
                                } else if (isfloat(type)){
                                    emit2(fload, place);
                                    emit(swap);
                                    emit(fdiv);
                                    emit(dup);
                                    emit2(fstore, place); 
                                } else {
                                    error("Type Error for /=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   MA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for MA");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(swap);
                                emit(irem);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(swap);
                                    emit(irem);
                                    emit(dup);
                                    emit2(istore, place);
                                } else if (isfloat(type)){
                                    emit2(fload, place);
                                    emit(swap);
                                    emit(frem);
                                    emit(dup);
                                    emit2(fstore, place);
                                } else {
                                    error("Type Error for %=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   AA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for &=");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(iand);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(iand);
                                    emit(dup);
                                    emit2(istore, place); 
                                } else {
                                    error("Type Error for &=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   XA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for ^=");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(ixor);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(ixor);
                                    emit(dup);
                                    emit2(istore, place); 
                                } else {
                                    error("Type Error for ^=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   OA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for |=");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(ior);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(ior);
                                    emit(dup);
                                    emit2(istore, place); 
                                } else {
                                    error("Type Error for |=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   LA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for <<=");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(swap);
                                emit(ishl);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(swap);
                                    emit(ishl);
                                    emit(dup);
                                    emit2(istore, place); 
                                } else {
                                    error("Type Error for <<=");
                                }
                            }
                            $$.type = type;
                        }
        | ID   RA  expr { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(isint(type)){
                                if(isfloat($3.type)){
                                    emit(f2i);
                                }
                            } else if (isfloat(type)){
                                if(isint($3.type)){
                                    emit(i2f);
                                }
                            } else {
                                printf("Type Error for >>=");
                            }
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(swap);
                                emit(ishr);
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(swap);
                                    emit(ishr);
                                    emit(dup);
                                    emit2(istore, place); 
                                } else {
                                    error("Type Error for >>=");
                                }
                            }
                            $$.type = type;
                        }
        | expr OR  L expr { 
                            if($1.type != NULL){
                                if($1.val == 1){
                                    emit(iconst_1);
                                }else{
                                    emit(iconst_0);
                                }
                                emit(pop);
                            }else if($4.type != NULL){
                                if($4.val == 1){
                                    emit(iconst_1);
                                }else{
                                    emit(iconst_0);
                                }
                            }else{
                                backpatchlist($1.falselist, $3);
                                $$.truelist = mergelist($1.truelist, $4.truelist);
                                $$.falselist = $4.falselist;
                            }
                        }
        | expr AN  L expr { 
                            if($1.type != NULL){
                                if($1.val == 0){
                                    emit(iconst_0);
                                }else{
                                    emit(iconst_1);
                                }
                                emit(pop);
                            }else if($4.type != NULL){
                                if($4.val == 0){
                                    emit(iconst_0);
                                }else{
                                    emit(iconst_1);
                                }
                            }else{
                                backpatchlist($1.truelist, $3);
                                $$.falselist = mergelist($1.falselist, $4.falselist);
                                $$.truelist = $4.truelist;
                            }
                        }
        | expr '|' expr { 
                            if(isint($1.type) && isint($3.type)){
                                emit(ior); 
                            } else {
                                printf("Type Error for '|'");
                            }
                            $$.type = $3.type;
                        }
        | expr '^' expr { 
                            if(isint($1.type) && isint($3.type)){
                                emit(ixor);
                            } else {
                                printf("Type Error for '^'");
                            }
                            $$.type = $3.type;
                        }
        | expr '&' expr { 
                            if(isint($1.type) && isint($3.type)){
                                emit(iand);
                            } else {
                                printf("Type Error for '&''");
                            }
                            $$.type = $3.type;
                        }
        | expr EQ  expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmpeq, 0); 
                                    emit3(goto_, 0);
                                } else {
                                    emit(fcmpg);
                                    emit(iconst_0);
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmpeq, 0); 
                                    emit3(goto_, 0);
                                }
                            } else {
                                printf("Type Error for ==");
                            }
                            $$.type = NULL;
                        }
        | expr NE  expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmpne, 0);  
                                    emit3(goto_, 0);
                                } else {
                                    emit(fcmpg);
                                    emit(iconst_0);
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmpne, 0);  
                                    emit3(goto_, 0);
                                }
                            } else {
                                printf("Type Error for !=");
                            }
                            $$.type = NULL;
                        }
        | expr '<' expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmplt, 0);  
                                    emit3(goto_, 0);  
                                } else {
                                    emit(fcmpg);
                                    emit(iconst_1);
                                    emit(ineg);

                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmplt, 0);  
                                    emit3(goto_, 0);  
                                }
                            } else {
                                printf("Type Error for <");
                            }
                            $$.type = NULL;
                        }
        | expr '>' expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmpgt, 0);
                                    emit3(goto_, 0);   
                                } else {
                                    emit(fcmpg);
                                    emit(iconst_1);
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmpgt, 0);
                                    emit3(goto_, 0); 
                                }
                            } else {
                                printf("Type Error for >");
                            }
                        }
        | expr LE  expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmple, 0);
                                    emit3(goto_, 0);  
                                } else {
                                    emit(fcmpg);
                                    emit(iconst_1);
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmple, 0);
                                    emit3(goto_, 0);
                                }
                            } else {
                                printf("Type Error for <=");
                            }
                            $$.type = NULL;
                        }
        | expr GE  expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmpge, 0);
                                    emit3(goto_, 0);      
                                } else {
                                    emit(fcmpg);
                                    emit(iconst_1);
                                    emit(ineg);

                                    $$.truelist = makelist(pc);
                                    $$.falselist = makelist(pc+3);
                                    emit3(if_icmpge, 0);
                                    emit3(goto_, 0);     
                                }
                            } else {
                                printf("Type Error for >=");
                            }
                            $$.type = NULL;
                        }
        | expr LS  expr { 
                            if(isint($1.type))
                            {
                                emit(ishl);
                            } else {
                                error("Type Error for <<");
                            }
                            $$.type = $1.type;
                        }
        | expr RS  expr { 
                            if(isint($1.type))
                            {
                                emit(ishr);
                            } else {
                                error("Type Error for >>");
                            }
                            $$.type = $1.type;
                        }
        | expr '+' expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    emit(iadd);
                                } else {
                                    emit(fadd);
                                }
                            } else {
                                printf("Type Error for '+'");
                            }
                            $$.type = $1.type;
                        }
        | expr '-' expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    emit(isub);
                                } else {
                                    emit(fsub);
                                }
                            } else {
                                printf("Type Error for '-'");
                            }
                            $$.type = $1.type;
                            
                        }
        | expr '*' expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    emit(imul);
                                } else {
                                    emit(fmul);
                                }
                            } else {
                                printf("Type Error for '*'");
                            }
                            $$.type = $1.type;
                        }
        | expr '/' expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    emit(idiv);
                                } else {
                                    emit(fdiv);
                                }
                            } else {
                                printf("Type Error for '/'");
                            }
                            $$.type = $1.type;
                        }
        | expr '%' expr { 
                            if(iseq($1.type, $3.type)){
                                if(isint($1.type)){
                                    emit(irem);
                                } else {
                                    emit(frem);
                                }
                            } else {
                                printf("Type Error for rem");
                            }
                            $$.type = $1.type;
                        }
        | '!' expr      { 
                            $$.truelist = $2.falselist; 
			                $$.falselist = $2.truelist;
                        }
        | '~' expr      { 
                            if(isint($2.type)){
                                emit(ineg);
                                emit(iconst_1); 
                                emit(isub);
                            } else {
                                printf("Type Error for '~'");
                            }
                        }
        | '+' expr %prec '!'
                        { 
                            
                        }
        | '-' expr %prec '!'
                        { 
                            if(isint($2.type)){
                                emit(ineg);
                            } else if (isfloat($2.type)){
                                emit(fneg);
                            } else {
                                printf("Type Error for '~'");
                            }
                            $$.type = $2.type;
                        }
        | '(' expr ')'  { $$ = $2;}
        | '$' INT8      { // check that we are in main()
                            if (is_in_main)
                            {	emit(aload_1);
                                emit2(bipush, $2);
                                emit(iaload);
                            }
                            else
                                error("invalid use of $# in function");
                            $$.type = mkint();
                        }
        | PP ID         { 
                            Type type = gettype(top_tblptr, $2);
                            int place = getplace(top_tblptr, $2);
                            if(getlevel(top_tblptr, $2) == 0){
                                emit3(getstatic, place);
                                emit(iconst_1);
                                emit(iadd);  
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $2) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(iconst_1);
                                    emit(iadd);  
                                    emit(dup);
                                    emit2(istore, place); 
                                } else {
                                    emit2(fload, place);
                                    emit(fconst_1);
                                    emit(fadd);  
                                    emit(dup);
                                    emit2(fstore, place); 
                                }
                            }
                            $$.type = type;
                        }
        | NN ID         { 
                            Type type = gettype(top_tblptr, $2);
                            int place = getplace(top_tblptr, $2);
                            if(getlevel(top_tblptr, $2) == 0){
                                emit3(getstatic, place);
                                emit(iconst_1);
                                emit(isub);  
                                emit(dup);
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $2) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(iconst_1);
                                    emit(isub);  
                                    emit(dup);
                                    emit2(istore, place); 
                                } else {
                                    emit2(fload, place);
                                    emit(fconst_1);
                                    emit(fsub);  
                                    emit(dup);
                                    emit2(fstore, place);
                                }
                            }
                            $$.type = type;
                        }
        | ID PP         { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(dup);
                                emit(iconst_1);
                                emit(iadd);  
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(dup);
                                    emit(iconst_1);
                                    emit(iadd);  
                                    emit2(istore, place); 
                                } else {
                                    emit2(fload, place);
                                    emit(dup);
                                    emit(fconst_1);
                                    emit(fadd);  
                                    emit2(fstore, place);
                                }
                            }
                            $$.type = type;
                        }
        | ID NN         { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if(getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                                emit(dup);
                                emit(iconst_1);
                                emit(isub);  
                                emit3(putstatic, place); 
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place);
                                    emit(dup);
                                    emit(iconst_1);
                                    emit(isub);  
                                    emit2(istore, place); 
                                } else {
                                    emit2(fload, place);
                                    emit(dup);
                                    emit(fconst_1);
                                    emit(fsub);  
                                    emit2(fstore, place);
                                }
                            }
                            $$.type = type;
                        }
        | ID            { 
                            Type type = gettype(top_tblptr, $1);
                            int place = getplace(top_tblptr, $1);
                            if (getlevel(top_tblptr, $1) == 0){
                                emit3(getstatic, place);
                            } else if (getlevel(top_tblptr, $1) > 0){
                                if(isint(type)){
                                    emit2(iload, place); 
                                } else {
                                    emit2(fload, place); 
                                }
                            }
                            $$.type = type;
                        }
        | INT8          { 
                            emit2(bipush, $1); 
                            $$.truelist.last_entry=0;
                            $$.falselist.last_entry=0;
                            $$.val = $1;
                            $$.type = mkint();
                        }
        | INT16         { 
                            emit3(sipush, $1); 
                            $$.truelist.last_entry=0;
                            $$.falselist.last_entry=0;
                            $$.val = $1;
                            $$.type = mkint();
                        }
        | INT32         { 
                            emit2(ldc, constant_pool_add_Integer(&cf, $1)); 
                            $$.truelist.last_entry=0;
                            $$.falselist.last_entry=0;
                            $$.val = $1;
                            $$.type = mkint();
                        }
	| FLT		        { 
                            emit2(ldc, constant_pool_add_Float(&cf, $1)); 
                            $$.type = mkfloat();
                        }
	| STR		        { 
                            emit2(ldc, constant_pool_add_String(&cf, constant_pool_add_Utf8(&cf, $1))); 
                            $$.type = mkstr();
                        }
	| ID '(' exprs ')'
			{ /* TASK 3: TO BE COMPLETED */
			  emit3(invokestatic, constant_pool_add_Methodref(&cf, cf.name, $1->lexptr, gettype(top_tblptr, $1)));
              $$.type = mkret(gettype(top_tblptr, $1));
			}
        ;

K       : /* empty */   { $$ = pc; emit3(ifne, 0); }
        ;

L       : /* empty */   { $$ = pc; }
        ;

M       : /* empty */   { $$ = pc;	/* location of inst. to backpatch */
			  emit3(ifeq, 0);
			}
        ;

N       : /* empty */   { $$ = pc;	/* location of inst. to backpatch */
			  emit3(goto_, 0);
			}
        ;

P       : /* empty */   { emit(pop); }
        ;

%%

int main(int argc, char **argv)
{
	// init the compiler
	init();

	// set up a new class file structure
	init_ClassFile(&cf);

	// class has public access
	cf.access = ACC_PUBLIC;

	// class name is "Code"
	cf.name = "Code";

	// field counter (incremented for each field we add)
	cf.field_count = 0;

	// method counter (incremented for each method we add)
	cf.method_count = 0;

	// allocate an array of MAXFLD fields
	cf.fields = (struct FieldInfo*)malloc(MAXFLD * sizeof(struct FieldInfo));

	// allocate an array of MAXFUN methods
	cf.methods = (struct MethodInfo*)malloc(MAXFUN * sizeof(struct MethodInfo));

	if (!cf.methods)
		error("Out of memory");

	if (argc > 1)
		if (!(yyin = fopen(argv[1], "r")))
			error("Cannot open file for reading");

	if (yyparse() || errnum > 0)
		error("Compilation errors: class file not saved");

	fprintf(stderr, "Compilation successful: saving %s.class\n", cf.name);

	// save class file
	save_classFile(&cf);

	return 0;
}

