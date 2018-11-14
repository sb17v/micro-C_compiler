/* TODO: TO BE COMPLETED */

#include "global.h"
#define STRMAX 999
#define SYMMAX 999

static char lexemes[STRMAX];
static Symbol symtable[SYMMAX];

static int lastchar = -1;
static int lastentry = -1;
/* TODO: define a symbol table/array, reuse project Pr1 */

Symbol *lookup(const char *s)
{
        /* TODO: TO BE COMPLETED */
    int p;
	for (p = lastentry; p >= 0; p--)
	if (strcmp(symtable[p].lexptr, s) == 0)
		return &symtable[p];

	return NULL;
}

Symbol *insert(const char *s, int token)
{
        /* TODO: TO BE COMPLETED */
    int len = strlen(s);
	if (lastentry + 1 >= SYMMAX)
		error("Symbol table full");
	if (lastchar + len + 1 >= STRMAX)
		error("Lexemes array full");
	lastentry = lastentry + 1;
	symtable[lastentry].token = token;
	symtable[lastentry].lexptr = &lexemes[lastchar + 1];
	lastchar = lastchar + len + 1;
	strcpy(symtable[lastentry].lexptr, s);

	return &symtable[lastentry];
}
