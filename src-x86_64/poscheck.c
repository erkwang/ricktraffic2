//
//  poscheck.c
//  
//


#include <stdio.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include "poscheck.h"

/* To check the position of each car and revert to the old position
 for the ones that should be blocked by a different colored car*/

SEXP
poscheck(SEXP newrow, SEXP newcol, SEXP checkrow, SEXP checkcol, SEXP oldrow, SEXP oldcol)
{
    int i, j;
    int x, y;
    
    PROTECT(newrow = AS_INTEGER(newrow));
    int *pnr;
    pnr = INTEGER_POINTER(newrow);

    PROTECT(newcol = AS_INTEGER(newcol));
    int *pnc;
    pnc = INTEGER_POINTER(newcol);

    PROTECT(checkrow = AS_INTEGER(checkrow));
    int *pcr;
    pcr = INTEGER_POINTER(checkrow);

    PROTECT(checkcol = AS_INTEGER(checkcol));
    int *pcc;
    pcc = INTEGER_POINTER(checkcol);

    PROTECT(oldrow = AS_INTEGER(oldrow));
    int *por;
    por = INTEGER_POINTER(oldrow);

    PROTECT(oldcol = AS_INTEGER(oldcol));
    int *poc;
    poc = INTEGER_POINTER(oldcol);

    SEXP list, list_names;
    char *names[2] = {"row", "column"};
    
    for (i = 0; i<LENGTH(newrow); i++) {
        for (j = 0; j<LENGTH(checkrow); j++) {
            x = fabs(pnr[i] - pcr[j]);
            y = fabs(pnc[i] - pcc[j]);
            if ((x + y) == 0) {
                pnr[i] = por[i];
                pnc[i] = poc[i];
		break;
            }
        }
    }
    
    PROTECT(list_names = allocVector(STRSXP,2));
    for(i = 0; i < 2; i++) SET_STRING_ELT(list_names,i,mkChar(names[i]));
    PROTECT(list = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(list, 0, newrow);
    SET_VECTOR_ELT(list, 1, newcol);
    setAttrib(list, R_NamesSymbol, list_names);
    
    UNPROTECT(8);
    return list;
}
