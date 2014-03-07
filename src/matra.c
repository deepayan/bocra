#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


SEXP segment(SEXP data, SEXP srow, SEXP scol, SEXP RmatraPosition, 
	     SEXP RmatraWidth, SEXP RwordHeight, SEXP RtolLevel) 
{
    int matraPosition, matraWidth, wordHeight, nrow, ncol, *pleaveBlack, *preEnterBlack, i, j;
    double tolLevel, *cdata;

    SEXP leaveBlack, reEnterBlack, stayBlack, ans, Rnames;
    nrow = asInteger(srow);
    ncol = asInteger(scol);
    matraPosition = asInteger(RmatraPosition);
    matraWidth = asInteger(RmatraWidth);
    wordHeight = asInteger(RwordHeight);
    tolLevel = asReal(RtolLevel);
    cdata = REAL(data);

    char *names[2] = {"leaveBlack", "reEnterBlack"};

    PROTECT(ans = allocVector(VECSXP, 2));
    PROTECT(Rnames = allocVector(STRSXP, 2));

    PROTECT(leaveBlack = allocVector(INTSXP, ncol));
    PROTECT(reEnterBlack = allocVector(INTSXP, ncol));

    pleaveBlack = INTEGER_POINTER(leaveBlack);
    preEnterBlack = INTEGER_POINTER(reEnterBlack);

    for(i = 0; i < ncol; i++) {
	/* Image coding: black=printed pixel=1, white=background=0.  

	   i is the current (vertical) column (row=matraPosition is
	   fixed).  The goal is to find the pixel positions where we
	   first leave black, and then reenter black (if any).

	   If the starting position is already white, then
	   leaveBlack=0.  If we never reenter black, then
	   reenterBlack=-1.
	*/
	pleaveBlack[i] = 0;
	preEnterBlack[i] = 0;

	j = matraPosition;
	if (cdata[matraPosition + i * nrow - 1] == 1) {
	    while (j < nrow && cdata[i * nrow + j] == 1) j++;
	    pleaveBlack[i] = j;
	    j++;
	}
	while (j < nrow && cdata[i * nrow + j] == 0) j++;
	if (j < nrow) preEnterBlack[i] = j;
	else preEnterBlack[i] = -1;
    }
    for(i = 0; i < 2; i++) {
	SET_STRING_ELT(Rnames, i, mkChar(names[i]));
    }
    SET_VECTOR_ELT(ans, 0, leaveBlack);
    SET_VECTOR_ELT(ans, 1, reEnterBlack);
    setAttrib(ans, R_NamesSymbol, Rnames);

    UNPROTECT(4);
    return ans;
}




SEXP detectDeletePortion(SEXP array, SEXP n, SEXP tolLevel)
{
    SEXP deletePortion;
    int *parray, pn, ptolLevel, *pdeletePortion,
	i, j, indicator;

    parray = INTEGER(array);
    pn = asInteger(n);
    ptolLevel = asInteger(tolLevel);

    PROTECT(deletePortion = allocVector(INTSXP, pn));
    pdeletePortion = INTEGER_POINTER(deletePortion);

    for (i = 0; i < pn; i++) {
	pdeletePortion[i] = 0;
    }

    for (i = 0; i < pn - ptolLevel; i++) {
	indicator = 1;

	for (j = 1; j <= ptolLevel; j++) {
	    if(parray[i + j] != parray[i] + j) {
		indicator = 0;
		j = ptolLevel + 1;
	    }
	}

	if (indicator == 1) {
	    pdeletePortion[i] = 1;
	    i = i + ptolLevel;
	}
    }

    UNPROTECT(1);
    return deletePortion;
}


/* Identify connected components after matra deletion */

struct Nbor {
    int row;
    int col;
};

typedef struct Nbor nbor;

SEXP componentsRosenfeld(SEXP data, SEXP scol, SEXP srow);
void negate(int *plabel, int *cdata, int n);
void find_component(int *plabel, int ind, int nrow, int ncol);
void search(int *plabel, int ind, int i, int j, int nrow);
void neighbor(nbor *nset, int i, int j);

/* FIXME: don't need nrow/ncol to be presepcified */
SEXP componentsRosenfeld(SEXP data, SEXP scol, SEXP srow)
{
    int nrow, ncol, *plabel, *cdata, ind = 0;

    SEXP label;

    nrow = asInteger(srow);
    ncol = asInteger(scol);
    cdata = INTEGER(asInteger(data));

    PROTECT(label = allocVector(INTSXP, ncol * nrow));
    plabel = INTEGER_POINTER(label);

    negate(plabel, cdata, nrow * ncol);
    find_component(plabel, ind, nrow, ncol);
    UNPROTECT(1);
    return label;
}

void negate(int *plabel, int *cdata, int n) 
{
    int i;
    for(i = 0; i < n; i++) plabel[i] = -cdata[i];
    return;
}

void find_component(int *plabel, int ind, int nrow, int ncol) 
{
    int i, j;
    for(i = 1; i < nrow - 1; i++) {
	for(j = 1; j < ncol - 1; j++) {
	    if(plabel[j * nrow + i] == -1) {
		ind++;
		search(plabel, ind, i, j, nrow);
	    }
	}
    }
    return;
}


void search(int *plabel, int ind, int i, int j, int nrow)
{
    int k;
    static nbor nset[8];
    neighbor(nset, i, j);
    plabel[j * nrow + i] = ind;
    for(k = 0; k < 8; k++) {
	if(plabel[nset[k].row + nset[k].col * nrow] == -1) 
	    search(plabel, ind, nset[k].row, nset[k].col, nrow);
    }
    return;
}


void neighbor(nbor *nset, int i, int j) 
{
    nset[0].col = (j - 1);
    nset[0].row = i;

    nset[1].col = (j - 1);
    nset[1].row = (i - 1);

    nset[2].col = (j - 1);
    nset[2].row = (i + 1);

    nset[3].col = (j + 1);
    nset[3].row = i;

    nset[4].col = (j + 1);
    nset[4].row = (i + 1);

    nset[5].col = (j + 1);
    nset[5].row = (i - 1);

    nset[6].col = j;
    nset[6].row = (i - 1);

    nset[7].col = j;
    nset[7].row = (i + 1);

    return;
}
