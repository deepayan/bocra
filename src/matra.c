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
	pleaveBlack[i] = 0;
	preEnterBlack[i] = 0;

	if (cdata[matraPosition + i * nrow - 1] == 1) {
	    j = matraPosition;
	    while (j < nrow && cdata[i * nrow + j] == 1) {
		j++;
	    }
	    pleaveBlack[i] = j;
	    j++;

	    while(j < nrow && cdata[i * nrow + j] == 0) {
		j++;
	    }

	    if(j < nrow && cdata[i * nrow + j] == 1)
		preEnterBlack[i] = j;

	    else if (j == nrow)
		preEnterBlack[i] = -1;
	}
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
