
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_bocra(DllInfo *dll);

SEXP qt_qimage2matrix(SEXP);
SEXP diffCalBW(SEXP data, SEXP srow, SEXP scol, SEXP sh, SEXP smaxDist);

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_CallMethodDef CallEntries[] = {

    CALLDEF(qt_qimage2matrix, 1),
    CALLDEF(diffCalBW, 5),

    {NULL, NULL, 0}
};


void R_init_bocra(DllInfo *dll)
{
    // Register C routines
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

