
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_bocra(DllInfo *dll);

SEXP qt_qimage2matrix(SEXP);

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_CallMethodDef CallEntries[] = {

    CALLDEF(qt_qimage2matrix, 1),

    {NULL, NULL, 0}
};


void R_init_bocra(DllInfo *dll)
{
    // Register C routines
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

