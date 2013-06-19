/*
#################################################################################################
#################################################################################################
## difCalBW() : Calculate the derivatives at all the points in the image
## xyDifFun() : It calculates derivative at some pixel point. 
## data: contains the pixel grey level value at all the points.
## srow: Number of rows in the image
## scol: Number of column in the image
## sh: Bandwidth
## maxDist: maximum distance in every direction upto which the sum in kernel density will be calculated
################################################################################################# 
#################################################################################################
*/

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef MAX
  #define MAX( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif


#ifndef MIN
  #define MIN( a, b ) ( ((a) < (b)) ? (a) : (b) )
#endif

void xyDifFun(double *cdata, double *cdx, double *cdy, double *table,
	      double h, int nrow, int ncol, int m, int n, int maxDist);

void createDiffTable(double *table, double h, int maxDist); 
/* Calculate the value of kernel function upto distance maximum distance maxDist */

SEXP diffCalBW(SEXP data, SEXP srow, SEXP scol, SEXP sh, SEXP smaxDist)
{
    int nrow, ncol, nlength, maxDist, m, i;
    double *cdata, h, *cdx, *cdy, *table;
    SEXP sdx, sdy, retList, listNames;
    char *names[2] = {"diff_x", "diff_y"};

    nrow = asInteger(srow);
    ncol = asInteger(scol);
    maxDist = asInteger(smaxDist);
    h = asReal(sh);
    cdata = REAL(data);

    nlength = nrow * ncol;
    table = malloc(((maxDist + 1) * maxDist) * sizeof(double));
 
    sdx = PROTECT(NEW_NUMERIC(nlength));
    sdy = PROTECT(NEW_NUMERIC(nlength));
    cdx = NUMERIC_POINTER(sdx);
    cdy = NUMERIC_POINTER(sdy);

    PROTECT(retList = allocVector(VECSXP, 2));
    PROTECT(listNames = allocVector(STRSXP, 2));

    createDiffTable(table, h, maxDist);

    for(m = 0; m < nlength; m++){
	xyDifFun(cdata, cdx, cdy, table,
		 h, nrow, ncol,  m, nlength, maxDist);
    }
 
    SET_VECTOR_ELT(retList, 0, sdx);
    SET_VECTOR_ELT(retList, 1, sdy);
    for(i = 0; i < 2; i++) {
	SET_STRING_ELT(listNames, i, mkChar(names[i]));
    }
    setAttrib(retList, R_NamesSymbol, listNames);
    UNPROTECT(4);
    free(table);
    return retList;
}


/* #################################################################################
## cdata: pointer to data
## cdx: pointer to array containing derivative wrt x
## cdy: pointer to containing derivative wrt y 
## table: table containing kernel values
## h: bandwidth
## nrow: number of row in the image
## ncol: number of column in the image
## m: pixel at which we are calculating derivative
## nlength: nrow * ncol
## maxDist: maximum distance upto which derivatives are calculated
####################################################################################*/

void xyDifFun(double *cdata, double *cdx, double *cdy, double *table,
	      double h, int nrow, int ncol, int m, int nlength, int maxDist) 
{
    int rowPixel, colPixel, beginRowPixel, beginColPixel, endRowPixel, endColPixel, 
	i, j, row, col, index;
    double sumx = 0, sumy = 0;
    rowPixel = m % nrow;
    colPixel = m/nrow;
    beginRowPixel = MAX(0, rowPixel - maxDist);
    beginColPixel = MAX(0, colPixel - maxDist);
    endRowPixel = MIN(nrow - 1, rowPixel + maxDist);
    endColPixel = MIN(ncol - 1, colPixel + maxDist);

    for(row = beginRowPixel; row <= endRowPixel; row++) {
	for(col = beginColPixel; col <= endColPixel; col++) {
	    index = nrow * col + row;
	    if(cdata[index] > 0) {
		i = row - rowPixel;
		j = col - colPixel;
		sumx = sumx + i * cdata[index] * table[abs(j) + abs(i) * maxDist]; 
		sumy = sumy + j * cdata[index] * table[abs(i) + abs(j) * maxDist]; 
	    }
	}
    }
    cdx[m] = sumx;
    cdy[m] = sumy;
    return;
}

void createDiffTable(double *table, double h, int maxDist) 
{
    int i, j;
    for(i = 0; i <= maxDist; i++) { 
	for(j = 0; j <= maxDist; j++) {
	    table[i * maxDist + j] = (1 - pow(i/h, 2)) * pow((1 - pow(j/h,2)),2); 
	}
    }
    return;
}
