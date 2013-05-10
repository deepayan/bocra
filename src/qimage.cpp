
#include <QImage>
#include <QColor>

#include <qtbase.h>

extern "C" {
SEXP qt_qimage2matrix(SEXP x);
}

SEXP qt_qimage2matrix(SEXP x)
{
    SEXP ans, dim;
    double *ip;
    QImage *img = unwrapSmoke(x, QImage);
    dim = PROTECT(allocVector(INTSXP, 2));
    int i, j, w = img->width(), h = img->height();
    INTEGER(dim)[0] = w;
    INTEGER(dim)[1] = h;
    ans = PROTECT(allocVector(REALSXP, w * h));
    setAttrib(ans, R_DimSymbol, dim);
    ip = REAL(ans);
    for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
            ip[i + j*w] = 1.0 * qGray(img->pixel(i, j)) / 255.0;
	}
    }
    UNPROTECT(2);
    return ans;
}
