/*
	<gamma.h> -- definitions for gamma-function routines

	last edit:	88/09/09	D A Gwyn

	SCCS ID:	@(#)gamma.h	1.1 (edited for publication)
*/

/* library routines: */

#ifdef __STDC__
extern double	Gamma( double z ), LGamma( double z ),
		Factorial( int n ), LFactorial( int n ),
		BCoeff( int n, int k ),
		Beta( double z, double w ),
		PGamma( double a, double x ), QGamma( double a, double x ),
		Erf( double x ), Erfc( double x ),
		CPoisson( double x, int k ),
		PChiSq( double chisq, int nu ), QChiSq( double chisq, int nu );
#else
extern double	Gamma(), LGamma(), Factorial(), LFactorial(),
		BCoeff(), Beta(), PGamma(), QGamma(), Erf(), Erfc(),
		CPoisson(), PChiSq(), QChiSq();
#endif
