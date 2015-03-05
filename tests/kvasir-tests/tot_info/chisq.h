/*
	chisq.h -- definitions for contingency-table routines

	last edit:	88/09/19	D A Gwyn

	SCCS ID:	@(#)chisq.h	1.1 (edited for publication)
*/

/* library routines: */

#ifdef __STDC__
extern double	ChiSqTbl( int r, int c, const long *f, int *pdf );
extern double	InfoTbl( int r, int c, const long *f, int *pdf );
#else
extern double	ChiSqTbl();
extern double	InfoTbl();
#endif
