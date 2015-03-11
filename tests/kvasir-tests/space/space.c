


#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "ctype.h"
#include "math.h"
#include "limits.h"
#include "float.h"





#include "strutt.h"


#define FPRINTF printf

char            GLOBLE_FILENAME[UNLEN + 1];




void            parserro(struct charac * err_ptr, int errcode, char *s);
void            kwdsinit(void);
void            emsginit(void);
void            unaminit(void);
void            moseinit(void);
void            glvainit(void);
char            TapeGet(struct charac ** tp);
void            waitcont(void);
int             isletter(char ch);
int             InKWords(char *WORD);
int             InUNames(char *WORD);
void            interror(char *subname);
int             InserUN(char *WORD);
int             EndOfTape(struct charac ** p);
void            floatsup(void);
void            nomefile(char fn[UNLEN + 1]);
void            prnfile(struct charac * firstel);
int             answer(char domanda[EMSGSLEN]);


int             readfil3(char filename[UNLEN + 1], struct charac ** firstel, struct charac ** lastel);
int             GetKeyword(char *kw, struct charac ** tp);
int             GetUName(char word[UNLEN + 1], struct charac ** tp);
int             GetReal(double *reale, struct charac ** tp);
int             GetUReal(struct charac * p1, struct charac ** pp2, double *ureal_ptr);
int             Get1Real(struct charac * p1, struct charac ** pp2, double *ureal_ptr);
int             GetInt(struct charac * p1, struct charac ** pp2, int *intero_ptr);
int             GetNatur(struct charac * p1, struct charac ** pp2, int *natur_ptr);
int             lenunit(struct charac ** tp, int *unit);
int 
angunit(struct charac * p1, struct charac ** pp2,
	int *angle_unit_ptr);
int             ampunit(struct charac ** tp, int *unit_ptr);
int 
squgrdef(struct charac * p1, struct charac ** pp2,
	 double *pstep_ptr,
	 int *pstep_unit_ptr);
int 
recgrdef(struct charac * p1, struct charac ** pp2,
	 double *pstep_ptr,
	 double *qstep_ptr,
	 int *pqstep_unit_ptr);
int 
hexgrdef(struct charac * p1, struct charac ** pp2,
	 double *pstep_ptr,
	 int *pstep_unit_ptr);
int 
trigrdef(struct charac * p1, struct charac ** pp2,
	 double *angle_ptr,
	 int *angle_unit_ptr,
	 double *pstep_ptr,
	 double *qstep_ptr,
	 int *pqstep_unit_ptr,
	 double *px_ptr,
	 double *py_ptr,
	 double *qx_ptr,
	 double *qy_ptr,
	 int *pqxy_unit_ptr);
int 
griddef1(struct charac * p1, struct charac ** pp2,
	 struct Grid ** grid_ptr);
int 
doubstep(struct charac * p1, struct charac ** pp2,
	 double *pstep_ptr,
	 double *qstep_ptr,
	 int *pqstep_unit_ptr);
int 
angclaus(struct charac * p1, struct charac ** pp2,
	 double *angle_ptr,
	 int *angle_unit_ptr);
int 
angstep(struct charac * p1, struct charac ** pp2,
	double *angle_ptr,
	int *angle_unit_ptr,
	double *pstep_ptr,
	double *qstep_ptr,
	int *pqstep_unit_ptr);
int 
versdef(struct charac * p1, struct charac ** pp2,
	double *px_ptr,
	double *py_ptr,
	double *qx_ptr,
	double *qy_ptr,
	int *pqxy_unit_ptr);
int 
sinstep(struct charac * p1, struct charac ** pp2,
	double *step_ptr,
	int *step_unit_ptr);
int 
recspec(struct charac * p1, struct charac ** pp2,
	double *pdim_ptr,
	double *qdim_ptr,
	int *pqdim_unit_ptr);
int 
geomspec(struct charac * p1, struct charac ** pp2,
	 int *geometry_ptr,
	 double *radius_ptr,
	 int *radius_unit_ptr,
	 double *pdim_ptr,
	 double *qdim_ptr,
	 int *pqdim_unit_ptr);
int 
circspec(struct charac * p1, struct charac ** pp2,
	 double *radius_ptr,
	 int *radius_unit_ptr);
int             extmod(struct charac * p1, struct charac ** pp2);
int 
gaussel(struct charac * p1, struct charac ** pp2,
	double *anglee_ptr,
	double *angleh_ptr,
	int *angle_unit_ptr,
	double *tapere_ptr,
	double *taperh_ptr,
	int *taper_unit_ptr);
int 
cosinus(struct charac * p1, struct charac ** pp2,
	double *exp_ptr);
int 
modspec(struct charac * p1, struct charac ** pp2,
	int *model_ptr,
	double *exp_ptr,
	double *anglee_ptr,
	double *angleh_ptr,
	int *angle_unit_ptr,
	double *tapere_ptr,
	double *taperh_ptr,
	int *taper_unit_ptr);
int 
angval(struct charac * p1, struct charac ** pp2,
       double *angle_ptr,
       int *angle_unit_ptr);
int 
polor(struct charac * p1, struct charac ** pp2,
      double *phiangle_ptr,
      int *phiangle_unit_ptr);
int 
linpol(struct charac * p1, struct charac ** pp2,
       double *angle_ptr,
       int *angle_unit_ptr);
int 
circpol(struct charac * p1, struct charac ** pp2,
	int *direction_ptr);
int 
polspec(struct charac * p1, struct charac ** pp2,
	int *polarization_ptr,
	double *phepol_ptr,
	int *phepol_unit_ptr,
	int *direction_ptr);
int 
gainunit(struct charac * p1, struct charac ** pp2,
	 int *gain_unit_ptr);
int 
gainval(struct charac * p1, struct charac ** pp2,
	double *gain_ptr,
	int *gain_unit_ptr);
int 
gainspec(struct charac * p1, struct charac ** pp2,
	 double *gain_ptr,
	 int *gain_unit_ptr);
int 
sinelem(struct charac * p1, struct charac ** pp2,
	struct Elem ** elem_ptr);
int 
pshdef1(struct charac * p1, struct charac ** pp2,
	double *psh_ptr,
	int *psh_unit_ptr);
int 
pscdef1(struct charac * p1, struct charac ** pp2,
	double *psc_ptr,
	int *psc_unit_ptr);
int 
ppadef1(struct charac * p1, struct charac ** pp2,
	double *ppa_ptr,
	int *ppa_unit_ptr);
int 
polorbis(struct charac * p1, struct charac ** pp2,
	 double *phepol_ptr,
	 int *phepol_unit_ptr);
int 
ampval(struct charac * p1, struct charac ** pp2,
       double *amp_ptr,
       int *amp_unit_ptr);
int 
portamp(struct charac * p1, struct charac ** pp2,
	double *amp_ptr,
	int *amp_unit_ptr);
int 
portdef(struct charac * p1, struct charac ** pp2,
	struct Port * PORT_PTR,
	int *portnum_ptr);
int 
portspec(struct charac * p1, struct charac ** pp2,
	 int *nports_ptr,
	 struct Port ** port_ptr);
int 
elemdef(struct charac * p1, struct charac ** pp2,
	struct Elem ** elem_ptr);
int 
nodorien(struct charac * p1, struct charac ** pp2,
	 double *theta_ptr,
	 double *phi_ptr,
	 double *psi_ptr,
	 int *angle_unit_ptr);
int 
nodecoor(struct charac * p1, struct charac ** pp2,
	 int *pcoord_ptr,
	 int *qcoord_ptr);
int 
nodedef(struct charac * p1, struct charac ** pp2,
	struct Node ** node_ptr);
int 
blockdef(struct charac * p1, struct charac ** pp2,
	 struct Node ** node_ptr);
int 
adddef(struct charac * p1, struct charac ** pp2,
       struct AddRem ** addrem_ptr);
int 
remdef(struct charac * p1, struct charac ** pp2,
       struct AddRem ** addrem_ptr);
int 
adremdef(struct charac * p1, struct charac ** pp2,
	 struct AddRem ** addrem_ptr);
int 
grgeodef(struct charac * p1, struct charac ** pp2,
	 struct AddRem ** addrem_ptr);
int 
hexdef(struct charac * p1, struct charac ** pp2,
       struct Node ** node_ptr);
int 
polydef(struct charac * p1, struct charac ** pp2,
	struct Node ** node_ptr);
int 
greldef(struct charac * p1, struct charac ** pp2,
	struct Grid ** grid_ptr,
	struct Elem ** elem_ptr);
int 
secoramp(struct charac * p1, struct charac ** pp2,
	 double *centre_ptr,
	 double *p1_et_ptr,
	 double *p2_et_ptr,
	 double *q1_et_ptr,
	 double *q2_et_ptr,
	 int *amp_unit_ptr);
int 
unifamp(struct charac * p1, struct charac ** pp2,
	double *unif_val_ptr,
	int *amp_unit_ptr);
int 
grampexc(struct charac * p1, struct charac ** pp2,
	 struct GrAmpExc ** grampexc_ptr);
int 
angledir(struct charac * p1, struct charac ** pp2,
	 double *theta_ptr,
	 double *phi_ptr,
	 int *angle_unit_ptr);
int 
uvval(struct charac * p1,
      struct charac ** pp2, double *val_ptr);
int 
uvdir(struct charac * p1, struct charac ** pp2,
      double *u_ptr,
      double *v_ptr);
int 
beampha(struct charac * p1, struct charac ** pp2,
	double *u_ptr,
	double *v_ptr,
	double *theta_ptr,
	double *phi_ptr,
	int *angle_unit_ptr);
int 
phaserot(struct charac * p1, struct charac ** pp2,
	 double *phase_start_ptr,
	 double *phase_step_ptr,
	 int *phase_unit_ptr);
int 
anglerot(struct charac * p1, struct charac ** pp2,
	 double *angle_start_ptr,
	 double *angle_step_ptr,
	 int *angle_unit_ptr);
int 
serotdir(struct charac * p1, struct charac ** pp2,
	 int *direction_ptr);
int 
serotpar(struct charac * p1, struct charac ** pp2,
	 double *angle_start_ptr,
	 double *angle_step_ptr,
	 int *omit_phase_ptr,
	 double *phase_start_ptr,
	 double *phase_step_ptr,
	 int *angle_unit_ptr,
	 int *phase_unit_ptr);
int 
serotpha(struct charac * p1, struct charac ** pp2,
	 int *direction_ptr,
	 int *omit_angle_ptr,
	 double *angle_start_ptr,
	 double *angle_step_ptr,
	 int *omit_phase_ptr,
	 double *phase_start_ptr,
	 double *phase_step_ptr,
	 int *angle_unit_ptr,
	 int *phase_unit_ptr);
int 
secorpha(struct charac * p1, struct charac ** pp2,
	 double *centre_ptr,
	 double *p1_ep_ptr,
	 double *p2_ep_ptr,
	 double *q1_ep_ptr,
	 double *q2_ep_ptr,
	 int *phase_unit_ptr);
int 
unifpha(struct charac * p1, struct charac ** pp2,
	double *unif_val_ptr,
	int *phase_unit_ptr);
int 
grphaexc(struct charac * p1, struct charac ** pp2,
	 struct GrPhaExc ** grphaexc_ptr);
int 
grexcdef(struct charac * p1, struct charac ** pp2,
	 struct GrAmpExc ** grampexc_ptr,
	 struct GrPhaExc ** grphaexc_ptr);
int 
groupdef(struct charac * p1, struct charac ** pp2,
	 struct Group ** group_ptr);








int             intmin(int a, int b);
int             intmax(int a, int b);
double          doubmin(double a, double b);
double          doubmax(double a, double b);
double          dcos(double x);
double          dsin(double x);
double          dacos(double x);
double          dasin(double x);
double          datan(double x);
int             mmconv(double *x_ptr, int unit);
int             degconv(double *x_ptr, int unit);
int             linconv(double *x_ptr, int unit);
int             grwrite(struct Group * group_ptr, char filename[UNLEN + 1]);


int 
mksnode(struct AddRem * addrem_ptr,
	struct Geomnode ** first_node_ptr,
	struct Geomnode ** last_node_ptr,
	double pstep,
	double qstep,
	double angle,
	int *nodes_num_ptr,
	int elem_shape,
	double elem_pdim,
	double elem_qdim);
int 
mksblock(struct AddRem * addrem_ptr,
	 struct Geomnode ** first_node_ptr,
	 struct Geomnode ** last_node_ptr,
	 double pstep,
	 double qstep,
	 double angle,
	 int *nodes_num_ptr,
	 int elem_shape,
	 double elem_pdim,
	 double elem_qdim);
int 
mkshex(struct AddRem * addrem_ptr,
       struct Geomnode ** first_node_ptr,
       struct Geomnode ** last_node_ptr,
       double pstep,
       double qstep,
       double angle,
       int *nodes_num_ptr,
       int elem_shape,
       double elem_pdim,
       double elem_qdim);
int             addscan(struct Group * group_ptr);
int 
extremes(struct Geomnode * gnode_ptr,
	 int *pnum_ptr,
	 int *qnum_ptr);
int             simgroup(struct Group * group_ptr);
int             fixnodor(struct Node * node_ptr);
int             fixsgrel(struct Group * group_ptr);
int             fixsgrid(struct Grid * grid);
int             fixselem(struct Elem * elem);
int 
extsize(struct Geomnode * gnode_ptr,
	double elem_pdim,
	double elem_qdim,
	double *xsize_ptr,
	double *ysize_ptr);
int 
sgrrot(struct Geomnode * start_ptr,
       double XC,
       double YC,
       double phi);
int             fixgramp(struct GrAmpExc * g);
int             fixport(struct Elem * elem_ptr);
int 
sgrampun(struct GrAmpExc * grampexc_ptr,
	 struct Geomnode * gnode_ptr,
	 struct Port * port_ptr);
int             simamp(struct Group * group_ptr);
int 
pqlimits(struct Geomnode * gnode_ptr,
	 struct Geomnode ** pmin_ptr,
	 struct Geomnode ** pmax_ptr,
	 struct Geomnode ** qmin_ptr,
	 struct Geomnode ** qmax_ptr);
int 
sgramp2n(struct GrAmpExc * grampexc_ptr,
	 struct Geomnode * gnode_ptr,
	 struct Port * port_ptr,
	 int group_shape);
double 
secorder(double a,
	 double b,
	 double c,
	 double d,
	 double e,
	 double x,
	 double y);
int             simpol(struct Group * group_ptr);
int             simpha(struct Group * group_ptr);
int             fixgrpha(struct GrPhaExc * gphaexc_ptr);
int 
sgrphaun(struct GrPhaExc * grphaexc_ptr,
	 struct Geomnode * geomnode_ptr,
	 struct Port * port_ptr);
int 
sgrpha2n(struct GrPhaExc * grphaexc_ptr,
	 struct Geomnode * geomnode_ptr,
	 struct Port * port_ptr,
	 int group_shape);
int 
gnodfind(struct Geomnode * gnode_list_start_ptr,
	 struct Geomnode ** gnode_ptr,
	 int p,
	 int q);
void 
gnodevis(int p,
	 int q,
	 double angle,
	 double phase,
	 struct Geomnode * gnode_list_start_ptr);
void 
seqrotrg(double angle_start,
	 double angle_step,
	 double phase_start,
	 double phase_step,
	 struct Geomnode * g);
void 
seqrothg(double angle_start,
	 double angle_step,
	 double phase_start,
	 double phase_step,
	 struct Geomnode * g);
int             sgrphasr(struct Group * group_ptr);







int 
main(int argc, char *argv[])
{

	struct charac  *first = NULL, *last = NULL;
	struct charac  *cp = NULL;
	struct Group   *group_ptr = NULL;
	int             error = 0;
	char            fn[UNLEN + 1];


	if (argc != 2) {
		printf("\n\tUSAGE: %s [<source-file-name>]\n\n", "space.exe");
		exit(1);
	}
	strcpy(GLOBLE_FILENAME, argv[1]);


	glvainit();


	nomefile(fn);
	if (readfil3(fn, &first, &last) != 0) {
		exit(1);
	};


	prnfile(first);


	error = groupdef(first, &cp, &group_ptr);
	waitcont();
	if (error != 0) {
		exit(1);
	};


	if (error = simgroup(group_ptr)) {

		exit(1);
	};


	if (error = simamp(group_ptr)) {

		exit(1);
	};


	if (error = simpha(group_ptr)) {

		exit(1);
	};


	simpol(group_ptr);

	waitcont();





	grwrite(group_ptr, fn);

}




int 
adddef(struct charac * p1, struct charac ** pp2,
       struct AddRem ** addrem_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

	struct Node    *no_ptr = NULL, **node_ptr = &no_ptr;


#ifdef DEBUG1
	printf("\nFunzione adddef()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[79], curr_ptr));
	if (error != 0) {
		*addrem_ptr = NULL;
		return 1;
	};






	error = nodedef(*curr_ptr, curr_ptr, node_ptr);


	if (error == 17) {
		*addrem_ptr = NULL;
		parserro(*curr_ptr, 58, " ");

		return 17;
	};


	if (error == 0) {

		*addrem_ptr = (struct AddRem *) malloc(sizeof(struct AddRem));

		if (*addrem_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("adddef()");
		};

		(*addrem_ptr)->ADDREM_TYPE = ADD_TYPE;
		(*addrem_ptr)->BLOCK_TYPE = NODE_BLOCK;
		(*addrem_ptr)->NODE_PTR = *node_ptr;
		(*addrem_ptr)->NEXT = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};







	error = blockdef(*curr_ptr, curr_ptr, node_ptr);

	if (error == 17) {
		*addrem_ptr = NULL;
		parserro(*curr_ptr, 59, " ");

		return 17;
	};


	if (error == 0) {

		*addrem_ptr = (struct AddRem *) malloc(sizeof(struct AddRem));

		if (*addrem_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("adddef()");
		};

		(*addrem_ptr)->ADDREM_TYPE = ADD_TYPE;
		(*addrem_ptr)->BLOCK_TYPE = BLOCK_BLOCK;
		(*addrem_ptr)->NODE_PTR = *node_ptr;
		(*addrem_ptr)->NEXT = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};





	error = polydef(*curr_ptr, curr_ptr, node_ptr);

	if (error == 17) {
		*addrem_ptr = NULL;
		parserro(*curr_ptr, 60, " ");

		return 17;
	};


	if (error == 0) {

		*addrem_ptr = (struct AddRem *) malloc(sizeof(struct AddRem));

		if (*addrem_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("adddef()");
		};

		(*addrem_ptr)->ADDREM_TYPE = ADD_TYPE;
		(*addrem_ptr)->BLOCK_TYPE = POLY_BLOCK;
		(*addrem_ptr)->NODE_PTR = *node_ptr;
		(*addrem_ptr)->NEXT = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};





	error = hexdef(*curr_ptr, curr_ptr, node_ptr);

	if (error == 17) {
		*addrem_ptr = NULL;
		parserro(*curr_ptr, 61, " ");

		return 17;
	};


	if (error == 0) {

		*addrem_ptr = (struct AddRem *) malloc(sizeof(struct AddRem));

		if (*addrem_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("adddef()");
		};

		(*addrem_ptr)->ADDREM_TYPE = ADD_TYPE;
		(*addrem_ptr)->BLOCK_TYPE = HEX_BLOCK;
		(*addrem_ptr)->NODE_PTR = *node_ptr;
		(*addrem_ptr)->NEXT = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};




	*addrem_ptr = NULL;
	parserro(*curr_ptr, 62, " ");

	return 17;
}




int 
addscan(struct Group * group_ptr)
{

	struct AddRem  *add_ptr = NULL;
	struct Geomnode *ap1_ptr = NULL, **app1_ptr = &ap1_ptr, *ap2_ptr = NULL, **app2_ptr = &ap2_ptr;
	struct Geomnode *primo_ptr = NULL, *ultimo_ptr = NULL;

	int             new_nodes = 0, *new_nodes_ptr = &new_nodes;
	int             pnum = 0, *pnum_ptr = &pnum, qnum = 0, *qnum_ptr = &qnum;


	int             elem_shape;
	double          elem_pdim, elem_qdim;

	double          pstep, qstep, angle, rot_angle;

	int             nodes_num, group_shape;
	double          group_psize, group_qsize, *group_psize_ptr = &group_psize, *group_qsize_ptr = &group_qsize;

#ifdef DEBUG1
	printf("\nFunzione addscan()");
#endif


	pstep = (group_ptr->GRID_PTR)->PSTEP;
	qstep = (group_ptr->GRID_PTR)->QSTEP;
	angle = (group_ptr->GRID_PTR)->ANGLE;
	rot_angle = (group_ptr->GRID_PTR)->ROT_ANGLE;
	elem_pdim = (group_ptr->ELEM_PTR)->PDIM;
	elem_qdim = (group_ptr->ELEM_PTR)->QDIM;

	if ((group_ptr->ELEM_PTR)->GEOMETRY == RECTANGULAR_ELEM) {
		elem_shape = RECTAN_SHAPE;
	} else {
		elem_shape = CIRCUL_SHAPE;
	};

	add_ptr = (group_ptr->ADDREM_PTR);


	nodes_num = 0;
	group_shape = NO_SHAPE;


	while (add_ptr != NULL) {

#ifdef DEBUG5
		printf("\nStruttura /p", add_ptr);
#endif


		if (add_ptr->ADDREM_TYPE == REM_TYPE) {

			group_shape = IRR_SHAPE;

			add_ptr = add_ptr->NEXT;

			continue;
		};


#ifdef DEBUG5
		printf("\nCreo blocco");
#endif

		switch (add_ptr->BLOCK_TYPE) {

		case NODE_BLOCK:
			mksnode(add_ptr, app1_ptr, app2_ptr, pstep, qstep, angle, new_nodes_ptr, elem_shape, elem_pdim, elem_qdim);

			if (group_shape == NO_SHAPE) {
				if ((group_ptr->ELEM_PTR)->GEOMETRY == RECTANGULAR_ELEM)
					group_shape = RECTAN_SHAPE;
				else
					group_shape = CIRCUL_SHAPE;
			} else
				group_shape = IRR_SHAPE;
			break;

		case BLOCK_BLOCK:
			mksblock(add_ptr, app1_ptr, app2_ptr, pstep, qstep, angle, new_nodes_ptr, elem_shape, elem_pdim, elem_qdim);

			if ((group_shape == NO_SHAPE) && (angle == 90)) {
				group_shape = RECTAN_SHAPE;
			} else
				group_shape = IRR_SHAPE;
			break;

		case POLY_BLOCK:
			printf("\nFunzione  mkpoly() non disponibile. ");
			
			exit(1);
			break;

		case HEX_BLOCK:
			if (elem_shape != CIRCUL_SHAPE) {
				printf("%s", ErrorMessages[7]);
				return 7;
			}
			mkshex(add_ptr, app1_ptr, app2_ptr, pstep, qstep, angle, new_nodes_ptr, elem_shape, elem_pdim, elem_qdim);
			if (group_shape == NO_SHAPE)
				group_shape = CIRCUL_SHAPE;
			else
				group_shape = IRR_SHAPE;
			break;

		default:
			interror("addscan() -1- ");
		};


		nodes_num = nodes_num + *new_nodes_ptr;

#ifdef DEBUG5
		printf("\n%d nodes created.", nodes_num);
#endif




		if (primo_ptr == NULL)
			primo_ptr = *app1_ptr;
		else
			ultimo_ptr->NEXT = *app1_ptr;


		ultimo_ptr = *app2_ptr;

		ultimo_ptr->NEXT = NULL;


		add_ptr = add_ptr->NEXT;

	};


	if (rot_angle != 0) {
		sgrrot(primo_ptr, 0, 0, rot_angle);
		group_shape = IRR_SHAPE;
	};



	if (group_shape == CIRCUL_SHAPE) {
		group_psize = 2 * pstep + 2 * (elem_pdim / 2);
		group_qsize = group_psize;
	} else if (group_shape == RECTAN_SHAPE) {

		extremes(primo_ptr, pnum_ptr, qnum_ptr);

		group_psize = ((*pnum_ptr) - 1) * (pstep) + (2 * (elem_pdim / 2));
		group_qsize = ((*qnum_ptr) - 1) * (qstep) + (2 * (elem_qdim / 2));
	} else {
		extsize(primo_ptr, elem_pdim, elem_qdim, group_psize_ptr, group_qsize_ptr);
	};


	group_ptr->NUM_OF_NODES = nodes_num;
	group_ptr->SHAPE = group_shape;
	group_ptr->PSIZE = group_psize;
	group_ptr->QSIZE = group_qsize;
	group_ptr->GEOMNODE_PTR = primo_ptr;

	return 0;
}













int 
adremdef(struct charac * p1, struct charac ** pp2,
	 struct AddRem ** addrem_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


#ifdef DEBUG1
	printf("\nFunzione adremdef()");
#endif


	*curr_ptr = p1;
	error = 0;




	*addrem_ptr = NULL;





	error = adddef(*curr_ptr, curr_ptr, addrem_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[62]);
		return 17;
	};


	if (error == 1) {
		error = remdef(*curr_ptr, curr_ptr, addrem_ptr);
	};

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[63]);
		return 17;
	};

	if (error == 1) {
		return 1;
	};
	return 17;
}



int 
ampunit(struct charac ** tp, int *unit_ptr)
{

	struct charac  *curr, **curr_ptr;

#ifdef DEBUG1
	printf("\nFunzione ampunit()");
#endif


	curr_ptr = &curr;
	*curr_ptr = *tp;


	*unit_ptr = 0;
	if (GetKeyword(Keywords[15], curr_ptr) == 0)
		*unit_ptr = LINEAR_UNIT;
	else if (GetKeyword(Keywords[63], curr_ptr) == 0)
		*unit_ptr = POWER_UNIT;
	else if (GetKeyword(Keywords[64], curr_ptr) == 0)
		*unit_ptr = DB_UNIT;
	else
		return 1;



	*tp = *curr_ptr;
	return 0;
}





int 
ampval(struct charac * p1, struct charac ** pp2,
       double *amp_ptr,
       int *amp_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione ampval()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetReal(amp_ptr, curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = ampunit(curr_ptr, amp_unit_ptr);
#ifdef DEBUG5
	printf("Unità di misura guadagno trovata amp_unit=%d\n", *amp_unit_ptr);
#endif
	if (error != 0)
		*amp_unit_ptr = LINEAR_UNIT;


	*pp2 = *curr_ptr;
	return 0;
}


int 
angclaus(struct charac * p1,
	 struct charac ** pp2, double *angle_ptr, int *angle_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;
	*angle_ptr = 0;
	*angle_unit_ptr = 0;


	error = (GetKeyword(Keywords[23], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetReal(angle_ptr, curr_ptr));
	if (error != 0) {
		*angle_ptr = 0;
		return 19;
	};



	error = angunit(*curr_ptr, curr_ptr, angle_unit_ptr);
	if (error != 0)
		*angle_unit_ptr = 0;


	*pp2 = *curr_ptr;
	return 0;
}












int 
angledir(struct charac * p1, struct charac ** pp2,
	 double *theta_ptr,
	 double *phi_ptr,
	 int *angle_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione angledir()\n");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[70], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetReal(theta_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 73, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[71], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 1, Keywords[71]);
		return 17;
	};


	error = angval(*curr_ptr, curr_ptr, phi_ptr, angle_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 73, " ");

		return 17;
	};

#ifdef DEBUG5
	printf("Unità di misura trovata angle_unit=%d\n", *angle_unit_ptr);
#endif


	*pp2 = *curr_ptr;
	return 0;
}





int 
anglerot(struct charac * p1, struct charac ** pp2,
	 double *angle_start_ptr,
	 double *angle_step_ptr,
	 int *angle_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione anglerot()");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[23], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[95], curr_ptr));
	error = 0;


	error = (GetReal(angle_start_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 75, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[16], curr_ptr));
	error = 0;


	error = angval(*curr_ptr, curr_ptr, angle_step_ptr, angle_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 75, " ");

		return 17;
	};

#ifdef DEBUG5
	printf("Unità di misura trovata *angle_unit_ptr=%d\n", *angle_unit_ptr);
#endif


	*pp2 = *curr_ptr;
	return 0;
}




int 
angstep(struct charac * p1,
	struct charac ** pp2, double *angle_ptr, int *angle_unit_ptr,
	double *pstep_ptr, double *qstep_ptr, int *pqstep_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error1, error2;


	*curr_ptr = p1;


	error1 = 0;
	error2 = 0;
	*angle_ptr = 0;
	*angle_unit_ptr = 0;
	*pstep_ptr = 0;
	*qstep_ptr = 0;
	*pqstep_unit_ptr = 0;




	error1 = angclaus(*curr_ptr, curr_ptr, angle_ptr, angle_unit_ptr);
	if (error1 == 1) {
		*angle_ptr = 0;
		*angle_unit_ptr = 0;
	};


	error2 = doubstep(*curr_ptr, curr_ptr, pstep_ptr, qstep_ptr, pqstep_unit_ptr);


	if ((error1 == 1) && (error2 == 1)) {
		*angle_ptr = 0;
		*angle_unit_ptr = 0;
		*pstep_ptr = 0;
		*qstep_ptr = 0;
		*pqstep_unit_ptr = 0;
		return 1;
	};


	if (error2 != 0) {
		parserro(*curr_ptr, error2, " ");

		*angle_ptr = 0;
		*angle_unit_ptr = 0;
		*pstep_ptr = 0;
		*qstep_ptr = 0;
		*pqstep_unit_ptr = 0;
		return 17;
	};




	*pp2 = *curr_ptr;
	return 0;
}



int 
angunit(struct charac * p1,
	struct charac ** pp2, int *angle_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;

#ifdef DEBUG1
	printf("\nFunzione angunit()");
#endif


	*curr_ptr = p1;



	*angle_unit_ptr = 0;
	if (GetKeyword(Keywords[51], curr_ptr) == 0)
		*angle_unit_ptr = 0;
	else if (GetKeyword(Keywords[52], curr_ptr) == 0)
		*angle_unit_ptr = 1;
	else
		return 20;


	*pp2 = *curr_ptr;
	return 0;
}





int 
angval(struct charac * p1, struct charac ** pp2,
       double *angle_ptr,
       int *angle_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;
	error = 0;


	error = (GetReal(angle_ptr, curr_ptr));
	if (error != 0) {
		return 1;
	};



	error = angunit(*curr_ptr, curr_ptr, angle_unit_ptr);
#ifdef DEBUG5
	printf("Unità di misura angolo trovata *angle_unit_ptr=%d\n", *angle_unit_ptr);
#endif
	if (error != 0)
		*angle_unit_ptr = DEGREES;


	*pp2 = *curr_ptr;
	return 0;
}

int 
answer(char domanda[EMSGSLEN])
{

	char            s[2];

	printf("\n%s (Y/N): ", domanda);
	gets(s);
	if ((s[0] == 'N') || (s[0] == 'n'))
		return 0;
	else
		return 1;
}



int 
beampha(struct charac * p1, struct charac ** pp2,
	double *u_ptr,
	double *v_ptr,
	double *theta_ptr,
	double *phi_ptr,
	int *angle_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione beampha()\n");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[96], curr_ptr));
	error = 0;


	error = uvdir(*curr_ptr, curr_ptr, u_ptr, v_ptr);


	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[73]);
		return 17;
	};


	if (error == 1) {
		error = angledir(*curr_ptr, curr_ptr, theta_ptr, phi_ptr, angle_unit_ptr);
	};

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[73]);
		return 17;
	};


	parserro(*curr_ptr, 73, " ");

	return 17;
}











int 
blockdef(struct charac * p1, struct charac ** pp2,
	 struct Node ** node_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;




	int             pcoord, *pcoord_ptr = &pcoord;
	int             qcoord, *qcoord_ptr = &qcoord;


	struct Node    *primo_ptr = NULL, *ultimo_ptr = NULL, *app_ptr = NULL;


#ifdef DEBUG1
	printf("\nFunzione blockdef()");
#endif


	*curr_ptr = p1;
	error = 0;

	*node_ptr = NULL;


	error = (GetKeyword(Keywords[76], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = nodecoor(*curr_ptr, curr_ptr, pcoord_ptr, qcoord_ptr);

	if (error == 1) {
		parserro(*curr_ptr, 56, " ");

		return 17;
	};

	if (error == 17) {
		parserro(*curr_ptr, 56, " ");

		return 17;
	};


	app_ptr = (struct Node *) malloc(sizeof(struct Node));

	if (app_ptr == NULL) {
		parserro(*curr_ptr, 55, " ");

		interror("blockdef()");
		exit(1);
	};



	app_ptr->PCOORD = pcoord;
	app_ptr->QCOORD = qcoord;
	app_ptr->THEA = 0;
	app_ptr->PHEA = 0;
	app_ptr->PSEA = 0;
	app_ptr->ANGLE_UNIT = 0;
	app_ptr->NEXT = NULL;


	primo_ptr = app_ptr;
	ultimo_ptr = app_ptr;


	error = nodecoor(*curr_ptr, curr_ptr, pcoord_ptr, qcoord_ptr);

	if (error == 1) {
		parserro(*curr_ptr, 56, " ");

		return 17;
	};

	if (error == 17) {
		parserro(*curr_ptr, 57, " ");

		return 17;
	};


	app_ptr = (struct Node *) malloc(sizeof(struct Node));

	if (app_ptr == NULL) {
		parserro(*curr_ptr, 55, " ");

		interror("blockdef()");
		exit(1);
	};


	app_ptr->PCOORD = pcoord;
	app_ptr->QCOORD = qcoord;
	app_ptr->THEA = 0;
	app_ptr->PHEA = 0;
	app_ptr->PSEA = 0;
	app_ptr->ANGLE_UNIT = 0;
	app_ptr->NEXT = NULL;


	ultimo_ptr->NEXT = app_ptr;
	ultimo_ptr = app_ptr;


	*node_ptr = primo_ptr;


	*pp2 = *curr_ptr;
	return 0;
}




int 
circpol(struct charac * p1, struct charac ** pp2,
	int *direction_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error = 0;

#ifdef DEBUG1
	printf("\nFunzione circpol()");
#endif


	*curr_ptr = p1;
	*direction_ptr = DEF_CIRC_POL_DIR;


	error = (GetKeyword(Keywords[38], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[65], curr_ptr));
	if (error == 0) {
		*direction_ptr = LEFT_HAND;
		*pp2 = *curr_ptr;
		return 0;
	};


	error = (GetKeyword(Keywords[66], curr_ptr));
	if (error == 0) {
		*direction_ptr = RIGHT_HAND;
		*pp2 = *curr_ptr;
		return 0;
	};


	*pp2 = *curr_ptr;
	return 0;
}



int 
circspec(struct charac * p1, struct charac ** pp2,
	 double *radius_ptr,
	 int *radius_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error = 0;


	*curr_ptr = p1;


	error = (GetKeyword(Keywords[38], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[5], curr_ptr));
	error = 0;


	error = (Get1Real(*curr_ptr, curr_ptr, radius_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		return 17;
	};


	error = lenunit(curr_ptr, radius_unit_ptr);
#ifdef DEBUG5
	printf("Unità di misura trovata %d", *radius_unit_ptr);
#endif
	if (error != 0)
		*radius_unit_ptr = 0;


	*pp2 = *curr_ptr;
	return 0;
}






int 
cosinus(struct charac * p1, struct charac ** pp2,
	double *exp_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error = 0;


	*curr_ptr = p1;


	error = (GetKeyword(Keywords[34], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetReal(exp_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 26, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}




double 
dacos(double x)
{

	return acos(x) * RTD;

}




double 
dasin(double x)
{

	return asin(x) * RTD;

}




double 
datan(double x)
{

	return atan(x) * RTD;

}



double 
dcos(double x)
{

	return cos(x * DTR);

}

int 
degconv(double *x_ptr, int unit)
{

	switch (unit) {

		case DEGREES:
		break;

	case RADIANTS:
		*x_ptr *= RTD;
		break;

	default:
		interror("degconv");

	}

	return 0;
}



double 
doubmax(double a, double b)
{

	if (a > b)
		return a;
	else
		return b;

}



double 
doubmin(double a, double b)
{

	if (a < b)
		return a;
	else
		return b;

}




int 
doubstep(struct charac * p1,
	 struct charac ** pp2, double *pstep_ptr, double *qstep_ptr, int *pqstep_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;
	*pstep_ptr = 0;
	*qstep_ptr = 0;
	*pqstep_unit_ptr = 0;


	error = (GetKeyword(Keywords[19], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetUReal(*curr_ptr, curr_ptr, pstep_ptr));
	if (error != 0) {
		*pstep_ptr = 0;
		*qstep_ptr = 0;
		*pqstep_unit_ptr = 0;
		return error;
	};


	error = (GetKeyword(Keywords[20], curr_ptr));
	if (error != 0) {
		return 17;
	};


	error = (GetUReal(*curr_ptr, curr_ptr, qstep_ptr));
	if (error != 0) {
		*pstep_ptr = 0;
		*qstep_ptr = 0;
		*pqstep_unit_ptr = 0;
		return error;
	};



	error = lenunit(curr_ptr, pqstep_unit_ptr);
	if (error != 0)
		*pqstep_unit_ptr = 0;


	*pp2 = *curr_ptr;
	return 0;
}




double 
dsin(double x)
{

	return sin(x * DTR);

}




int 
elemdef(struct charac * p1, struct charac ** pp2,
	struct Elem ** elem_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;
	char            name[UNLEN + 1];

#ifdef DEBUG1
	printf("\nFunzione elemdef()");
#endif


	*curr_ptr = p1;
	error = 0;



	*elem_ptr = (struct Elem *) malloc(sizeof(struct Elem));

	if (*elem_ptr == NULL) {
		parserro(*curr_ptr, 55, " ");
		interror("elemdef()");
	};


	error = (GetKeyword(Keywords[28], curr_ptr));
	if (error != 0) {
		*elem_ptr = NULL;
		return 1;
	};


	error = GetUName(name, curr_ptr);

	if (error == 0) {
		strcpy((*elem_ptr)->NAME, name);
		(*elem_ptr)->TYPE = SUBGROUP;
		*pp2 = *curr_ptr;
		return 0;
	};


	error = sinelem(*curr_ptr, curr_ptr, elem_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};


	*elem_ptr == NULL;
	return 17;
}





void 
emsginit(void)
{

	int             i;

#ifdef DEBUG4
	printf("\nFUNZIONE emsgsinit INIZIALIZZAZIONE messaggi d'errore\n");
#endif

	for (i = 0; (i <= EMSGSNUM); i++)
		ErrorMessages[i][0] = '\0';

	strcpy(ErrorMessages[0], "** ERROR 0: NO ERRORS");
	strcpy(ErrorMessages[1], "** ERROR 1: Keyword expected: ");
	strcpy(ErrorMessages[2], "** ERROR 2: User name expected: ");
	strcpy(ErrorMessages[3], "** ERROR 3: User name already used: ");
	strcpy(ErrorMessages[4], "** ERROR 4: Unexpected end of file.");
	strcpy(ErrorMessages[5], "** ERROR 5: Illegal user name: ");
	strcpy(ErrorMessages[6], "** ERROR 6: Keywords may not be used as user names: ");
	strcpy(ErrorMessages[7], "** WARNING 7: User name too long: ");
	strcpy(ErrorMessages[8], "** ERROR 8: Missing array name declaration.");
	strcpy(ErrorMessages[9], "** ERROR 9: Missing array surface definition.");
	strcpy(ErrorMessages[10], "** ERROR 10: Incorrect array surface definition.");
	strcpy(ErrorMessages[11], "** ERROR 11: No valid real value found.");
	strcpy(ErrorMessages[12], "** ERROR 12: Negative value found: ");
	strcpy(ErrorMessages[13], "** ERROR 13: Real not found.");
	strcpy(ErrorMessages[14], "** ERROR 14: No length unit found, millimetres assumed.");
	strcpy(ErrorMessages[15], "** ERROR 15: Incorrect cylinder AXIS declaration. Axis must be X or Y.");
	strcpy(ErrorMessages[16], "** ERROR 16: Incorrect GRID definition.");
	strcpy(ErrorMessages[17], "** ERROR 17: Incorrect definition.");
	strcpy(ErrorMessages[18], "** ERROR 18: Missing double step definition.");
	strcpy(ErrorMessages[19], "** ERROR 19: No valid angle value found.");
	strcpy(ErrorMessages[20], "** ERROR 20: No angle unit found, degrees assumed.");
	strcpy(ErrorMessages[21], "** ERROR 21: Can not open input file:");
	strcpy(ErrorMessages[22], "** ERROR 22: Out of memory during input file reading.");
	strcpy(ErrorMessages[23], "** ERROR 23: Empty input file:");
	strcpy(ErrorMessages[24], "** ERROR 24: Incorrect ELEMENT definition.");
	strcpy(ErrorMessages[25], "** ERROR 25: Incorrect RECTANGULAR element definition.");
	strcpy(ErrorMessages[26], "** ERROR 26: Missing COSINUS parameter.");
	strcpy(ErrorMessages[27], "** ERROR 27: No valid amplitude value found.");
	strcpy(ErrorMessages[28], "** ERROR 28: Incorrect CIRCULAR element definition ");
	strcpy(ErrorMessages[29], "** ERROR 29: Missing GAUSSIAN parameter(s).");
	strcpy(ErrorMessages[30], "** ERROR 30: Incorrect element ORIENTATION definition.");
	strcpy(ErrorMessages[31], "** ERROR 31: Incorrect element LINEAR polarization definition.");
	strcpy(ErrorMessages[32], "** ERROR 32: Incorrect element CIRCULAR polarization definition.");
	strcpy(ErrorMessages[33], "** ERROR 33: No angle unit found, dB assumed.");
	strcpy(ErrorMessages[34], "** ERROR 34: Incorrect element GEOMETRY definition.");
	strcpy(ErrorMessages[35], "** ERROR 35: Incorrect element MODEL definition.");
	strcpy(ErrorMessages[36], "** ERROR 36: Incorrect element POLARIZATION definition.");
	strcpy(ErrorMessages[37], "** ERROR 37: No phase value found.");
	strcpy(ErrorMessages[38], "** ERROR 38: Incorrect element POL_ORIENT definition.");
	strcpy(ErrorMessages[39], "** ERROR 39: Incorrect element PORT AMPLITUDE definition.");
	strcpy(ErrorMessages[40], "** ERROR 40: Incorrect element PORT phase definition.");
	strcpy(ErrorMessages[41], "** ERROR 41: Natural number expected.");
	strcpy(ErrorMessages[42], "** ERROR 42: Integer not found.");
	strcpy(ErrorMessages[43], "** ERROR 43: Number too long: ");
	strcpy(ErrorMessages[44], "** ERROR 44: Natural not found.");
	strcpy(ErrorMessages[45], "** ERROR 45: Null value found.");
	strcpy(ErrorMessages[46], "** ERROR 46: Too few PORT definitions.");
	strcpy(ErrorMessages[47], "** ERROR 47: Incorrect definition at PORT: ");
	strcpy(ErrorMessages[48], "** ERROR 48: Incorrect element GAIN definition.");
	strcpy(ErrorMessages[49], "** ERROR 49: Incorrect element PORTS definition.");
	strcpy(ErrorMessages[50], "** ERROR 50: Missing P coordinate.");
	strcpy(ErrorMessages[51], "** ERROR 51: Missing Q coordinate.");
	strcpy(ErrorMessages[52], "** ERROR 52: Missing NODE coordinates.");
	strcpy(ErrorMessages[53], "** ERROR 53: Incorrect NODE coordinates definition.");
	strcpy(ErrorMessages[54], "** ERROR 54: Incorrect NODE ORIENTATION definition.");
	strcpy(ErrorMessages[55], "** ERROR 55: Out of memory during input data reading.");
	strcpy(ErrorMessages[56], "** ERROR 56: Missing BLOCK coordinate(s).");
	strcpy(ErrorMessages[57], "** ERROR 57: Incorrect BLOCK coordinates definition.");
	strcpy(ErrorMessages[58], "** ERROR 58: Incorrect NODE definition.");
	strcpy(ErrorMessages[59], "** ERROR 59: Incorrect BLOCK definition.");
	strcpy(ErrorMessages[60], "** ERROR 60: Incorrect POLYGON definition.");
	strcpy(ErrorMessages[61], "** ERROR 61: Incorrect HEXAGON definition.");
	strcpy(ErrorMessages[62], "** ERROR 62: Incorrect ADD statement.");
	strcpy(ErrorMessages[63], "** ERROR 63: Incorrect REMOVE statement.");
	strcpy(ErrorMessages[64], "** ERROR 64: Incorrect ADD or REMOVE statement.");
	strcpy(ErrorMessages[65], "** ERROR 65: 3 or more node coordinates expected.");
	strcpy(ErrorMessages[66], "** ERROR 66: Incorrect or missing node coordinates.");
	strcpy(ErrorMessages[67], "** ERROR 67: Missing ELEMENT definition after GRID definition.");
	strcpy(ErrorMessages[68], "** ERROR 68: Missing SECOND ORDER parameter(s).");
	strcpy(ErrorMessages[69], "** ERROR 69: Incorrect SECOND ORDER amplitude definition.");
	strcpy(ErrorMessages[70], "** ERROR 70: Incorrect GROUP EXCITATION AMPLITUDE definition.");
	strcpy(ErrorMessages[71], "** ERROR 71: Missing POINTING parameter(s).");
	strcpy(ErrorMessages[72], "** ERROR 72: U or V value out of range [-1,1].");
	strcpy(ErrorMessages[73], "** ERROR 73: Incorrect POINTING direction definition.");
	strcpy(ErrorMessages[74], "** ERROR 74: Incorrect PHASE parameters.");
	strcpy(ErrorMessages[75], "** ERROR 75: Incorrect ANGLE parameters.");
	strcpy(ErrorMessages[76], "** ERROR 76: Incorrect SECOND ORDER phase definition.");
	strcpy(ErrorMessages[77], "** ERROR 77: Incorrect ROTATION SEQUENTIAL phase definition.");
	strcpy(ErrorMessages[78], "** ERROR 78: Incorrect POINTING phase definition.");
	strcpy(ErrorMessages[79], "** ERROR 79: Incorrect AMPLITUDE excitation definition.");
	strcpy(ErrorMessages[80], "** ERROR 80: Incorrect PHASE excitation definition.");
	strcpy(ErrorMessages[81], "** ERROR 81: Missing GROUP name.");
	strcpy(ErrorMessages[82], "** ERROR 82: Incorrect GROUP definition, group: ");
	strcpy(ErrorMessages[83], "** ERROR 83: Missing coordinate(s) in: ADD_GROUP ");
	strcpy(ErrorMessages[84], "** ERROR 84: Missing DISPLACE coordinate(s).");
	strcpy(ErrorMessages[85], "** ERROR 85: Missing MOVE coordinate(s).");
	strcpy(ErrorMessages[86], "** ERROR 86: Missing coordinate(s) in: PLACE_GROUP ");
	strcpy(ErrorMessages[87], "** ERROR 87: Missing GROUP definition in array: ");
	strcpy(ErrorMessages[88], "** ERROR 88: Incorrect GROUP definition in array:");
	strcpy(ErrorMessages[89], "** ERROR 89: Incorrect DISPLACE_AT statement.");
	strcpy(ErrorMessages[90], "** ERROR 90: Incorrect MOVE_TO statement.");
	strcpy(ErrorMessages[91], "** ERROR 91: Missing ADD_GROUP or PLACE_GROUP statement.");
	strcpy(ErrorMessages[92], "** ERROR 92: Incorrect ADD_GROUP or PLACE_GROUP statement.");
	strcpy(ErrorMessages[93], "** ERROR 93: Incorrect PORTS number.");
	strcpy(ErrorMessages[94], "** ERROR 94: At least one ADD statement is required.");
	strcpy(ErrorMessages[95], "** ERROR 95: ");
	strcpy(ErrorMessages[96], "** ERROR 96: ");
	strcpy(ErrorMessages[97], "** ERROR 97: ");
	strcpy(ErrorMessages[98], "** ERROR 98: ");
	strcpy(ErrorMessages[99], "** ERROR 99: ");

#ifdef DEBUG5
	for (i = 0; ((i <= EMSGSNUM) && (ErrorMessages[i][0])); i++)
		printf("%d - %s\n", i, ErrorMessages[i]);
	waitcont();
#endif

}



int 
EndOfTape(struct charac ** p)
{

	if ((*p) == NULL) {
		return 1;
	};
	return 0;
}




int 
extmod(struct charac * p1, struct charac ** pp2)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error = 0;


	*curr_ptr = p1;


	error = (GetKeyword(Keywords[33], curr_ptr));
	if (error != 0) {
		return 1;
	};


	*pp2 = *curr_ptr;
	return 0;
}



int 
extremes(struct Geomnode * gnode_ptr,
	 int *pnum_ptr,
	 int *qnum_ptr
)
{

	struct Geomnode *app_ptr;
	int             pmin = INT_MAX, pmax = INT_MIN, qmin = INT_MAX, qmax = INT_MIN;

#ifdef DEBUG1
	printf("\nFunzione extremes()");
#endif

	app_ptr = gnode_ptr;

	while (app_ptr != NULL) {

		if ((app_ptr->LOC_P) < pmin)
			pmin = app_ptr->LOC_P;
		if ((app_ptr->LOC_P) > pmax)
			pmax = app_ptr->LOC_P;
		if ((app_ptr->LOC_Q) < qmin)
			qmin = app_ptr->LOC_Q;
		if ((app_ptr->LOC_Q) > qmax)
			qmax = app_ptr->LOC_Q;

		app_ptr = app_ptr->NEXT;
	};

	*pnum_ptr = pmax - pmin + 1;
	*qnum_ptr = qmax - qmin + 1;
	return 0;
}




int 
extsize(struct Geomnode * gnode_ptr,
	double elem_pdim,
	double elem_qdim,
	double *xsize_ptr,
	double *ysize_ptr
)
{

	struct Geomnode *app_ptr;
	double          xmin = DBL_MAX, xmax = DBL_MIN, ymin = DBL_MAX, ymax = DBL_MIN;

#ifdef DEBUG1
	printf("\nFunzione extsize()");
#endif

	app_ptr = gnode_ptr;

	while (app_ptr != NULL) {

		if ((app_ptr->XA) < xmin)
			xmin = app_ptr->XA;
		if ((app_ptr->XA) > xmax)
			xmax = app_ptr->XA;
		if ((app_ptr->YA) < ymin)
			ymin = app_ptr->YA;
		if ((app_ptr->YA) > ymax)
			ymax = app_ptr->YA;

		app_ptr = app_ptr->NEXT;
	};



	*xsize_ptr = (xmax - xmin) + (2 * (elem_pdim / 2));
	*ysize_ptr = (ymax - ymin) + (2 * (elem_qdim / 2));
	return 0;
}




int 
fixgramp(struct GrAmpExc * g)
{

	double          unif_val, *unif_val_ptr = &unif_val;
	double          centre, *centre_ptr = &centre;
	double          p1_et, *p1_et_ptr = &p1_et;
	double          p2_et, *p2_et_ptr = &p2_et;
	double          q1_et, *q1_et_ptr = &q1_et;
	double          q2_et, *q2_et_ptr = &q2_et;
	int             amp_unit;


	unif_val = g->UNIF_VAL;
	centre = g->CENTRE;
	p1_et = g->P1_ET;
	p2_et = g->P2_ET;
	q1_et = g->Q1_ET;
	q2_et = g->Q2_ET;
	amp_unit = g->AMP_UNIT;


	switch (g->TYPE) {

	case UNIFORM_LAW:

		if (unif_val == 0)
			unif_val = DEF_AMPLITUDE_UNIFORM_VALUE;


		linconv(unif_val_ptr, amp_unit);
		break;

	case SECOND_ORDER_LAW:
		linconv(centre_ptr, amp_unit);
		linconv(p1_et_ptr, amp_unit);
		linconv(p2_et_ptr, amp_unit);
		linconv(q1_et_ptr, amp_unit);
		linconv(q2_et_ptr, amp_unit);
		break;

	case PQEXPRESSION:
		printf("\nPQEXPR: not available");
		exit(1);
		interror("fixgramp()");

		break;

	default:
		interror("\nfixgramp(): group amplitude type not correct");

	}


	g->UNIF_VAL = unif_val;
	g->CENTRE = centre;
	g->P1_ET = p1_et;
	g->P2_ET = p2_et;
	g->Q1_ET = q1_et;
	g->Q2_ET = q2_et;
	g->AMP_UNIT = LINEAR_UNIT;
	return 0;
}





int 
fixgrpha(struct GrPhaExc * g)
{

#ifdef DEBUG1
	printf("\nFunzione fixgrpha()");
#endif

	switch (g->TYPE) {

	case UNIFORM_LAW:

		if (g->UNIF_VAL == 0)
			g->UNIF_VAL = DEF_PHASE_UNIFORM_VALUE;

		degconv(&(g->UNIF_VAL), g->PHASE_UNIT);






		break;

	case SECOND_ORDER_LAW:
		degconv(&(g->CENTRE), g->PHASE_UNIT);
		degconv(&(g->P1_EP), g->PHASE_UNIT);
		degconv(&(g->P2_EP), g->PHASE_UNIT);
		degconv(&(g->Q1_EP), g->PHASE_UNIT);
		degconv(&(g->Q2_EP), g->PHASE_UNIT);
		break;

	case PQEXPRESSION:
		printf("\nPQEXPR: not available");
		exit(1);
		interror("fixgrpha()");

		break;

	case ROTATION_SEQUENTIAL_LAW:
		if (g->OMIT_ANGLE == NO) {
			degconv(&(g->ANGLE_START), g->ANGLE_UNIT);
			degconv(&(g->ANGLE_STEP), g->ANGLE_UNIT);
		};
		if (g->OMIT_PHASE == NO) {
			degconv(&(g->PHASE_START), g->PHASE_UNIT);
			degconv(&(g->PHASE_STEP), g->PHASE_UNIT);
		};
		break;

	case BEAM_POINTING_LAW:
		degconv(&(g->THETA), g->ANGLE_UNIT);
		degconv(&(g->PHI), g->ANGLE_UNIT);
		break;

	default:
		interror("\nfixgrpha(): GROUP_EXCITATION phase law type not correct");

	}



	g->PHASE_UNIT = DEGREES;

	return 0;
}




int 
fixnodor(struct Node * node_ptr)
{


	if ((node_ptr->OMIT_ORIENT) == YES) {
		node_ptr->THEA = 0;
		node_ptr->PHEA = 0;
		node_ptr->PSEA = 0;
		node_ptr->ANGLE_UNIT = DEGREES;
	} else if ((node_ptr->ANGLE_UNIT) == RADIANTS) {
		node_ptr->THEA *= RTD;
		node_ptr->PHEA *= RTD;
		node_ptr->PSEA *= RTD;
		node_ptr->ANGLE_UNIT = DEGREES;
	}
	return 0;
}




int 
fixport(struct Elem * elem_ptr)
{

	struct Port    *app_ptr;
	double          amp, *amp_ptr = &amp;
	double          ang, *ang_ptr = &ang;

	app_ptr = elem_ptr->PORT_PTR;


	while (app_ptr != NULL) {
		amp = app_ptr->AMP;
		linconv(amp_ptr, app_ptr->AMP_UNIT);
		app_ptr->AMP = amp;

		ang = app_ptr->PSC;
		degconv(ang_ptr, app_ptr->PSC_UNIT);
		app_ptr->PSC = ang;

		ang = app_ptr->PSH;
		degconv(ang_ptr, app_ptr->PSH_UNIT);
		app_ptr->PSH = ang;

		ang = app_ptr->PPA;
		degconv(ang_ptr, app_ptr->PPA_UNIT);
		app_ptr->PPA = ang;


		if (elem_ptr->POLARIZATION == LIN_POL) {
			if (app_ptr->OMIT_POL == YES) {
				app_ptr->PHEPOL = elem_ptr->PHEPOL;
				app_ptr->PHEPOL_UNIT = elem_ptr->PHEPOL_UNIT;
			};

			ang = app_ptr->PHEPOL;
			degconv(ang_ptr, app_ptr->PHEPOL_UNIT);
			app_ptr->PHEPOL = ang;
		};


		app_ptr->AMP_UNIT = LINEAR_UNIT;
		app_ptr->PSC_UNIT = DEGREES;
		app_ptr->PSH_UNIT = DEGREES;
		app_ptr->PPA_UNIT = DEGREES;
		app_ptr->PHEPOL_UNIT = DEGREES;

		app_ptr = app_ptr->NEXT;

	};

	return 0;
}



int 
fixselem(struct Elem * elem_ptr)
{


	double          radius, *radius_ptr = &radius;
	int             radius_unit;
	double          pdim, *pdim_ptr = &pdim;
	double          qdim, *qdim_ptr = &qdim;
	int             pqdim_unit;

	struct Port    *port_ptr = NULL;


#ifdef DEBUG1
	printf("\nFunzione fixselem()");
#endif





	radius = elem_ptr->RADIUS;
	radius_unit = elem_ptr->RADIUS_UNIT;
	pdim = elem_ptr->PDIM;
	qdim = elem_ptr->QDIM;
	pqdim_unit = elem_ptr->PQDIM_UNIT;

	switch (elem_ptr->GEOMETRY) {

	case RECTANGULAR_ELEM:
		mmconv(pdim_ptr, pqdim_unit);
		mmconv(qdim_ptr, pqdim_unit);
		radius = sqrt(pdim * pdim + qdim * qdim);
		break;

	case CIRCULAR_ELEM:
		mmconv(radius_ptr, radius_unit);
		pdim = 2 * radius;
		qdim = 2 * radius;
		break;

	default:

		interror("fixselem - 1");
	};

	radius_unit = DEGREES;
	pqdim_unit = MM_UNIT;




	if (elem_ptr->NPORTS == 0) {


		port_ptr = (struct Port *) malloc(sizeof(struct Port));

		if (port_ptr == NULL) {
			printf("%s", ErrorMessages[55]);
			interror("fixselem() -2- ");
		};


		port_ptr->PORTNUM = 1;
		port_ptr->AMP = 1;
		port_ptr->AMP_UNIT = LINEAR_UNIT;
		port_ptr->PSC = 0;
		port_ptr->PSC_UNIT = DEGREES;
		port_ptr->PSH = 0;
		port_ptr->PSH_UNIT = DEGREES;
		port_ptr->PPA = 0;
		port_ptr->PPA_UNIT = DEGREES;
		port_ptr->PHEPOL = DEF_ELEM_LIN_POL_ANGLE;
		port_ptr->PHEPOL_UNIT = DEF_ELEM_LIN_POL_ANGLE_UNIT;

		port_ptr->OMIT_POL = YES;

		port_ptr->NEXT = NULL;

		elem_ptr->NPORTS = 1;
		elem_ptr->PORT_PTR = port_ptr;
	};


	fixport(elem_ptr);


	elem_ptr->RADIUS = radius;
	elem_ptr->RADIUS_UNIT = radius_unit;
	elem_ptr->PDIM = pdim;
	elem_ptr->QDIM = qdim;
	elem_ptr->PQDIM_UNIT = pqdim_unit;

	return 0;
}




int 
fixsgrel(struct Group * group_ptr)
{


	struct Grid    *grid;
	struct Elem    *elem;
	double          pstep, qstep, angle, rot_angle;
	double          radius, pdim, qdim;
	double          px, py, qx, qy;

#ifdef DEBUG1
	printf("\nFunzione fixsgrel()");
#endif







	grid = group_ptr->GRID_PTR;
	elem = group_ptr->ELEM_PTR;


	fixselem(elem);

	fixsgrid(grid);



	grid->ROT_ANGLE = 0;

	switch (grid->TYPE) {

	case SQU_GRID:

		if ((grid->PSTEP) == 0) {



			if ((group_ptr->GRPHAEXC_PTR)->TYPE == ROTATION_SEQUENTIAL_LAW) {

				grid->PSTEP = elem->RADIUS;
				grid->QSTEP = elem->RADIUS;
			} else {


				grid->PSTEP = doubmax(elem->PDIM, elem->QDIM);
				grid->QSTEP = doubmax(elem->PDIM, elem->QDIM);


			}

		} else {


			if ((group_ptr->GRPHAEXC_PTR)->TYPE == ROTATION_SEQUENTIAL_LAW) {
				if (grid->PSTEP < elem->RADIUS)
					printf("\n%s%s", MOSErrors[12], group_ptr->NAME);
			} else {


				if (grid->PSTEP < doubmax(elem->PDIM, elem->QDIM))
					printf("\n%s%s", MOSErrors[2], group_ptr->NAME);

			}

		}
		grid->ANGLE = 90;
		break;

	case REC_GRID:

		if ((grid->PSTEP) == 0) {



			if ((group_ptr->GRPHAEXC_PTR)->TYPE == ROTATION_SEQUENTIAL_LAW) {

				grid->PSTEP = elem->RADIUS;
				grid->QSTEP = elem->RADIUS;
			} else {


				grid->PSTEP = elem->PDIM;
				grid->QSTEP = elem->QDIM;

			}



		} else {



			if ((group_ptr->GRPHAEXC_PTR)->TYPE == ROTATION_SEQUENTIAL_LAW) {
				if (grid->PSTEP < elem->RADIUS)
					printf("\n%s%s", MOSErrors[12], group_ptr->NAME);
			} else {

				if ((grid->PSTEP < elem->PDIM) || (grid->QSTEP < elem->QDIM))
					printf("\n%s%s", MOSErrors[2], group_ptr->NAME);
			}

		}

		grid->ANGLE = 90;
		break;

	case HEX_GRID:

		if (elem->GEOMETRY != CIRCULAR_ELEM) {
			printf("\n%s%s", MOSErrors[3], group_ptr->NAME);
			exit(1);
		};

		if ((grid->PSTEP) == 0) {
			grid->PSTEP = elem->PDIM;
			grid->QSTEP = elem->PDIM;
		} else if (grid->PSTEP < elem->PDIM) {
			printf("\n%s%s", MOSErrors[2], group_ptr->NAME);
		};
		grid->ANGLE = 60;
		break;

	case TRI_GRID:

		if ((grid->PX) != 0) {



			px = grid->PX;
			py = grid->PY;
			qx = grid->QX;
			qy = grid->QY;

			pstep = sqrt((px * px) + (py * py));
			qstep = sqrt((qx * qx) + (qy * qy));

			angle = dacos(((px * qx) + (py * qy)) / (pstep * qstep));

			rot_angle = dacos(px / pstep);

			grid->PSTEP = pstep;
			grid->QSTEP = qstep;
			grid->ANGLE = angle;
			grid->ROT_ANGLE = rot_angle;
		} else if (((grid->PSTEP) == 0) && ((grid->QSTEP) == 0) && ((grid->ANGLE) == 0)) {

			switch (elem->GEOMETRY) {

			case CIRCULAR_ELEM:

				radius = elem->PDIM / 2;

				angle = DEF_TRI_GRID_CIRC_EL_ANGLE;

				pstep = 2 * radius;
				if ((angle > 45) && (angle <= 60))
					qstep = 2 * radius * 2 * dcos(angle);

				else if ((angle > 60) && (angle <= 90))
					qstep = 2 * radius;
				else
					interror("fixsgrel - 3");


				grid->PSTEP = pstep;
				grid->QSTEP = qstep;
				grid->ANGLE = angle;
				break;

			case RECTANGULAR_ELEM:

				pdim = elem->PDIM;
				qdim = elem->QDIM;



				angle = datan((2 * qdim) / (pdim));





				pstep = pdim;
				qstep = (qdim) / (dsin(angle));


				grid->PSTEP = pstep;
				grid->QSTEP = qstep;
				grid->ANGLE = angle;
			};
		} else if (((grid->PSTEP) != 0) && ((grid->QSTEP) != 0) && ((grid->ANGLE) == 0)) {

			if (elem->GEOMETRY == CIRCULAR_ELEM) {

				pstep = grid->PSTEP;
				qstep = grid->QSTEP;
				radius = elem->PDIM / 2;

				if ((pstep < 2 * radius) || (qstep < 2 * radius)) {
					printf("\n%s%s", MOSErrors[2], group_ptr->NAME);
					pstep = 2 * radius;
					qstep = 2 * radius;
				};

				angle = dacos(((qstep * qstep) + (pstep * pstep) - (4 * radius * radius)) / (2 * pstep * qstep));

				grid->ANGLE = angle;
			} else if (elem->GEOMETRY == RECTANGULAR_ELEM) {

				pstep = grid->PSTEP;
				qstep = grid->QSTEP;
				pdim = elem->PDIM;
				qdim = elem->QDIM;

				if ((qstep < qdim) || (pstep < pdim)) {
					printf("\n%s%s", MOSErrors[2], group_ptr->NAME);
					qstep = qdim;
				};

				angle = dasin(qdim / qstep);

				grid->ANGLE = angle;
			};

		}
		switch (elem->GEOMETRY) {

		case CIRCULAR_ELEM:
			pstep = grid->PSTEP;
			qstep = grid->QSTEP;
			radius = elem->PDIM / 2;

			if ((pstep < 2 * radius) || (qstep < 2 * radius)) {
				printf("\n%s%s", MOSErrors[2], group_ptr->NAME);
			};

			if ((grid->ANGLE <= 45) || (grid->ANGLE > 90)) {
				printf("\n%s%f", MOSErrors[4], grid->ANGLE);
				exit(3);
			};
			break;

		case RECTANGULAR_ELEM:
			pstep = grid->PSTEP;
			qstep = grid->QSTEP;
			pdim = elem->PDIM;
			qdim = elem->QDIM;

			if ((qstep < qdim) || (pstep < pdim)) {
				printf("\n%s%s", MOSErrors[2], group_ptr->NAME);
			};


			if ((grid->ANGLE <= (datan(elem->QDIM / elem->PDIM))) || (grid->ANGLE > 90)) {

				printf("\n%s%f", MOSErrors[4], grid->ANGLE);
				exit(3);
			};
			break;
		};

	};

	printf("\n** Generating GROUP: %s.", group_ptr->NAME);
	printf("\n** PSTEP,QSTEP set to:%f,%f", grid->PSTEP, grid->QSTEP);
	printf("\n** Grid ANGLE set to:%f ", grid->ANGLE);
	printf("\n** Group ROT_ANGLE set to: %f \n", grid->ROT_ANGLE);

	return 0;
}


int 
fixsgrid(struct Grid * grid)
{

	double          angle, *angle_ptr = &angle;
	int             angle_unit;
	double          pstep, *pstep_ptr = &pstep;
	double          qstep, *qstep_ptr = &qstep;
	int             pqstep_unit;
	double          px, *px_ptr = &px;
	double          py, *py_ptr = &py;
	double          qx, *qx_ptr = &qx;
	double          qy, *qy_ptr = &qy;
	int             pqxy_unit;
	double          rot_angle;


	angle = grid->ANGLE;
	angle_unit = grid->ANGLE_UNIT;
	pstep = grid->PSTEP;
	qstep = grid->QSTEP;
	pqstep_unit = grid->PQSTEP_UNIT;
	px = grid->PX;
	py = grid->PY;
	qx = grid->QX;
	qy = grid->QY;
	pqxy_unit = grid->PQXY_UNIT;
	rot_angle = grid->ROT_ANGLE;

	switch (grid->TYPE) {

	case LIN_GRID:

		interror("fixsgrid - 1");
		break;

	case SQU_GRID:
		if (pstep != 0) {
			mmconv(pstep_ptr, pqstep_unit);
			qstep = pstep;
		} else {
			qstep = 0;
		};
		angle = 90;
		break;

	case REC_GRID:
		if (pstep != 0) {
			mmconv(pstep_ptr, pqstep_unit);
			mmconv(qstep_ptr, pqstep_unit);
		} else {
			qstep = 0;
		};
		angle = 90;
		break;

	case HEX_GRID:
		if (pstep != 0) {
			mmconv(pstep_ptr, pqstep_unit);
			qstep = pstep;
		} else {
			qstep = 0;
		};
		angle = 60;
		break;

	case TRI_GRID:
		if (angle != 0) {
			degconv(angle_ptr, angle_unit);
			px = 0;
			py = 0;
			qx = 0;
			qy = 0;
		};
		if (pstep != 0) {
			mmconv(pstep_ptr, pqstep_unit);
			mmconv(qstep_ptr, pqstep_unit);
			px = 0;
			py = 0;
			qx = 0;
			qy = 0;
		};
		if (px != 0) {
			mmconv(px_ptr, pqxy_unit);
			mmconv(py_ptr, pqxy_unit);
			mmconv(qx_ptr, pqxy_unit);
			mmconv(qy_ptr, pqxy_unit);
			pstep = 0;
			qstep = 0;
			angle = 0;
		};
		break;
	};

	angle_unit = DEGREES;
	pqstep_unit = MM_UNIT;
	pqxy_unit = MM_UNIT;
	rot_angle = 0;


	grid->ANGLE = angle;
	grid->ANGLE_UNIT = angle_unit;
	grid->PSTEP = pstep;
	grid->QSTEP = qstep;
	grid->PQSTEP_UNIT = pqstep_unit;
	grid->PX = px;
	grid->PY = py;
	grid->QX = qx;
	grid->QY = qy;
	grid->PQXY_UNIT = pqxy_unit;
	grid->ROT_ANGLE = rot_angle;

	return 0;
}





void 
floatsup(void)
{

	double          dummy;
	dummy = 1.0 + 0.0;

	printf("*******************************************************************************\n");
	printf("                         ARRAY PREPROCESSOR ver. %3.1f\n", dummy);
	printf("*******************************************************************************\n");
}




int 
gainspec(struct charac * p1, struct charac ** pp2,
	 double *gain_ptr,
	 int *gain_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione gainspec()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[32], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = gainval(*curr_ptr, curr_ptr, gain_ptr, gain_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 11, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}



int 
gainunit(struct charac * p1,
	 struct charac ** pp2, int *gain_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;

#ifdef DEBUG1
	printf("\nFunzione gainunit()");
#endif


	*curr_ptr = p1;



	*gain_unit_ptr = 2;
	if (GetKeyword(Keywords[64], curr_ptr) == 0)
		*gain_unit_ptr = DB_UNIT;
	else if (GetKeyword(Keywords[15], curr_ptr) == 0)
		*gain_unit_ptr = LINEAR_UNIT;
	else
		return 33;


	*pp2 = *curr_ptr;
	return 0;
}





int 
gainval(struct charac * p1, struct charac ** pp2,
	double *gain_ptr,
	int *gain_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione gainval()\n");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetReal(gain_ptr, curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = gainunit(*curr_ptr, curr_ptr, gain_unit_ptr);
#ifdef DEBUG5
	printf("Unità di misura guadagno trovata gain_unit=%d\n", *gain_unit_ptr);
#endif
	if (error != 0)
		*gain_unit_ptr = DB_UNIT;


	*pp2 = *curr_ptr;
	return 0;
}



int 
gaussel(struct charac * p1, struct charac ** pp2,
	double *anglee_ptr,
	double *angleh_ptr,
	int *angle_unit_ptr,
	double *tapere_ptr,
	double *taperh_ptr,
	int *taper_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione gaussel()\n");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[35], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[59], curr_ptr));
	error = 0;


	error = (GetReal(anglee_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 19, " ");

		printf("\n%s", ErrorMessages[29]);
		return 17;
	};


	error = (GetKeyword(Keywords[60], curr_ptr));
	error = 0;


	error = (GetReal(angleh_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 19, " ");

		printf("\n%s", ErrorMessages[29]);
		return 17;
	};



	error = angunit(*curr_ptr, curr_ptr, angle_unit_ptr);
#ifdef DEBUG5
	printf("Unità di misura angolo trovata angle_unit=%d\n", *angle_unit_ptr);
#endif
	if (error != 0)
		*angle_unit_ptr = 0;


	error = (GetKeyword(Keywords[61], curr_ptr));
	error = 0;


	error = (GetReal(tapere_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 27, " ");

		printf("\n%s", ErrorMessages[29]);
		return 17;
	};


	error = (GetKeyword(Keywords[62], curr_ptr));
	error = 0;


	error = (GetReal(taperh_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 27, " ");

		printf("\n%s", ErrorMessages[29]);
		return 17;
	};



	error = ampunit(curr_ptr, taper_unit_ptr);
#ifdef DEBUG5
	printf("Unità di misura angolo trovata angle_unit=%d\n", *angle_unit_ptr);
#endif
	if (error != 0)
		*taper_unit_ptr = 0;


	*pp2 = *curr_ptr;
	return 0;
}






int 
geomspec(struct charac * p1, struct charac ** pp2,
	 int *geometry_ptr,
	 double *radius_ptr,
	 int *radius_unit_ptr,
	 double *pdim_ptr,
	 double *qdim_ptr,
	 int *pqdim_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[29], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 1, Keywords[29]);

		return 1;
	};


	*geometry_ptr = 0;
	error = recspec(*curr_ptr, curr_ptr, pdim_ptr, qdim_ptr, pqdim_unit_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[25]);
		return 17;
	};




	*geometry_ptr = 1;
	error = circspec(*curr_ptr, curr_ptr, radius_ptr, radius_unit_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[28]);
		return 17;
	};



	return 17;
}






int 
Get1Real(struct charac * p1,
	 struct charac ** pp2, double *ureal_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;



	*curr_ptr = p1;



	error = (GetReal(ureal_ptr, curr_ptr));
	if (error != 0) {
		return 11;
	};

#ifdef DEBUG5
	printf("\nTrovato <unsigned_real> = %f\n", *ureal_ptr);
#endif


	if (*ureal_ptr < 0) {
		return 12;
	};



	*pp2 = *curr_ptr;

	return 0;
}




int 
GetInt(struct charac * p1,
       struct charac ** pp2, int *intero)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             i = 0;
	char            num[MAX_INT_LENGTH + 1];
	char            ch;


	*curr_ptr = p1;

#ifdef DEBUG2
	printf("\nINIZIA FUNZIONE GetInt() - scansione caratteri()\n");
#endif




	ch = TapeGet(curr_ptr);
#ifdef DEBUG5
	printf("ch=>%c<", ch);
#endif
	if ((isdigit(ch) == 0) && (ch != '+') && (ch != '-'))
		return 42;


	num[i] = ch;

#ifdef DEBUG5
	printf("    num[%d]=>%c<\n", i, num[i]);
#endif

	i = i + 1;
	ch = TapeGet(curr_ptr);

#ifdef DEBUG5
	printf("ch=>%c<", ch);
#endif

	while (isdigit(ch) && ((*curr_ptr) != NULL)) {
		if (i < MAX_INT_LENGTH) {
			num[i] = ch;
#ifdef DEBUG5
			printf("    num[%d]=>%c<\n", i, num[i]);
#endif
		};
		i = i + 1;
		ch = TapeGet(curr_ptr);
#ifdef DEBUG5
		printf("ch=>%c<", ch);
#endif
	};


	if (i >= MAX_INT_LENGTH) {
		num[MAX_INT_LENGTH] = '\0';
		parserro(p1, 43, num);
		return 43;
	} else
		num[i] = '\0';

#ifdef DEBUG5
	printf("\nStringa num[] letta da GetInt:>%s<\n", num);
	printf("Lunghezza stringa: %d\n", strlen(num));
#endif


	*intero = atoi(num);
#ifdef DEBUG5
	printf("Numero convertito da atoi()=>%d<\n", *intero);
#endif


	*pp2 = *curr_ptr;
	return 0;
}





int 
GetKeyword(char *kw, struct charac ** tp)
{

	struct charac  *curr, **curr_ptr;
	char            word[KWDSLEN + 1], ch;
	int             i = 0;

#ifdef DEBUG2
	printf("Funzione GetKeyword() - cerco kwd:>%s<\n", kw);
#endif

#ifdef DEBUG5
	printf("\nScansione caratteri\n");
#endif


	curr_ptr = &curr;
	*curr_ptr = *tp;



	ch = TapeGet(curr_ptr);
#ifdef DEBUG5
	printf("ch=>%c<", ch);
#endif


	while (((isalnum(ch)) || (ch == '_')) && (i < KWDSLEN) && ((*curr_ptr) != NULL)) {
		word[i] = ch;
#ifdef DEBUG5
		printf("    word[%d]=>%c<\n", i, word[i]);
#endif
		i = i + 1;
		ch = TapeGet(curr_ptr);
#ifdef DEBUG5
		printf("ch=>%c<", ch);
#endif
	};


	word[i] = '\0';

#ifdef DEBUG5
	printf("\nPAROLA LETTA da GetKeyword:>%s<\n", word);
#endif


	if (strcmp(kw, word) == 0) {
		*tp = *curr_ptr;
		return 0;
	} else
		return 1;
}






int 
GetNatur(struct charac * p1,
	 struct charac ** pp2, int *natur_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;
	int             val, *val_ptr = &val;


	*curr_ptr = p1;


	error = (GetInt(*curr_ptr, curr_ptr, val_ptr));
	if (error != 0) {
		return 44;
	};

#ifdef DEBUG5
	printf("\nTrovato <natural> = %d\n", *val_ptr);
#endif


	if (*val_ptr < 0) {
		return 12;
	};


	if (*val_ptr == 0) {
		return 45;
	};


	*natur_ptr = *val_ptr;
	*pp2 = *curr_ptr;
	return 0;
}




int 
GetReal(double *reale, struct charac ** tp)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             i = 0;
	char            num[MAX_REAL_LENGTH + 1];
	char            ch;



	*curr_ptr = *tp;

#ifdef DEBUG2
	printf("\nINIZIA FUNZIONE GetReal() - scansione caratteri()\n");
#endif




	ch = TapeGet(curr_ptr);
#ifdef DEBUG5
	printf("ch=>%c<", ch);
#endif
	if ((isdigit(ch) == 0) && (ch != '+') && (ch != '-') && (ch != '.'))
		return 13;

	num[i] = ch;

#ifdef DEBUG5
	printf("    num[%d]=>%c<\n", i, num[i]);
#endif

	i = i + 1;
	ch = TapeGet(curr_ptr);

#ifdef DEBUG5
	printf("ch=>%c<", ch);
#endif

	while (((isdigit(ch) || (ch == '.') || (ch == 'e') || (ch == 'E') || (ch == '-')) && ((*curr_ptr) != NULL))) {
		if (i < MAX_REAL_LENGTH)
			num[i] = ch;
#ifdef DEBUG5
		printf("    num[%d]=>%c<\n", i, num[i]);
#endif
		i = i + 1;
		ch = TapeGet(curr_ptr);
#ifdef DEBUG5
		printf("ch=>%c<", ch);
#endif
	};

	if (i >= MAX_REAL_LENGTH)
		num[MAX_REAL_LENGTH] = '\0';
	else
		num[i] = '\0';

#ifdef DEBUG5
	printf("\nStringa num[] letta da GetReal:>%s<\n", num);
	printf("Lunghezza stringa: %d\n", strlen(num));
#endif


	*reale = atof(num);
#ifdef DEBUG5
	printf("Numero convertito da atof()=>%f<\n", *reale);
#endif




	*tp = *curr_ptr;

	return 0;

}





int 
GetUName(char word[UNLEN + 1], struct charac ** tp)
{
	struct charac  *curr, **curr_ptr;
	char            ch;
	int             i = 0;


	curr_ptr = &curr;
	*curr_ptr = *tp;
#ifdef DEBUG2
	printf("\nINIZIA FUNZIONE GetUName() - scansione caratteri()\n");
#endif

	ch = TapeGet(curr_ptr);
	while (((isletter(ch)) || (isdigit(ch))) && ((*curr_ptr) != NULL)) {
		if (i < UNLEN)
			word[i] = ch;
#ifdef DEBUG5
		printf("\n%d %c\n", i, word[i]);
#endif
		i = i + 1;
		ch = TapeGet(curr_ptr);
	};

	{
		if (i >= UNLEN)
			word[UNLEN] = '\0';
		else
			word[i] = '\0';
	};

	if (i > UNLEN)
		parserro(*tp, 7, word);

#ifdef DEBUG5
	printf("\nPAROLA LETTA da GetUName:%s-%d caratteri.", word, strlen(word));
#endif




	if (strlen(word) == 0)
		return 2;

	if ((isletter(word[0])) == 0) {

		return 5;
	};
	if (InKWords(word) == 1) {

		return 6;
	};


	*tp = *curr_ptr;
	return 0;
}





int 
GetUReal(struct charac * p1,
	 struct charac ** pp2, double *ureal_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;
	double          val, *val_ptr = &val;




	*curr_ptr = p1;



	error = (GetReal(val_ptr, curr_ptr));
	if (error != 0) {
		return 11;
	};

#ifdef DEBUG5
	printf("\nTrovato <unsigned_real> = %f\n", *val_ptr);
#endif


	if (*val_ptr < 0) {
		return 12;
	};


	*ureal_ptr = *val_ptr;


	*pp2 = *curr_ptr;


	return 0;
}




void 
glvainit(void)
{


#ifdef DEBUG3
	printf("\nglvainit ()");
#endif

	floatsup();
	kwdsinit();
	emsginit();
	unaminit();
	moseinit();

}



void 
gnodevis(int p,
	 int q,
	 double angle,
	 double phase,
	 struct Geomnode * gnode_list_start_ptr
)
{

	int             error = 0;
	struct Geomnode *gnode_ptr;

#ifdef DEBUG5
	printf("\nRotating local node p,q=%d,%d", p, q);
#endif


	error = gnodfind(gnode_list_start_ptr, &gnode_ptr, p, q);
	if (error != 0) {
		interror("nodevis() - node not found -");
		exit(1);
	};




	(gnode_ptr->GEOMPORT_PTR)->PPA = phase;



	gnode_ptr->PHEA += angle;

	return;
}





int 
gnodfind(struct Geomnode * gnode_list_start_ptr,
	 struct Geomnode ** gnode_ptr,
	 int p,
	 int q
)
{

	*gnode_ptr = gnode_list_start_ptr;

	while ((!(((*gnode_ptr)->LOC_P == p) && ((*gnode_ptr)->LOC_Q == q))) && ((*gnode_ptr)->NEXT != NULL))
		(*gnode_ptr) = (*gnode_ptr)->NEXT;


	if (!(((*gnode_ptr)->LOC_P == p) && ((*gnode_ptr)->LOC_Q == q)))
		return 1;
	else
		return 0;
}




int 
grampexc(struct charac * p1, struct charac ** pp2,
	 struct GrAmpExc ** grampexc_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

	double          unif_val, *unif_val_ptr = &unif_val;
	double          centre, *centre_ptr = &centre;
	double          p1_et, *p1_et_ptr = &p1_et;
	double          p2_et, *p2_et_ptr = &p2_et;
	double          q1_et, *q1_et_ptr = &q1_et;
	double          q2_et, *q2_et_ptr = &q2_et;
	int             amp_unit, *amp_unit_ptr = &amp_unit;


#ifdef DEBUG1
	printf("\nFunzione grampexc()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[42], curr_ptr));
	if (error != 0) {
		*grampexc_ptr = NULL;
		return 1;
	};






	error = unifamp(*curr_ptr, curr_ptr, unif_val_ptr, amp_unit_ptr);


	if (error == 0) {

		*grampexc_ptr = (struct GrAmpExc *) malloc(sizeof(struct GrAmpExc));

		if (*grampexc_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("grampexc()");
		};

		(*grampexc_ptr)->TYPE = UNIFORM_LAW;
		(*grampexc_ptr)->UNIF_VAL = unif_val;
		(*grampexc_ptr)->AMP_UNIT = amp_unit;
		(*grampexc_ptr)->PQEXP_PTR = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};







	error = secoramp(*curr_ptr, curr_ptr, centre_ptr, p1_et_ptr, p2_et_ptr, q1_et_ptr, q2_et_ptr, amp_unit_ptr);

	if (error == 17) {
		*grampexc_ptr = NULL;
		printf("\n%s", ErrorMessages[69]);
		return 17;
	};


	if (error == 0) {

		*grampexc_ptr = (struct GrAmpExc *) malloc(sizeof(struct GrAmpExc));

		if (*grampexc_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("grampexc()");
		};

		(*grampexc_ptr)->TYPE = SECOND_ORDER_LAW;
		(*grampexc_ptr)->CENTRE = centre;
		(*grampexc_ptr)->P1_ET = p1_et;
		(*grampexc_ptr)->P2_ET = p2_et;
		(*grampexc_ptr)->Q1_ET = q1_et;
		(*grampexc_ptr)->Q2_ET = q2_et;
		(*grampexc_ptr)->AMP_UNIT = amp_unit;
		(*grampexc_ptr)->PQEXP_PTR = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};








	*grampexc_ptr = NULL;
	return 17;
}



int 
greldef(struct charac * p1, struct charac ** pp2,
	struct Grid ** grid_ptr, struct Elem ** elem_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;

	int             error;

#ifdef DEBUG1
	printf("\nFunzione greldef()");
#endif


	*curr_ptr = p1;


	*grid_ptr = NULL;
	*elem_ptr = NULL;
	error = 0;


	error = griddef1(*curr_ptr, curr_ptr, grid_ptr);

	if (error == 1) {
		return 1;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[16]);
		return 17;
	};



	error = elemdef(*curr_ptr, curr_ptr, elem_ptr);

	if (error == 1) {
		parserro(*curr_ptr, 67, " ");

		return 1;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[24]);
		return 17;
	};




	*pp2 = *curr_ptr;
	return 0;
}





int 
grexcdef(struct charac * p1, struct charac ** pp2,
	 struct GrAmpExc ** grampexc_ptr, struct GrPhaExc ** grphaexc_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;

	int             error;

#ifdef DEBUG1
	printf("\nFunzione grexcdef()");
#endif


	*curr_ptr = p1;


	*grampexc_ptr = NULL;
	*grphaexc_ptr = NULL;
	error = 0;


	error = (GetKeyword(Keywords[101], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = grampexc(*curr_ptr, curr_ptr, grampexc_ptr);


	if (error != 0) {
		printf("\n%s", ErrorMessages[79]);
		return 17;
	};


	error = grphaexc(*curr_ptr, curr_ptr, grphaexc_ptr);


	if (error != 0) {
		printf("\n%s", ErrorMessages[80]);
		return 17;
	};




	*pp2 = *curr_ptr;
	return 0;
}






int 
grgeodef(struct charac * p1, struct charac ** pp2,
	 struct AddRem ** addrem_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	struct AddRem  *ultimo_ptr = NULL;
	struct AddRem  *a_ptr = NULL, **app_ptr = &a_ptr;

#ifdef DEBUG1
	printf("\nFunzione grgeodef()");
#endif


	*curr_ptr = p1;
	error = 0;


	*addrem_ptr = NULL;


	error = adddef(*curr_ptr, curr_ptr, addrem_ptr);

	if (error == 1) {
		parserro(*curr_ptr, 94, " ");
		return 17;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[62]);
		return 17;
	};





	*app_ptr = *addrem_ptr;

	ultimo_ptr = *addrem_ptr;


	while (1) {

		error = adremdef(*curr_ptr, curr_ptr, app_ptr);
		if (error == 1) {
			ultimo_ptr->NEXT = NULL;
			*pp2 = *curr_ptr;
			return 0;
		};
		if (error == 17) {
			*addrem_ptr = NULL;
			printf("\n%s", ErrorMessages[64]);
			return 17;
		};


		ultimo_ptr->NEXT = *app_ptr;

		ultimo_ptr = *app_ptr;

		ultimo_ptr->NEXT = NULL;
	};

}




int 
griddef1(struct charac * p1, struct charac ** pp2,
	 struct Grid ** grid_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;

	int             grid_type;
	int             orient, *orient_ptr = &orient;
	double          pstep, *pstep_ptr = &pstep;
	double          qstep, *qstep_ptr = &qstep;
	int             pqstep_unit, *pqstep_unit_ptr = &pqstep_unit;
	double          angle, *angle_ptr = &angle;
	int             angle_unit, *angle_unit_ptr = &angle_unit;
	double          px, *px_ptr = &px;
	double          py, *py_ptr = &py;
	double          qx, *qx_ptr = &qx;
	double          qy, *qy_ptr = &qy;
	int             pqxy_unit, *pqxy_unit_ptr = &pqxy_unit;


	int             error;

#ifdef DEBUG1
	printf("\nFunzione griddef1()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[14], curr_ptr));
	if (error != 0) {
		*grid_ptr = NULL;
		return 1;
	};




	grid_type = SQU_GRID;
	error = squgrdef(*curr_ptr, curr_ptr, pstep_ptr, pqstep_unit_ptr);

	if (error == 17) {
		*grid_ptr = NULL;
		printf("\n%s", ErrorMessages[16]);
		return 17;

	};



	if (error != 0) {
		grid_type = REC_GRID;
		error = recgrdef(*curr_ptr, curr_ptr, pstep_ptr, qstep_ptr, pqstep_unit_ptr);

		if (error == 17) {
			printf("\n%s", ErrorMessages[16]);
			*grid_ptr = NULL;
			return 17;
		};
	};


	if (error != 0) {
		grid_type = HEX_GRID;
		error = hexgrdef(*curr_ptr, curr_ptr, pstep_ptr, pqstep_unit_ptr);

		if (error == 17) {
			*grid_ptr = NULL;
			printf("\n%s", ErrorMessages[16]);
			return 17;
		};
	};


	if (error != 0) {
		grid_type = TRI_GRID;
		error = trigrdef(*curr_ptr, curr_ptr, angle_ptr, angle_unit_ptr, pstep_ptr, qstep_ptr, pqstep_unit_ptr, px_ptr, py_ptr, qx_ptr, qy_ptr, pqxy_unit_ptr);

		if (error == 17) {
			*grid_ptr = NULL;
			printf("\n%s", ErrorMessages[16]);
			return 17;
		};
	};


	if (error != 0) {
		*grid_ptr = NULL;
		parserro(*curr_ptr, 16, " ");

		return 17;
	};


	*grid_ptr = (struct Grid *) malloc(sizeof(struct Grid));

	if (*grid_ptr == NULL) {
		printf("%s", ErrorMessages[55]);
		interror("griddef1()");
	};

	(*grid_ptr)->TYPE = grid_type;
	(*grid_ptr)->ORIENT = orient;
	(*grid_ptr)->PSTEP = pstep;
	(*grid_ptr)->QSTEP = qstep;
	(*grid_ptr)->PQSTEP_UNIT = pqstep_unit;
	(*grid_ptr)->ANGLE = angle;
	(*grid_ptr)->ANGLE_UNIT = angle_unit;
	(*grid_ptr)->PX = px;
	(*grid_ptr)->PY = py;
	(*grid_ptr)->QX = qx;
	(*grid_ptr)->QY = qy;
	(*grid_ptr)->PQXY_UNIT = pqxy_unit;


	*pp2 = *curr_ptr;
	return 0;
}






int 
groupdef(struct charac * p1, struct charac ** pp2,
	 struct Group ** group_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

	char            groupname[UNLEN + 1];

	struct Grid    *grid = NULL, **grid_ptr = &grid;
	struct Elem    *elem = NULL, **elem_ptr = &elem;
	struct AddRem  *addrem = NULL, **addrem_ptr = &addrem;
	struct GrAmpExc *grampexc = NULL, **grampexc_ptr = &grampexc;
	struct GrPhaExc *grphaexc = NULL, **grphaexc_ptr = &grphaexc;
	struct Err     *err = NULL, **err_ptr = &err;
	struct Fail    *fail = NULL, **fail_ptr = &fail;

#ifdef DEBUG1
	printf("\nFunzione groupdef()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[47], curr_ptr));
	if (error != 0) {
		*group_ptr = NULL;
		return 1;
	};


	if (GetUName(groupname, curr_ptr) != 0) {
		parserro(*curr_ptr, 81, " ");

		return 17;
	};


	if (InUNames(groupname)) {
		parserro(*curr_ptr, 3, groupname);

		return 17;
	};


	InserUN(groupname);


	error = greldef(*curr_ptr, curr_ptr, grid_ptr, elem_ptr);


	if (error == 17) {
		return 17;
	};


	if (error == 0) {
		(*grid_ptr)->LEVEL = INTERNAL;
		(*elem_ptr)->LEVEL = INTERNAL;
		strcpy((*elem_ptr)->NAME, "ELEMENT");
	};




	error = grgeodef(*curr_ptr, curr_ptr, addrem_ptr);


	if (error != 0) {
		parserro(*curr_ptr, 82, groupname);

		return 17;
	};


	error = grexcdef(*curr_ptr, curr_ptr, grampexc_ptr, grphaexc_ptr);


	if (error != 0) {
		parserro(*curr_ptr, 82, " ");

		return 17;
	};






	error = (GetKeyword(Keywords[50], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 1, Keywords[50]);

		return 17;
	};


	*group_ptr = (struct Group *) malloc(sizeof(struct Group));

	if (*group_ptr == NULL) {
		parserro(*curr_ptr, 55, " ");

		interror("groupdef()");
	};



	strcpy((*group_ptr)->NAME, groupname);
	(*group_ptr)->NUM_OF_NODES = 0;
	(*group_ptr)->SHAPE = NO_SHAPE;
	(*group_ptr)->PSIZE = 0;
	(*group_ptr)->QSIZE = 0;
	(*group_ptr)->GRID_PTR = *grid_ptr;
	(*group_ptr)->ELEM_PTR = *elem_ptr;
	(*group_ptr)->GRAMPEXC_PTR = *grampexc_ptr;
	(*group_ptr)->GRPHAEXC_PTR = *grphaexc_ptr;
	(*group_ptr)->ADDREM_PTR = *addrem_ptr;
	(*group_ptr)->ERR_PTR = *err_ptr;
	(*group_ptr)->FAIL_PTR = *fail_ptr;

	(*group_ptr)->GEOMNODE_PTR = NULL;
	(*group_ptr)->BUILT = NO;
	(*group_ptr)->SUPER_GROUP_PTR[0] = NULL;



	*pp2 = *curr_ptr;
	return 0;
}



int 
grphaexc(struct charac * p1, struct charac ** pp2,
	 struct GrPhaExc ** grphaexc_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

	double          unif_val, *unif_val_ptr = &unif_val;
	double          centre, *centre_ptr = &centre;
	double          p1_ep, *p1_ep_ptr = &p1_ep;
	double          p2_ep, *p2_ep_ptr = &p2_ep;
	double          q1_ep, *q1_ep_ptr = &q1_ep;
	double          q2_ep, *q2_ep_ptr = &q2_ep;
	int             angle_unit, *angle_unit_ptr = &angle_unit;
	int             direction, *direction_ptr = &direction;
	int             omit_angle, *omit_angle_ptr = &omit_angle;
	double          angle_start, *angle_start_ptr = &angle_start;
	double          angle_step, *angle_step_ptr = &angle_step;
	int             omit_phase, *omit_phase_ptr = &omit_phase;
	double          phase_start, *phase_start_ptr = &phase_start;
	double          phase_step, *phase_step_ptr = &phase_step;
	double          u, *u_ptr = &u;
	double          v, *v_ptr = &v;
	double          theta, *theta_ptr = &theta;
	double          phi, *phi_ptr = &phi;
	int             phase_unit, *phase_unit_ptr = &phase_unit;

#ifdef DEBUG1
	printf("\nFunzione grphaexc()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[43], curr_ptr));
	if (error != 0) {
		*grphaexc_ptr = NULL;
		return 1;
	};






	error = unifpha(*curr_ptr, curr_ptr, unif_val_ptr, phase_unit_ptr);


	if (error == 0) {

		*grphaexc_ptr = (struct GrPhaExc *) malloc(sizeof(struct GrPhaExc));

		if (*grphaexc_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("grphaexc()");
		};

		(*grphaexc_ptr)->TYPE = UNIFORM_LAW;
		(*grphaexc_ptr)->UNIF_VAL = unif_val;
		(*grphaexc_ptr)->PHASE_UNIT = phase_unit;
		(*grphaexc_ptr)->PQEXP_PTR = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};






	error = secorpha(*curr_ptr, curr_ptr, centre_ptr, p1_ep_ptr, p2_ep_ptr, q1_ep_ptr, q2_ep_ptr, phase_unit_ptr);

	if (error == 17) {
		*grphaexc_ptr = NULL;
		printf("\n%s", ErrorMessages[76]);
		return 17;
	};


	if (error == 0) {

		*grphaexc_ptr = (struct GrPhaExc *) malloc(sizeof(struct GrPhaExc));

		if (*grphaexc_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("grphaexc()");
		};

		(*grphaexc_ptr)->TYPE = SECOND_ORDER_LAW;
		(*grphaexc_ptr)->CENTRE = centre;
		(*grphaexc_ptr)->P1_EP = p1_ep;
		(*grphaexc_ptr)->P2_EP = p2_ep;
		(*grphaexc_ptr)->Q1_EP = q1_ep;
		(*grphaexc_ptr)->Q2_EP = q2_ep;
		(*grphaexc_ptr)->PHASE_UNIT = phase_unit;
		(*grphaexc_ptr)->PQEXP_PTR = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};






	error = serotpha(*curr_ptr, curr_ptr, direction_ptr, omit_angle_ptr, angle_start_ptr, angle_step_ptr, omit_phase_ptr, phase_start_ptr, phase_step_ptr, angle_unit_ptr, phase_unit_ptr);

	if (error == 17) {
		*grphaexc_ptr = NULL;
		printf("\n%s", ErrorMessages[77]);
		return 17;
	};


	if (error == 0) {

		*grphaexc_ptr = (struct GrPhaExc *) malloc(sizeof(struct GrPhaExc));

		if (*grphaexc_ptr == NULL) {
			printf("%s", ErrorMessages[55]);
			interror("grphaexc()");
		};


		(*grphaexc_ptr)->TYPE = ROTATION_SEQUENTIAL_LAW;
		(*grphaexc_ptr)->DIRECTION = direction;
		(*grphaexc_ptr)->OMIT_ANGLE = omit_angle;
		(*grphaexc_ptr)->ANGLE_START = angle_start;
		(*grphaexc_ptr)->ANGLE_STEP = angle_step;
		(*grphaexc_ptr)->OMIT_PHASE = omit_phase;
		(*grphaexc_ptr)->PHASE_START = phase_start;
		(*grphaexc_ptr)->PHASE_STEP = phase_step;
		(*grphaexc_ptr)->ANGLE_UNIT = angle_unit;
		(*grphaexc_ptr)->PHASE_UNIT = phase_unit;
		(*grphaexc_ptr)->PQEXP_PTR = NULL;


		*pp2 = *curr_ptr;
		return 0;
	};







	error = beampha(*curr_ptr, curr_ptr, u_ptr, v_ptr, theta_ptr, phi_ptr, angle_unit_ptr);


	if (error == 17) {
		*grphaexc_ptr = NULL;
		printf("\n%s", ErrorMessages[78]);
		return 17;
	};


	if (error == 0) {

		*grphaexc_ptr = (struct GrPhaExc *) malloc(sizeof(struct GrPhaExc));

		if (*grphaexc_ptr == NULL) {
			printf("%s", ErrorMessages[55]);
			interror("grphaexc()");
		};


		(*grphaexc_ptr)->TYPE = BEAM_POINTING_LAW;
		(*grphaexc_ptr)->U = u;
		(*grphaexc_ptr)->V = v;
		(*grphaexc_ptr)->THETA = theta;
		(*grphaexc_ptr)->PHI = phi;
		(*grphaexc_ptr)->ANGLE_UNIT = angle_unit;
		(*grphaexc_ptr)->PQEXP_PTR = NULL;


		*pp2 = *curr_ptr;
		return 0;
	};






	*grphaexc_ptr = NULL;
	return 17;
}




int 
grwrite(struct Group * group_ptr, char filename[UNLEN + 1])
{

	int             NEL, NPORTS, IDUM = 0;
	struct Geomnode *geomnode_ptr;
	struct Geomport *geomport_ptr;
	FILE           *f;
	char            datfilename[UNLEN + 1];



	if (group_ptr == NULL) {
		interror(" grwrite() -1- ");
		exit(1);
	};

	geomnode_ptr = group_ptr->GEOMNODE_PTR;


	strcpy(datfilename, filename);
	strcat(datfilename, ".dat");




	printf("\n** Writing output file: %s", datfilename);
	printf("\n\nNEL \nNPORTS");
	printf("\nIDUM      REXA     REYA     REZA     THEA     PHEA     PSEA ");
	printf("\n          AMP      PSH      PSC      PHEPOL\n");


	NEL = (group_ptr->NUM_OF_NODES);
	FPRINTF("%4d", NEL);
	FPRINTF("\n");
	printf("%4d", NEL);
	printf("\n");


	NPORTS = (group_ptr->ELEM_PTR)->NPORTS;
	FPRINTF("%4d", NPORTS);
	FPRINTF("\n");

	printf("%4d", NPORTS);
	printf("\n");


	while (geomnode_ptr != NULL) {

		IDUM = IDUM + 1;
		FPRINTF("%4d ", IDUM);
		FPRINTF("\t%8.2f ", geomnode_ptr->XA);
		FPRINTF("%8.2f ", geomnode_ptr->YA);
		FPRINTF("%8.2f ", geomnode_ptr->ZA);
		FPRINTF("%8.2f ", geomnode_ptr->THEA);
		FPRINTF("%8.2f ", geomnode_ptr->PHEA);
		FPRINTF("%8.2f", geomnode_ptr->PSEA);
		FPRINTF("\n");

		printf("%4d ", IDUM);
		printf("\t%8.2f ", geomnode_ptr->XA);
		printf("%8.2f ", geomnode_ptr->YA);
		printf("%8.2f ", geomnode_ptr->ZA);
		printf("%8.2f ", geomnode_ptr->THEA);
		printf("%8.2f ", geomnode_ptr->PHEA);
		printf("%8.2f", geomnode_ptr->PSEA);
		printf("\n");

		geomport_ptr = geomnode_ptr->GEOMPORT_PTR;


		while (geomport_ptr != NULL) {
			FPRINTF("   \t%8.2f ", geomport_ptr->AMP);

			FPRINTF("%8.2f ", geomport_ptr->PSH + geomport_ptr->PPA);
			FPRINTF("%8.2f ", geomport_ptr->PSC);
			FPRINTF("%8.2f", geomport_ptr->PHEPOL);
			FPRINTF("\n");

			printf("   \t%8.2f ", geomport_ptr->AMP);
			printf("%8.2f ", geomport_ptr->PSH + geomport_ptr->PPA);
			printf("%8.2f ", geomport_ptr->PSC);
			printf("%8.2f", geomport_ptr->PHEPOL);
			printf("\n");

			geomport_ptr = geomport_ptr->NEXT;
		};

		geomnode_ptr = geomnode_ptr->NEXT;
	};


	printf("\n** End writing output file: %s \n", datfilename);
	return 0;
}


int 
hexdef(struct charac * p1, struct charac ** pp2,
       struct Node ** node_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;




	int             pcoord, *pcoord_ptr = &pcoord;
	int             qcoord, *qcoord_ptr = &qcoord;

#ifdef DEBUG1
	printf("\nFunzione hexdef()");
#endif


	*curr_ptr = p1;
	error = 0;

	*node_ptr = NULL;


	error = (GetKeyword(Keywords[78], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = nodecoor(*curr_ptr, curr_ptr, pcoord_ptr, qcoord_ptr);

	if (error == 1) {
		parserro(*curr_ptr, 66, " ");

		return 17;
	};

	if (error == 17) {
		parserro(*curr_ptr, 66, " ");

		return 17;
	};




	*node_ptr = (struct Node *) malloc(sizeof(struct Node));

	if (*node_ptr == NULL) {
		parserro(*curr_ptr, 55, " ");

		interror("hexdef()");
	};




	(*node_ptr)->NEXT = NULL;

	(*node_ptr)->PCOORD = pcoord;
	(*node_ptr)->QCOORD = qcoord;
	(*node_ptr)->THEA = 0;
	(*node_ptr)->PHEA = 0;
	(*node_ptr)->PSEA = 0;
	(*node_ptr)->ANGLE_UNIT = 0;


	*pp2 = *curr_ptr;
	return 0;
}



int 
hexgrdef(struct charac * p1,
	 struct charac ** pp2, double *pstep_ptr, int *pstep_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;
	*pstep_ptr = 0;
	*pstep_unit_ptr = 0;


	error = (GetKeyword(Keywords[22], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = sinstep(*curr_ptr, curr_ptr, pstep_ptr, pstep_unit_ptr);
	if (error == 1) {
		*pstep_ptr = 0;
		*pstep_unit_ptr = 0;

		*pp2 = *curr_ptr;
		return 0;
	};


	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		*pstep_ptr = 0;
		*pstep_unit_ptr = 0;
		return 17;
	};




	*pp2 = *curr_ptr;
	return 0;
}




int 
InKWords(char *WORD)
{

	int             i;

#ifdef DEBUG3
	printf("\nRICERCA IN TABELLA KeyWords della parola-%s-\n", WORD);
#endif
	if (strlen(WORD) == 0) {
		printf("**INTERNAL ERROR - InKWords() - empty string passed.**");
		exit(1);
	};
	for (i = 0; i <= KWDSNUM; i++) {
#ifdef DEBUG5
		printf("Keywords[%d]-%s-\n", i, Keywords[i]);
#endif
		if ((strcmp(WORD, Keywords[i])) == 0)
			return 1;
	};
	return 0;
}



int 
InserUN(char *WORD)
{

	int             i;

#ifdef DEBUG4
	printf("\nInserimento in tabella UserNames della parola-%s-\n", WORD);
#endif

	if (strlen(WORD) == 0) {
		printf("**INTERNAL ERROR - InserUN() - empty string passed.**");
		exit(1);
	};

	for (i = 0; (i <= UNNUM); i++) {
		if (strlen(UserNames[i]) == 0) {
			strcpy(UserNames[i], WORD);
		};
		return 0;
	};


	printf("** Internal error ** - InserUN()-: too many user names");
	exit(1);
	return 1;
}




void 
interror(char *subname)
{

	printf("\n\n**ERROR INTERNAL** in function %s\n\n", subname);
	printf("** PROGRAM STOPPED **\n\n");
	exit(1);
}

int 
intmax(int a, int b)
{

	if (a > b)
		return a;
	else
		return b;

}



int 
intmin(int a, int b)
{

	if (a < b)
		return a;
	else
		return b;

}





int 
InUNames(char *WORD)
{

	int             i;

#ifdef DEBUG4
	printf("\nRICERCA IN TABELLA UserNames [] della parola-%s-\n", WORD);
#endif

	if (strlen(WORD) == 0) {
		printf("**INTERNAL ERROR - InUNames() - empty string passed.**");
		exit(1);
	};
	for (i = 0; i <= UNNUM; i++) {
#ifdef DEBUG4
		printf("UserNames[%d]-%s-\n", i, UserNames[i]);
#endif
		if ((strcmp(WORD, UserNames[i])) == 0)
			return 1;
	};
	return 0;
}



int 
isletter(char ch)
{

	if ((isalpha(ch)) || (ch == '_') || (ch == '-'))
		return 1;
	else
		return 0;
}



void 
kwdsinit(void)
{
	char            i;

#ifdef DEBUG3
	printf("\nFUNZIONE kwdsinit() INIZIALIZZAZIONE parole chiave");
#endif

	for (i = 0; (i <= KWDSNUM); i++)
		Keywords[i][0] = '\0';

	strcpy(Keywords[0], "PLANE");
	strcpy(Keywords[1], "ARRAY");
	strcpy(Keywords[2], "IS");
	strcpy(Keywords[3], "SURFACE");
	strcpy(Keywords[4], "CYLINDER");
	strcpy(Keywords[5], "RADIUS");
	strcpy(Keywords[6], "mm");
	strcpy(Keywords[7], "cm");
	strcpy(Keywords[8], "dm");
	strcpy(Keywords[9], "m");
	strcpy(Keywords[10], "wl");
	strcpy(Keywords[11], "AXIS");
	strcpy(Keywords[12], "X");
	strcpy(Keywords[13], "Y");
	strcpy(Keywords[14], "GRID");
	strcpy(Keywords[15], "LINEAR");
	strcpy(Keywords[16], "STEP");
	strcpy(Keywords[17], "SQUARE");
	strcpy(Keywords[18], "RECTANGULAR");
	strcpy(Keywords[19], "PSTEP");
	strcpy(Keywords[20], "QSTEP");
	strcpy(Keywords[21], "TRIANGULAR");
	strcpy(Keywords[22], "HEXAGONAL");
	strcpy(Keywords[23], "ANGLE");
	strcpy(Keywords[24], "PX");
	strcpy(Keywords[25], "PY");
	strcpy(Keywords[26], "QX");
	strcpy(Keywords[27], "QY");
	strcpy(Keywords[28], "ELEMENT");
	strcpy(Keywords[29], "GEOMETRY");
	strcpy(Keywords[30], "MODEL");
	strcpy(Keywords[31], "POLARIZATION");
	strcpy(Keywords[32], "GAIN");
	strcpy(Keywords[33], "EXTERNAL");
	strcpy(Keywords[34], "COSINUS");
	strcpy(Keywords[35], "GAUSSIAN");
	strcpy(Keywords[36], "LINEAR");
	strcpy(Keywords[37], "ORIENTATION");
	strcpy(Keywords[38], "CIRCULAR");
	strcpy(Keywords[39], "PORTS");
	strcpy(Keywords[40], "PORT");
	strcpy(Keywords[41], "POL_ORIENT");
	strcpy(Keywords[42], "AMPLITUDE");
	strcpy(Keywords[43], "PHASE");
	strcpy(Keywords[44], "SHAPING");
	strcpy(Keywords[45], "SCANNING");
	strcpy(Keywords[46], "POL_ARRANGEMENT");
	strcpy(Keywords[47], "GROUP");
	strcpy(Keywords[48], "ERRORS");
	strcpy(Keywords[49], "FAILURES");
	strcpy(Keywords[50], "END");
	strcpy(Keywords[51], "deg");
	strcpy(Keywords[52], "rad");
	strcpy(Keywords[53], "PX");
	strcpy(Keywords[54], "PY");
	strcpy(Keywords[55], "QX");
	strcpy(Keywords[56], "QY");
	strcpy(Keywords[57], "PDIM");
	strcpy(Keywords[58], "QDIM");
	strcpy(Keywords[59], "E_ANG");
	strcpy(Keywords[60], "H_ANG");
	strcpy(Keywords[61], "E_TAP");
	strcpy(Keywords[62], "H_TAP");
	strcpy(Keywords[63], "POWER");
	strcpy(Keywords[64], "dB");
	strcpy(Keywords[65], "LH");
	strcpy(Keywords[66], "RH");
	strcpy(Keywords[67], "PHASE_SHAPING");
	strcpy(Keywords[68], "PHASE_SCANNING");
	strcpy(Keywords[69], "PHASE_POL_ARRANG");
	strcpy(Keywords[70], "THETA");
	strcpy(Keywords[71], "PHI");
	strcpy(Keywords[72], "PSI");
	strcpy(Keywords[73], "NODE");
	strcpy(Keywords[74], "P");
	strcpy(Keywords[75], "Q");
	strcpy(Keywords[76], "BLOCK");
	strcpy(Keywords[77], "POLYGON");
	strcpy(Keywords[78], "HEXAGON");
	strcpy(Keywords[79], "ADD");
	strcpy(Keywords[80], "REMOVE");
	strcpy(Keywords[81], "SECOND");
	strcpy(Keywords[82], "ORDER");
	strcpy(Keywords[83], "CENTRE");

	strcpy(Keywords[84], "P1_VAL");
	strcpy(Keywords[85], "P2_VAL");
	strcpy(Keywords[86], "Q1_VAL");
	strcpy(Keywords[87], "Q2_VAL");


	strcpy(Keywords[88], "UNIFORM");


	strcpy(Keywords[89], "P1_VAL");
	strcpy(Keywords[90], "P2_VAL");
	strcpy(Keywords[91], "Q1_VAL");
	strcpy(Keywords[92], "Q2_VAL");

	strcpy(Keywords[93], "CW");
	strcpy(Keywords[94], "CCW");
	strcpy(Keywords[95], "START");
	strcpy(Keywords[96], "POINTING");
	strcpy(Keywords[97], "U");
	strcpy(Keywords[98], "V");
	strcpy(Keywords[99], "ROTATION");
	strcpy(Keywords[100], "SEQUENTIAL");
	strcpy(Keywords[101], "GROUP_EXCITATION");
	strcpy(Keywords[102], "ADD_GROUP");
	strcpy(Keywords[103], "AT");
	strcpy(Keywords[104], "PLACE_GROUP");
	strcpy(Keywords[105], "DISPLACE");
	strcpy(Keywords[106], "MOVE");
	strcpy(Keywords[107], "TO");
	strcpy(Keywords[108], "");
	strcpy(Keywords[109], "");
	strcpy(Keywords[110], "");
	strcpy(Keywords[111], "");
	strcpy(Keywords[112], "");
	strcpy(Keywords[113], "");
	strcpy(Keywords[114], "");
	strcpy(Keywords[115], "");
	strcpy(Keywords[116], "");
	strcpy(Keywords[117], "");
	strcpy(Keywords[118], "");
	strcpy(Keywords[119], "");
	strcpy(Keywords[120], "");
	strcpy(Keywords[121], "");
	strcpy(Keywords[122], "");
	strcpy(Keywords[123], "");
	strcpy(Keywords[124], "");
	strcpy(Keywords[125], "");


	for (i = 0; (i <= KWDSNUM); i++) {
		if (strlen(Keywords[i]) > KWDSLEN) {
			printf("\n ******** ERROR: KEYWORD no.%d TOO LONG - CORRECT Keywords[] ASSIGNMENT\n", i);
			interror("kwdsinit()");
		};
	};

#ifdef DEBUG4

	for (i = 0; ((i <= KWDSNUM) && (Keywords[i][0])); i++)
		printf("%d - %s\n", i, Keywords[i]);
	waitcont();
#endif
}




int 
lenunit(struct charac ** tp, int *unit)
{

	struct charac  *curr, **curr_ptr;


	curr_ptr = &curr;
	*curr_ptr = *tp;


	*unit = MM_UNIT;

	if (GetKeyword(Keywords[6], curr_ptr) == 0)
		*unit = MM_UNIT;
	else if (GetKeyword(Keywords[7], curr_ptr) == 0)
		*unit = CM_UNIT;
	else if (GetKeyword(Keywords[8], curr_ptr) == 0)
		*unit = DM_UNIT;
	else if (GetKeyword(Keywords[9], curr_ptr) == 0)
		*unit = MT_UNIT;
	else
		return 14;



	*tp = *curr_ptr;
	return 0;
}



int 
linconv(double *x_ptr, int unit)
{

	switch (unit) {


		case LINEAR_UNIT:
		break;

	case POWER_UNIT:
		*x_ptr = sqrt(*x_ptr);
		break;

	case DB_UNIT:
		*x_ptr = pow(10, ((*x_ptr) / 20));
		break;


	default:
		interror("linconv() -1-");

	}

	return 0;
}





int 
linpol(struct charac * p1, struct charac ** pp2,
       double *angle_ptr,
       int *angle_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione linpol()\n");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[15], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = polor(*curr_ptr, curr_ptr, angle_ptr, angle_unit_ptr);
	if (error == 1) {
		*angle_ptr = DEF_ELEM_LIN_POL_ANGLE;
		*angle_unit_ptr = DEF_ELEM_LIN_POL_ANGLE_UNIT;
		*pp2 = *curr_ptr;
		return 0;
	};


	if (error == 17) {
		printf("\n%s", ErrorMessages[31]);
		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}



int 
mksblock(struct AddRem * addrem_ptr,
	 struct Geomnode ** first_node_ptr,
	 struct Geomnode ** last_node_ptr,
	 double pstep,
	 double qstep,
	 double angle,
	 int *nodes_num_ptr,
	 int elem_shape,
	 double elem_pdim,
	 double elem_qdim
)
{

	struct Geomnode *app_ptr = NULL, *primo_ptr = NULL, *ultimo_ptr = NULL;
	int             p, q;
	int             p1, q1, p2, q2;
	double          x, y;

	int            *pp1 = &p1, *pp2 = &p2, *qq1 = &q1, *qq2 = &q2;

#ifdef DEBUG1
	printf("\nFunzione mksblock()");
#endif


	p1 = (addrem_ptr->NODE_PTR)->PCOORD;
	q1 = (addrem_ptr->NODE_PTR)->QCOORD;
	p2 = ((addrem_ptr->NODE_PTR)->NEXT)->PCOORD;
	q2 = ((addrem_ptr->NODE_PTR)->NEXT)->QCOORD;



#ifdef DEBUG5
	printf("\np1,q1=%d,%d", p1, q1);
	printf("\np2,q2=%d,%d", p2, q2);
#endif


	*nodes_num_ptr = 0;



	for (q = intmin(q1, q2); q <= intmax(q1, q2); q++) {
		for (p = intmin(p1, p2); p <= intmax(p1, p2); p++) {

#ifdef DEBUG5
			printf("\np,q=%d,%d", p, q);
#endif

			app_ptr = (struct Geomnode *) malloc(sizeof(struct Geomnode));

			if (app_ptr == NULL) {
				printf("%s", MOSErrors[0]);
				interror("mksblock()");
			};



			y = q * qstep * dsin(angle);
			x = p * pstep + q * qstep * dcos(angle);


			*nodes_num_ptr = *nodes_num_ptr + 1;

			app_ptr->LOC_P = p;
			app_ptr->LOC_Q = q;
			app_ptr->XA = x;
			app_ptr->YA = y;
			app_ptr->ZA = 0;
			app_ptr->SUBGROUP_PTR = NULL;
			app_ptr->SHAPE = elem_shape;
			app_ptr->PSIZE = elem_pdim;
			app_ptr->QSIZE = elem_qdim;

			app_ptr->THEA = 0;
			app_ptr->PHEA = 0;
			app_ptr->PSEA = 0;




			if (primo_ptr == NULL)
				primo_ptr = app_ptr;
			else
				ultimo_ptr->NEXT = app_ptr;


			ultimo_ptr = app_ptr;

			ultimo_ptr->NEXT = NULL;

		};
	};


	*first_node_ptr = primo_ptr;
	*last_node_ptr = ultimo_ptr;
	return 0;
}




int 
mkshex(struct AddRem * addrem_ptr,
       struct Geomnode ** first_node_ptr,
       struct Geomnode ** last_node_ptr,
       double pstep,
       double qstep,
       double angle,
       int *nodes_num_ptr,
       int elem_shape,
       double elem_pdim,
       double elem_qdim
)
{

	struct Geomnode *app_ptr = NULL, *primo_ptr = NULL, *ultimo_ptr = NULL;
	int             p, q, P[7 + 1], Q[7 + 1], i;
	double          x, y;

#ifdef DEBUG1
	printf("\nFunzione mkshex()");
#endif


	p = (addrem_ptr->NODE_PTR)->PCOORD;
	q = (addrem_ptr->NODE_PTR)->QCOORD;

#ifdef DEBUG5
	printf("\np,q=%d,%d", p, q);
#endif


	*nodes_num_ptr = 0;


	P[1] = p - 1;
	Q[1] = q + 1;
	P[2] = p;
	Q[2] = q + 1;
	P[3] = p + 1;
	Q[3] = q;
	P[4] = p + 1;
	Q[4] = q - 1;
	P[5] = p;
	Q[5] = q - 1;
	P[6] = p - 1;
	Q[6] = q;
	P[7] = p;
	Q[7] = q;


	for (i = 1; i <= 7; i++) {

#ifdef DEBUG5
		printf("\ni,p,q=%d,%d,%d", i, P[i], Q[i]);
#endif

		app_ptr = (struct Geomnode *) malloc(sizeof(struct Geomnode));

		if (app_ptr == NULL) {
			printf("%s", MOSErrors[0]);
			interror("mkshex()");
		};



		x = P[i] * pstep + Q[i] * qstep * dcos(angle);
		y = Q[i] * qstep * dsin(angle);



		*nodes_num_ptr = *nodes_num_ptr + 1;

		app_ptr->LOC_P = P[i];
		app_ptr->LOC_Q = Q[i];
		app_ptr->XA = x;
		app_ptr->YA = y;
		app_ptr->ZA = 0;
		app_ptr->SUBGROUP_PTR = NULL;
		app_ptr->SHAPE = elem_shape;
		app_ptr->PSIZE = elem_pdim;
		app_ptr->QSIZE = elem_qdim;

		app_ptr->THEA = 0;
		app_ptr->PHEA = 0;
		app_ptr->PSEA = 0;




		if (primo_ptr == NULL)
			primo_ptr = app_ptr;
		else
			ultimo_ptr->NEXT = app_ptr;


		ultimo_ptr = app_ptr;

		ultimo_ptr->NEXT = NULL;

	};


	*first_node_ptr = primo_ptr;
	*last_node_ptr = ultimo_ptr;
	return 0;
}





int 
mksnode(struct AddRem * addrem_ptr,
	struct Geomnode ** first_node_ptr,
	struct Geomnode ** last_node_ptr,
	double pstep,
	double qstep,
	double angle,
	int *nodes_num_ptr,
	int elem_shape,
	double elem_pdim,
	double elem_qdim
)
{

	struct Geomnode *app_ptr = NULL;
	int             p, q;
	double          x, y;

	int            *pp = &p, *qq = &q;


#ifdef DEBUG1
	printf("\nFunzione mksnode()");
#endif


	p = (addrem_ptr->NODE_PTR)->PCOORD;
	q = (addrem_ptr->NODE_PTR)->QCOORD;

#ifdef DEBUG5
	printf("\np,q=%d,%d", p, q);
#endif

	*nodes_num_ptr = 0;

	app_ptr = (struct Geomnode *) malloc(sizeof(struct Geomnode));

	if (app_ptr == NULL) {
		printf("%s", MOSErrors[0]);
		interror("mksnode()");
	};



	x = p * pstep + q * qstep * dcos(angle);
	y = q * qstep * dsin(angle);


	*nodes_num_ptr = *nodes_num_ptr + 1;

	app_ptr->LOC_P = p;
	app_ptr->LOC_Q = q;
	app_ptr->XA = x;
	app_ptr->YA = y;
	app_ptr->ZA = 0;
	app_ptr->SUBGROUP_PTR = NULL;
	app_ptr->NEXT = NULL;
	app_ptr->SHAPE = elem_shape;
	app_ptr->PSIZE = elem_pdim;
	app_ptr->QSIZE = elem_qdim;


	fixnodor(addrem_ptr->NODE_PTR);

	app_ptr->THEA = (addrem_ptr->NODE_PTR)->THEA;
	app_ptr->PHEA = (addrem_ptr->NODE_PTR)->PHEA;
	app_ptr->PSEA = (addrem_ptr->NODE_PTR)->PSEA;


	*first_node_ptr = app_ptr;
	*last_node_ptr = app_ptr;
	return 0;
}




int 
mmconv(double *x_ptr, int unit)
{



	switch (unit) {




		case MM_UNIT:
		break;

	case CM_UNIT:
		*x_ptr *= 10;
		break;

	case DM_UNIT:
		*x_ptr *= 100;
		break;

	case MT_UNIT:
		*x_ptr *= 1000;
		break;


	default:
		interror("mmconv -1- ");

	}

	return 0;
}







int 
modspec(struct charac * p1, struct charac ** pp2,
	int *model_ptr,
	double *exp_ptr,
	double *anglee_ptr,
	double *angleh_ptr,
	int *angle_unit_ptr,
	double *tapere_ptr,
	double *taperh_ptr,
	int *taper_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione modspec()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[30], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 1, Keywords[30]);

		return 1;
	};


	*model_ptr = 0;
	error = extmod(*curr_ptr, curr_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		return 17;
	};




	*model_ptr = 1;
	error = cosinus(*curr_ptr, curr_ptr, exp_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		return 17;
	};


	*model_ptr = 2;
	error = gaussel(*curr_ptr, curr_ptr, anglee_ptr, angleh_ptr, angle_unit_ptr, tapere_ptr, taperh_ptr, taper_unit_ptr);


	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[28]);
		return 17;
	};


	*model_ptr = 0;
	parserro(*curr_ptr, 35, " ");
	return 17;
}





void 
moseinit(void)
{

	int             i;

#ifdef DEBUG1
	printf("\nFUNZIONE moseinit INIZIALIZZAZIONE messaggi d'errore Modulo Scrittura \n");
#endif

	for (i = 0; (i <= MOSEMSGSNUM); i++)
		MOSErrors[i][0] = '\0';

	strcpy(MOSErrors[0], "** ERROR  B0: Out of memory during MOS.");
	strcpy(MOSErrors[1], "** ERROR  B1: External ELEMENT not suitable for GROUP: ");
	strcpy(MOSErrors[2], "** WARNING B2: GRID step(s) too small for specified element in GROUP: ");
	strcpy(MOSErrors[3], "** ERROR  B3: RECTANGULAR element not allowed on HEXAGONAL grid in GROUP: ");
	strcpy(MOSErrors[4], "** ERROR  B4: GRID angle out of non ambiguity range; angle=");
	strcpy(MOSErrors[5], "** ERROR  B5: SECOND ORDER law not allowed on irregular groups.");
	strcpy(MOSErrors[6], "** ERROR  B6: AMPLITUDE excitation error in GROUP: ");
	strcpy(MOSErrors[7], "** ERROR  B7: Only CIRCULAR elements allowed for ADD HEXAGON statement.");
	strcpy(MOSErrors[8], "** ERROR  B8: Error in group geometry; GROUP: ");
	strcpy(MOSErrors[9], "** ERROR  B9: PHASE excitation error in GROUP: ");
	strcpy(MOSErrors[10], "** ERROR B10: SEQUENTIAL ROTATION not allowed to multiport elements.");
	strcpy(MOSErrors[11], "** ERROR B11: SEQUENTIAL ROTATION not allowed on irregular shaped groups.");
	strcpy(MOSErrors[12], "** WARNING B12: GRID step(s) too small for sequential rotation in GROUP: ");
	strcpy(MOSErrors[13], "** ERROR B13: Cannot open output file:");
	strcpy(MOSErrors[14], "** ERROR B14: Simple element duplicate definition in GROUP: ");
	strcpy(MOSErrors[15], "** ERROR B15: ");
	strcpy(MOSErrors[16], "** ERROR B16: ");
	strcpy(MOSErrors[17], "** ERROR B17: ");
	strcpy(MOSErrors[18], "** ERROR B18: ");
	strcpy(MOSErrors[19], "** ERROR B19: ");



}




int 
nodecoor(struct charac * p1, struct charac ** pp2,
	 int *pcoord_ptr,
	 int *qcoord_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione nodecoor()");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[74], curr_ptr));
	error = 0;


	error = (GetInt(*curr_ptr, curr_ptr, pcoord_ptr));
	if (error != 0) {

		return 17;
	};


	error = (GetKeyword(Keywords[75], curr_ptr));
	error = 0;


	error = (GetInt(*curr_ptr, curr_ptr, qcoord_ptr));
	if (error != 0) {

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}


int 
nodedef(struct charac * p1, struct charac ** pp2,
	struct Node ** node_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;




	int             pcoord, *pcoord_ptr = &pcoord;
	int             qcoord, *qcoord_ptr = &qcoord;
	double          thea, *thea_ptr = &thea;
	double          phea, *phea_ptr = &phea;
	double          psea, *psea_ptr = &psea;
	int             angle_unit, *angle_unit_ptr = &angle_unit;
	int             omit_orient;

#ifdef DEBUG1
	printf("\nFunzione nodedef()");
#endif


	*curr_ptr = p1;
	error = 0;
	omit_orient = NO;


	*node_ptr = NULL;


	error = (GetKeyword(Keywords[73], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = nodecoor(*curr_ptr, curr_ptr, pcoord_ptr, qcoord_ptr);

	if (error == 1) {
		parserro(*curr_ptr, 52, " ");

		return 17;
	};

	if (error == 17) {
		parserro(*curr_ptr, 53, " ");

		return 17;
	};


	if (error == 0) {
		error = nodorien(*curr_ptr, curr_ptr, thea_ptr, phea_ptr, psea_ptr, angle_unit_ptr);
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[54]);
		return 17;
	};

	if (error == 1) {
		omit_orient = YES;

	};




	*node_ptr = (struct Node *) malloc(sizeof(struct Node));

	if (*node_ptr == NULL) {
		parserro(*curr_ptr, 55, " ");

		interror("nodedef()");
	};




	(*node_ptr)->NEXT = NULL;

	(*node_ptr)->PCOORD = pcoord;
	(*node_ptr)->QCOORD = qcoord;
	(*node_ptr)->THEA = thea;
	(*node_ptr)->PHEA = phea;
	(*node_ptr)->PSEA = psea;
	(*node_ptr)->ANGLE_UNIT = angle_unit;
	(*node_ptr)->OMIT_ORIENT = omit_orient;


	*pp2 = *curr_ptr;
	return 0;
}




int 
nodorien(struct charac * p1, struct charac ** pp2,
	 double *theta_ptr,
	 double *phi_ptr,
	 double *psi_ptr,
	 int *angle_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione nodorien()");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[37], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[70], curr_ptr));
	error = 0;


	error = (GetReal(theta_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 19, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[71], curr_ptr));
	error = 0;


	error = (GetReal(phi_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 19, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[72], curr_ptr));
	error = 0;


	error = (GetReal(psi_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 19, " ");

		return 17;
	};



	error = angunit(*curr_ptr, curr_ptr, angle_unit_ptr);
	if (error != 0)
		*angle_unit_ptr = DEGREES;


	*pp2 = *curr_ptr;
	return 0;
}




void 
nomefile(char fn[UNLEN + 1])
{





	strcpy(fn, GLOBLE_FILENAME);
	return;
}



#include "stdarg.h"

void 
parserro(struct charac * err_ptr, int errcode, char *s)
{

	struct charac  *p;
	int             n = 0, i;
	static int      errline_printed = NO;

	p = err_ptr;

	if (!errline_printed) {

		errline_printed = YES;


		printf("\nError occurred at line %d", err_ptr->LINE_NUM);


		while ((p->PREV != NULL) && ((p->PREV)->LINE_NUM == err_ptr->LINE_NUM)) {
			n++;
			p = p->PREV;
		};


		printf("\n%4d:\t", p->LINE_NUM);
		while ((p != NULL) && (p->LINE_NUM == err_ptr->LINE_NUM)) {
			printf("%c", p->info);
			p = p->NEXT;
		};


		printf("\n%    \t");
		for (i = 1; i <= n; i++) {
			printf(" ");

		};
		printf("^");
	};

	if (s == NULL)
		printf("\n%s\n", ErrorMessages[errcode]);
	else
		printf("\n%s%s\n", ErrorMessages[errcode], s);

	return;
}






int 
phaserot(struct charac * p1, struct charac ** pp2,
	 double *phase_start_ptr,
	 double *phase_step_ptr,
	 int *phase_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione phaserot()");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[43], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[95], curr_ptr));
	error = 0;


	error = (GetReal(phase_start_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 74, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[16], curr_ptr));
	error = 0;


	error = angval(*curr_ptr, curr_ptr, phase_step_ptr, phase_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 74, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}





int 
polor(struct charac * p1, struct charac ** pp2,
      double *angle_ptr,
      int *angle_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione polor()\n");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[37], curr_ptr));
	if (error != 0) {
		*angle_ptr = DEF_ELEM_LIN_POL_ANGLE;
		*angle_unit_ptr = DEF_ELEM_LIN_POL_ANGLE_UNIT;
		return 1;
	};


	*angle_ptr = 0;
	*angle_unit_ptr = DEGREES;
	error = (GetKeyword(Keywords[12], curr_ptr));

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};




	*angle_ptr = 90;
	*angle_unit_ptr = DEGREES;
	error = (GetKeyword(Keywords[13], curr_ptr));

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};


	error = angval(*curr_ptr, curr_ptr, angle_ptr, angle_unit_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		parserro(*curr_ptr, 30, " ");

		return 17;
	};


	return 17;

}





int 
polorbis(struct charac * p1, struct charac ** pp2,
	 double *phepol_ptr,
	 int *phepol_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione polorbis()\n");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[41], curr_ptr));
	if (error != 0) {
		*phepol_ptr = DEF_ELEM_LIN_POL_ANGLE;
		*phepol_unit_ptr = DEF_ELEM_LIN_POL_ANGLE_UNIT;
		return 1;
	};


	*phepol_ptr = 0;
	*phepol_unit_ptr = DEGREES;
	error = (GetKeyword(Keywords[12], curr_ptr));

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};




	*phepol_ptr = 90;
	*phepol_unit_ptr = DEGREES;
	error = (GetKeyword(Keywords[13], curr_ptr));

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};


	error = angval(*curr_ptr, curr_ptr, phepol_ptr, phepol_unit_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		parserro(*curr_ptr, 30, " ");

		return 17;
	};


	return 17;

}








int 
polspec(struct charac * p1, struct charac ** pp2,
	int *polarization_ptr,
	double *phepol_ptr,
	int *phepol_unit_ptr,
	int *direction_ptr)
{


	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione polspec()");
#endif


	*curr_ptr = p1;
	error = 0;
	*polarization_ptr = DEF_ELEM_POL;
	*phepol_ptr = DEF_ELEM_LIN_POL_ANGLE;
	*phepol_unit_ptr = DEF_ELEM_LIN_POL_ANGLE_UNIT;
	*direction_ptr = DEF_CIRC_POL_DIR;


	error = (GetKeyword(Keywords[31], curr_ptr));
	if (error != 0) {
		return 1;
	};


	*polarization_ptr = 0;
	error = linpol(*curr_ptr, curr_ptr, phepol_ptr, phepol_unit_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[31]);
		return 17;
	};




	*polarization_ptr = 1;
	error = circpol(*curr_ptr, curr_ptr, direction_ptr);

	if (error == 0) {
		*pp2 = *curr_ptr;
		return 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[32]);
		return 17;
	};



	return 17;
}




int 
polydef(struct charac * p1, struct charac ** pp2,
	struct Node ** node_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error, cont = 0;


	struct Node    *primo_ptr = NULL, *ultimo_ptr = NULL, *app_ptr = NULL;




	int             pcoord, *pcoord_ptr = &pcoord;
	int             qcoord, *qcoord_ptr = &qcoord;

#ifdef DEBUG1
	printf("\nFunzione polydef()");
#endif


	*curr_ptr = p1;
	error = 0;

	*node_ptr = NULL;


	error = (GetKeyword(Keywords[77], curr_ptr));
	if (error != 0) {
		return 1;
	};







	while (1) {


		error = nodecoor(*curr_ptr, curr_ptr, pcoord_ptr, qcoord_ptr);

		if (error == 0) {
			cont = cont + 1;

			app_ptr = (struct Node *) malloc(sizeof(struct Node));

			if (app_ptr == NULL) {
				parserro(*curr_ptr, 55, " ");

				interror("polydef()");
			};

			app_ptr->PCOORD = pcoord;
			app_ptr->QCOORD = qcoord;
			app_ptr->THEA = 0;
			app_ptr->PHEA = 0;
			app_ptr->PSEA = 0;
			app_ptr->ANGLE_UNIT = 0;


			if (primo_ptr == NULL) {
				primo_ptr = app_ptr;
				ultimo_ptr = app_ptr;
				app_ptr->NEXT = NULL;
			} else {
				ultimo_ptr->NEXT = app_ptr;
				ultimo_ptr = app_ptr;
				ultimo_ptr->NEXT = NULL;
			};
		};

		if (error != 0) {
			break;
		};

	}


	if (cont < 3) {
		*node_ptr = NULL;
		parserro(*curr_ptr, 65, " ");

		return 17;
	};


	*node_ptr = primo_ptr;
	*pp2 = *curr_ptr;
	return 0;
}




int 
portamp(struct charac * p1, struct charac ** pp2,
	double *amp_ptr,
	int *amp_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione portamp()\n");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[42], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = ampval(*curr_ptr, curr_ptr, amp_ptr, amp_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 27, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}




int 
portdef(struct charac * p1, struct charac ** pp2, struct Port * PORT_PTR, int *portnum_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;




	double          amp, *amp_ptr = &amp;
	int             amp_unit, *amp_unit_ptr = &amp_unit;
	double          psc, *psc_ptr = &psc;
	int             psc_unit, *psc_unit_ptr = &psc_unit;
	double          psh, *psh_ptr = &psh;
	int             psh_unit, *psh_unit_ptr = &psh_unit;
	double          ppa, *ppa_ptr = &ppa;
	int             ppa_unit, *ppa_unit_ptr = &ppa_unit;
	double          phepol, *phepol_ptr = &phepol;
	int             phepol_unit, *phepol_unit_ptr = &phepol_unit;
	int             omit_pol, *omit_pol_ptr = &omit_pol;

#ifdef DEBUG1
	printf("\nFunzione portdef()");
#endif


	*curr_ptr = p1;
	error = 0;



	error = (GetKeyword(Keywords[40], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 1, Keywords[40]);

		return 1;
	};


	error = (GetNatur(*curr_ptr, curr_ptr, portnum_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		return 17;
	};




	*omit_pol_ptr = NO;
	error = polorbis(*curr_ptr, curr_ptr, phepol_ptr, phepol_unit_ptr);


	if (error == 1) {
		*omit_pol_ptr = YES;
		*phepol_ptr = DEF_ELEM_LIN_POL_ANGLE;
		*phepol_unit_ptr = DEF_ELEM_LIN_POL_ANGLE_UNIT;
	};


	if (error == 17) {
		printf("\n%s", ErrorMessages[38]);
		return 17;
	};


	error = portamp(*curr_ptr, curr_ptr, amp_ptr, amp_unit_ptr);


	if (error != 0) {
		printf("\n%s", ErrorMessages[39]);
		return 17;
	};


	error = pshdef1(*curr_ptr, curr_ptr, psh_ptr, psh_unit_ptr);

	if (error == 1) {
		*psh_ptr = 0;
		*psh_unit_ptr = 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[40]);
		return 17;
	};


	error = pscdef1(*curr_ptr, curr_ptr, psc_ptr, psc_unit_ptr);

	if (error == 1) {
		*psc_ptr = 0;
		*psc_unit_ptr = 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[40]);
		return 17;
	};


	error = ppadef1(*curr_ptr, curr_ptr, ppa_ptr, ppa_unit_ptr);

	if (error == 1) {
		*ppa_ptr = 0;
		*ppa_unit_ptr = 0;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[40]);
		return 17;
	};


	PORT_PTR->AMP = amp;
	PORT_PTR->AMP_UNIT = amp_unit;
	PORT_PTR->PSC = psc;
	PORT_PTR->PSC_UNIT = psc_unit;
	PORT_PTR->PSH = psh;
	PORT_PTR->PSH_UNIT = psh_unit;
	PORT_PTR->PPA = ppa;
	PORT_PTR->PPA_UNIT = ppa_unit;
	PORT_PTR->PHEPOL = phepol;
	PORT_PTR->PHEPOL_UNIT = phepol_unit;
	PORT_PTR->OMIT_POL = omit_pol;


	*pp2 = *curr_ptr;
	return 0;
}








int 
portspec(struct charac * p1, struct charac ** pp2,
	 int *nports_ptr,
	 struct Port ** port_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error, n;
	int             portnum, *portnum_ptr = &portnum;


	struct Port    *primo_ptr = NULL, *ultimo_ptr = NULL, *app_ptr = NULL;

#ifdef DEBUG1
	printf("\nFunzione portspec()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[39], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetNatur(*curr_ptr, curr_ptr, nports_ptr));
	if (error != 0) {
		*nports_ptr = 0;
		parserro(*curr_ptr, 93, " ");
		return 17;
	};





	for (n = 1; n <= *nports_ptr; n++) {

		app_ptr = (struct Port *) malloc(sizeof(struct Port));

		if (app_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("portspec()");
		};
		error = portdef(*curr_ptr, curr_ptr, app_ptr, portnum_ptr);


		if (error == 1) {
			parserro(*curr_ptr, 46, " ");

			return 17;
		};

		if (error == 17) {
			printf("\n%s%d", ErrorMessages[47], n);
			return 17;
		};



		app_ptr->PORTNUM = *portnum_ptr;




		if (primo_ptr == NULL)
			primo_ptr = app_ptr;
		else
			ultimo_ptr->NEXT = app_ptr;


		ultimo_ptr = app_ptr;


		ultimo_ptr->NEXT = NULL;

	};




	*port_ptr = primo_ptr;


	*pp2 = *curr_ptr;
	return 0;
}






int 
ppadef1(struct charac * p1, struct charac ** pp2,
	double *ppa_ptr,
	int *ppa_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione ppadef1()\n");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[69], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = angval(*curr_ptr, curr_ptr, ppa_ptr, ppa_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 37, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}




int 
pqlimits(struct Geomnode * gnode_ptr,
	 struct Geomnode ** pmin_ptr,
	 struct Geomnode ** pmax_ptr,
	 struct Geomnode ** qmin_ptr,
	 struct Geomnode ** qmax_ptr
)
{

	struct Geomnode *app_ptr;
	int             pmin = INT_MAX, pmax = INT_MIN, qmin = INT_MAX, qmax = INT_MIN;

#ifdef DEBUG1
	printf("\nFunzione pqlimits()");
#endif

	*pmin_ptr = *pmax_ptr = *qmin_ptr = *qmax_ptr = app_ptr = gnode_ptr;


	while (app_ptr != NULL) {

		if ((app_ptr->LOC_P) < pmin) {
			pmin = app_ptr->LOC_P;
			*pmin_ptr = app_ptr;
		};
		if ((app_ptr->LOC_P) > pmax) {
			pmax = app_ptr->LOC_P;
			*pmax_ptr = app_ptr;
		};
		if ((app_ptr->LOC_Q) < qmin) {
			qmin = app_ptr->LOC_Q;
			*qmin_ptr = app_ptr;
		};
		if ((app_ptr->LOC_Q) > qmax) {
			qmax = app_ptr->LOC_Q;
			*qmax_ptr = app_ptr;
		};

		app_ptr = app_ptr->NEXT;
	};

	return 0;
}




void 
prnfile(struct charac * firstel)
{

	struct charac  *p;
	char            ch = ' ';
	int             line_num = 0;

#ifdef DEBUG3
	printf("\nprnfile() - input file content. ");
#endif
	printf("\n");

	p = firstel;
	while ((p->NEXT) != NULL) {
		ch = p->info;

		if (p->LINE_NUM > line_num) {
			line_num = p->LINE_NUM;
			printf("\n%4d\t", line_num);
		}
		putchar(ch);
		p = p->NEXT;
	};

	printf("\n");
	return;
}




int 
pscdef1(struct charac * p1, struct charac ** pp2,
	double *psc_ptr,
	int *psc_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione pscdef1()\n");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[68], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = angval(*curr_ptr, curr_ptr, psc_ptr, psc_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 37, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}






int 
pshdef1(struct charac * p1, struct charac ** pp2,
	double *psh_ptr,
	int *psh_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione pshdef1()\n");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[67], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = angval(*curr_ptr, curr_ptr, psh_ptr, psh_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 37, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}


int 
readfil3(char filename[UNLEN + 1], struct charac ** firstel, struct charac ** lastel)
{

	FILE           *f;
	char            c;
	struct charac  *p;
	int             line_num = 1;
	char            CR = '\n';
	char            adlfilename[UNLEN + 1];

#ifdef DEBUG3
	printf("\nreadfil3()");
#endif

	*firstel = NULL;
	*lastel = NULL;


	strcpy(adlfilename, filename);
	strcat(adlfilename, ".adl");
	if ((f = fopen(adlfilename, "r")) == NULL) {
		printf("%s %s", ErrorMessages[21], adlfilename);
		return 21;
	};


	c = (char) getc(f);

	if (c == EOF) {
		printf("%s %s", ErrorMessages[23], adlfilename);
		return 23;
	};


	while (isspace(c) || (c == ',')) {
		if (c == CR)
			line_num++;
		c = (char) getc(f);
	};

	p = (struct charac *) malloc(sizeof(struct charac));
	if (p == NULL) {
		printf("%s", ErrorMessages[22]);
		return 22;
	};

	p->info = c;
	p->LINE_NUM = line_num;
	p->PREV = NULL;
	p->NEXT = NULL;

	*firstel = p;
	*lastel = p;
	printf("\n");

	c = (char) getc(f);


	while (c != EOF) {
		if (isspace(c) || (c == ',')) {
			do {
				if (c == CR)
					line_num++;
				c = (char) getc(f);
			} while (isspace(c) || (c == ','));

			p = (struct charac *) malloc(sizeof(struct charac));
			if (p == NULL) {
				printf("%s", ErrorMessages[22]);
				return 22;
			};
			p->info = ' ';
			p->LINE_NUM = line_num;
			p->PREV = *lastel;
			p->NEXT = NULL;
			(*lastel)->NEXT = p;
			*lastel = p;
		} else {

			p = (struct charac *) malloc(sizeof(struct charac));
			if (p == NULL) {
				printf("%s", ErrorMessages[22]);
				return 22;
			};

			p->info = c;
			p->LINE_NUM = line_num;

			p->PREV = *lastel;
			p->NEXT = NULL;
			(*lastel)->NEXT = p;
			*lastel = p;

			c = (char) getc(f);
		};

	};

	fclose(f);
	return 0;
}




int 
recgrdef(struct charac * p1,
	 struct charac ** pp2, double *pstep_ptr, double *qstep_ptr, int *pqstep_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;
	*pstep_ptr = 0;
	*qstep_ptr = 0;
	*pqstep_unit_ptr = 0;


	error = (GetKeyword(Keywords[18], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = doubstep(*curr_ptr, curr_ptr, pstep_ptr, qstep_ptr, pqstep_unit_ptr);
	if (error == 1) {
		*pstep_ptr = 0;
		*qstep_ptr = 0;
		*pqstep_unit_ptr = 0;

		*pp2 = *curr_ptr;
		return 0;
	};


	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		*pstep_ptr = 0;
		*qstep_ptr = 0;
		*pqstep_unit_ptr = 0;
		return 17;
	};




	*pp2 = *curr_ptr;
	return 0;
}




int 
recspec(struct charac * p1, struct charac ** pp2,
	double *pdim_ptr,
	double *qdim_ptr,
	int *pqdim_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[18], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[57], curr_ptr));
	error = 0;


	error = (Get1Real(*curr_ptr, curr_ptr, pdim_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[58], curr_ptr));
	error = 0;


	error = (Get1Real(*curr_ptr, curr_ptr, qdim_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		return 17;
	};


	error = lenunit(curr_ptr, pqdim_unit_ptr);
#ifdef DEBUG5
	printf("Unità di misura trovata %d", *pqdim_unit_ptr);
#endif
	if (error != 0)
		*pqdim_unit_ptr = 0;


	*pp2 = *curr_ptr;
	return 0;
}



int 
remdef(struct charac * p1, struct charac ** pp2,
       struct AddRem ** addrem_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

	struct Node    *no_ptr = NULL, **node_ptr = &no_ptr;


#ifdef DEBUG1
	printf("\nFunzione remdef()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[80], curr_ptr));
	if (error != 0) {
		*addrem_ptr = NULL;
		return 1;
	};






	error = nodedef(*curr_ptr, curr_ptr, node_ptr);


	if (error == 17) {
		*addrem_ptr = NULL;
		printf("\n%s", ErrorMessages[58]);
		return 17;
	};


	if (error == 0) {

		*addrem_ptr = (struct AddRem *) malloc(sizeof(struct AddRem));

		if (*addrem_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("remdef()");
		};

		(*addrem_ptr)->ADDREM_TYPE = REM_TYPE;
		(*addrem_ptr)->BLOCK_TYPE = NODE_BLOCK;
		(*addrem_ptr)->NODE_PTR = *node_ptr;
		(*addrem_ptr)->NEXT = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};






	error = blockdef(*curr_ptr, curr_ptr, node_ptr);

	if (error == 17) {
		*addrem_ptr = NULL;
		printf("\n%s", ErrorMessages[59]);
		return 17;
	};


	if (error == 0) {

		*addrem_ptr = (struct AddRem *) malloc(sizeof(struct AddRem));

		if (*addrem_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("remdef()");
		};

		(*addrem_ptr)->ADDREM_TYPE = REM_TYPE;
		(*addrem_ptr)->BLOCK_TYPE = BLOCK_BLOCK;
		(*addrem_ptr)->NODE_PTR = *node_ptr;
		(*addrem_ptr)->NEXT = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};





	error = polydef(*curr_ptr, curr_ptr, node_ptr);

	if (error == 17) {
		*addrem_ptr = NULL;
		printf("\n%s", ErrorMessages[60]);
		return 17;
	};


	if (error == 0) {

		*addrem_ptr = (struct AddRem *) malloc(sizeof(struct AddRem));

		if (*addrem_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("adddef()");
		};

		(*addrem_ptr)->ADDREM_TYPE = REM_TYPE;
		(*addrem_ptr)->BLOCK_TYPE = POLY_BLOCK;
		(*addrem_ptr)->NODE_PTR = *node_ptr;
		(*addrem_ptr)->NEXT = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};







	error = hexdef(*curr_ptr, curr_ptr, node_ptr);

	if (error == 17) {
		*addrem_ptr = NULL;
		printf("\n%s", ErrorMessages[61]);
		return 17;
	};


	if (error == 0) {

		*addrem_ptr = (struct AddRem *) malloc(sizeof(struct AddRem));

		if (*addrem_ptr == NULL) {
			parserro(*curr_ptr, 55, " ");

			interror("adddef()");
		};

		(*addrem_ptr)->ADDREM_TYPE = REM_TYPE;
		(*addrem_ptr)->BLOCK_TYPE = HEX_BLOCK;
		(*addrem_ptr)->NODE_PTR = *node_ptr;
		(*addrem_ptr)->NEXT = NULL;

		*pp2 = *curr_ptr;
		return 0;
	};






	*addrem_ptr = NULL;
	parserro(*curr_ptr, 63, " ");

	return 17;
}




int 
secoramp(struct charac * p1, struct charac ** pp2,
	 double *centre_ptr,
	 double *p1_et_ptr,
	 double *p2_et_ptr,
	 double *q1_et_ptr,
	 double *q2_et_ptr,
	 int *amp_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione secoramp()\n");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[81], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[82], curr_ptr));
	error = 0;


	error = (GetKeyword(Keywords[83], curr_ptr));
	error = 0;


	error = (GetReal(centre_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[84], curr_ptr));
	error = 0;


	error = (GetReal(p1_et_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[85], curr_ptr));
	error = 0;


	error = (GetReal(p2_et_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[86], curr_ptr));
	error = 0;


	error = (GetReal(q1_et_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[87], curr_ptr));
	error = 0;


	error = ampval(*curr_ptr, curr_ptr, q2_et_ptr, amp_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}





double 
secorder(double a,
	 double b,
	 double c,
	 double d,
	 double e,
	 double x,
	 double y
)
{


	double          z;

	z = (a * x * x + b * y * y + c * x + d * y + e);

	return z;

}



int 
secorpha(struct charac * p1, struct charac ** pp2,
	 double *centre_ptr,
	 double *p1_ep_ptr,
	 double *p2_ep_ptr,
	 double *q1_ep_ptr,
	 double *q2_ep_ptr,
	 int *phase_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione secorpha()");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[81], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[82], curr_ptr));
	error = 0;


	error = (GetKeyword(Keywords[83], curr_ptr));
	error = 0;


	error = (GetReal(centre_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[89], curr_ptr));
	error = 0;


	error = (GetReal(p1_ep_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[90], curr_ptr));
	error = 0;


	error = (GetReal(p2_ep_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[91], curr_ptr));
	error = 0;


	error = (GetReal(q1_ep_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	error = (GetKeyword(Keywords[92], curr_ptr));
	error = 0;


	error = angval(*curr_ptr, curr_ptr, q2_ep_ptr, phase_unit_ptr);
	if (error != 0) {
		parserro(*curr_ptr, 68, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}




void 
seqrothg(double angle_start,
	 double angle_step,
	 double phase_start,
	 double phase_step,
	 struct Geomnode * g
)
{



	double          cph, can;
	struct Geomnode *g_app_ptr;

#ifdef DEBUG1
	printf("\nseqrothg()");
#endif

	cph = phase_start;
	can = angle_start;
	g_app_ptr = g;




	while (g_app_ptr != NULL) {

		(g_app_ptr->GEOMPORT_PTR)->PPA = cph;

		g_app_ptr->PHEA = g_app_ptr->PHEA + can;




		cph += phase_step;




		can += angle_step;


		g_app_ptr = g_app_ptr->NEXT;

	};

	return;
}



void 
seqrotrg(double angle_start,
	 double angle_step,
	 double phase_start,
	 double phase_step,
	 struct Geomnode * g
)
{


	struct Geomnode *pmin_ptr, *pmax_ptr, *qmin_ptr, *qmax_ptr;
	int             pmin, pmax, qmin, qmax;
	int             p, q, pcorr, qcorr, p1, p2, q1, q2, nodes_num, endvisit, cont;
	double          cph, can;

#ifdef DEBUG1
	printf("\nseqrotrg()");
#endif


	pqlimits(g, &pmin_ptr, &pmax_ptr, &qmin_ptr, &qmax_ptr);

	pmin = pmin_ptr->LOC_P;
	pmax = pmax_ptr->LOC_P;
	qmin = qmin_ptr->LOC_Q;
	qmax = qmax_ptr->LOC_Q;


	if ((pmin > pmax) || (qmin > qmax)) {
		interror("seqrotrg() -1- ");
		exit(1);
	};

	cph = phase_start;
	can = angle_start;


	nodes_num = (pmax - pmin + 1) * (qmax - qmin + 1);
	endvisit = 0;
	cont = 0;

	while (!endvisit) {


		if ((pmin == pmax) && (qmin == qmax)) {
			gnodevis(pmin, qmin, cph, can, g);
			cph += phase_step;
			can += angle_step;
			cont++;
			if (cont == nodes_num)
				endvisit = 1;
		} else {


			p1 = pmin;
			p2 = pmax - 1;
			q = qmax;
			if (p2 >= p1) {
				pcorr = p1;
				while ((pcorr <= p2) && (!endvisit)) {
					gnodevis(pcorr, q, cph, can, g);
					cph += phase_step;
					can += angle_step;
					(cont)++;
					if (cont == nodes_num)
						endvisit = 1;
					pcorr++;
				};
			};


			q1 = qmax;
			q2 = qmin + 1;
			p = pmax;
			if (q1 >= q2) {
				qcorr = q1;
				while ((qcorr >= q2) && (!endvisit)) {
					gnodevis(p, qcorr, cph, can, g);
					cph += phase_step;
					can += angle_step;
					(cont)++;
					if (cont == nodes_num)
						endvisit = 1;
					qcorr--;
				};
			};


			p1 = pmax;
			p2 = pmin + 1;
			q = qmin;
			if (p1 >= p2) {
				pcorr = p1;
				while ((pcorr >= p2) && (!endvisit)) {
					gnodevis(pcorr, q, cph, can, g);
					cph += phase_step;
					can += angle_step;
					(cont)++;
					if (cont == nodes_num)
						endvisit = 1;
					pcorr--;
				};
			};


			q1 = qmin;
			q2 = qmax - 1;
			p = pmin;
			if (q2 >= q1) {
				qcorr = q1;
				while ((qcorr <= q2) && (!endvisit)) {
					gnodevis(p, qcorr, cph, can, g);
					cph += phase_step;
					can += angle_step;
					(cont)++;
					if (cont == nodes_num)
						endvisit = 1;
					qcorr++;
				};
			};


		};


		pmin++;
		pmax--;
		qmin++;
		qmax--;

	};

	return;

}



int 
serotdir(struct charac * p1, struct charac ** pp2,
	 int *direction_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione serotdir()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[93], curr_ptr));
	if (error == 0) {
		*direction_ptr = CW_ROTATION;
		*pp2 = *curr_ptr;
		return 0;
	};


	error = (GetKeyword(Keywords[94], curr_ptr));
	if (error == 0) {
		*direction_ptr = CCW_ROTATION;
		*pp2 = *curr_ptr;
		return 0;
	};



	return 1;
}




int 
serotpar(struct charac * p1, struct charac ** pp2,
	 double *angle_start_ptr,
	 double *angle_step_ptr,
	 int *omit_phase_ptr,
	 double *phase_start_ptr,
	 double *phase_step_ptr,
	 int *angle_unit_ptr,
	 int *phase_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione serotpar()");
#endif


	*curr_ptr = p1;
	error = 0;


	*omit_phase_ptr = NO;


	error = anglerot(*curr_ptr, curr_ptr, angle_start_ptr, angle_step_ptr, angle_unit_ptr);

	if (error == 17) {
		return 17;
	};

	if (error == 1) {
		*omit_phase_ptr = YES;
		return 1;
	};


	*omit_phase_ptr = NO;
	error = phaserot(*curr_ptr, curr_ptr, phase_start_ptr, phase_step_ptr, phase_unit_ptr);

	if (error == 17) {
		return 17;
	};

	if (error == 1) {
		*omit_phase_ptr = YES;
		*pp2 = *curr_ptr;
		return 0;
	};


	*omit_phase_ptr = NO;
	*pp2 = *curr_ptr;
	return 0;

}











int 
serotpha(struct charac * p1, struct charac ** pp2,
	 int *direction_ptr,
	 int *omit_angle_ptr,
	 double *angle_start_ptr,
	 double *angle_step_ptr,
	 int *omit_phase_ptr,
	 double *phase_start_ptr,
	 double *phase_step_ptr,
	 int *angle_unit_ptr,
	 int *phase_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione serotpha()");
#endif


	*curr_ptr = p1;
	error = 0;

	*omit_angle_ptr = NO;
	*omit_phase_ptr = NO;


	error = (GetKeyword(Keywords[99], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetKeyword(Keywords[100], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 1, Keywords[100]);
		return 17;
	};


	error = serotdir(*curr_ptr, curr_ptr, direction_ptr);

	if (error == 1) {
		*direction_ptr = DEF_ROTATION_DIRECTION;
	};

	error = serotpar(*curr_ptr, curr_ptr, angle_start_ptr, angle_step_ptr, omit_phase_ptr, phase_start_ptr, phase_step_ptr, angle_unit_ptr, phase_unit_ptr);

	if (error == 17) {
		return 17;
	};

	if (error == 1) {
		*omit_angle_ptr = YES;
	};


	*pp2 = *curr_ptr;
	return 0;
}






int 
sgramp2n(struct GrAmpExc * grampexc_ptr,
	 struct Geomnode * geomnode_ptr,
	 struct Port * port_ptr,
	 int group_shape
)
{

	struct Geomport *primo_ptr = NULL, *app_ptr = NULL, *ultimo_ptr = NULL;
	struct Geomnode *geomnode_app_ptr = NULL;
	struct Geomnode *pmin_tp = NULL, **pmin_ptr = &pmin_tp;
	struct Geomnode *pmax_tp = NULL, **pmax_ptr = &pmax_tp;
	struct Geomnode *qmin_tp = NULL, **qmin_ptr = &qmin_tp;
	struct Geomnode *qmax_tp = NULL, **qmax_ptr = &qmax_tp;
	struct Port    *port_app_ptr = NULL;
	double          centre;
	double          p1_et;
	double          p2_et;
	double          q1_et;
	double          q2_et;
	double          a, b, c, d, e, x1, y1;
	double          A, B, C, D, E;
	double          amp_val;
	double          p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y, cex, cey;
	double          Hx[7 + 1], Hy[7 + 1];
	int             i;

#ifdef DEBUG1
	printf("\nFunzione sgramp2n()");
#endif


	centre = grampexc_ptr->CENTRE;
	p1_et = grampexc_ptr->P1_ET;
	p2_et = grampexc_ptr->P2_ET;
	q1_et = grampexc_ptr->Q1_ET;
	q2_et = grampexc_ptr->Q2_ET;




	if (group_shape == RECTAN_SHAPE) {

		pqlimits(geomnode_ptr, pmin_ptr, pmax_ptr, qmin_ptr, qmax_ptr);
		p1x = (*pmin_ptr)->XA;
		p2x = (*pmax_ptr)->XA;
		q1y = (*qmin_ptr)->YA;
		q2y = (*qmax_ptr)->YA;
		p1y = (q1y + q2y) / 2;
		p2y = p1y;
		q1x = (p1x + p2x) / 2;
		q2x = q1x;
		cex = q1x;
		cey = p1y;
	} else if (group_shape == CIRCUL_SHAPE) {


		geomnode_app_ptr = geomnode_ptr;
		for (i = 1; i <= 7; i++) {
			/* add pointer check */
			if (geomnode_app_ptr) {
				Hx[i] = geomnode_app_ptr->XA;
				Hy[i] = geomnode_app_ptr->YA;
				geomnode_app_ptr = geomnode_app_ptr->NEXT;
			} else {
				printf("NULL POINTER!\n");
				exit(1);
			}
		};
		p1x = Hx[5];
		p1y = Hy[5];
		p2x = Hx[2];
		p2y = Hy[2];
		q1x = (Hx[6] + Hx[7]) / 2;
		q1y = Hy[6];
		q2x = (Hx[4] + Hx[3]) / 2;
		q2y = Hy[4];
		cex = Hx[1];
		cey = Hy[1];
	} else {
		printf("\n%s", MOSErrors[5]);
		return 5;
	};




	x1 = p2x - cex;
	y1 = q2y - cey;

	e = centre;

	if (x1 == 0) {
		a = c = 0;
	} else {
		a = ((p1_et + p2_et - 2 * e) / (2 * x1 * x1));
		c = ((a * x1 * x1 + e - p1_et) / x1);
	};
	if (y1 == 0) {
		b = d = 0;
	} else {
		b = ((q1_et + q2_et - 2 * e) / (2 * y1 * y1));
		d = ((b * y1 * y1 + e - q1_et) / y1);
	};

	A = a;
	B = b;
	C = c - 2 * a * cex;
	D = d - 2 * b * cey;
	E = a * cex * cex + b * cey * cey - c * cex - d * cey + e;


	geomnode_app_ptr = geomnode_ptr;
	port_app_ptr = port_ptr;


	while (geomnode_app_ptr != NULL) {


		amp_val = secorder(A, B, C, D, E, geomnode_app_ptr->XA, geomnode_app_ptr->YA);


		primo_ptr = ultimo_ptr = app_ptr = NULL;


		port_app_ptr = port_ptr;


		while (port_app_ptr != NULL) {


			app_ptr = (struct Geomport *) malloc(sizeof(struct Geomport));

			if (app_ptr == NULL) {
				printf("%s", ErrorMessages[55]);
				interror("sgramp2n()");
			};


			app_ptr->AMP = amp_val * port_app_ptr->AMP;



			if (primo_ptr == NULL)
				primo_ptr = app_ptr;
			else
				ultimo_ptr->NEXT = app_ptr;


			ultimo_ptr = app_ptr;

			ultimo_ptr->NEXT = NULL;


			port_app_ptr = port_app_ptr->NEXT;

		}




		geomnode_app_ptr->GEOMPORT_PTR = primo_ptr;


		geomnode_app_ptr = geomnode_app_ptr->NEXT;
	};

	return 0;
}




int 
sgrampun(struct GrAmpExc * grampexc_ptr,
	 struct Geomnode * geomnode_ptr,
	 struct Port * port_ptr
)
{

	struct Geomport *primo_ptr = NULL, *app_ptr = NULL, *ultimo_ptr = NULL;
	struct Geomnode *geomnode_app_ptr = NULL;
	struct Port    *port_app_ptr = NULL;
	double          unif_val;

#ifdef DEBUG1
	printf("\nFunzione sgrampun()");
#endif


	unif_val = grampexc_ptr->UNIF_VAL;


	geomnode_app_ptr = geomnode_ptr;
	port_app_ptr = port_ptr;

#ifdef DEBUG1
	printf("\nsgrampun()");
#endif



	while (geomnode_app_ptr != NULL) {


		primo_ptr = ultimo_ptr = app_ptr = NULL;


		port_app_ptr = port_ptr;


		while (port_app_ptr != NULL) {


			app_ptr = (struct Geomport *) malloc(sizeof(struct Geomport));

			if (app_ptr == NULL) {
				printf("%s", ErrorMessages[55]);
				interror("sgrampun()");
			};


			app_ptr->AMP = unif_val * port_app_ptr->AMP;



			if (primo_ptr == NULL)
				primo_ptr = app_ptr;
			else
				ultimo_ptr->NEXT = app_ptr;


			ultimo_ptr = app_ptr;

			ultimo_ptr->NEXT = NULL;


			port_app_ptr = port_app_ptr->NEXT;

		}




		geomnode_app_ptr->GEOMPORT_PTR = primo_ptr;


		geomnode_app_ptr = geomnode_app_ptr->NEXT;
	};

	return 0;
}






int 
sgrpha2n(struct GrPhaExc * grphaexc_ptr,
	 struct Geomnode * geomnode_ptr,
	 struct Port * port_ptr,
	 int group_shape
)
{

	struct Geomport *geomport_app_ptr = NULL;
	struct Geomnode *geomnode_app_ptr = NULL;
	struct Port    *port_app_ptr = NULL;
	struct Geomnode *pmin_tp = NULL, **pmin_ptr = &pmin_tp;
	struct Geomnode *pmax_tp = NULL, **pmax_ptr = &pmax_tp;
	struct Geomnode *qmin_tp = NULL, **qmin_ptr = &qmin_tp;
	struct Geomnode *qmax_tp = NULL, **qmax_ptr = &qmax_tp;
	double          centre;
	double          p1_ep;
	double          p2_ep;
	double          q1_ep;
	double          q2_ep;
	double          a, b, c, d, e, x1, y1;
	double          A, B, C, D, E;
	double          phase_val;
	double          p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y, cex, cey;
	double          Hx[7 + 1], Hy[7 + 1];
	int             i;

#ifdef DEBUG1
	printf("\nFunzione sgrpha2n()");
#endif


	centre = grphaexc_ptr->CENTRE;
	p1_ep = grphaexc_ptr->P1_EP;
	p2_ep = grphaexc_ptr->P2_EP;
	q1_ep = grphaexc_ptr->Q1_EP;
	q2_ep = grphaexc_ptr->Q2_EP;




	if (group_shape == RECTAN_SHAPE) {

		pqlimits(geomnode_ptr, pmin_ptr, pmax_ptr, qmin_ptr, qmax_ptr);
		p1x = (*pmin_ptr)->XA;
		p2x = (*pmax_ptr)->XA;
		q1y = (*qmin_ptr)->YA;
		q2y = (*qmax_ptr)->YA;
		p1y = (q1y + q2y) / 2;
		p2y = p1y;
		q1x = (p1x + p2x) / 2;
		q2x = q1x;
		cex = q1x;
		cey = p1y;
	} else if (group_shape == CIRCUL_SHAPE) {


		geomnode_app_ptr = geomnode_ptr;
		for (i = 1; i <= 7; i++) {
			/* add pointer check */
			if (geomnode_app_ptr) {
				Hx[i] = geomnode_app_ptr->XA;
				Hy[i] = geomnode_app_ptr->YA;
				geomnode_app_ptr = geomnode_app_ptr->NEXT;
			} else {
				printf("NULL POINTER!\n");
				exit(1);
			}
		};
		p1x = Hx[5];
		p1y = Hy[5];
		p2x = Hx[2];
		p2y = Hy[2];
		q1x = (Hx[6] + Hx[7]) / 2;
		q1y = Hy[6];
		q2x = (Hx[4] + Hx[3]) / 2;
		q2y = Hy[4];
		cex = Hx[1];
		cey = Hy[1];
	} else {
		printf("\n%s", MOSErrors[5]);
		return 5;
	};



	x1 = p2x - cex;
	y1 = q2y - cey;

	e = centre;



	if (x1 == 0) {
		a = c = 0;
	} else {
		a = ((p1_ep + p2_ep - 2 * e) / (2 * x1 * x1));
		c = ((a * x1 * x1 + e - p1_ep) / x1);
	};
	if (y1 == 0) {
		b = d = 0;
	} else {
		b = ((q1_ep + q2_ep - 2 * e) / (2 * y1 * y1));
		d = ((b * y1 * y1 + e - q1_ep) / y1);
	};


	A = a;
	B = b;
	C = c - 2 * a * cex;
	D = d - 2 * b * cey;
	E = a * cex * cex + b * cey * cey - c * cex - d * cey + e;


	geomnode_app_ptr = geomnode_ptr;


	while (geomnode_app_ptr != NULL) {


		phase_val = secorder(A, B, C, D, E, geomnode_app_ptr->XA, geomnode_app_ptr->YA);


		geomport_app_ptr = geomnode_app_ptr->GEOMPORT_PTR;

		port_app_ptr = port_ptr;


		while (port_app_ptr != NULL) {




			geomport_app_ptr->PSH = phase_val + port_app_ptr->PSH;

			geomport_app_ptr->PSC = port_app_ptr->PSC;
			geomport_app_ptr->PPA = port_app_ptr->PPA;


			port_app_ptr = port_app_ptr->NEXT;
			geomport_app_ptr = geomport_app_ptr->NEXT;
		}


		geomnode_app_ptr = geomnode_app_ptr->NEXT;
	};

	return 0;
}




int 
sgrphasr(struct Group * group_ptr)
{

	struct Geomnode *geomnode_app_ptr = NULL;
	double          angle_start, angle_step, phase_start, phase_step, sign;

#ifdef DEBUG1
	printf("\nsgrphasr()");
#endif



	if ((group_ptr->ELEM_PTR)->NPORTS > 1) {
		printf("\n%s", MOSErrors[10]);
		return 10;
	};

	if (group_ptr->SHAPE == IRR_SHAPE) {
		printf("\n%s", MOSErrors[11]);
		return 11;
	};




	if ((group_ptr->GRPHAEXC_PTR)->DIRECTION == CCW_ROTATION)
		sign = +1;
	else
		sign = -1;



	if (((group_ptr->GRPHAEXC_PTR)->OMIT_ANGLE == YES) && ((group_ptr->GRPHAEXC_PTR)->OMIT_PHASE == YES)) {


		if (group_ptr->NUM_OF_NODES <= 2) {
			angle_start = phase_start = 0;
			angle_step = phase_step = sign * 90;
		} else {
			angle_start = phase_start = 0;
			angle_step = phase_step = sign * 360 / (group_ptr->NUM_OF_NODES);
		};

	} else if ((group_ptr->GRPHAEXC_PTR)->OMIT_PHASE == YES) {
		angle_start = (group_ptr->GRPHAEXC_PTR)->ANGLE_START;
		angle_step = (group_ptr->GRPHAEXC_PTR)->ANGLE_STEP;
		phase_start = angle_start;
		phase_step = angle_step;

	} else {
		angle_start = (group_ptr->GRPHAEXC_PTR)->ANGLE_START;
		angle_step = (group_ptr->GRPHAEXC_PTR)->ANGLE_STEP;
		phase_start = (group_ptr->GRPHAEXC_PTR)->PHASE_START;
		phase_step = (group_ptr->GRPHAEXC_PTR)->PHASE_STEP;
	};


	if (group_ptr->SHAPE == RECTAN_SHAPE) {
		seqrotrg(angle_start, angle_step, phase_start, phase_step, group_ptr->GEOMNODE_PTR);
	} else if (group_ptr->SHAPE == CIRCUL_SHAPE) {
		seqrothg(angle_start, angle_step, phase_start, phase_step, group_ptr->GEOMNODE_PTR);
	}
	geomnode_app_ptr = group_ptr->GEOMNODE_PTR;
	while (geomnode_app_ptr != NULL) {










		(geomnode_app_ptr->GEOMPORT_PTR)->PSH = (group_ptr->ELEM_PTR)->PORT_PTR->PSH;
		(geomnode_app_ptr->GEOMPORT_PTR)->PSC = (group_ptr->ELEM_PTR)->PORT_PTR->PSC;
		(geomnode_app_ptr->GEOMPORT_PTR)->PPA += (group_ptr->ELEM_PTR)->PORT_PTR->PPA;



		geomnode_app_ptr = geomnode_app_ptr->NEXT;

	};

	return 0;
}




int 
sgrphaun(struct GrPhaExc * grphaexc_ptr,
	 struct Geomnode * geomnode_ptr,
	 struct Port * port_ptr
)
{

	struct Geomport *geomport_app_ptr = NULL;
	struct Geomnode *geomnode_app_ptr = NULL;
	struct Port    *port_app_ptr = NULL;
	double          unif_val;

#ifdef DEBUG1
	printf("\nsgrphaun()");
#endif


	unif_val = grphaexc_ptr->UNIF_VAL;


	geomnode_app_ptr = geomnode_ptr;


	while (geomnode_app_ptr != NULL) {


		geomport_app_ptr = geomnode_app_ptr->GEOMPORT_PTR;

		port_app_ptr = port_ptr;


		while (port_app_ptr != NULL) {




			geomport_app_ptr->PSH = unif_val + port_app_ptr->PSH;

			geomport_app_ptr->PSC = port_app_ptr->PSC;
			geomport_app_ptr->PPA = port_app_ptr->PPA;


			port_app_ptr = port_app_ptr->NEXT;
			geomport_app_ptr = geomport_app_ptr->NEXT;
		}


		geomnode_app_ptr = geomnode_app_ptr->NEXT;
	};

	return 0;
}





int 
sgrrot(struct Geomnode * start_ptr,
       double XC,
       double YC,
       double phi
)
{

	struct Geomnode *app_ptr;
	double          XD, YD, XE, YE;

#ifdef DEBUG1
	printf("\nFunzione sgrrot()");
#endif

	app_ptr = start_ptr;


	if (app_ptr == NULL) {
		interror("sgrrot - 1 ");
	};

	while (app_ptr != NULL) {

		XD = app_ptr->XA;
		YD = app_ptr->YA;





		XE = ((XD - XC) * dcos(phi)) - ((YD - YC) * dsin(phi)) + XC;
		YE = ((XD - XC) * dsin(phi)) + ((YD - YC) * dcos(phi)) + YC;


		app_ptr->XA = XE;
		app_ptr->YA = YE;



		app_ptr->PHEA += phi;


		app_ptr = app_ptr->NEXT;
	};

	return 0;

}






int 
simamp(struct Group * group_ptr)
{


	int             error = 0;

#ifdef DEBUG1
	printf("\nFunzione simamp()");
#endif


	fixgramp(group_ptr->GRAMPEXC_PTR);


	switch ((group_ptr->GRAMPEXC_PTR)->TYPE) {

	case UNIFORM_LAW:
		error = sgrampun(group_ptr->GRAMPEXC_PTR, group_ptr->GEOMNODE_PTR, (group_ptr->ELEM_PTR)->PORT_PTR);
		break;

	case SECOND_ORDER_LAW:
		error = sgramp2n(group_ptr->GRAMPEXC_PTR, group_ptr->GEOMNODE_PTR, (group_ptr->ELEM_PTR)->PORT_PTR, group_ptr->SHAPE);
		break;


	};


	if (error != 0) {
		printf("\n%s%s", MOSErrors[6], group_ptr->NAME);
		return 6;
	} else
		return 0;
}





int 
simgroup(struct Group * group_ptr
)
{

	int             error = 0;

#ifdef DEBUG1
	printf("\nFunzione simgroup()");
#endif


	fixsgrel(group_ptr);


	error = addscan(group_ptr);

	if (error != 0) {
		printf("\n%s%s", MOSErrors[8], group_ptr->NAME);
		return 8;
	} else
		return 0;
}





int 
simpha(struct Group * group_ptr)
{

	int             error = 0;


	fixgrpha(group_ptr->GRPHAEXC_PTR);


	switch ((group_ptr->GRPHAEXC_PTR)->TYPE) {

	case UNIFORM_LAW:
		error = sgrphaun(group_ptr->GRPHAEXC_PTR, group_ptr->GEOMNODE_PTR, (group_ptr->ELEM_PTR)->PORT_PTR);
		break;

	case SECOND_ORDER_LAW:
		error = sgrpha2n(group_ptr->GRPHAEXC_PTR, group_ptr->GEOMNODE_PTR, (group_ptr->ELEM_PTR)->PORT_PTR, group_ptr->SHAPE);
		break;

	case ROTATION_SEQUENTIAL_LAW:
		error = sgrphasr(group_ptr);
		break;



	};


	if (error != 0) {
		printf("\n%s%s", MOSErrors[9], group_ptr->NAME);
		return 9;
	} else
		return 0;
}






int 
simpol(struct Group * group_ptr)
{

	struct Geomport *geomport_app_ptr;
	struct Geomnode *geomnode_app_ptr;
	struct Port    *port_app_ptr;

#ifdef DEBUG1
	printf("\nFunzione simpol()");
#endif









	geomnode_app_ptr = group_ptr->GEOMNODE_PTR;


	while (geomnode_app_ptr != NULL) {


		geomport_app_ptr = geomnode_app_ptr->GEOMPORT_PTR;

		port_app_ptr = (group_ptr->ELEM_PTR)->PORT_PTR;


		while (port_app_ptr != NULL) {


			geomport_app_ptr->PHEPOL = port_app_ptr->PHEPOL;


			port_app_ptr = port_app_ptr->NEXT;
			geomport_app_ptr = geomport_app_ptr->NEXT;
		}


		geomnode_app_ptr = geomnode_app_ptr->NEXT;
	};

	return 0;
}






int 
sinelem(struct charac * p1, struct charac ** pp2,
	struct Elem ** elem_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;




	int             geometry, *geometry_ptr = &geometry;
	double          radius, *radius_ptr = &radius;
	int             radius_unit, *radius_unit_ptr = &radius_unit;
	double          pdim, *pdim_ptr = &pdim;
	double          qdim, *qdim_ptr = &qdim;
	int             pqdim_unit, *pqdim_unit_ptr = &pqdim_unit;
	int             model, *model_ptr = &model;
	double          exp, *exp_ptr = &exp;
	double          anglee, *anglee_ptr = &anglee;
	double          angleh, *angleh_ptr = &angleh;
	int             angle_unit, *angle_unit_ptr = &angle_unit;
	double          tapere, *tapere_ptr = &tapere;
	double          taperh, *taperh_ptr = &taperh;
	int             taper_unit, *taper_unit_ptr = &taper_unit;
	int             polarization, *polarization_ptr = &polarization;
	double          phepol, *phepol_ptr = &phepol;
	int             phepol_unit, *phepol_unit_ptr = &phepol_unit;
	int             direction, *direction_ptr = &direction;
	double          gain, *gain_ptr = &gain;
	int             gain_unit, *gain_unit_ptr = &gain_unit;
	int             nports, *nports_ptr = &nports;
	struct Port    *p_ptr, **port_ptr = &p_ptr;

#ifdef DEBUG1
	printf("\nFunzione sinelem()");
#endif


	*curr_ptr = p1;
	error = 0;



	error = geomspec(*curr_ptr, curr_ptr, geometry_ptr, radius_ptr, radius_unit_ptr, pdim_ptr, qdim_ptr, pqdim_unit_ptr);


	if (error == 1) {
		return 1;
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[34]);
		return 17;
	};




	*model_ptr = 0;






	if (error == 0) {
		error = polspec(*curr_ptr, curr_ptr, polarization_ptr, phepol_ptr, phepol_unit_ptr, direction_ptr);
	};


	if (error == 17) {
		printf("\n%s", ErrorMessages[36]);
		return 17;
	};


	if (error == 1) {
		*polarization_ptr = DEF_ELEM_POL;
		*phepol_ptr = DEF_ELEM_LIN_POL_ANGLE;
		*phepol_unit_ptr = DEF_ELEM_LIN_POL_ANGLE_UNIT;
		*direction_ptr = DEF_CIRC_POL_DIR;
		error = 0;
	};



	*gain_ptr = 0;
	*gain_unit_ptr = 0;




	if (error == 0) {
		error = portspec(*curr_ptr, curr_ptr, nports_ptr, port_ptr);
	};

	if (error == 17) {
		printf("\n%s", ErrorMessages[49]);
		return 17;
	};

	if (error == 1) {
		*nports_ptr = 0;
		*port_ptr = NULL;
		error = 0;
	};




	(*elem_ptr)->TYPE = SIMPLE_ELEMENT;
	strcpy((*elem_ptr)->NAME, "");

	(*elem_ptr)->GEOMETRY = geometry;
	(*elem_ptr)->RADIUS = radius;
	(*elem_ptr)->RADIUS_UNIT = radius_unit;
	(*elem_ptr)->PDIM = pdim;
	(*elem_ptr)->QDIM = qdim;
	(*elem_ptr)->PQDIM_UNIT = pqdim_unit;
	(*elem_ptr)->MODEL = model;
	(*elem_ptr)->EXP = exp;
	(*elem_ptr)->ANGLEE = anglee;
	(*elem_ptr)->ANGLEH = angleh;
	(*elem_ptr)->ANGLE_UNIT = angle_unit;
	(*elem_ptr)->TAPERE = tapere;
	(*elem_ptr)->TAPERH = taperh;
	(*elem_ptr)->TAPER_UNIT = taper_unit;
	(*elem_ptr)->POLARIZATION = polarization;
	(*elem_ptr)->PHEPOL = phepol;
	(*elem_ptr)->PHEPOL_UNIT = phepol_unit;
	(*elem_ptr)->DIRECTION = direction;
	(*elem_ptr)->GAIN = gain;
	(*elem_ptr)->GAIN_UNIT = gain_unit;
	(*elem_ptr)->NPORTS = nports;
	(*elem_ptr)->PORT_PTR = *port_ptr;



	*pp2 = *curr_ptr;
	return 0;
}







int 
sinstep(struct charac * p1,
	struct charac ** pp2, double *step_ptr, int *step_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;
	*step_ptr = 0;
	*step_unit_ptr = 0;


	error = (GetKeyword(Keywords[16], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetUReal(*curr_ptr, curr_ptr, step_ptr));
	if (error != 0) {
		*step_ptr = 0;
		return error;
	};



	error = lenunit(curr_ptr, step_unit_ptr);
	if (error != 0)
		*step_unit_ptr = 0;


	*pp2 = *curr_ptr;
	return 0;
}



int 
squgrdef(struct charac * p1,
	 struct charac ** pp2, double *pstep_ptr, int *pstep_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;
	*pstep_ptr = 0;
	*pstep_unit_ptr = 0;


	error = (GetKeyword(Keywords[17], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = sinstep(*curr_ptr, curr_ptr, pstep_ptr, pstep_unit_ptr);
	if (error == 1) {
		*pstep_ptr = 0;
		*pstep_unit_ptr = 0;

		*pp2 = *curr_ptr;
		return 0;
	};


	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		*pstep_ptr = 0;
		*pstep_unit_ptr = 0;
		return 17;
	};




	*pp2 = *curr_ptr;
	return 0;
}




char 
TapeGet(struct charac ** tp)
{

	char            ch;

	if (*tp == NULL)
		ch = '\0';
	else {
		ch = (*tp)->info;
		(*tp) = (*tp)->NEXT;
	};
	return ch;
}





int 
trigrdef(struct charac * p1,
	 struct charac ** pp2, double *angle_ptr, int *angle_unit_ptr, double *pstep_ptr, double *qstep_ptr, int *pqstep_unit_ptr,
	 double *px_ptr, double *py_ptr, double *qx_ptr, double *qy_ptr, int *pqxy_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;
	*angle_ptr = 0;
	*angle_unit_ptr = 0;
	*pstep_ptr = 0;
	*qstep_ptr = 0;
	*pqstep_unit_ptr = 0;
	*px_ptr = 0;
	*py_ptr = 0;
	*qx_ptr = 0;
	*qy_ptr = 0;
	*pqxy_unit_ptr = 0;


	error = (GetKeyword(Keywords[21], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = angstep(*curr_ptr, curr_ptr, angle_ptr, angle_unit_ptr, pstep_ptr, qstep_ptr, pqstep_unit_ptr);





	if (error == 17) {

		*angle_ptr = 0;
		*angle_unit_ptr = 0;
		*pstep_ptr = 0;
		*qstep_ptr = 0;
		*pqstep_unit_ptr = 0;
		parserro(*curr_ptr, 94, " ");
		return 17;
	};


	if (error != 0) {
		error = versdef(*curr_ptr, curr_ptr, px_ptr, py_ptr, qx_ptr, qy_ptr, pqxy_unit_ptr);

		if ((error != 1) && (error != 0)) {
			parserro(*curr_ptr, error, " ");

			*px_ptr = 0;
			*py_ptr = 0;
			*qx_ptr = 0;
			*qy_ptr = 0;
			*pqxy_unit_ptr = 0;
			return 17;
		};


		if (error == 1) {
			*angle_ptr = 0;
			*angle_unit_ptr = 0;
			*pstep_ptr = 0;
			*qstep_ptr = 0;
			*pqstep_unit_ptr = 0;
			*px_ptr = 0;
			*py_ptr = 0;
			*qx_ptr = 0;
			*qy_ptr = 0;
			*pqxy_unit_ptr = 0;
		};
	};




	*pp2 = *curr_ptr;
	return 0;

}





void 
unaminit(void)
{

	char            i;
#ifdef DEBUG1
	printf("\nFUNZIONE unaminit() INIZIALIZZAZIONE della tabella usernames \n");
#endif
	for (i = 0; (i <= UNNUM); i++)
		strcpy(UserNames[i], "");

}



int 
unifamp(struct charac * p1, struct charac ** pp2,
	double *unif_val_ptr,
	int *amp_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione unifamp()");
#endif


	*curr_ptr = p1;


	error = 0;



	error = (GetKeyword(Keywords[88], curr_ptr));

	if (error != 0) {
		return 1;
	};


	error = ampval(*curr_ptr, curr_ptr, unif_val_ptr, amp_unit_ptr);
	if (error != 0) {
		*unif_val_ptr = 0;
		*amp_unit_ptr = 0;
	};


	*pp2 = *curr_ptr;
	return 0;
}





int 
unifpha(struct charac * p1, struct charac ** pp2,
	double *unif_val_ptr,
	int *phase_unit_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione unifpha()\n");
#endif


	*curr_ptr = p1;


	error = 0;



	error = (GetKeyword(Keywords[88], curr_ptr));

	if (error != 0) {
		return 1;
	};


	error = angval(*curr_ptr, curr_ptr, unif_val_ptr, phase_unit_ptr);
	if (error != 0) {
		*unif_val_ptr = 0;
		*phase_unit_ptr = 0;
	};


	*pp2 = *curr_ptr;
	return 0;
}





int 
uvdir(struct charac * p1, struct charac ** pp2,
      double *u_ptr,
      double *v_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("Funzione uvdir()\n");
#endif


	*curr_ptr = p1;


	error = 0;


	error = (GetKeyword(Keywords[97], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (uvval(*curr_ptr, curr_ptr, u_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 71, " ");
		return 17;
	};


	error = (GetKeyword(Keywords[98], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, 1, Keywords[98]);
		return 17;
	};


	error = (uvval(*curr_ptr, curr_ptr, v_ptr));
	if (error != 0) {
		printf("\n%s", ErrorMessages[71]);
		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}






int 
uvval(struct charac * p1,
      struct charac ** pp2, double *val_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = (GetReal(val_ptr, curr_ptr));
	if (error != 0) {
		return 1;
	};


	if ((*val_ptr < -1) || (*val_ptr > 1)) {
		parserro(*curr_ptr, 72, " ");

		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}




int 
versdef(struct charac * p1,
	struct charac ** pp2, double *px_ptr, double *py_ptr, double *qx_ptr, double *qy_ptr, int *pqxy_unit_ptr)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;


	*curr_ptr = p1;


	error = 0;
	*px_ptr = 0;
	*py_ptr = 0;
	*qx_ptr = 0;
	*qy_ptr = 0;
	*pqxy_unit_ptr = 0;


	error = (GetKeyword(Keywords[53], curr_ptr));
	if (error != 0) {
		return 1;
	};


	error = (GetReal(px_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		*px_ptr = 0;
		*py_ptr = 0;
		*qx_ptr = 0;
		*qy_ptr = 0;
		*pqxy_unit_ptr = 0;
		return 17;
	};


	error = (GetKeyword(Keywords[54], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, Keywords[54]);

		return 17;
	};


	error = (GetReal(py_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		*px_ptr = 0;
		*py_ptr = 0;
		*qx_ptr = 0;
		*qy_ptr = 0;
		*pqxy_unit_ptr = 0;
		return 17;
	};


	error = (GetKeyword(Keywords[55], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, Keywords[55]);

		return 17;
	};


	error = (GetReal(qx_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		*px_ptr = 0;
		*py_ptr = 0;
		*qx_ptr = 0;
		*qy_ptr = 0;
		*pqxy_unit_ptr = 0;
		return 17;
	};


	error = (GetKeyword(Keywords[56], curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, Keywords[56]);

		return 17;
	};


	error = (GetReal(qy_ptr, curr_ptr));
	if (error != 0) {
		parserro(*curr_ptr, error, " ");

		*px_ptr = 0;
		*py_ptr = 0;
		*qx_ptr = 0;
		*qy_ptr = 0;
		*pqxy_unit_ptr = 0;
		return 17;
	};



	error = lenunit(curr_ptr, pqxy_unit_ptr);
	if (error != 0)
		*pqxy_unit_ptr = 0;


	*pp2 = *curr_ptr;
	return 0;
}



void 
waitcont(void)
{


}




int 
xycoord(struct charac * p1, struct charac ** pp2,
	double *xcoord_ptr,
	double *ycoord_ptr
)
{

	struct charac  *curr, **curr_ptr = &curr;
	int             error;

#ifdef DEBUG1
	printf("\nFunzione xycoord()");
#endif


	*curr_ptr = p1;
	error = 0;


	error = (GetKeyword(Keywords[12], curr_ptr));
	error = 0;


	error = (GetReal(xcoord_ptr, curr_ptr));
	if (error != 0) {
		return 17;
	};


	error = (GetKeyword(Keywords[13], curr_ptr));
	error = 0;


	error = (GetReal(ycoord_ptr, curr_ptr));
	if (error != 0) {
		return 17;
	};


	*pp2 = *curr_ptr;
	return 0;
}
