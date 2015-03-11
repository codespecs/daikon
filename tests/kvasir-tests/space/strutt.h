/* strutt.h */

/* PARAMETRI uso interno del programma */
#define UNLEN 30    				/* user names max length */
#define UNNUM 20   				/* max number of user names */
#define KWDSLEN 20				/* keywords max length */
#define KWDSNUM 125   			/* number of language keywords */
#define EMSGSLEN 100				/* PARSER error messages max length */
#define EMSGSNUM 100				/* number of Parser error messages */
#define MAX_NUM_OF_SUPGRPS 5	/* max number of supergroups that can use a certain group as an element */
#define MOSEMSGSNUM 20			/* number of MOS error messages */
#define MOSEMSGSLEN 100			/* MOS error messages max length */

/* Max length of numbers handled by parser (std atof() and atoi() functions size) */
#define MAX_REAL_LENGTH 100   			
#define MAX_INT_LENGTH 5	         	

#define PI 	3.141592654
#define RTD  180/PI     
#define DTR  PI/180


/*********	SWITCH VALUES *********/
/* Angle units */
#define DEGREES  0
#define RADIANTS 1

/* Amplitude units */
#define LINEAR_UNIT 0
#define POWER_UNIT  1
#define DB_UNIT     2

/* Length units */
#define MM_UNIT 0
#define CM_UNIT 1	
#define DM_UNIT 2
#define MT_UNIT 3
#define WL_UNIT 4

/* Type of polarization */
#define LIN_POL  0
#define CIRC_POL 1

/* Circular polarization direction */
#define LEFT_HAND 0
#define RIGHT_HAND 1

/*	Element type */   
#define SIMPLE_ELEMENT 0
#define SUBGROUP 1   
                           
/* Element geometry */                           
#define RECTANGULAR_ELEM   0
#define CIRCULAR_ELEM		1

                           
/* Element and Grid level */
#define EXTERNAL 0
#define INTERNAL 1

/*	Grid types */
#define SQU_GRID 0
#define REC_GRID 1
#define HEX_GRID 2
#define TRI_GRID 3
#define LIN_GRID 4

/*	Block types */
#define NODE_BLOCK  0
#define BLOCK_BLOCK 1
#define POLY_BLOCK  2
#define HEX_BLOCK   3

/*	AddRem type, type of ADD command stored into an AddRem struct */
#define ADD_TYPE 0
#define REM_TYPE 1

/*	LINEAR GRID orientation */
#define LIN_GRID_X_ORIENT 0  /* Xa axis */
#define LIN_GRID_Y_ORIENT 1  /* Ya axis */

/*	Group exc. law */   
#define UNIFORM_LAW             0 
#define SECOND_ORDER_LAW        1  
#define PQEXPRESSION      		  2
#define ROTATION_SEQUENTIAL_LAW 3
#define BEAM_POINTING_LAW       4

/* Sequential rotation rotating directon */
#define CW_ROTATION  0
#define CCW_ROTATION 1

/*	Boolean flag */
#define NO  0
#define YES 1

/*	Group shape */
#define NO_SHAPE		0 /* used during shape determination */
#define CIRCUL_SHAPE 2
#define RECTAN_SHAPE 3
#define IRR_SHAPE		5 

/* Surface type */ 
#define PLANE_SURF    0
#define CYLINDER_SURF 1

/* Cylindrical surface axis */ 
#define X_AXIS  0
#define Y_AXIS  1

/* Coordinates type (for AddGroup) */
#define PQ_COORD 0 
#define XY_COORD 1


/* ************************************	DEFAULTS *************************************** */             

/* Default element polarization */
#define DEF_ELEM_POL 0 		

/*	Linear polarization default oirientation angle (PHEPOL)and unit: Ye axis */
#define DEF_ELEM_LIN_POL_ANGLE 90    					
#define DEF_ELEM_LIN_POL_ANGLE_UNIT DEGREES 			

/* Circular polarization default direction */
#define DEF_CIRC_POL_DIR  LEFT_HAND
   
/*	Element default orientation (THEA,PHEA,PSEA) w.r.t. (xyz)a*/
#define DEF_THEA 0
#define DEF_PHEA 0      
#define DEF_PSEA 0

/*	LINEAR GRID default orientation: 0 deg=Xa axis */
#define DEF_LIN_GRID_ORIENT 0  

/*	Sequential rotation default direction */
#define DEF_ROTATION_DIRECTION CW_ROTATION /* per ora implementata solo clockwise (8/6) */

/* Uniform amplitude distribution default value */
#define DEF_AMPLITUDE_UNIFORM_VALUE 1
#define DEF_AMPLITUDE_UNIFORM_UNIT  LINEAR_UNIT

/* Uniform phase distribution default value */
#define DEF_PHASE_UNIFORM_VALUE 0
#define DEF_PHASE_UNIFORM_UNIT  DEGREES

/* Default angle for TRIANGULAR grid of circular elements: 
	must be within ambiguity range [45deg,90deg]*/
#define DEF_TRI_GRID_CIRC_EL_ANGLE  60



struct Surf {
	int 						TYPE;					/*	-1- */ 
	double         		RADIUS; 
 	int 						RADIUS_UNIT;		/* -2- */
	int						AXIS;					/* -3- */ 
};
/* -1- {PLANE_SURF (=0),CYLINDER_SURF (=1)}			*/
/* -3- {X_AXIS(=0),Y_AXIS(=1)}							*/

struct Disp {                           	 
	int						COORD_TYPE;			/* -4- */
	int						PCOORD;
	int						QCOORD;
	double					XCOORD;
	double					YCOORD;
};
/* -4- {0=pq,1=xy} 						*/

/* Strutture puntate da Array */
struct Grid {
	char          			NAME[UNLEN+1];
	int						LEVEL;  				/* -5- */
	int						TYPE;					/* -6- */
	int						ORIENT;				/* -7- */
	double        			ANGLE;
	int						ANGLE_UNIT;
	double					PSTEP;
	double					QSTEP;
	int						PQSTEP_UNIT;
	double					PX;                /* -8- */			
	double					PY;
	double					QX;
	double					QY;
	int						PQXY_UNIT;
	double					ROT_ANGLE;			
};
/* -5-  GridLevel {EXTERNAL(=0), INTERNAL(=1)} */
/* -6-  {SQU_GRID(=0),REC_GRID(=1),HEX_GRID(=2),TRI_GRID(=3),LIN_GRID(=4)} */
/* -7- 0=X,1=Y  */
/* -8- p and q axis vectors w.r.t (xy)a */
		
struct	Port {
	int						PORTNUM;				
	double				AMP;
	int						AMP_UNIT;			/* -10- */
	double				PSC;
	int						PSC_UNIT;
	double				PSH;
	int						PSH_UNIT;
	double				PPA;						/* -11- */
	int						PPA_UNIT;
	double				PHEPOL;
	int						PHEPOL_UNIT;
	int					OMIT_POL;				/* {NO(=0),YES(=1)}*/
	struct Port				*NEXT;
};
/* -10 -0=LINEAR_UNIT  1= POWER_UNIT  2=DB_UNIT */
/* -11- PHASE POLARIZATION ARRANGEMENT has to be added to PSC */

struct Elem {
	int	 			TYPE;						/* -12- */ 
	char        	NAME[UNLEN+1];
	int				LEVEL;					/* -13- */
	int			GEOMETRY;					/* -14- */
	double      	RADIUS;
	int	      	RADIUS_UNIT;
	double			PDIM;
	double			QDIM;
	int	      	PQDIM_UNIT;
	int			MODEL;						/* -15- */
	double			EXP;
	double			ANGLEE;					
	double			ANGLEH;
	int				ANGLE_UNIT;          /*	-16- */
	double			TAPERE;
	double			TAPERH;
	int				TAPER_UNIT;          /* -17- */
	int			POLARIZATION;				/* -18- */
	double			PHEPOL;
	int				PHEPOL_UNIT;
	int				DIRECTION;				/* -19- */
	double		GAIN;
	int				GAIN_UNIT;
	int				NPORTS;					/* -20- */	
	struct Port		*PORT_PTR;
};

/* -12-	{SIMPLE_ELEMENT(=0),SUBGROUP(=1)}  */
/* -13- 	ElemLevel {EXTERNAL(=0), INTERNAL(=1)} */
/* -14- 	Element GEOMETRY {RECTANGULAR_ELEM=0, CIRCULAR_ELEM=1} */
/* -15- 	Model {0=external,1=cosinus,2=gaussian}	*/
/* -16- 	DEGREES(=0), RADIANTS(=1) */ 
/* -17- 	0=LINEAR  1= POWER  2=dB */
/* -18- 	Polarization {0=LIN_POL,1=CIRC_POL} */
/* -19- 	Only for circ polar:  0=left hand   1=right hand */
/*	-20- 	0 if no ports specified by user. set to 1 by WMO */

struct Array {
	char          			NAME[UNLEN+1];
	struct Surf 			SURF;
	struct Grid				*GRID_PTR;
	struct Elem				*ELEM_PTR;
	struct Group			*GROUP_PTR;
	struct AddGroup		*ADDGROUP_PTR;
	struct Disp 			DISP;
};


/* Srutture contenute nella Group */

struct PQExp { int dummy; };

struct GrAmpExc {
	int						TYPE;					/* -24- */
	double					UNIF_VAL;			/* if omitted set =0 */
	double					CENTRE;
	double					P1_ET;
	double					P2_ET;
	double					Q1_ET;
	double					Q2_ET;
	int						AMP_UNIT;			/* -10- */
	struct PQExp			*PQEXP_PTR;
};
/* -24- {UNIFORM_LAW(=0),SECOND_ORDER_LAW (=1), PQEXPRESSION (=2)	} */

struct GrPhaExc {
	int						TYPE;					/* -25- */
	double					UNIF_VAL;
	double					CENTRE;
	double					P1_EP;
	double					P2_EP;
	double					Q1_EP;
	double					Q2_EP;
	int						DIRECTION;			/* -26- */					
	int						OMIT_ANGLE;       /* {YES=1, NO=0} */
	double					ANGLE_START;
	double					ANGLE_STEP;
	int						OMIT_PHASE;       /* {YES=1, NO=0} */
	double					PHASE_START;
	double					PHASE_STEP;
	double					U;
	double					V;
	double					THETA;
	double					PHI;	
	struct PQExp			*PQEXP_PTR;
	int						ANGLE_UNIT;          /*	-16- */
	int						PHASE_UNIT;			/* - 29 - */
};
/* -25- {UNIFORM_LAW(=0),SECOND_ORDER_LAW (=1), PQEXPRESSION (=2),
	ROTATION_SEQUENTIAL_LAW (=3),BEAM_POINTING_LAW (=4) } */
/* -26- 0=clockwise,1=counterclockwise */
/*	OMIT_PHASE if clause PHASE START ... STEP ... omitted by user */
/*	OMIT_ANGLE if clause ANGLE START ... STEP ... omitted by user */

struct Err {
	int					AMP_TYPE;				/* -27- */
	double					AMP_STDEV;
	double					AMP_LINOFFS;
	double					AMP_LINSTEP;
	double					AMP_2NDCENT;
	double					AMP_2NDPET;
	double					AMP_2NDQET;	
	int						AMP_UNIT;
	int					PHASE_TYPE;				/* -28- */
	double					PHASE_STDEV;
	double					PHASE_LINOFFS;
	double					PHASE_LINSTEP;
	double					PHASE_2NDCENT;
	double					PHASE_2NDPSID;
	double					PHASE_2NDQSID;
	double					PHASE_QUANTSTEP;	
	int						PHASE_UNIT;          /* - 29 - */
};
/* -27- Amp_Err_Type {0=gaussian,1=linear,2=quadratic} */
/* -28- Phase_Err_Type {0=gaussian,1=linear,2=quadratic,3=quantization} */
/*	-29- Phase unit = angle unit => 0=DEGREES= deg  1=RADIANTS= rad */
         
struct Fail {
	int						NUM_FAULTY_EL;
	double					FIXED_AMP;
	double					FIXED_PHASE;
};                                                       
      

struct Group	 {
	char          			NAME[UNLEN+1];
	struct Grid   			*GRID_PTR;
	struct Elem   			*ELEM_PTR;
	struct AddRem			*ADDREM_PTR;
	struct GrAmpExc		*GRAMPEXC_PTR;
	struct GrPhaExc		*GRPHAEXC_PTR;
 	struct Err    			*ERR_PTR;
	struct Fail 			*FAIL_PTR;
	struct Group 			*NEXT;
	int						NUM_OF_NODES;
	int						SHAPE;					
	double					PSIZE;					
	double					QSIZE;
	struct Geomnode		*GEOMNODE_PTR;
	int 						BUILT;					/* { YES, NO }*/
/*	array of pointers to supergroups */
	struct Group			*SUPER_GROUP_PTR[MAX_NUM_OF_SUPGRPS];
};

struct AddGroup {
	char						NAME[UNLEN+1];
	int						COORD_TYPE;		/* -21- */
	int						PCOORD;
	int						QCOORD;
	double					XCOORD;
	double					YCOORD;
	struct Group			*GROUP_PTR;
	struct AddGroup		*NEXT;
};
/* -21- {PQ_COORD(=0), XY_COORD(=1)} */

struct AddRem {         						
	int 						ADDREM_TYPE;      /* -22- */
	int						BLOCK_TYPE;			/* -23- */
	struct Node				*NODE_PTR;
	struct AddRem			*NEXT;
};        
/* -22- { ADD_TYPE=0 if ADD statement stored , 
			 REM_TYPE=1 if REMOVE statement stored } */
/* -23- { NODE_BLOCK=0,BLOCK_BLOCK=1,POLY_BLOCK=2,HEX_BLOCK=3} */

struct Node {
	int             		OMIT_ORIENT;       /* {YES=1, NO=0} */
	int						PCOORD;
	int						QCOORD;
	double					THEA;
	double					PHEA;
	double					PSEA;
	int						ANGLE_UNIT;          /*	-16- */
	struct Node				*NEXT;
};

/* Element of input "tape"  - list used for input by processor */
struct charac {
	char 					info; 
	int 					LINE_NUM;
	struct charac 		*PREV;
	struct charac 		*NEXT;
}; 

/* User names table */
char UserNames[UNNUM+1][UNLEN+1];

/* Keywords table */
char Keywords[KWDSNUM+1][KWDSLEN+1];
                       
/* Parser Error Messages table */
char ErrorMessages[EMSGSNUM+1][EMSGSLEN+1];

                        
                           /* * * * * * * * * * *  Writing MOdule * * * * * * * * * */
struct Geomnode {
	int					LOC_P;			
	int					LOC_Q;
	double		      XA;				
	double				YA;
	double				ZA;
	double				THEA;				
	double				PHEA;
	double				PSEA;
	struct Geomport	*GEOMPORT_PTR;	
	struct Geomnode	*NEXT;			
	struct Geomnode	*SUBGROUP_PTR; 
	int 					SHAPE;			
	double				PSIZE;			
	double 				QSIZE;
};

struct Geomport {
	double				AMP;				
	double				PSH;
	double				PSC;
	double				PPA;				
	double				PHEPOL;			
	struct Geomport   *NEXT;
};
                           
/* Writing MODule error messages table */
char MOSErrors[MOSEMSGSNUM+1][MOSEMSGSLEN+1];
                           
                           
