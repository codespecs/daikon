/****************************************************************************
 *               tokenize.cpp
 *
 * This module implements the first part of a two part parser for the scene
 * description files.  This phase changes the input file into tokens.
 *
 * This module tokenizes the input file and sends the tokens created
 * to the parser (the second stage).  Tokens sent to the parser contain a
 * token ID, the line number of the token, and if necessary, some data for
 * the token.
 *
 * from Persistence of Vision(tm) Ray Tracer version 3.6.
 * Copyright 1991-2003 Persistence of Vision Team
 * Copyright 2003-2004 Persistence of Vision Raytracer Pty. Ltd.
 *---------------------------------------------------------------------------
 * NOTICE: This source code file is provided so that users may experiment
 * with enhancements to POV-Ray and to port the software to platforms other
 * than those supported by the POV-Ray developers. There are strict rules
 * regarding how you are permitted to use this file. These rules are contained
 * in the distribution and derivative versions licenses which should have been
 * provided with this file.
 *
 * These licences may be found online, linked from the end-user license
 * agreement that is located at http://www.povray.org/povlegal.html
 *---------------------------------------------------------------------------
 * This program is based on the popular DKB raytracer version 2.12.
 * DKBTrace was originally written by David K. Buck.
 * DKBTrace Ver 2.0-2.12 were written by David K. Buck & Aaron A. Collins.
 *---------------------------------------------------------------------------
 * $File: //depot/povray/3.6-release/source/tokenize.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <ctype.h>
#include "frame.h"
#include "povray.h"
#include "parse.h"
#include "parsestr.h"
#include "povray.h"
#include "colour.h"
#include "render.h"
#include "texture.h"
#include "tokenize.h"
#include "express.h"
#include "matrices.h"
#include "optout.h"
#include "pov_util.h"
#include "pov_util.h"
#include "povmsend.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global variables
******************************************************************************/


/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

// needs to be a power of two
const int MAX_STRING_LEN_FAST = 256;
// this needs to be usable as bit mask, so keep it MAX_STRING_LEN_FAST - 1
const int MAX_STRING_LEN_MASK = (MAX_STRING_LEN_FAST - 1);



/*****************************************************************************
* Local typedefs
******************************************************************************/

const int MAX_INCLUDE_FILES = 32;

#define CALL(x) { if (!(x)) return (false); }

const int COND_STACK_SIZE = 200;

typedef enum cond_type
{
  ROOT_COND=0,
  WHILE_COND,
  IF_TRUE_COND,
  IF_FALSE_COND,
  ELSE_COND,
  SWITCH_COND,
  CASE_TRUE_COND,
  CASE_FALSE_COND,
  SKIP_TIL_END_COND,
  INVOKING_MACRO_COND,
  DECLARING_MACRO_COND
} COND_TYPE;


const int SYM_TABLE_SIZE = 257;


/*****************************************************************************
* Local variables
******************************************************************************/

typedef struct Sym_Table_Struct SYM_TABLE;

struct Sym_Table_Struct
{
  char *Table_Name;
  SYM_ENTRY *Table[SYM_TABLE_SIZE];
};

SYM_TABLE *Tables[MAX_NUMBER_OF_TABLES]; // GLOBAL VARIABLE

int Table_Index; // GLOBAL VARIABLE

char String_Fast_Buffer[MAX_STRING_LEN_FAST]; // GLOBAL VARIABLE

int String_Index = 0; // GLOBAL VARIABLE
int String_Buffer_Free = 0; // GLOBAL VARIABLE

char *String = NULL; // GLOBAL VARIABLE
char *String2 = NULL; // GLOBAL VARIABLE

/* moved here to allow reinitialization */

POV_LONG Current_Token_Count = 0; // GLOBAL VARIABLE

int token_count = 0; // GLOBAL VARIABLE

int line_count = 10; // GLOBAL VARIABLE

struct InputFileData
{
	POV_BASE_NAMESPACE::ITextStream *In_File;
	bool R_Flag;
};

int Include_File_Index; // GLOBAL VARIABLE
InputFileData *Input_File; // GLOBAL VARIABLE
InputFileData Include_Files[MAX_INCLUDE_FILES]; // GLOBAL VARIABLE

struct Token_Struct Token;

int Echo_Indx; // GLOBAL VARIABLE

typedef struct Cond_Stack_Entry CS_ENTRY;

struct Cond_Stack_Entry
{
  COND_TYPE Cond_Type;
  DBL Switch_Value;
  POV_BASE_NAMESPACE::ITextStream *While_File;
  POV_BASE_NAMESPACE::ITextStream *Macro_File;
  const char *Macro_Return_Name;
  int Macro_Same_Flag;
  POV_MACRO *PMac;
  POV_BASE_NAMESPACE::ITextStream::FilePos File_Pos;
};

CS_ENTRY *Cond_Stack; // GLOBAL VARIABLE
int CS_Index, Skipping, Inside_Ifdef, Inside_MacroDef; // GLOBAL VARIABLE

// WARNING: Changes to the use of this variable are very dangerous as it is
// used in many places assuming certain non-obvious side effects! [trf]
int Got_EOF; // GLOBAL VARIABLE

/*
 * Here are the reserved words.  If you need to add new words,
 * be sure to declare them in parse.h
 */

TOKEN Conversion_Util_Table[LAST_TOKEN]; // GLOBAL VARIABLE

const RESERVED_WORD Reserved_Words[LAST_TOKEN] = {
  {AA_THRESHOLD_TOKEN, "aa_threshold"},
  {AA_LEVEL_TOKEN, "aa_level"},
  {ABSORPTION_TOKEN, "absorption"},
  {ABS_TOKEN, "abs"},
  {ACCURACY_TOKEN, "accuracy"},
  {ACOSH_TOKEN, "acosh"},
  {ACOS_TOKEN, "acos"},
  {ADAPTIVE_TOKEN, "adaptive"},
  {ADC_BAILOUT_TOKEN, "adc_bailout"},
  {AGATE_TOKEN, "agate"},
  {AGATE_TURB_TOKEN, "agate_turb"},
  {ALL_INTERSECTIONS_TOKEN, "all_intersections"},
  {ALL_TOKEN, "all"},
  {ALPHA_TOKEN, "alpha"},
  {ALWAYS_SAMPLE_TOKEN, "always_sample"},
  {ALTITUDE_TOKEN, "altitude"},
  {AMBIENT_LIGHT_TOKEN, "ambient_light"},
  {AMBIENT_TOKEN, "ambient"},
  {AMPERSAND_TOKEN, "&"},
  {ANGLE_TOKEN, "angle"},
  {APERTURE_TOKEN, "aperture"},
  {APPEND_TOKEN, "append"},
  {ARC_ANGLE_TOKEN, "arc_angle"},
  {AREA_LIGHT_TOKEN, "area_light"},
  {ARRAY_ID_TOKEN, "array identifier"},
  {ARRAY_TOKEN, "array"},
  {ASCII_TOKEN, "ascii"},
  {ASC_TOKEN, "asc"},
  {ASINH_TOKEN, "asinh"},
  {ASIN_TOKEN, "asin"},
  {ASSUMED_GAMMA_TOKEN, "assumed_gamma"},
  {ATAN2_TOKEN, "atan2"},
  {ATANH_TOKEN, "atanh"},
  {ATAN_TOKEN, "atan"},
  {AT_TOKEN, "@"},
  {AVERAGE_TOKEN, "average"},
  {BACKGROUND_TOKEN, "background"},
  {BACK_QUOTE_TOKEN, "`"},
  {BACK_SLASH_TOKEN, "\\"},
  {BAR_TOKEN, "|"},
  {BEZIER_SPLINE_TOKEN, "bezier_spline"},
  {BICUBIC_PATCH_TOKEN, "bicubic_patch"},
  {BLACK_HOLE_TOKEN, "black_hole"},
  {BLOB_TOKEN, "blob"},
  {BLUE_TOKEN, "blue"},
  {BLUR_SAMPLES_TOKEN, "blur_samples"},
  {BOUNDED_BY_TOKEN, "bounded_by"},
  {BOXED_TOKEN, "boxed"},
  {BOX_TOKEN, "box"},
  {BOZO_TOKEN, "bozo"},
  {BREAK_TOKEN, "break"},
  {BRICK_SIZE_TOKEN, "brick_size"},
  {BRICK_TOKEN, "brick"},
  {BRIGHTNESS_TOKEN, "brightness" },
  {BRILLIANCE_TOKEN, "brilliance"},
  {BUMPS_TOKEN, "bumps"},
  {BUMP_MAP_TOKEN, "bump_map"},
  {BUMP_SIZE_TOKEN, "bump_size"},
  {B_SPLINE_TOKEN, "b_spline"},
  {CAMERA_ID_TOKEN, "camera identifier"},
  {CAMERA_TOKEN, "camera"},
  {CASE_TOKEN, "case"},
  {CAUSTICS_TOKEN, "caustics"},
  {CEIL_TOKEN, "ceil"},
  {CELLS_TOKEN, "cells"},
  {CHARSET_TOKEN, "charset"},
  {CHECKER_TOKEN, "checker"},
  {CHR_TOKEN, "chr"},
  {CIRCULAR_TOKEN,"circular"},
  {CLIPPED_BY_TOKEN, "clipped_by"},
  {CLOCK_DELTA_TOKEN, "clock_delta"},
  {CLOCK_ON_TOKEN, "clock_on"},
  {CLOCK_TOKEN, "clock"},
  {COLON_TOKEN, ":"},
  {COLOUR_ID_TOKEN, "colour identifier"},
  {COLOUR_KEY_TOKEN, "color keyword"},
  {COLOUR_MAP_ID_TOKEN, "colour map identifier"},
  {COLOUR_MAP_TOKEN, "color_map"},
  {COLOUR_MAP_TOKEN, "colour_map"},
  {COLOUR_TOKEN, "color"},
  {COLOUR_TOKEN, "colour"},
  {COMMA_TOKEN, ", "},
  {COMPONENT_TOKEN, "component"},
  {COMPOSITE_TOKEN, "composite"},
  {CONCAT_TOKEN, "concat"},
  {CONE_TOKEN, "cone"},
  {CONFIDENCE_TOKEN, "confidence"},
  {CONIC_SWEEP_TOKEN, "conic_sweep"},
  {CONTAINED_BY_TOKEN,"contained_by"},
  {CONTROL0_TOKEN, "control0"},
  {CONTROL1_TOKEN, "control1"},
  {COORDS_TOKEN, "coords" },
  {COSH_TOKEN, "cosh"},
  {COS_TOKEN, "cos"},
  {COUNT_TOKEN, "count" },
  {CRACKLE_TOKEN, "crackle"},
  {CRAND_TOKEN, "crand"},
  {CUBE_TOKEN, "cube"},
  {CUBIC_SPLINE_TOKEN, "cubic_spline"},
  {CUBIC_TOKEN, "cubic"},
  {CUBIC_WAVE_TOKEN, "cubic_wave"},
  {CYLINDER_TOKEN, "cylinder"},
  {CYLINDRICAL_TOKEN, "cylindrical"},
  {DASH_TOKEN, "-"},
  {DEBUG_TOKEN, "debug"},
  {DECLARE_TOKEN, "declare"},
  {DEFAULT_TOKEN, "default"},
  {DEFINED_TOKEN, "defined"},
  {DEGREES_TOKEN, "degrees"},
  {DENSITY_FILE_TOKEN, "density_file"},
  {DENSITY_ID_TOKEN, "density identifier"},
  {DENSITY_MAP_ID_TOKEN, "density_map identifier"},
  {DENSITY_MAP_TOKEN, "density_map"},
  {DENSITY_TOKEN, "density"},
  {DENTS_TOKEN, "dents"},
  {DF3_TOKEN, "df3"},
  {DIFFERENCE_TOKEN, "difference"},
  {DIFFUSE_TOKEN, "diffuse"},
  {DIMENSIONS_TOKEN, "dimensions"},
  {DIMENSION_SIZE_TOKEN, "dimension_size"},
  {DIRECTION_TOKEN, "direction"},
  {DISC_TOKEN, "disc"},
  {DISPERSION_TOKEN, "dispersion"},
  {DISPERSION_SAMPLES_TOKEN, "dispersion_samples"},
  {DISTANCE_MAXIMUM_TOKEN, "distance_maximum" },
  {DISTANCE_TOKEN, "distance"},
  {DIST_EXP_TOKEN, "dist_exp"},
  {DIV_TOKEN, "div"},
  {DOLLAR_TOKEN, "$"},
  {DOUBLE_ILLUMINATE_TOKEN, "double_illuminate"},
  {ECCENTRICITY_TOKEN, "eccentricity"},
  {ELSE_TOKEN, "else"},
  {EMISSION_TOKEN, "emission"},
  {EMPTY_ARRAY_TOKEN, "empty array"},
  {END_OF_FILE_TOKEN, "End of File"},
  {END_TOKEN, "end"},
  {EQUALS_TOKEN, "="},
  {ERROR_BOUND_TOKEN, "error_bound" },
  {ERROR_TOKEN, "error"},
  {EVALUATE_TOKEN, "evaluate"},
  {EXCLAMATION_TOKEN, "!"},
  {EXP_TOKEN, "exp"},
  {EXPONENT_TOKEN, "exponent"},
  {EXTERIOR_TOKEN, "exterior"},
  {EXTINCTION_TOKEN, "extinction"},
  {FACETS_TOKEN, "facets"},
  {FACE_INDICES_TOKEN, "face_indices"},
  {FADE_COLOUR_TOKEN, "fade_colour"},
  {FADE_COLOUR_TOKEN, "fade_color"},
  {FADE_DISTANCE_TOKEN, "fade_distance"},
  {FADE_POWER_TOKEN, "fade_power"},
  {FALLOFF_ANGLE_TOKEN, "falloff_angle"},
  {FALLOFF_TOKEN, "falloff"},
  {FALSE_TOKEN, "false"},
  {FCLOSE_TOKEN, "fclose"},
  {FILE_EXISTS_TOKEN, "file_exists"},
  {FILE_ID_TOKEN, "file identifier"},
  {FILL_LIGHT_TOKEN, "shadowless"},
  {FILTER_TOKEN, "filter"},
  {FINALCLOCK_TOKEN, "final_clock"},
  {FINALFRAME_TOKEN, "final_frame"},
  {FINISH_ID_TOKEN, "finish identifier"},
  {FINISH_TOKEN, "finish"},
  {FISHEYE_TOKEN, "fisheye"},
  {FLATNESS_TOKEN, "flatness"},
  {FLIP_TOKEN, "flip"},
  {FLOAT_FUNCT_TOKEN, "float function"},
  {FLOAT_ID_TOKEN, "float identifier"},
  {FLOAT_TOKEN, "float constant"},
  {FLOOR_TOKEN, "floor"},
  {FOCAL_POINT_TOKEN, "focal_point"},
  {FOG_ALT_TOKEN, "fog_alt"},
  {FOG_ID_TOKEN, "fog identifier"},
  {FOG_OFFSET_TOKEN, "fog_offset"},
  {FOG_TOKEN, "fog"},
  {FOG_TYPE_TOKEN, "fog_type"},
  {FOPEN_TOKEN, "fopen"},
  {FORM_TOKEN, "form"},
  {FRAMENUMBER_TOKEN, "frame_number"},
  {FREQUENCY_TOKEN, "frequency"},
  {FRESNEL_TOKEN, "fresnel"},
  {FUNCTION_TOKEN, "function"},
  {FUNCT_ID_TOKEN, "function identifier"},
  {VECTFUNCT_ID_TOKEN, "vector function identifier"},
  {GIF_TOKEN, "gif"},
  {GLOBAL_SETTINGS_TOKEN, "global_settings" },
  {GRADIENT_TOKEN, "gradient"},
  {GRANITE_TOKEN, "granite"},
  {GRAY_TOKEN, "gray"},
  {GRAY_THRESHOLD_TOKEN, "gray_threshold" },
  {GREEN_TOKEN, "green"},
  {HASH_TOKEN, "#"},
  {HAT_TOKEN, "^"},
  {HEIGHT_FIELD_TOKEN, "height_field"},
  {HEXAGON_TOKEN, "hexagon"},
  {HF_GRAY_16_TOKEN, "hf_gray_16" },
  {HIERARCHY_TOKEN, "hierarchy"},
  {HOLLOW_TOKEN, "hollow"},
  {HYPERCOMPLEX_TOKEN, "hypercomplex"},
  {IDENTIFIER_TOKEN, "undeclared identifier"},
  {IMAGE_WIDTH_TOKEN,"image_width"},
  {IMAGE_HEIGHT_TOKEN,"image_height"},
  {INITIALCLOCK_TOKEN, "initial_clock"},
  {INITIALFRAME_TOKEN, "initial_frame"},
  {INTERIOR_TEXTURE_TOKEN, "interior_texture"},
  {IFDEF_TOKEN, "ifdef"},
  {IFF_TOKEN, "iff"},
  {IFNDEF_TOKEN, "ifndef"},
  {IF_TOKEN, "if"},
  {IMAGE_MAP_TOKEN, "image_map"},
  {IMAGE_PATTERN_TOKEN, "image_pattern"},
  {INCLUDE_TOKEN, "include"},
  {INSIDE_TOKEN, "inside"},
  {INSIDE_VECTOR_TOKEN, "inside_vector"},
  {INTERIOR_ID_TOKEN, "interior identifier"},
  {INTERIOR_TOKEN, "interior"},
  {INTERPOLATE_TOKEN, "interpolate"},
  {INTERSECTION_TOKEN, "intersection"},
  {INTERVALS_TOKEN, "intervals"},
  {INT_TOKEN, "int"},
  {INVERSE_TOKEN, "inverse"},
  {IOR_TOKEN, "ior"},
  {IRID_TOKEN, "irid"},
  {IRID_WAVELENGTH_TOKEN, "irid_wavelength"},
  {ISOSURFACE_TOKEN, "isosurface"},
  {JITTER_TOKEN, "jitter"},
  {JULIA_TOKEN, "julia"},
  {JULIA_FRACTAL_TOKEN, "julia_fractal"},
  {JPEG_TOKEN, "jpeg"},
  {LAMBDA_TOKEN, "lambda"},
  {LATHE_TOKEN, "lathe"},
  {LEFT_ANGLE_TOKEN, "<"},
  {LEFT_CURLY_TOKEN, "{"},
  {LEFT_PAREN_TOKEN, "("},
  {LEFT_SQUARE_TOKEN, "["},
  {LEOPARD_TOKEN, "leopard"},
  {LIGHT_GROUP_TOKEN, "light_group"},
  {LIGHT_SOURCE_TOKEN, "light_source"},
  {LINEAR_SPLINE_TOKEN, "linear_spline"},
  {LINEAR_SWEEP_TOKEN, "linear_sweep"},
  {LN_TOKEN, "ln"},
  {LOAD_FILE_TOKEN, "load_file"},
  {LOCAL_TOKEN, "local"},
  {LOCATION_TOKEN, "location"},
  {LOG_TOKEN, "log"},
  {LOOKS_LIKE_TOKEN, "looks_like"},
  {LOOK_AT_TOKEN, "look_at"},
  {LOW_ERROR_FACTOR_TOKEN, "low_error_factor" },
  {MACRO_ID_TOKEN, "macro identifier"},
  {MACRO_TOKEN, "macro"},
  {MAGNET_TOKEN, "magnet"},
  {MAJOR_RADIUS_TOKEN, "major_radius"},
  {MANDEL_TOKEN, "mandel"},
  {MAP_TYPE_TOKEN, "map_type"},
  {MARBLE_TOKEN, "marble"},
  {MATERIAL_ID_TOKEN, "material identifier"},
  {MATERIAL_MAP_TOKEN, "material_map"},
  {MATERIAL_TOKEN, "material"},
  {MATRIX_TOKEN, "matrix"},
  {MAX_EXTENT_TOKEN, "max_extent"},
  {MAX_GRADIENT_TOKEN, "max_gradient"},
  {MAX_INTERSECTIONS, "max_intersections"},
  {MAX_ITERATION_TOKEN, "max_iteration"},
  {MAX_SAMPLE_TOKEN, "max_sample"},
  {MAX_TOKEN, "max"},
  {MAX_TRACE_LEVEL_TOKEN, "max_trace_level"},
  {MAX_TRACE_TOKEN, "max_trace"},
  {MEDIA_ATTENUATION_TOKEN, "media_attenuation"},
  {MEDIA_ID_TOKEN, "media identifier"},
  {MEDIA_INTERACTION_TOKEN, "media_interaction"},
  {MEDIA_TOKEN, "media"},
  {MERGE_TOKEN, "merge"},
  {MESH2_TOKEN, "mesh2"},
  {MESH_TOKEN, "mesh"},
  {METALLIC_TOKEN, "metallic"},
  {METHOD_TOKEN, "method"},
  {METRIC_TOKEN, "metric" },
  {MINIMUM_REUSE_TOKEN, "minimum_reuse" },
  {MIN_EXTENT_TOKEN, "min_extent" },
  {MIN_TOKEN, "min"},
  {MOD_TOKEN, "mod"},
  {MORTAR_TOKEN, "mortar"},
  {NATURAL_SPLINE_TOKEN, "natural_spline"},
  {NEAREST_COUNT_TOKEN, "nearest_count" },
  {NORMAL_INDICES_TOKEN, "normal_indices"},
  {NORMAL_MAP_ID_TOKEN, "normal_map identifier"},
  {NORMAL_MAP_TOKEN, "normal_map"},
  {NORMAL_VECTORS_TOKEN, "normal_vectors"},
  {NO_IMAGE_TOKEN, "no_image"},
  {NO_REFLECTION_TOKEN, "no_reflection"},
  {NO_SHADOW_TOKEN, "no_shadow"},
  {NO_TOKEN, "no"},
  {NUMBER_OF_WAVES_TOKEN, "number_of_waves"},
  {OBJECT_ID_TOKEN, "object identifier"},
  {OBJECT_TOKEN, "object"},
  {OCTAVES_TOKEN, "octaves"},
  {OFFSET_TOKEN, "offset"},
  {OFF_TOKEN, "off"},
  {OMEGA_TOKEN, "omega"},
  {OMNIMAX_TOKEN, "omnimax"},
  {ONCE_TOKEN, "once"},
  {ONION_TOKEN, "onion"},
  {ON_TOKEN, "on"},
  {OPEN_TOKEN, "open"},
  {ORIENTATION_TOKEN, "orientation"},
  {ORIENT_TOKEN,"orient"},
  {ORTHOGRAPHIC_TOKEN, "orthographic"},
  {PANORAMIC_TOKEN, "panoramic"},
  {PARALLEL_TOKEN, "parallel"},
  {PARAMETER_ID_TOKEN, "parameter identifier"},
  {PARAMETRIC_TOKEN,"parametric"},
  {PATTERN_TOKEN, "pattern"},
  {PERCENT_TOKEN, "%"},
  {PERIOD_TOKEN, ". (period)"},
  {PERSPECTIVE_TOKEN, "perspective"},
  {PGM_TOKEN, "pgm"},
  {PHASE_TOKEN, "phase"},
  {PHONG_SIZE_TOKEN, "phong_size"},
  {PHONG_TOKEN, "phong"},
  {PIGMENT_ID_TOKEN, "pigment identifier"},
  {PIGMENT_MAP_ID_TOKEN, "pigment_map identifier"},
  {PIGMENT_MAP_TOKEN, "pigment_map"},
  {PIGMENT_TOKEN, "pigment"},
  {PI_TOKEN, "pi"},
  {PLANAR_TOKEN, "planar"},
  {PLANE_TOKEN, "plane"},
  {PLUS_TOKEN, "+"},
  {PNG_TOKEN, "png"},
  {POINT_AT_TOKEN, "point_at"},
  {POLYGON_TOKEN, "polygon"},
  {POLY_TOKEN, "poly"},
  {POLY_WAVE_TOKEN, "poly_wave"},
  {POT_TOKEN, "pot"},
  {POW_TOKEN, "pow"},
  {PPM_TOKEN, "ppm"},
  {PRECISION_TOKEN, "precision"},
  {PRECOMPUTE_TOKEN,"precompute"},
  {PRETRACE_START_TOKEN, "pretrace_start"},
  {PRETRACE_END_TOKEN, "pretrace_end"},
  {PRISM_TOKEN, "prism"},
  {PROD_TOKEN, "prod"},
  {PROJECTED_THROUGH_TOKEN, "projected_through"},
  {PWR_TOKEN, "pwr"},
  {QUADRATIC_SPLINE_TOKEN, "quadratic_spline"},
  {QUADRIC_TOKEN, "quadric"},
  {QUARTIC_TOKEN, "quartic"},
  {QUATERNION_TOKEN, "quaternion"},
  {QUESTION_TOKEN, "?"},
  {QUICK_COLOUR_TOKEN, "quick_color"},
  {QUICK_COLOUR_TOKEN, "quick_colour"},
  {QUILTED_TOKEN, "quilted"},
  {RADIAL_TOKEN, "radial"},
  {RADIANS_TOKEN, "radians"},
  {RADIOSITY_TOKEN, "radiosity" },
  {RADIUS_TOKEN, "radius"},
  {RAINBOW_ID_TOKEN, "rainbow identifier"},
  {RAINBOW_TOKEN, "rainbow"},
  {RAMP_WAVE_TOKEN, "ramp_wave"},
  {RAND_TOKEN, "rand"},
  {RANGE_TOKEN, "range"},
  {RATIO_TOKEN, "ratio"},
  {READ_TOKEN, "read"},
  {RECIPROCAL_TOKEN, "reciprocal" },
  {RECURSION_LIMIT_TOKEN, "recursion_limit" },
  {RED_TOKEN, "red"},
  {REFLECTION_EXPONENT_TOKEN, "reflection_exponent"},
  {REFLECTION_TOKEN, "reflection"},
  {REFRACTION_TOKEN, "refraction"},
  {REL_GE_TOKEN, ">="},
  {REL_LE_TOKEN, "<="},
  {REL_NE_TOKEN, "!="},
  {RENDER_TOKEN, "render"},
  {REPEAT_TOKEN, "repeat"},
  {RGBFT_TOKEN, "rgbft"},
  {RGBF_TOKEN, "rgbf"},
  {RGBT_TOKEN, "rgbt"},
  {RGB_TOKEN, "rgb"},
  {RIGHT_ANGLE_TOKEN, ">"},
  {RIGHT_CURLY_TOKEN, "}"},
  {RIGHT_PAREN_TOKEN, ")"},
  {RIGHT_SQUARE_TOKEN, "]"},
  {RIGHT_TOKEN, "right"},
  {RIPPLES_TOKEN, "ripples"},
  {ROTATE_TOKEN, "rotate"},
  {ROUGHNESS_TOKEN, "roughness"},
  {SAMPLES_TOKEN, "samples"},
  {SAVE_FILE_TOKEN, "save_file"},
  {SCALE_TOKEN, "scale"},
  {SCALLOP_WAVE_TOKEN, "scallop_wave"},
  {SCATTERING_TOKEN, "scattering"},
  {SEED_TOKEN, "seed"},
  {SELECT_TOKEN, "select"},
  {SEMI_COLON_TOKEN, ";"},
  {SINE_WAVE_TOKEN, "sine_wave"},
  {SINGLE_QUOTE_TOKEN, "'"},
  {SINH_TOKEN, "sinh"},
  {SIN_TOKEN, "sin"},
  {SIZE_TOKEN, "size" },
  {SKYSPHERE_ID_TOKEN, "sky_sphere identifier"},
  {SKYSPHERE_TOKEN, "sky_sphere"},
  {SKY_TOKEN, "sky"},
  {SLASH_TOKEN, "/"},
  {SLICE_TOKEN, "slice"},
  {SLOPE_MAP_ID_TOKEN, "slope_map identifier"},
  {SLOPE_MAP_TOKEN, "slope_map"},
  {SLOPE_TOKEN,"slope"},
  {SMOOTH_TOKEN, "smooth"},
  {SMOOTH_TRIANGLE_TOKEN, "smooth_triangle"},
  {SOLID_TOKEN, "solid" },
  {SOR_TOKEN, "sor"},
  {SPECULAR_TOKEN, "specular"},
  {SPHERE_SWEEP_TOKEN, "sphere_sweep"},
  {SPHERE_TOKEN, "sphere"},
  {SPHERICAL_TOKEN, "spherical"},
  {SPIRAL1_TOKEN, "spiral1"},
  {SPIRAL2_TOKEN, "spiral2"},
  {SPLINE_ID_TOKEN, "spline identifier"},
  {SPLINE_TOKEN, "spline"},
  {SPOTLIGHT_TOKEN, "spotlight"},
  {SPOTTED_TOKEN, "spotted"},
  {SQRT_TOKEN, "sqrt"},
  {SQR_TOKEN, "sqr"},
  {STAR_TOKEN, "*"},
  {STATISTICS_TOKEN, "statistics"},
  {STRCMP_TOKEN, "strcmp"},
  {STRENGTH_TOKEN, "strength"},
  {STRING_ID_TOKEN, "string identifier"},
  {STRING_LITERAL_TOKEN, "string literal"},
  {STRLEN_TOKEN, "strlen"},
  {STRLWR_TOKEN, "strlwr"},
  {STRUPR_TOKEN, "strupr"},
  {STR_TOKEN, "str"},
  {STURM_TOKEN, "sturm"},
  {SUBSTR_TOKEN, "substr"},
  {SUM_TOKEN, "sum"},
  {SUPERELLIPSOID_TOKEN, "superellipsoid"},
  {SWITCH_TOKEN, "switch"},
  {SYS_TOKEN, "sys"},
  {TANH_TOKEN, "tanh"},
  {TAN_TOKEN, "tan"},
  {TEMPORARY_MACRO_ID_TOKEN, "unfinished macro declaration"},
  {TEXTURE_ID_TOKEN, "texture identifier"},
  {TEXTURE_LIST_TOKEN, "texture_list"},
  {TEXTURE_MAP_ID_TOKEN, "texture_map identifier"},
  {TEXTURE_MAP_TOKEN, "texture_map"},
  {TEXTURE_TOKEN, "texture"},
  {TEXT_TOKEN, "text"},
  {TGA_TOKEN, "tga"},
  {THICKNESS_TOKEN, "thickness"},
  {THRESHOLD_TOKEN, "threshold"},
  {TIFF_TOKEN, "tiff"},
  {TIGHTNESS_TOKEN, "tightness"},
  {TILDE_TOKEN, "~"},
  {TILE2_TOKEN, "tile2"},
  {TILES_TOKEN, "tiles"},
  {TNORMAL_ID_TOKEN, "normal identifier"},
  {TNORMAL_TOKEN, "normal"},
  {TOLERANCE_TOKEN, "tolerance"},
  {TOROIDAL_TOKEN, "toroidal"},
  {TORUS_TOKEN, "torus"},
  {TRACE_TOKEN, "trace"},
  {TRANSFORM_ID_TOKEN, "transform identifier"},
  {TRANSFORM_TOKEN, "transform"},
  {TRANSLATE_TOKEN, "translate"},
  {TRANSMIT_TOKEN, "transmit"},
  {TRIANGLE_TOKEN, "triangle"},
  {TRIANGLE_WAVE_TOKEN, "triangle_wave"},
  {TRUE_TOKEN, "true"},
  {TTF_TOKEN, "ttf"},
  {TURBULENCE_TOKEN, "turbulence"},
  {TURB_DEPTH_TOKEN, "turb_depth"},
  {TYPE_TOKEN, "type"},
  {T_TOKEN, "t"},
  {ULTRA_WIDE_ANGLE_TOKEN, "ultra_wide_angle"},
  {UNDEF_TOKEN, "undef"},
  {UNION_TOKEN, "union"},
  {UP_TOKEN, "up"},
  {USE_ALPHA_TOKEN, "use_alpha"},
  {USE_COLOUR_TOKEN, "use_color"},
  {USE_COLOUR_TOKEN, "use_colour"},
  {USE_INDEX_TOKEN, "use_index"},
  {UTF8_TOKEN, "utf8"},
  {UV_ID_TOKEN, "uv vector identifier"},
  {UV_INDICES_TOKEN, "uv_indices"},
  {UV_MAPPING_TOKEN, "uv_mapping"},
  {UV_VECTORS_TOKEN, "uv_vectors"},
  {U_STEPS_TOKEN, "u_steps"},
  {U_TOKEN, "u"},
  {VAL_TOKEN, "val"},
  {VARIANCE_TOKEN, "variance"},
  {VAXIS_ROTATE_TOKEN, "vaxis_rotate"},
  {VCROSS_TOKEN, "vcross"},
  {VDOT_TOKEN, "vdot"},
  {VECTOR_4D_ID_TOKEN, "4d-vector identifier"},
  {VECTOR_FUNCT_TOKEN, "vector function"},
  {VECTOR_ID_TOKEN, "vector identifier"},
  {VERSION_TOKEN, "version"},
  {VERTEX_VECTORS_TOKEN, "vertex_vectors"},
  {VLENGTH_TOKEN, "vlength"},
  {VNORMALIZE_TOKEN, "vnormalize"},
  {VROTATE_TOKEN, "vrotate"},
  {VSTR_TOKEN, "vstr"},
  {VTURBULENCE_TOKEN, "vturbulence"},
  {V_STEPS_TOKEN, "v_steps"},
  {V_TOKEN, "v"},
  {WARNING_TOKEN, "warning"},
  {WARP_TOKEN, "warp"},
  {WATER_LEVEL_TOKEN, "water_level"},
  {WAVES_TOKEN, "waves"},
  {WHILE_TOKEN, "while"},
  {WIDTH_TOKEN, "width"},
  {WOOD_TOKEN, "wood"},
  {WRINKLES_TOKEN, "wrinkles"},
  {WRITE_TOKEN, "write"},
  {X_TOKEN, "x"},
  {YES_TOKEN, "yes"},
  {Y_TOKEN, "y"},
  {Z_TOKEN, "z"},
  /* NK phmap */
  {PHOTONS_TOKEN, "photons"},
  {STEPS_TOKEN,"steps"},
  {PASS_THROUGH_TOKEN, "pass_through"},
  {COLLECT_TOKEN, "collect"},
  {AUTOSTOP_TOKEN, "autostop"},
  {GATHER_TOKEN, "gather"},
  {SPLIT_UNION_TOKEN, "split_union"},
  {EXPAND_THRESHOLDS_TOKEN, "expand_thresholds"},
  {SPACING_TOKEN, "spacing"},
#ifdef GLOBAL_PHOTONS
  {GLOBAL_TOKEN, "global"},
#endif
  {TARGET_TOKEN, "target"},
  {CONSERVE_ENERGY_TOKEN, "conserve_energy"},
  {CUTAWAY_TEXTURES_TOKEN, "cutaway_textures"},
  {PIGMENT_PATTERN_TOKEN,"pigment_pattern"},
  {NO_BUMP_SCALE_TOKEN, "no_bump_scale"},
  {GLOBAL_LIGHTS_TOKEN, "global_lights"},
  {INTERNAL_TOKEN, "internal"},
  {NOISE_GENERATOR_TOKEN, "noise_generator"}
};



/*****************************************************************************
* Static functions
******************************************************************************/

static void Echo_ungetc (int c);
static int Echo_getc (void);
static bool Skip_Spaces (void);
static int Parse_C_Comments (void);
inline void Begin_String (void);
inline void Stuff_Character (int c);
inline void End_String (void);
inline void Begin_String_Fast (void);
inline void Stuff_Character_Fast (int c);
inline void End_String_Fast (void);
static bool Read_Float (void);
static void Read_Symbol (void);
static SYM_ENTRY *Find_Symbol (int Index, char *s);
static void Skip_Tokens (COND_TYPE cond);

static int get_hash_value (char *s);
inline void Write_Token (TOKEN Token_Id, int col);
static void Destroy_Table (int index);
static void init_sym_tables (void);
static void Add_Sym_Table (const char *s);
static void Remove_Symbol (int Index, char *Name, bool is_array_elem, void **DataPtr, int ttype);
static POV_MACRO *Parse_Macro(void);
static void Invoke_Macro(void);
static void Return_From_Macro(void);
static void Add_Entry (int Index,SYM_ENTRY *Table_Entry);
static void Parse_Initalizer (int Sub, int Base, POV_ARRAY *a);

static void Parse_Fopen(void);
static void Parse_Fclose(void);
static void Parse_Read(void);
static void Parse_Write(void);
static int Parse_Read_Value(DATA_FILE *User_File,int Previous,int *NumberPtr,void **DataPtr);
static void Check_Macro_Vers(void);
static DBL Parse_Cond_Param(void);
static void Parse_Cond_Param2(DBL *V1,DBL *V2);
static void Inc_CS_Index(void);




/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Initialize_Tokenizer()
{
  IStream *rfile = NULL;
  char b[FILE_NAME_LENGTH];
  int c;

  Stage = STAGE_TOKEN_INIT;

  pre_init_tokenizer ();

  if (opts.Options & FROM_STDIN)
  {
    rfile = New_IStream("stdin", POV_File_Text_POV);
    if(rfile != NULL)
      Input_File->In_File = new ITextStream("stdin", rfile);
  }
  else
  {
    rfile = Locate_File(opts.Input_File_Name,POV_File_Text_POV,b,true);
    if(rfile != NULL)
    {
      Input_File->In_File = new ITextStream(b, rfile);
      strcpy(opts.Input_File_Name,b);
    }
  }

  if (Input_File->In_File == NULL)
  {
    Error ("Cannot open input file.");
  }

  Input_File->R_Flag = false;

  Got_EOF  = false;

  /* Init conditional stack. */

  Cond_Stack = (CS_ENTRY*)POV_MALLOC(sizeof(CS_ENTRY) * COND_STACK_SIZE, "conditional stack");

  Cond_Stack[0].Cond_Type    = ROOT_COND;
  Cond_Stack[0].Switch_Value = 0.0;

  init_sym_tables();
  Max_Trace_Level = 5;
  Had_Max_Trace_Level = false;

  /* ignore any leading characters if they have character codes above 127, this
     takes care of UTF-8 files with encoding info at the beginning of the file */
  for(c = Echo_getc(); c > 127; c = Echo_getc())
    opts.String_Encoding = 1; // switch to UTF-8 automatically [trf]
  Echo_ungetc(c);
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void pre_init_tokenizer ()
{
  int i;

  Token.Token_File_Pos.lineno = 0;
  Token.Token_File_Pos.offset = 0;
  Token.Token_Col_No = 0;
  Token.Token_String  = NULL;
  Token.Unget_Token   = false;
  Token.End_Of_File   = false;
  Token.Data = NULL;

  line_count = 10;
  token_count = 0;
  Current_Token_Count = 0;
  Include_File_Index = 0;
  Echo_Indx=0;

  CS_Index            = 0;
  Skipping            = false;
  Inside_Ifdef        = false;
  Inside_MacroDef     = false;
  Cond_Stack          = NULL;
  Input_File = &Include_Files[0];
  Table_Index         = -1;

  for(i = 0; i < LAST_TOKEN; i++)
  {
    Conversion_Util_Table[i] = i;
    if(i < FLOAT_FUNCT_TOKEN)
       Conversion_Util_Table[i] = FLOAT_FUNCT_TOKEN;
    else
    {
      if(i < VECTOR_FUNCT_TOKEN)
         Conversion_Util_Table[i] = VECTOR_FUNCT_TOKEN;
      else
      {
         if(i < COLOUR_KEY_TOKEN)
            Conversion_Util_Table[i] = COLOUR_KEY_TOKEN;
      }
    } 
  }
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Terminate_Tokenizer()
{
  while(Table_Index >= 0)
  {
     Destroy_Table(Table_Index--);
  }

  if(Input_File->In_File != NULL)
  {
    delete Input_File->In_File;
    Input_File->In_File = NULL;
    Got_EOF = false;
  }

  while(Include_File_Index >= 0)
  {
    Input_File = &Include_Files[Include_File_Index--];

    if(Input_File->In_File != NULL)
    {
      delete Input_File->In_File;
      Input_File->In_File = NULL;
      Got_EOF = false;
    }
  }

  if(Cond_Stack != NULL)
  {
    for(int i = 0; i <= CS_Index; i++)
    {
      if((Cond_Stack[i].Cond_Type == INVOKING_MACRO_COND) && (Cond_Stack[i].Macro_Same_Flag == false))
        delete Cond_Stack[i].Macro_File;
    }
    POV_FREE(Cond_Stack);

    Cond_Stack = NULL;
  }

  if((String != NULL) && (String != String_Fast_Buffer))
     POV_FREE(String);
  String=NULL;

  if((String2 != NULL) && (String2 != String_Fast_Buffer))
     POV_FREE(String2);
  String2=NULL;
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
*   The main tokenizing routine.  Set up the files and continue parsing
*   until the end of file
*
*   Read a token from the input file and store it in the Token variable.
*   If the token is an INCLUDE token, then set the include file name and
*   read another token.
*
*   This function performs most of the work involved in tokenizing.  It
*   reads the first character of the token and decides which function to
*   call to tokenize the rest.  For simple tokens, it simply writes them
*   out to the token buffer.
*
* CHANGES
*
******************************************************************************/

void Get_Token ()
{
  register int c,c2;
  int col;

  if (Token.Unget_Token)
  {
    Token.Unget_Token = false;

    return;
  }

  if (Token.End_Of_File)
  {
    return;
  }

  Token.Token_Id = END_OF_FILE_TOKEN;
  Token.is_array_elem = false;

  while (Token.Token_Id == END_OF_FILE_TOKEN)
  {
    Skip_Spaces();

    col = Echo_Indx;
    c = Echo_getc();

    if (c == EOF)
    {
      if (Input_File->R_Flag)
      {
        Token.Token_Id = END_OF_FILE_TOKEN;
        Token.is_array_elem = false;
        Token.End_Of_File = true;
        return;
      }

      if (Include_File_Index == 0)
      {
        if (CS_Index !=0)
          Error("End of file reached but #end expected.");

        Token.Token_Id = END_OF_FILE_TOKEN;
        Token.is_array_elem = false;

        Token.End_Of_File = true;

        return;
      }

      delete Input_File->In_File; /* added to fix open file buildup JLN 12/91 */
      Input_File->In_File = NULL ;
      Got_EOF=false;

      Destroy_Table(Table_Index--);

      Input_File = &Include_Files[--Include_File_Index];

      continue;
    }

    Begin_String_Fast();

    String[0] = c; /* This isn't necessary but helps debugging */

    String[1] = '\0';

    /*String_Index = 0;*/

    switch (c)
    {
      case '\n':
        break;

      case '{' :
        Write_Token (LEFT_CURLY_TOKEN, col);
        break;

      case '}' :
        Write_Token (RIGHT_CURLY_TOKEN, col);
        break;

      case '@' :
        Write_Token (AT_TOKEN, col);
        break;

      case '&' :
        Write_Token (AMPERSAND_TOKEN, col);
        break;

      case '`' :
        Write_Token (BACK_QUOTE_TOKEN, col);
        break;

      case '\\':
        Write_Token (BACK_SLASH_TOKEN, col);
        break;

      case '|' :
        Write_Token (BAR_TOKEN, col);
        break;

      case ':' :
        Write_Token (COLON_TOKEN, col);
        break;

      case ',' :
        Write_Token (COMMA_TOKEN, col);
        break;

      case '-' :
        Write_Token (DASH_TOKEN, col);
        break;

      case '$' :
        Write_Token (DOLLAR_TOKEN, col);
        break;

      case '=' :
        Write_Token (EQUALS_TOKEN, col);
        break;

      case '!' :
        c2 = Echo_getc();
        if (c2 == (int)'=')
        {
          Write_Token (REL_NE_TOKEN, col);
        }
        else
        {
          Echo_ungetc(c2);
          Write_Token (EXCLAMATION_TOKEN, col);
        }
        break;

      case '#' :
        Parse_Directive(true);
        /* Write_Token (HASH_TOKEN, col);*/
        break;

      case '^' :
        Write_Token (HAT_TOKEN, col);
        break;

      case '<' :
        c2 = Echo_getc();
        if (c2 == (int)'=')
        {
          Write_Token (REL_LE_TOKEN, col);
        }
        else
        {
          Echo_ungetc(c2);
          Write_Token (LEFT_ANGLE_TOKEN, col);
        }
        break;

      case '(' :
        Write_Token (LEFT_PAREN_TOKEN, col);
        break;

      case '[' :
        Write_Token (LEFT_SQUARE_TOKEN, col);
        break;

      case '%' :
        Write_Token (PERCENT_TOKEN, col);
        break;

      case '+' :
        Write_Token (PLUS_TOKEN, col);
        break;

      case '?' :
        Write_Token (QUESTION_TOKEN, col);
        break;

      case '>' :
        c2 = Echo_getc();
        if (c2 == (int)'=')
        {
          Write_Token (REL_GE_TOKEN, col);
        }
        else
        {
          Echo_ungetc(c2);
          Write_Token (RIGHT_ANGLE_TOKEN, col);
        }
        break;

      case ')' :
        Write_Token (RIGHT_PAREN_TOKEN, col);
        break;

      case ']' :
        Write_Token (RIGHT_SQUARE_TOKEN, col);
        break;

      case ';' : /* Parser doesn't use it, so let's ignore it */
        Write_Token (SEMI_COLON_TOKEN, col);
        break;

      case '\'':
        Write_Token (SINGLE_QUOTE_TOKEN, col);
        break;

        /* enable C++ style commenting */
      case '/' :
        c2 = Echo_getc();
        if(c2 != (int) '/' && c2 != (int) '*')
        {
          Echo_ungetc(c2);
          Write_Token (SLASH_TOKEN, col);
          break;
        }
        if(c2 == (int)'*')
        {
          Parse_C_Comments();
          break;
        }
        while(c2 != (int)'\n')
        {
          c2=Echo_getc();
          if(c2==EOF)
          {
            Echo_ungetc(c2);
            break;
          }
        }
        break;

      case '*' :
        Write_Token (STAR_TOKEN, col);
        break;

      case '~' :
        Write_Token (TILDE_TOKEN, col);
        break;

      case '"' :
        Parse_String_Literal ();
        break;

      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
      case '.':
        Echo_ungetc(c);
        if (Read_Float () != true)
          return;
        break;

      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case 'k':
      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case 'p':
      case 'q':
      case 'r':
      case 's':
      case 't':
      case 'u':
      case 'v':
      case 'w':
      case 'x':
      case 'y':
      case 'z':

      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'G':
      case 'H':
      case 'I':
      case 'J':
      case 'K':
      case 'L':
      case 'M':
      case 'N':
      case 'O':
      case 'P':
      case 'Q':
      case 'R':
      case 'S':
      case 'T':
      case 'U':
      case 'V':
      case 'W':
      case 'X':
      case 'Y':
      case 'Z':
      case '_':
        Echo_ungetc(c);
        Read_Symbol ();
        break;
      case '\t':
      case '\r':
      case '\032':   /* Control Z - EOF on many systems */
      case '\0':
        break;

      default:
        Error("Illegal character in input file, value is %02x.", c);
        break;
    }
  }

  Current_Token_Count++;
  token_count++;

  if (token_count > 2500)
  {
    token_count = 0;

    Do_Cooperate(0);

    Check_User_Abort(false);

    Send_ProgressUpdate(PROGRESS_PARSING);
  }
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
*   Mark that the token has been put back into the input stream.  The next
*   call to Get_Token will return the last-read token instead of reading a
*   new one from the file.
*
* CHANGES
*
******************************************************************************/

void Unget_Token ()
{
  Token.Unget_Token = true;
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
*   Skip over spaces in the input file.
*
* CHANGES
*
******************************************************************************/

static bool Skip_Spaces()
{
  register int c;

  while(true)
  {
    c = Echo_getc();

    if (c == EOF)
      return false;

    if(!(isspace(c)))
      break;
  }

  Echo_ungetc(c);

  return true;
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
*   C style comments with asterik and slash - CdW 8/91.
*
* CHANGES
*
******************************************************************************/

static int Parse_C_Comments()
{
  register int c, c2;
  bool End_Of_Comment = false;

  while(!End_Of_Comment)
  {
    c = Echo_getc();

    if(c == EOF)
      Error("No */ closing comment found.");

    if(c == (int) '*')
    {
      c2 = Echo_getc();

      if(c2 != (int) '/')
        Echo_ungetc(c2);
      else
        End_Of_Comment = true;
    }

    /* Check for and handle nested comments */

    if(c == (int) '/')
    {
      c2 = Echo_getc();

      if(c2 != (int) '*')
        Echo_ungetc(c2);
      else
        Parse_C_Comments();
    }
  }

  return true;
}



/* The following routines make it easier to handle strings.  They stuff
   characters into a string buffer one at a time making all the proper
   range checks.  Call Begin_String to start, Stuff_Character to put
   characters in, and End_String to finish.  The String variable contains
   the final string. */

/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

inline void Begin_String()
{
	if((String != NULL) && (String != String_Fast_Buffer))
		POV_FREE(String);

	String = (char *)POV_MALLOC(256, "C String");
	String_Buffer_Free = 256;
	String_Index = 0;
}



/*****************************************************************************
 *
 * FUNCTION
 *
 * INPUT
 *
 * OUTPUT
 *
 * RETURNS
 *
 * AUTHOR
 *
 * DESCRIPTION
 *
 * CHANGES
 *
******************************************************************************/

inline void Stuff_Character(int chr)
{
	if(String_Buffer_Free <= 0)
	{
		Error("String too long.");
// This caused too many problems with buffer overflows [trf]
//		String = (char *)POV_REALLOC(String, String_Index + 256, "String Literal Buffer");
//		String_Buffer_Free += 256;
	}

	String[String_Index] = chr;
	String_Buffer_Free--;
	String_Index++;
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

inline void End_String()
{
	Stuff_Character(0);

	if(String_Buffer_Free > 0)
		String = (char *)POV_REALLOC(String, String_Index, "String Literal Buffer");

	String_Buffer_Free = 0;
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

inline void Begin_String_Fast()
{
	if((String != NULL) && (String != String_Fast_Buffer))
		POV_FREE(String);

	String = String_Fast_Buffer;
	String_Index = 0;
}



/*****************************************************************************
 *
 * FUNCTION
 *
 * INPUT
 *
 * OUTPUT
 *
 * RETURNS
 *
 * AUTHOR
 *
 * DESCRIPTION
 *
 * CHANGES
 *
******************************************************************************/

inline void Stuff_Character_Fast(int chr)
{
	String[String_Index & MAX_STRING_LEN_MASK] = chr;
	String_Index++;
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

inline void End_String_Fast()
{
	Stuff_Character_Fast(0);

	String_Index--; // Stuff_Character_Fast incremented this

	if(String_Index != (String_Index & MAX_STRING_LEN_MASK))
		Error("String too long.");
}



/*****************************************************************************
 *
 * FUNCTION
 *
 * INPUT
 *
 * OUTPUT
 *
 * RETURNS
 *
 * AUTHOR
 *
 * DESCRIPTION
 *
 *   Parse a string from the input file into a token.
 *
 * CHANGES
 *
******************************************************************************/

void Parse_String_Literal()
{
	register int c;
	int col = Echo_Indx;

	Begin_String();

	while(true)
	{
		c = Echo_getc();

		if(c == EOF)
			Error("No end quote for string.");

		if(c == '\\')
		{
			switch(c = Echo_getc())
			{
				case '\n':
				case '\r':
					Error("Unterminated string literal.");
					break;
				case '\"':
					c = '\"';
					break;
				case EOF:
					Error("No end quote for string.");
					break;
				default:
					Stuff_Character('\\');
			}

			Stuff_Character(c);
		}
		else
		{
			if(c != (int)'"')
				Stuff_Character(c);
			else
				break;
		}
	}

	End_String();

	Write_Token(STRING_LITERAL_TOKEN, col);

	Token.Token_String = String;
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
*   Read a float from the input file and tokenize it as one token. The phase
*   variable is 0 for the first character, 1 for all subsequent characters
*   up to the decimal point, 2 for all characters after the decimal
*   point, 3 for the E+/- and 4 for the exponent.  This helps to insure
*   that the number is formatted properly. E format added 9/91 CEY
*
* CHANGES
*
******************************************************************************/

static bool Read_Float()
{
  register int c, Phase;
  register bool Finished;
  int col = Echo_Indx;

  Finished = false;

  Phase = 0;

  Begin_String_Fast();

  while (!Finished)
  {
    c = Echo_getc();

    if (c == EOF)
    {
      Error ("Unexpected end of file.");
    }

    switch (Phase)
    {
      case 0:

        Phase = 1;

        if (isdigit(c))
        {
          Stuff_Character_Fast(c);
        }
        else
        {
          if (c == '.')
          {
            c = Echo_getc();

            if (c == EOF)
            {
              Error ("Unexpected end of file");
            }

            if (isdigit(c))
            {
              Stuff_Character_Fast('0');
              Stuff_Character_Fast('.');
              Stuff_Character_Fast(c);

              Phase = 2;
            }
            else
            {
              Echo_ungetc(c);

              Write_Token (PERIOD_TOKEN, col);

              return(true);
            }
          }
          else
          {
            Error ("Invalid decimal number");
          }
        }

        break;

      case 1:
        if (isdigit(c))
        {
          Stuff_Character_Fast(c);
        }
        else
        {
          if (c == (int) '.')
          {
            Stuff_Character_Fast(c); Phase = 2;
          }
          else
          {
            if ((c == 'e') || (c == 'E'))
            {
              Stuff_Character_Fast(c); Phase = 3;
            }
            else
            {
              Finished = true;
            }
          }
        }

        break;

      case 2:

        if (isdigit(c))
        {
          Stuff_Character_Fast(c);
        }
        else
        {
          if ((c == 'e') || (c == 'E'))
          {
            Stuff_Character_Fast(c); Phase = 3;
          }
          else
          {
            Finished = true;
          }
        }

        break;

      case 3:

        if (isdigit(c) || (c == '+') || (c == '-'))
        {
          Stuff_Character_Fast(c); Phase = 4;
        }
        else
        {
          Finished = true;
        }

        break;

      case 4:

        if (isdigit(c))
        {
          Stuff_Character_Fast(c);
        }
        else
        {
          Finished = true;
        }

        break;
    }
  }

  Echo_ungetc(c);

  End_String_Fast();

  Write_Token (FLOAT_TOKEN, col);

  if (sscanf (String, DBL_FORMAT_STRING, &Token.Token_Float) == 0)
  {
    return (false);
  }

  return (true);
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
*   Read in a symbol from the input file. Check to see if it is a reserved
*   word. If it is, write out the appropriate token. Otherwise, write the
*   symbol out to the symbol table and write out an IDENTIFIER token. An
*   identifier token is a token whose token number is greater than the
*   highest reserved word.
*
* CHANGES
*
******************************************************************************/

static void Read_Symbol()
{
  register int c;
  int Local_Index,i,j,k;
  POV_ARRAY *a;
  SYM_ENTRY *Temp_Entry;
  POV_PARAM *Par;
  DBL val;

  Begin_String_Fast();

  while (true)
  {
    c = Echo_getc();

    if (c == EOF)
    {
      Error ("Unexpected end of file.");
    }

    if (isalpha(c) || isdigit(c) || c == (int) '_')
    {
      Stuff_Character_Fast(c);
    }
    else
    {
      Echo_ungetc(c);

      break;
    }
  }

  End_String_Fast();

  if (Inside_Ifdef)
  {
    Token.Token_Id = IDENTIFIER_TOKEN;
    Token.is_array_elem = false;

    return;
  }

  /* If its a reserved keyword, write it and return */
  if ( (Temp_Entry = Find_Symbol(0,String)) != NULL)
  {
    Write_Token (Temp_Entry->Token_Number, Token.Token_Col_No);
    return;
  }

  if (!Skipping)
  {
    /* Search tables from newest to oldest */
    for (Local_Index=Table_Index; Local_Index > 0; Local_Index--)
    {
      if ((Temp_Entry = Find_Symbol(Local_Index,String)) != NULL)
      {
         /* Here its a previously declared identifier. */

         if (Temp_Entry->Token_Number==MACRO_ID_TOKEN)
         {
           Token.Data = Temp_Entry->Data;
           if (Ok_To_Declare)
           {
              Invoke_Macro();
           }
           else
           {
              Token.Token_Id=MACRO_ID_TOKEN;
              Token.is_array_elem = false;
              Token.NumberPtr = &(Temp_Entry->Token_Number);
              Token.DataPtr   = &(Temp_Entry->Data);
              Write_Token (Token.Token_Id, Token.Token_Col_No);

              Token.Table_Index = Local_Index;
           }
           return;
         }

         Token.Token_Id  =   Temp_Entry->Token_Number;
         Token.is_array_elem = false;
         Token.NumberPtr = &(Temp_Entry->Token_Number);
         Token.DataPtr   = &(Temp_Entry->Data);

         while ((Token.Token_Id==PARAMETER_ID_TOKEN) ||
                (Token.Token_Id==ARRAY_ID_TOKEN))
         {
           if (Token.Token_Id==ARRAY_ID_TOKEN)
           {
             Skip_Spaces();
             c = Echo_getc();
             Echo_ungetc(c);

             if (c!='[')
             {
               break;
             }

             a = (POV_ARRAY *)(*(Token.DataPtr));
             j = 0;

             for (i=0; i <= a->Dims; i++)
             {
                GET(LEFT_SQUARE_TOKEN)
                val=Parse_Float();
                k=(int)(val + EPSILON);

                if ((k < 0) || (val < -EPSILON))
                {
                  Error("Negative subscript");
                }

                if (k >= a->Sizes[i])
                {
                   Error("Array subscript out of range");
                }
                j += k * a->Mags[i];
                GET(RIGHT_SQUARE_TOKEN)
             }

             Token.DataPtr   = &(a->DataPtrs[j]);
             Token.NumberPtr = &(a->Type);
             Token.Token_Id = a->Type;
             Token.is_array_elem = true;
             if (!LValue_Ok)
             {
                if (*Token.DataPtr == NULL)
                  Error("Attempt to access uninitialized array element.");
             }
           }
           else
           {
             Par             = (POV_PARAM *)(Temp_Entry->Data);
             Token.Token_Id  = *(Par->NumberPtr);
             Token.is_array_elem = false;
             Token.NumberPtr = Par->NumberPtr;
             Token.DataPtr   = Par->DataPtr;
           }
         }

         Write_Token (Token.Token_Id, Token.Token_Col_No);

         Token.Data        = *(Token.DataPtr);
         Token.Table_Index = Local_Index;
         return;
      }
    }
  }

  Write_Token(IDENTIFIER_TOKEN, Token.Token_Col_No);
}

inline void Write_Token (TOKEN Token_Id, int col)
{
   Token.Token_File_Pos = Input_File->In_File->tellg();
   Token.Token_Col_No   = col;
   Token.FileHandle     = Input_File->In_File;
   Token.Token_String   = String;
   Token.Data           = NULL;
   Token.Token_Id       = Conversion_Util_Table[Token_Id];
   Token.Function_Id    = Token_Id;
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static int Echo_getc()
{
  register int c;

  if((Input_File == NULL) || (Input_File->In_File == NULL) || (c = Input_File->In_File->getchar()) == EOF)
  {
    if (Got_EOF)
      return EOF;
    Got_EOF = true ;
    Echo_Indx = 0 ;
    return ('\n') ;
  }

  Echo_Indx++;
  if(c == '\n')
    Echo_Indx = 0;

  return c;
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static void Echo_ungetc(int c)
{
  if(Echo_Indx > 0)
    Echo_Indx--;

  Input_File->In_File->ungetchar(c);
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Where_Error(POVMSObjectPtr msg)
{
	// return if no filename is specified
	if(Token.FileHandle == NULL)
		return;

	(void)POVMSUtil_SetString(msg, kPOVAttrib_FileName, Token.FileHandle->name());
	(void)POVMSUtil_SetString(msg, kPOVAttrib_TokenName, Token.Token_String);
	(void)POVMSUtil_SetInt(msg, kPOVAttrib_Line, Token.Token_File_Pos.lineno);
	(void)POVMSUtil_SetInt(msg, kPOVAttrib_Column, Token.Token_Col_No);
	if(Token.FileHandle != NULL)
		(void)POVMSUtil_SetLong(msg, kPOVAttrib_FilePosition, Token.FileHandle->tellg().offset);
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Where_Warning(POVMSObjectPtr msg)
{
	// return if no filename is specified
	if(Token.FileHandle == NULL)
		return;

	(void)POVMSUtil_SetString(msg, kPOVAttrib_FileName, Token.FileHandle->name());
	(void)POVMSUtil_SetString(msg, kPOVAttrib_TokenName, Token.Token_String);
	(void)POVMSUtil_SetInt(msg, kPOVAttrib_Line, Token.Token_File_Pos.lineno);
	(void)POVMSUtil_SetInt(msg, kPOVAttrib_Column, Token.Token_Col_No);
	if(Token.FileHandle != NULL)
		(void)POVMSUtil_SetLong(msg, kPOVAttrib_FilePosition, Token.FileHandle->tellg().offset);
}



/*****************************************************************************
*
* FUNCTION    Parse_Directive
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR      Chris Young
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Parse_Directive(int After_Hash)
{
  DBL Value, Value2;
  int Flag;
  char *ts;
  POV_MACRO *PMac=NULL;
  COND_TYPE Curr_Type = Cond_Stack[CS_Index].Cond_Type;
  long Hash_Loc = Input_File->In_File->tellg().offset;

  if (Curr_Type == INVOKING_MACRO_COND)
  {
     if (Cond_Stack[CS_Index].PMac->Macro_End==Hash_Loc)
     {
        Return_From_Macro();
        if (--CS_Index < 0)
        {
           Error("Mis-matched '#end'.");
        }
        Token.Token_Id = END_OF_FILE_TOKEN;
        Token.is_array_elem = false;

        return;
     }
  }

  if (!Ok_To_Declare)
  {
    if (After_Hash)
    {
       Token.Token_Id=HASH_TOKEN;
       Token.is_array_elem = false;
    }
    Token.Unget_Token = false;

    return;
  }

  EXPECT
    CASE(IFDEF_TOKEN)
      Inc_CS_Index();

      if (Skipping)
      {
        Cond_Stack[CS_Index].Cond_Type = SKIP_TIL_END_COND;
        Skip_Tokens(SKIP_TIL_END_COND);
      }
      else
      {
        if (Parse_Ifdef_Param())
        {
           Cond_Stack[CS_Index].Cond_Type=IF_TRUE_COND;
        }
        else
        {
           Cond_Stack[CS_Index].Cond_Type=IF_FALSE_COND;
           Skip_Tokens(IF_FALSE_COND);
        }
      }
      EXIT
    END_CASE

    CASE(IFNDEF_TOKEN)
      Inc_CS_Index();

      if (Skipping)
      {
        Cond_Stack[CS_Index].Cond_Type = SKIP_TIL_END_COND;
        Skip_Tokens(SKIP_TIL_END_COND);
      }
      else
      {
        if (Parse_Ifdef_Param())
        {
           Cond_Stack[CS_Index].Cond_Type=IF_FALSE_COND;
           Skip_Tokens(IF_FALSE_COND);
        }
        else
        {
           Cond_Stack[CS_Index].Cond_Type=IF_TRUE_COND;
        }
      }
      EXIT
    END_CASE

    CASE(IF_TOKEN)
      Inc_CS_Index();

      if (Skipping)
      {
        Cond_Stack[CS_Index].Cond_Type = SKIP_TIL_END_COND;
        Skip_Tokens(SKIP_TIL_END_COND);
      }
      else
      {
        Value=Parse_Cond_Param();

        if (fabs(Value)>EPSILON)
        {
           Cond_Stack[CS_Index].Cond_Type=IF_TRUE_COND;
        }
        else
        {
           Cond_Stack[CS_Index].Cond_Type=IF_FALSE_COND;
           Skip_Tokens(IF_FALSE_COND);
        }
      }
      EXIT
    END_CASE

    CASE(WHILE_TOKEN)
      Inc_CS_Index();

      if (Skipping)
      {
        Cond_Stack[CS_Index].Cond_Type = SKIP_TIL_END_COND;
        Skip_Tokens(SKIP_TIL_END_COND);
      }
      else
      {
        Cond_Stack[CS_Index].While_File = Input_File->In_File;
        Cond_Stack[CS_Index].File_Pos   = Input_File->In_File->tellg();

        Value=Parse_Cond_Param();

        if (fabs(Value)>EPSILON)
        {
           Cond_Stack[CS_Index].Cond_Type = WHILE_COND;
        }
        else
        {
           Cond_Stack[CS_Index].Cond_Type = SKIP_TIL_END_COND;
           Skip_Tokens(SKIP_TIL_END_COND);
        }
      }
      EXIT
    END_CASE

    CASE(ELSE_TOKEN)
      switch (Curr_Type)
      {
         case IF_TRUE_COND:
           Cond_Stack[CS_Index].Cond_Type = SKIP_TIL_END_COND;
           Skip_Tokens(SKIP_TIL_END_COND);
           break;

         case IF_FALSE_COND:
           Cond_Stack[CS_Index].Cond_Type = ELSE_COND;
           Token.Token_Id=HASH_TOKEN; /*insures Skip_Token takes notice*/
           Token.is_array_elem = false;
           UNGET
           break;

         case CASE_TRUE_COND:
         case SKIP_TIL_END_COND:
           break;

         case CASE_FALSE_COND:
           Cond_Stack[CS_Index].Cond_Type = CASE_TRUE_COND;
           if (Skipping)
           {
              Token.Token_Id=HASH_TOKEN; /*insures Skip_Token takes notice*/
              Token.is_array_elem = false;
              UNGET
           }
           break;

         default:
           Error("Mis-matched '#else'.");
      }
      EXIT
    END_CASE

    CASE(SWITCH_TOKEN)
      Inc_CS_Index();

      if (Skipping)
      {
        Cond_Stack[CS_Index].Cond_Type = SKIP_TIL_END_COND;
        Skip_Tokens(SKIP_TIL_END_COND);
      }
      else
      {
        Cond_Stack[CS_Index].Switch_Value=Parse_Cond_Param();
        Cond_Stack[CS_Index].Cond_Type=SWITCH_COND;
        EXPECT
          CASE2(CASE_TOKEN,RANGE_TOKEN)
            if (Token.Token_Id==CASE_TOKEN)
            {
              Value=Parse_Cond_Param();
              Flag = (fabs(Value-Cond_Stack[CS_Index].Switch_Value)<EPSILON);
            }
            else
            {
              Parse_Cond_Param2(&Value,&Value2);
              Flag = ((Cond_Stack[CS_Index].Switch_Value >= Value) &&
                      (Cond_Stack[CS_Index].Switch_Value <= Value2));
            }

            if(Flag)
            {
              Cond_Stack[CS_Index].Cond_Type=CASE_TRUE_COND;
            }
            else
            {
              Cond_Stack[CS_Index].Cond_Type=CASE_FALSE_COND;
              Skip_Tokens(CASE_FALSE_COND);
            }
            EXIT
          END_CASE

          OTHERWISE
            Error("#switch not followed by #case or #range.");
          END_CASE
        END_EXPECT
      }
      EXIT
    END_CASE

    CASE(BREAK_TOKEN)
      if (Curr_Type==CASE_TRUE_COND)
      {
        Cond_Stack[CS_Index].Cond_Type=SKIP_TIL_END_COND;
        Skip_Tokens(SKIP_TIL_END_COND);
      }
      EXIT
    END_CASE

    CASE2(CASE_TOKEN,RANGE_TOKEN)
      switch(Curr_Type)
      {
        case CASE_TRUE_COND:
        case CASE_FALSE_COND:
          if (Token.Token_Id==CASE_TOKEN)
          {
            Value=Parse_Cond_Param();
            Flag = (fabs(Value-Cond_Stack[CS_Index].Switch_Value)<EPSILON);
          }
          else
          {
            Parse_Cond_Param2(&Value,&Value2);
            Flag = ((Cond_Stack[CS_Index].Switch_Value >= Value) &&
                    (Cond_Stack[CS_Index].Switch_Value <= Value2));
          }

          if(Flag && (Curr_Type==CASE_FALSE_COND))
          {
            Cond_Stack[CS_Index].Cond_Type=CASE_TRUE_COND;
            if (Skipping)
            {
               Token.Token_Id=HASH_TOKEN; /*insures Skip_Token takes notice*/
               Token.is_array_elem = false;
               UNGET
            }
          }
          break;

        case SWITCH_COND:
          UNGET
        case SKIP_TIL_END_COND:
          break;

        default:
          Error("Mis-matched '#case' or '#range'.");
      }
      EXIT
    END_CASE

    CASE(END_TOKEN)
      switch (Curr_Type)
      {
         case INVOKING_MACRO_COND:
           Return_From_Macro();
           if (--CS_Index < 0)
           {
              Error("Mis-matched '#end'.");
           }
           break;

         case IF_FALSE_COND:
           Token.Token_Id=HASH_TOKEN; /*insures Skip_Token takes notice*/
           Token.is_array_elem = false;
           UNGET
         case IF_TRUE_COND:
         case ELSE_COND:
         case CASE_TRUE_COND:
         case CASE_FALSE_COND:
         case DECLARING_MACRO_COND:
         case SKIP_TIL_END_COND:
           if (Curr_Type==DECLARING_MACRO_COND)
           {
             if ((PMac=Cond_Stack[CS_Index].PMac)!=NULL)
             {
                PMac->Macro_End=Hash_Loc;
             }
           }
           if (--CS_Index < 0)
           {
              Error("Mis-matched '#end'.");
           }
           if (Skipping)
           {
              Token.Token_Id=HASH_TOKEN; /*insures Skip_Token takes notice*/
              Token.is_array_elem = false;
              UNGET
           }
           break;

         case WHILE_COND:
           if (Cond_Stack[CS_Index].While_File != Input_File->In_File)
           {
              Error("#while loop did not end in file where it started.");
           }

           Got_EOF=false;
           if (!Input_File->In_File->seekg(Cond_Stack[CS_Index].File_Pos))
           {
              Error("Unable to seek in input file for #while directive.");
           }

           Value=Parse_Cond_Param();

           if (fabs(Value)<EPSILON)
           {
             Cond_Stack[CS_Index].Cond_Type = SKIP_TIL_END_COND;
             Skip_Tokens(SKIP_TIL_END_COND);
           }
           break;

         default:
           Error("Mis-matched '#end'.");
      }
      EXIT
    END_CASE

    CASE2 (DECLARE_TOKEN,LOCAL_TOKEN)
      if (Skipping)
      {
         UNGET
         EXIT
      }
      else
      {
         Parse_Declare(Token.Token_Id == LOCAL_TOKEN, After_Hash);
         Curr_Type = Cond_Stack[CS_Index].Cond_Type;
         if (Token.Unget_Token)
         {
           switch (Token.Token_Id)
           {
              case HASH_TOKEN:
                Token.Unget_Token=false;
                break;

              case MACRO_ID_TOKEN:
                break;

              default:
                EXIT
           }
         }
         else
         {
            EXIT
         }
      }
    END_CASE

    CASE (DEFAULT_TOKEN)
      if ( Skipping ) 
      {
        UNGET
      }
      else
      {
        Parse_Default();
      }
      EXIT
    END_CASE

    CASE (INCLUDE_TOKEN)
      if (Skipping)
      {
         UNGET
      }
      else
      {
         Open_Include();
      }
      EXIT
    END_CASE

    CASE (FLOAT_FUNCT_TOKEN)
      if (Skipping)
      {
        UNGET
        EXIT
      }
      else
      {
        switch(Token.Function_Id)
        {
           case VERSION_TOKEN:
             Ok_To_Declare = false;

             Get_Token();
#if POV_RAY_IS_OFFICIAL == 1
             if (pov_stricmp(Token.Token_String,"unofficial") != 0)
             {
               Unget_Token();
               opts.Language_Version = (int)(Parse_Float() * 100 + 0.5);
               Parse_Semi_Colon(false);
             }
             else
             {
               Get_Token();
               Error("This file was created for an unofficial version and\ncannot work as-is with this official version.");
             }
#else
             if (pov_stricmp(Token.Token_String,"unofficial") == 0)
             {
               // PATCH AUTHORS - you should not enable any extra features unless the
               // 'unofficial' keyword is set in the scene file.
             }
             else
               Unget_Token();
             opts.Language_Version = (int)(Parse_Float() * 100 + 0.5);
             Parse_Semi_Colon(false);
#endif
             if (opts.Language_Version > OFFICIAL_VERSION_NUMBER)
             {
                Error("Your scene file requires POV-Ray version %g or later!\n", (DBL)(opts.Language_Version / 100.0));
             }

             Ok_To_Declare = true;
             Curr_Type = Cond_Stack[CS_Index].Cond_Type;
             if (Token.Unget_Token && (Token.Token_Id==HASH_TOKEN))
             {
                Token.Unget_Token=false;
             }
             else
             {
                EXIT
             }
             break;

           default:
             UNGET
             Expectation_Error ("object or directive.");
             break;
        }
      }
    END_CASE

    CASE(WARNING_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        ts=Parse_C_String();
        if(strlen(ts) > 160) // intentional 160, not 128 [trf]
        {
          ts[124] = ts[125] = ts[126] = '.';
          ts[127] = 0;
        }
        Warning(0, "%s", ts);
        POV_FREE(ts);
      }
      EXIT
    END_CASE

    CASE(ERROR_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        ts=Parse_C_String();
        if(strlen(ts) > 160) // intentional 160, not 128 [trf]
        {
          ts[124] = ts[125] = ts[126] = '.';
          ts[127] = 0;
        }
        Error("User message '%s'\nParsing stopped at user's request!", ts);
        POV_FREE(ts);
      }
      EXIT
    END_CASE

/* Note: The new message driven output system does not support
   generic user output to the render and statistics streams.
   Both streams are now directed into the debug stream. */
    CASE(RENDER_TOKEN)
    CASE(STATISTICS_TOKEN)
        Warning(0, "#render and #statistics streams are no longer available.\nRedirecting output to #debug stream.");
      // Intentional, redirect output to debug stream.
    CASE(DEBUG_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        ts=Parse_C_String();
        if(strlen(ts) > 200) // intentional 200, not 160
        {
          ts[156] = ts[157] = ts[158] = '.';
          ts[159] = 0;
        }
        Debug_Info("%s", ts);
        POV_FREE(ts);
      }
      EXIT
    END_CASE

    CASE(FOPEN_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        Parse_Fopen();
      }
      EXIT
    END_CASE

    CASE(FCLOSE_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        Parse_Fclose();
      }
      EXIT
    END_CASE

    CASE(READ_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        Parse_Read();
      }
      EXIT
    END_CASE

    CASE(WRITE_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        Parse_Write();
      }
      EXIT
    END_CASE

    CASE(UNDEF_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        Ok_To_Declare = false;
        EXPECT
          CASE (IDENTIFIER_TOKEN)
            Warning(0,"Attempt to undef unknown identifier");
            EXIT
          END_CASE

          CASE2 (MACRO_ID_TOKEN, PARAMETER_ID_TOKEN)
          CASE3 (FILE_ID_TOKEN,  FUNCT_ID_TOKEN, VECTFUNCT_ID_TOKEN)
          // These have to match Parse_Declare in parse.cpp! [trf]
          CASE4 (TNORMAL_ID_TOKEN, FINISH_ID_TOKEN, TEXTURE_ID_TOKEN, OBJECT_ID_TOKEN)
          CASE4 (COLOUR_MAP_ID_TOKEN, TRANSFORM_ID_TOKEN, CAMERA_ID_TOKEN, PIGMENT_ID_TOKEN)
          CASE4 (SLOPE_MAP_ID_TOKEN, NORMAL_MAP_ID_TOKEN, TEXTURE_MAP_ID_TOKEN, COLOUR_ID_TOKEN)
          CASE4 (PIGMENT_MAP_ID_TOKEN, MEDIA_ID_TOKEN, STRING_ID_TOKEN, INTERIOR_ID_TOKEN)
          CASE4 (DENSITY_ID_TOKEN, ARRAY_ID_TOKEN, DENSITY_MAP_ID_TOKEN, UV_ID_TOKEN)
          CASE4 (VECTOR_4D_ID_TOKEN,  RAINBOW_ID_TOKEN, FOG_ID_TOKEN, SKYSPHERE_ID_TOKEN)
          CASE2 (MATERIAL_ID_TOKEN, SPLINE_ID_TOKEN)
            Remove_Symbol (Token.Table_Index, Token.Token_String, Token.is_array_elem, Token.DataPtr, Token.Token_Id);
            EXIT
          END_CASE

          CASE2 (VECTOR_FUNCT_TOKEN, FLOAT_FUNCT_TOKEN)
            switch(Token.Function_Id)
            {
              case VECTOR_ID_TOKEN:
              case FLOAT_ID_TOKEN:
                 Remove_Symbol (Token.Table_Index, Token.Token_String, Token.is_array_elem, Token.DataPtr, Token.Token_Id);
                 break;

              default:
                 Parse_Error(IDENTIFIER_TOKEN);
                 break;
            }
            EXIT
          END_CASE

          OTHERWISE
            Parse_Error(IDENTIFIER_TOKEN);
          END_CASE
        END_EXPECT
        Ok_To_Declare = true;
      }
      EXIT
    END_CASE

    CASE (MACRO_ID_TOKEN)
      if (Skipping)
      {
        UNGET
      }
      else
      {
        Invoke_Macro();
      }
      EXIT
    END_CASE

    CASE (MACRO_TOKEN)
      if (!Skipping)
      {
        if (Inside_MacroDef)
        {
          Error("Cannot nest macro definitions");
        }
        Inside_MacroDef=true;
        PMac=Parse_Macro();
        Inside_MacroDef=false;
      }
      Inc_CS_Index();
      Cond_Stack[CS_Index].Cond_Type = DECLARING_MACRO_COND;
      Cond_Stack[CS_Index].PMac      = PMac;
      Skip_Tokens(DECLARING_MACRO_COND);
      EXIT
    END_CASE

    OTHERWISE
      UNGET
      EXIT
    END_CASE
  END_EXPECT

  if (Token.Unget_Token)
  {
    Token.Unget_Token = false;
  }
  else
  {
    Token.Token_Id = END_OF_FILE_TOKEN;
    Token.is_array_elem = false;
  }
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Open_Include()
{
   char *temp;
   char b[FILE_NAME_LENGTH];

   if (Skip_Spaces () != true)
     Error ("Expecting a string after INCLUDE.");

   temp = Parse_C_String(true);

   Include_File_Index++;

   if (Include_File_Index >= MAX_INCLUDE_FILES)
   {
     Include_File_Index--;
     Error ("Too many nested include files.");
   }

   Echo_Indx = 0;

   Input_File = &Include_Files[Include_File_Index];

   IStream *is = Locate_File(temp,POV_File_Text_INC,b,true);
   if(is == NULL)
   {
      Input_File->In_File = NULL;  /* Keeps from closing failed file. */
      Stage=STAGE_INCLUDE_ERR;
      Error ("Cannot open include file %s.", temp);
   }
   else
      Input_File->In_File = new ITextStream(b, is);

   POV_FREE(temp);

   Input_File->R_Flag=false;

   Add_Sym_Table(Input_File->In_File->name());

   Token.Token_Id = END_OF_FILE_TOKEN;
   Token.is_array_elem = false;

}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static void Skip_Tokens(COND_TYPE cond)
{
  int Temp      = CS_Index;
  int Prev_Skip = Skipping;

  Skipping=true;

  while ((CS_Index > Temp) || ((CS_Index == Temp) && (Cond_Stack[CS_Index].Cond_Type == cond)))
  {
    Get_Token();
  }

  Skipping=Prev_Skip;

  if (Token.Token_Id==HASH_TOKEN)
  {
     Token.Token_Id=END_OF_FILE_TOKEN;
     Token.is_array_elem = false;
     Token.Unget_Token=false;
  }
  else
  {
     UNGET
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   get_hash_value
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Calculate hash value for a given string.
*
* CHANGES
*
*   Apr 1996 : Creation.
*
******************************************************************************/

static int get_hash_value(char *s)
{
  unsigned int i = 0;

  while (*s)
  {
    i = (i << 1) ^ *s++;
  }

  return((int)(i % SYM_TABLE_SIZE));
}



/*****************************************************************************
*
* FUNCTION
*
*   init_sym_tables
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Chris Young
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static void init_sym_tables()
{
  int i;

  Add_Sym_Table("reserved words");

  for (i = 0; i < LAST_TOKEN; i++)
  {
    Add_Symbol(0,Reserved_Words[i].Token_Name,Reserved_Words[i].Token_Number);
  }

  Add_Sym_Table("global identifiers");
}

static void Add_Sym_Table(const char *s)
{
  int i;

  SYM_TABLE *New;

  if ((++Table_Index)==MAX_NUMBER_OF_TABLES)
  {
    Table_Index--;
    Error("Too many nested symbol tables");
  }

  Tables[Table_Index]=New=(SYM_TABLE *)POV_MALLOC(sizeof(SYM_TABLE),"symbol table");

  New->Table_Name=POV_STRDUP(s);

  for (i = 0; i < SYM_TABLE_SIZE; i++)
  {
    New->Table[i] = NULL;
  }

}

static void Destroy_Table(int index)
{
   int i;
   SYM_TABLE *Table = Tables[index];
   SYM_ENTRY *Entry;

   for(i = SYM_TABLE_SIZE - 1; i >= 0; i--)
   {
      Entry = Table->Table[i];

      while(Entry)
      {
         Entry = Destroy_Entry(index, Entry);
      }

      Table->Table[i] = NULL;
   }

   POV_FREE(Table->Table_Name);
   POV_FREE(Table);

}


SYM_ENTRY *Create_Entry (int Index,char *Name,TOKEN Number)
{
  SYM_ENTRY *New;

  New = (SYM_ENTRY *)POV_MALLOC(sizeof(SYM_ENTRY), "symbol table entry");

  New->Token_Number = Number;
  New->Data         = NULL;
  if (Index != 0)
  {
     New->Token_Name= POV_STRDUP(Name);
  }
  else
  {
     New->Token_Name=Name;
  }

  return(New);
}

SYM_ENTRY *Destroy_Entry (int Index,SYM_ENTRY *Entry)
{
   SYM_ENTRY *Next;

   if(Entry == NULL)
      return NULL;

   Next = Entry->next;
   Entry->next = NULL;

   if(Index != 0)
   {
      POV_FREE(Entry->Token_Name);
      Entry->Token_Name = NULL;
      Destroy_Ident_Data (Entry->Data, Entry->Token_Number);
      Entry->Data = NULL;
   }

   POV_FREE(Entry);

   return Next;
}


static void Add_Entry (int Index,SYM_ENTRY *Table_Entry)
{
  int i = get_hash_value(Table_Entry->Token_Name);

  Table_Entry->next       = Tables[Index]->Table[i];
  Tables[Index]->Table[i] = Table_Entry;
}


SYM_ENTRY *Add_Symbol (int Index,char *Name,TOKEN Number)
{
  SYM_ENTRY *New;

  New = Create_Entry (Index,Name,Number);
  Add_Entry(Index,New);

  return(New);
}


static SYM_ENTRY *Find_Symbol(int Index,char *Name)
{
  SYM_ENTRY *Entry;

  int i = get_hash_value(Name);

  Entry = Tables[Index]->Table[i];

  while (Entry)
  {
    if (strcmp(Name, Entry->Token_Name) == 0)
    {
      return(Entry);
    }

    Entry = Entry->next;
  }

  return(Entry);
}


static void Remove_Symbol (int Index, char *Name, bool is_array_elem, void **DataPtr, int ttype)
{
  if(is_array_elem == true)
  {
    if(DataPtr == NULL)
      Error("Invalid array element!");

    if(ttype == FLOAT_FUNCT_TOKEN)
      ttype = FLOAT_ID_TOKEN;
    else if(ttype == VECTOR_FUNCT_TOKEN)
      ttype = VECTOR_ID_TOKEN;
    else if(ttype == COLOUR_KEY_TOKEN)
      ttype = COLOUR_ID_TOKEN;

    Destroy_Ident_Data (*DataPtr, ttype);
    *DataPtr = NULL;
  }
  else
  {
    SYM_ENTRY *Entry;
    SYM_ENTRY **EntryPtr;

    int i = get_hash_value(Name);

    EntryPtr = &(Tables[Index]->Table[i]);
    Entry    = *EntryPtr;

    while (Entry)
    {
      if (strcmp(Name, Entry->Token_Name) == 0)
      {
        *EntryPtr = Entry->next;
        Destroy_Entry(Index, Entry);
        return;
      }

      EntryPtr = &(Entry->next);
      Entry    = *EntryPtr;
    }

    Error("Tried to free undefined symbol");
  }
}

static void Check_Macro_Vers(void)
{
  if (opts.Language_Version < 310)
  {
     Error("Macros require #version 3.1 or later but #version %x.%02d is set.",
            opts.Language_Version / 100, opts.Language_Version % 100);
  }
}

static POV_MACRO *Parse_Macro()
{
  POV_MACRO *New;
  SYM_ENTRY *Table_Entry=NULL;
  int Old_Ok = Ok_To_Declare;

  Check_Macro_Vers();

  Ok_To_Declare = false;

  EXPECT
    CASE (IDENTIFIER_TOKEN)
      Table_Entry = Add_Symbol (1,Token.Token_String,TEMPORARY_MACRO_ID_TOKEN);
      EXIT
    END_CASE

    CASE (MACRO_ID_TOKEN)
      Remove_Symbol(1,Token.Token_String,false,NULL,0);
      Table_Entry = Add_Symbol (1,Token.Token_String,TEMPORARY_MACRO_ID_TOKEN);
      EXIT
    END_CASE

    OTHERWISE
      Parse_Error(IDENTIFIER_TOKEN);
    END_CASE
  END_EXPECT

  New=(POV_MACRO *)POV_MALLOC(sizeof(POV_MACRO),"macro");

  Table_Entry->Data=(void *)New;

  New->Macro_Filename = NULL;
  New->Num_Of_Pars=0;
  New->Macro_Name=POV_STRDUP(Token.Token_String);

  EXPECT
    CASE (LEFT_PAREN_TOKEN )
      EXIT
    END_CASE
    CASE (TEMPORARY_MACRO_ID_TOKEN)
      Error( "Can't invoke a macro while declaring its parameters");
    END_CASE

    OTHERWISE
      Expectation_Error ("identifier or expression.");
    END_CASE
  END_EXPECT

  EXPECT
    CASE3 (MACRO_ID_TOKEN, IDENTIFIER_TOKEN, PARAMETER_ID_TOKEN)
    CASE3 (FILE_ID_TOKEN,  FUNCT_ID_TOKEN, VECTFUNCT_ID_TOKEN)
    // These have to match Parse_Declare in parse.cpp! [trf]
    CASE4 (TNORMAL_ID_TOKEN, FINISH_ID_TOKEN, TEXTURE_ID_TOKEN, OBJECT_ID_TOKEN)
    CASE4 (COLOUR_MAP_ID_TOKEN, TRANSFORM_ID_TOKEN, CAMERA_ID_TOKEN, PIGMENT_ID_TOKEN)
    CASE4 (SLOPE_MAP_ID_TOKEN, NORMAL_MAP_ID_TOKEN, TEXTURE_MAP_ID_TOKEN, COLOUR_ID_TOKEN)
    CASE4 (PIGMENT_MAP_ID_TOKEN, MEDIA_ID_TOKEN, STRING_ID_TOKEN, INTERIOR_ID_TOKEN)
    CASE4 (DENSITY_ID_TOKEN, ARRAY_ID_TOKEN, DENSITY_MAP_ID_TOKEN, UV_ID_TOKEN)
    CASE4 (VECTOR_4D_ID_TOKEN,  RAINBOW_ID_TOKEN, FOG_ID_TOKEN, SKYSPHERE_ID_TOKEN)
    CASE2 (MATERIAL_ID_TOKEN, SPLINE_ID_TOKEN)
      New->Par_Name[New->Num_Of_Pars] = POV_STRDUP(Token.Token_String);
      if (++(New->Num_Of_Pars) == MAX_PARAMETER_LIST)
      {
        Error("Too many parameters");
      }
      Parse_Comma();
    END_CASE

    CASE2 (VECTOR_FUNCT_TOKEN, FLOAT_FUNCT_TOKEN)
      switch(Token.Function_Id)
      {
         case VECTOR_ID_TOKEN:
         case FLOAT_ID_TOKEN:
           New->Par_Name[New->Num_Of_Pars] = POV_STRDUP(Token.Token_String);
           if (++(New->Num_Of_Pars) == MAX_PARAMETER_LIST)
           {
             Error("Too many parameters");
           }
           Parse_Comma();
           break;

         default:
           Expectation_Error ("identifier or expression.");
           break;
      }
    END_CASE

    CASE(RIGHT_PAREN_TOKEN)
      UNGET
      EXIT
    END_CASE

    CASE(TEMPORARY_MACRO_ID_TOKEN) 
      Error( "Can't invoke a macro while declaring its parameters");
    END_CASE

    OTHERWISE
      Expectation_Error ("identifier or expression.");
    END_CASE
  END_EXPECT

  Ok_To_Declare = Old_Ok;

  Table_Entry->Token_Number = MACRO_ID_TOKEN;

  New->Macro_Filename = POV_STRDUP(Input_File->In_File->name());
  New->Macro_File_Pos = Input_File->In_File->tellg();

  Check_Macro_Vers();

  return (New);
}


static void Invoke_Macro()
{
  POV_MACRO *PMac=(POV_MACRO *)Token.Data;
  SYM_ENTRY **Table_Entries=NULL;
  int i,Local_Index;

  if(PMac == NULL)
  {
    if(Token.DataPtr!=NULL) 
       PMac = (POV_MACRO*)(*(Token.DataPtr));
    else
       Error("Error in Invoke_Macro");
  }

  Check_Macro_Vers();

  GET(LEFT_PAREN_TOKEN);

  if (PMac->Num_Of_Pars > 0)
  {
    Table_Entries = (SYM_ENTRY **)POV_MALLOC(sizeof(SYM_ENTRY *)*PMac->Num_Of_Pars,"parameters");

    /* We must parse all parameters before adding new symbol table
       or adding entries.  Otherwise recursion won't always work.
     */

    Local_Index = Table_Index;

    for (i=0; i<PMac->Num_Of_Pars; i++)
    {
      Table_Entries[i]=Create_Entry(1,PMac->Par_Name[i],IDENTIFIER_TOKEN);
      if (!Parse_RValue(IDENTIFIER_TOKEN, &(Table_Entries[i]->Token_Number), &(Table_Entries[i]->Data), NULL, true, false, true, true, Local_Index))
      {
        Error("Expected %d parameters but only %d found.",PMac->Num_Of_Pars,i);
      }
      Parse_Comma();
    }
  }

  GET(RIGHT_PAREN_TOKEN);

  Inc_CS_Index();
  Cond_Stack[CS_Index].Cond_Type = INVOKING_MACRO_COND;

  Cond_Stack[CS_Index].File_Pos          = Input_File->In_File->tellg();
  Cond_Stack[CS_Index].Macro_Return_Name = Input_File->In_File->name();
  Cond_Stack[CS_Index].PMac              = PMac;

  /* Gotta have new symbol table in case #local is used */
  Add_Sym_Table(PMac->Macro_Name);

  if (PMac->Num_Of_Pars > 0)
  {
    for (i=0; i<PMac->Num_Of_Pars; i++)
    {
      Add_Entry(Table_Index,Table_Entries[i]);
    }

    POV_FREE(Table_Entries);
  }

  if (strcmp(PMac->Macro_Filename,Input_File->In_File->name()))
  {
    /* Not in same file */
    Cond_Stack[CS_Index].Macro_Same_Flag=false;
    Cond_Stack[CS_Index].Macro_File = Input_File->In_File ;
//  POV_DELETE(Input_File->In_File, IStream);
    Got_EOF=false;
    Input_File->R_Flag=false;
    IStream *is = Locate_File (PMac->Macro_Filename,POV_File_Text_Macro,NULL,true);
    if(is == NULL)
    {
       Input_File->In_File = NULL;  /* Keeps from closing failed file. */
       Stage=STAGE_INCLUDE_ERR;
       Error ("Cannot open macro file '%s'.", PMac->Macro_Filename);
    }
    else
       Input_File->In_File = new ITextStream(PMac->Macro_Filename, is);
  }
  else
  {
    Cond_Stack[CS_Index].Macro_Same_Flag=true;
  }

  Got_EOF=false;
  if (!Input_File->In_File->seekg(PMac->Macro_File_Pos))
  {
    Error("Unable to file seek in macro.");
  }

  Token.Token_Id = END_OF_FILE_TOKEN;
  Token.is_array_elem = false;

  Check_Macro_Vers();

}

static void Return_From_Macro()
{
  Check_Macro_Vers();

  if (!Cond_Stack[CS_Index].Macro_Same_Flag)
  {
     delete Input_File->In_File;
     Input_File->R_Flag=false;
     Input_File->In_File = Cond_Stack[CS_Index].Macro_File ;
  }

  Got_EOF=false;

  if (!Input_File->In_File->seekg(Cond_Stack[CS_Index].File_Pos))
  {
    Error("Unable to file seek in return from macro.");
  }

  // Always destroy macro locals
  Destroy_Table(Table_Index--);
}

void Destroy_Macro(POV_MACRO *PMac)
{
  int i;
  if (PMac==NULL)
  {
    return;
  }

  POV_FREE(PMac->Macro_Name);
  if (PMac->Macro_Filename!=NULL)
  {
     POV_FREE(PMac->Macro_Filename);
  }

  for (i=0; i < PMac->Num_Of_Pars; i++)
  {
    POV_FREE(PMac->Par_Name[i]);
  }

  POV_FREE(PMac);
}

POV_ARRAY *Parse_Array_Declare (void)
{
  POV_ARRAY *New;
  int i,j;

  New=(POV_ARRAY *)POV_MALLOC(sizeof(POV_ARRAY),"array");

  i=0;
  j=1;

  Ok_To_Declare = false;

  EXPECT
    CASE (LEFT_SQUARE_TOKEN)
      if (i>4)
      {
         Error("Too many array dimensions");
      }
      New->Sizes[i]=(int)(Parse_Float() + EPSILON);
      j *= New->Sizes[i];
      if ( j <= 0) {
        Error("Invalid dimension size for an array");
      }
      i++;
      GET(RIGHT_SQUARE_TOKEN)
    END_CASE

    OTHERWISE
      UNGET
      EXIT
    END_CASE
  END_EXPECT
 
  if ( i < 1 ) {
    Error( "An array declaration must have at least one dimension");
  }

  New->Dims     = i-1;
  New->Total    = j;
  New->Type     = EMPTY_ARRAY_TOKEN;
  New->DataPtrs = (void **)POV_MALLOC(sizeof(void *)*j,"array");

  j = 1;

  for(i = New->Dims; i>=0; i--)
  {
     New->Mags[i] = j;
     j *= New->Sizes[i];
  }

  for (i=0; i<New->Total; i++)
  {
     New->DataPtrs[i] = NULL;
  }

  EXPECT
    CASE(LEFT_CURLY_TOKEN)
      UNGET
        Parse_Initalizer(0,0,New);
      EXIT
    END_CASE

    OTHERWISE
      UNGET
      EXIT
    END_CASE
  END_EXPECT

  Ok_To_Declare = true;
  return(New);

};

static void Parse_Initalizer (int Sub, int Base, POV_ARRAY *a)
{
  int i;

  Parse_Begin();
  if (Sub < a->Dims)
  {
     for(i=0; i < a->Sizes[Sub]; i++)
     {
        Parse_Initalizer(Sub+1,i*a->Mags[Sub]+Base,a);
     }
  }
  else
  {
     for(i=0; i < a->Sizes[Sub]; i++)
     {
        if (!Parse_RValue (a->Type, &(a->Type), &(a->DataPtrs[Base+i]), NULL, false, false, true, false, MAX_NUMBER_OF_TABLES))
        {
          Error("Insufficent number of initializers");
        }
        Parse_Comma();
     }
  }
  Parse_End();
  Parse_Comma();
};

static void Parse_Fopen(void)
{
   IStream *rfile = NULL;
   OStream *wfile = NULL;
   DATA_FILE *New;
   char *temp;
   SYM_ENTRY *Entry;

   New=(DATA_FILE *)POV_MALLOC(sizeof(DATA_FILE),"user file");
   New->In_File=NULL;
   New->Out_File=NULL;

   GET(IDENTIFIER_TOKEN)
   Entry = Add_Symbol (1,Token.Token_String,FILE_ID_TOKEN);
   Entry->Data=(void *)New;

   temp = Parse_C_String(true);

   EXPECT
     CASE(READ_TOKEN)
       New->R_Flag = true;
       rfile = Locate_File (temp,POV_File_Text_User,NULL,true);
       if(rfile != NULL)
          New->In_File = new ITextStream(temp, rfile);
       else
          New->In_File = NULL;

       if(New->In_File == NULL)
          Error ("Cannot open user file %s (read).", temp);
       EXIT
     END_CASE

     CASE(WRITE_TOKEN)
       New->R_Flag = false;
       wfile = New_Checked_OStream(temp, POV_File_Text_User, false);
       if(wfile != NULL)
          New->Out_File= new OTextStream(temp, wfile);
       else
          New->Out_File = NULL;
 
       if(New->Out_File == NULL)
          Error ("Cannot open user file %s (write).", temp);
       EXIT
     END_CASE

     CASE(APPEND_TOKEN)
       New->R_Flag = false;
       wfile = New_Checked_OStream(temp, POV_File_Text_User, true);
       if(wfile != NULL)
          New->Out_File= new OTextStream(temp, wfile);
       else
          New->Out_File = NULL;

       if(New->Out_File == NULL)
          Error ("Cannot open user file %s (append).", temp);
       EXIT
     END_CASE

     OTHERWISE
       Expectation_Error("read or write");
     END_CASE
   END_EXPECT

   POV_FREE(temp);
}

static void Parse_Fclose(void)
{
   DATA_FILE *Data;

   EXPECT
     CASE(FILE_ID_TOKEN)
       Data=(DATA_FILE *)Token.Data;
       if(Data->In_File != NULL)
         delete Data->In_File;
       if(Data->Out_File != NULL)
         delete Data->Out_File;
       Got_EOF=false;
       Data->In_File = NULL;
       Data->Out_File = NULL;
       Remove_Symbol (1,Token.Token_String,false,NULL,0);
       EXIT
     END_CASE

     OTHERWISE
       EXIT
     END_CASE
   END_EXPECT
}

static void Parse_Read()
{
   DATA_FILE *User_File;
   SYM_ENTRY *Temp_Entry;
   int End_File=false;
   char *File_Id;

   GET(LEFT_PAREN_TOKEN)

   GET(FILE_ID_TOKEN)
   User_File=(DATA_FILE *)Token.Data;
   File_Id=POV_STRDUP(Token.Token_String);
   if(User_File->In_File == NULL)
     Error("Cannot read from file %s because the file is open for writing only.", User_File->Out_File->name());

   Parse_Comma(); /* Scene file comma between File_Id and 1st data ident */

   LValue_Ok = true;

   EXPECT
     CASE (IDENTIFIER_TOKEN)
       if (!End_File)
       {
          Temp_Entry = Add_Symbol (1,Token.Token_String,IDENTIFIER_TOKEN);
          End_File=Parse_Read_Value (User_File,Token.Token_Id, &(Temp_Entry->Token_Number), &(Temp_Entry->Data));
          Token.is_array_elem = false;
          Parse_Comma(); /* Scene file comma between 2 idents */
       }
     END_CASE

     CASE (STRING_ID_TOKEN)
       if (!End_File)
       {
          End_File=Parse_Read_Value (User_File,Token.Token_Id,Token.NumberPtr,Token.DataPtr);
          Token.is_array_elem = false;
          Parse_Comma(); /* Scene file comma between 2 idents */
       }
     END_CASE

     CASE2 (VECTOR_FUNCT_TOKEN,FLOAT_FUNCT_TOKEN)
       switch(Token.Function_Id)
       {
         case VECTOR_ID_TOKEN:
         case FLOAT_ID_TOKEN:
           if (!End_File)
           {
              End_File=Parse_Read_Value (User_File,Token.Function_Id,Token.NumberPtr,Token.DataPtr);
              Parse_Comma(); /* Scene file comma between 2 idents */
           }
           break;

         default:
           Parse_Error(IDENTIFIER_TOKEN);
           break;
       }
     END_CASE

     CASE(COMMA_TOKEN)
       if (!End_File)
       {
          Parse_Error(IDENTIFIER_TOKEN);
       }
     END_CASE

     CASE(RIGHT_PAREN_TOKEN)
       EXIT
     END_CASE

     OTHERWISE
       Parse_Error(IDENTIFIER_TOKEN);
     END_CASE
   END_EXPECT

   LValue_Ok = false;

   if (End_File)
   {
      delete User_File->In_File;
      Got_EOF=false;
      User_File->In_File = NULL;
      Remove_Symbol (1,File_Id,false,NULL,0);
   }
   POV_FREE(File_Id);
}

static int Parse_Read_Value(DATA_FILE *User_File,int Previous,int *NumberPtr,void **DataPtr)
{
   POV_BASE_NAMESPACE::ITextStream *Temp;
   bool Temp_R_Flag;
   DBL Val;
   int End_File=false;
   int i;
   EXPRESS Express;

   Temp = Input_File->In_File;
   Temp_R_Flag = Input_File->R_Flag;
   Input_File->In_File = User_File->In_File;
   Input_File->R_Flag = User_File->R_Flag;
   if(User_File->In_File == NULL)
     Error("Cannot read from file '%s' because the file is open for writing only.", User_File->Out_File->name());
   User_File->In_File = NULL; // take control over pointer 

   EXPECT
     CASE3 (PLUS_TOKEN,DASH_TOKEN,FLOAT_FUNCT_TOKEN)
       UNGET
       Val=Parse_Signed_Float();
       *NumberPtr = FLOAT_ID_TOKEN;
       Test_Redefine(Previous,NumberPtr,*DataPtr);
       *DataPtr   = (void *) Create_Float();
       *((DBL *)*DataPtr) = Val;
       Parse_Comma(); /* data file comma between 2 data items  */
       EXIT
     END_CASE

     CASE (LEFT_ANGLE_TOKEN)
       i=1;
       Express[X]=Parse_Signed_Float();  Parse_Comma();
       Express[Y]=Parse_Signed_Float();  Parse_Comma();

       EXPECT
         CASE3 (PLUS_TOKEN,DASH_TOKEN,FLOAT_FUNCT_TOKEN)
           UNGET
           if (++i>4)
           {
              Error("Vector data too long");
           }
           Express[i]=Parse_Signed_Float(); Parse_Comma();
         END_CASE

         CASE (RIGHT_ANGLE_TOKEN)
           EXIT
         END_CASE

         OTHERWISE
           Expectation_Error("vector");
         END_CASE
       END_EXPECT

       switch(i)
       {
          case 1:
            *NumberPtr = UV_ID_TOKEN;
            Test_Redefine(Previous,NumberPtr,*DataPtr);
            *DataPtr   = (void *) Create_UV_Vect();
            Assign_UV_Vect((DBL *)*DataPtr, Express);
            break;

          case 2:
            *NumberPtr = VECTOR_ID_TOKEN;
            Test_Redefine(Previous,NumberPtr,*DataPtr);
            *DataPtr   = (void *) Create_Vector();
            Assign_Vector((DBL *)*DataPtr, Express);
            break;

          case 3:
            *NumberPtr = VECTOR_4D_ID_TOKEN;
            Test_Redefine(Previous,NumberPtr,*DataPtr);
            *DataPtr   = (void *) Create_Vector_4D();
            Assign_Vector_4D((DBL *)*DataPtr, Express);
            break;

          case 4:
            *NumberPtr    = COLOUR_ID_TOKEN;
            Test_Redefine(Previous,NumberPtr,*DataPtr);
            *DataPtr      = (void *) Create_Colour();
            Assign_Colour_Express((COLC*)(*DataPtr), Express); /* NK fix assign_colour bug */
            break;
       }

       Parse_Comma(); // data file comma between 2 data items
       EXIT
     END_CASE

     CASE(STRING_LITERAL_TOKEN)
       *NumberPtr = STRING_ID_TOKEN;
       Test_Redefine(Previous,NumberPtr,*DataPtr);
       *DataPtr   = String_To_UCS2(Token.Token_String, false);
       Parse_Comma(); // data file comma between 2 data items
       EXIT
     END_CASE

     CASE (END_OF_FILE_TOKEN)
       EXIT
     END_CASE

     OTHERWISE
       Input_File->In_File = Temp;
       Expectation_Error ("float, vector, or string literal");
     END_CASE
   END_EXPECT

   if (Token.Token_Id==END_OF_FILE_TOKEN)
      End_File = true;

   Token.End_Of_File = false;
   Token.Unget_Token = false;
   Got_EOF = false;
   User_File->In_File = Input_File->In_File; // return control over pointer 
   Input_File->In_File = Temp;
   Input_File->R_Flag = Temp_R_Flag;

   return End_File ;
}

static void Parse_Write(void)
{
   char *temp;
   DATA_FILE *User_File;
   EXPRESS Express;
   int Terms;

   GET(LEFT_PAREN_TOKEN)
   GET(FILE_ID_TOKEN)

   User_File=(DATA_FILE *)Token.Data;
   if(User_File->Out_File == NULL)
     Error("Cannot write to file %s because the file is open for reading only.", User_File->In_File->name());

   Parse_Comma();

   EXPECT
     CASE5 (STRING_LITERAL_TOKEN,CHR_TOKEN,SUBSTR_TOKEN,STR_TOKEN,VSTR_TOKEN)
     CASE4 (CONCAT_TOKEN,STRUPR_TOKEN,STRLWR_TOKEN,STRING_ID_TOKEN)
       UNGET
       temp=Parse_C_String();
       if(strlen(temp) > 512)
       {
          for(char *ptr = temp; *ptr != 0; ptr++)
            User_File->Out_File->printf("%c", *ptr);
       }
       else
          User_File->Out_File->printf("%s", temp);
       POV_FREE(temp);
     END_CASE

     CASE_VECTOR
       Terms = Parse_Unknown_Vector (Express);
       switch (Terms)
       {
         case 1:
           User_File->Out_File->printf("%g",Express[X]);
           break;

         case 2:
           User_File->Out_File->printf("<%g,%g> ",Express[U],Express[V]);
           break;

         case 3:
           User_File->Out_File->printf("<%g,%g,%g> ",Express[X],Express[Y],Express[Z]);
           break;

         case 4:
           User_File->Out_File->printf("<%g,%g,%g,%g> ",Express[X],Express[Y],Express[Z],Express[T]);
           break;

         case 5:
           User_File->Out_File->printf("<%g,%g,%g,%g,%g> ",Express[X],Express[Y],Express[Z],Express[3],Express[4]);
           break;

         default:
           Expectation_Error("expression");
       }
     END_CASE

     CASE (RIGHT_PAREN_TOKEN)
       EXIT
     END_CASE

     CASE (COMMA_TOKEN)
     END_CASE

     OTHERWISE
       Expectation_Error("string");
     END_CASE
   END_EXPECT
}

static DBL Parse_Cond_Param(void)
{
  int Old_Ok = Ok_To_Declare;
  int Old_Sk = Skipping;
  DBL Val;

  Ok_To_Declare = false;
  Skipping      = false;

  Val=Parse_Float_Param();

  Ok_To_Declare = Old_Ok;
  Skipping      = Old_Sk;

  return(Val);
}

static void Parse_Cond_Param2(DBL *V1,DBL *V2)
{
  int Old_Ok = Ok_To_Declare;
  int Old_Sk = Skipping;

  Ok_To_Declare = false;
  Skipping      = false;

  Parse_Float_Param2(V1,V2);

  Ok_To_Declare = Old_Ok;
  Skipping      = Old_Sk;
}

static void Inc_CS_Index(void)
{
  if (++CS_Index >= COND_STACK_SIZE)
  {
    Error("Too many nested conditionals or macros.");
  }
}

int Parse_Ifdef_Param (void)
{
   int Local_Index;
   SYM_ENTRY *Entry;
   int retval = false;
   int i,j,k;
   register int c;
   DBL val;
   POV_ARRAY *a;

   GET(LEFT_PAREN_TOKEN)
   Inside_Ifdef=true;
   Get_Token();
   String2 = POV_STRDUP(String);
   Inside_Ifdef=false;

   /* Search tables from newest to oldest */
   for (Local_Index=Table_Index; Local_Index > 0; Local_Index--)
   {
      Entry = Find_Symbol(Local_Index,String2);
      if ( Entry != NULL)
      {
         Token.Token_Id  =   Entry->Token_Number;
         Token.is_array_elem = false;
         Token.NumberPtr = &(Entry->Token_Number);
         Token.DataPtr   = &(Entry->Data);

         if ( Token.Token_Id == PARAMETER_ID_TOKEN )
         {
            Token.NumberPtr = ((POV_PARAM *)(Entry->Data))->NumberPtr;
            Token.DataPtr   = ((POV_PARAM *)(Entry->Data))->DataPtr;
         }

         if (Token.NumberPtr && *(Token.NumberPtr)==ARRAY_ID_TOKEN)
         {
            Skip_Spaces();
            c = Echo_getc();
            Echo_ungetc(c);

            if (c!='[')
            {
               retval = true;
               break;
            }

            a = (POV_ARRAY *)(*(Token.DataPtr));
            j = 0;

            for (i=0; i <= a->Dims; i++)
            {
               GET(LEFT_SQUARE_TOKEN)
               val=Parse_Float();

               k=(int)(val + EPSILON);

               if (k<0.0)
               {
                  Error("Negative subscript");
               }

               if (k>=a->Sizes[i])
               {
                  Error("Array subscript out of range");
               }
               j += k * a->Mags[i];
               GET(RIGHT_SQUARE_TOKEN)
            }

            Token.DataPtr   = &(a->DataPtrs[j]);
            Token.NumberPtr = &(a->Type);
            Token.Token_Id = a->Type;
            Token.is_array_elem = true;

            retval = (*Token.DataPtr != NULL);
            break;
         }
         else
         {
            retval = true;
            break;
         }
      }
   }

   GET(RIGHT_PAREN_TOKEN)

   POV_FREE(String2);
   String2 = NULL;

   return retval;
}

/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void IncludeHeader(char *temp)
{
   char b[FILE_NAME_LENGTH];

   if (opts.Header_File_Name == NULL)
     return;
   if (opts.Header_File_Name[0] == '\0')
     return;

   Include_File_Index++;

   if (Include_File_Index >= MAX_INCLUDE_FILES)
   {
     Include_File_Index--;
     Error ("Too many nested include files.");
   }

   Echo_Indx = 0;

   Input_File = &Include_Files[Include_File_Index];

   IStream *is = Locate_File (temp,POV_File_Text_INC,b,true);
   if(is == NULL)
   {
      Input_File->In_File = NULL;  /* Keeps from closing failed file. */
      Stage=STAGE_INCLUDE_ERR;
      Error ("Cannot open include header file %s.", temp);
   }
   else
      Input_File->In_File = new ITextStream(b, is);

   Input_File->R_Flag=false;

   Add_Sym_Table(Input_File->In_File->name());

   Token.Token_Id = END_OF_FILE_TOKEN;
   Token.is_array_elem = false;
}

END_POV_NAMESPACE
