//    Persistence of Vision Ray Tracer version 3.5 Include File
//    File: stdinc.inc
//    Last updated: 2001.8.16
//    Description: The most commonly used include files.

#ifndef(STDINC_INC_TEMP)
#declare STDINC_INC_TEMP = version;
#version 3.5;

#ifdef(View_POV_Include_Stack)
    #debug "including stdinc.inc\n"
#end

#include "colors.inc"
#include "shapes.inc"
#include "transforms.inc"
#include "consts.inc"
#include "functions.inc"
#include "math.inc"
#include "rand.inc"

#version STDINC_INC_TEMP;
#end//arrays.inc

