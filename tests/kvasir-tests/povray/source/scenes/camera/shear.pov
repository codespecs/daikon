// Persistence Of Vision raytracer version 3.5 sample file.
// shear.pov
// Created by Fabien Mosen - April 8 2001
// This file demonstrates shearing the camera,
// to prevent 'falling buildings'.
//
// -w320 -h240 -uv
// -w800 -h600 +a0.3 -uv

// ********** IMPORTANT *******************
// Use ' -uv ' on the commandline,
// because, after aplying the matrix,
// the camera vectors are not
// perpendicular any more.

#include "colors.inc"
#include "camera-context.inc" //common file containing object definitions for the camera demos

#declare CamLoc= <0,1,25>;
#declare CamLook=<0,10,0>;

#declare S_angle=(CamLook.y-CamLoc.y)/CamLoc.z;

#declare Shear= transform {
   matrix <  1,  0,  0,
             0,  1,  -S_angle,
             0,  0,  1,
             0,  0,  0 >
}
camera {
        perspective //keyword is facultative in this case
        location CamLoc
        angle 90
        transform Shear // comment out to see 'falling buildings'
        rotate <0,45,0>
        look_at CamLook
        }

box {<-15,0,-15>,<15,-1,15>
     pigment {checker White,Gray90 scale 5}
     finish {reflection {.1}}
     }

object {Cubes1 translate <5,0,-5>}
object {Cubes2 translate <9,8,-6>}
object {Cubes3 translate <-15,0,5>}
object {Cubes4 translate <-10,0,-5>}

sphere {<-2,3,14>,3 pigment {SteelBlue} finish {phong .2 reflection
.3}}

sky_sphere {pigment {rgb <0.859,0.910,0.831>}}

light_source {<100,120,130> White*2}
