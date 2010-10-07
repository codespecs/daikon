// Persistence Of Vision raytracer version 3.5 sample file.
// Created by Fabien Mosen - April 8 2001
// This file demonstrates the use of the "panoramic" camera.
//
// -w320 -h320
// -w512 -h512 +a0.3

#include "colors.inc"
#include "camera-context.inc" //common file containing object definitions for the camera demos

camera {
        panoramic
	location <8,10,5>
        angle 120
        look_at <0,10,0>
        }

//don't forget to render this with the image ratio equal to 1 (height = width),
//or, instead of being framed in a half-circle, the upper part of the image will
//be "squished" into a half-ellipse.

box {<-15,0,-15>,<15,-1,15>
     pigment {checker White,Gray90 scale 5}
     finish {reflection {.1}}
     }

object {Cubes1 translate <5,0,-5>}
object {Cubes2 translate <9,8,-6>}
object {Cubes3 translate <-15,0,5>}
object {Cubes4 translate <-10,0,-5>}

sphere {<-2,3,14>,3 pigment {SteelBlue} finish {phong .2 reflection {.3}}}

sky_sphere {pigment {rgb <0.859,0.910,0.831>}}

light_source {<100,120,130> White*2}

