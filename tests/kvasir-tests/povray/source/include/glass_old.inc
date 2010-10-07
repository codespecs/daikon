//    Persistence of Vision Ray Tracer version 3.5 Include File
//    File: glass_old.inc
//    Last updated: 2001.8.9
//    Description: Glass finishes and textures (was glass.inc in versions
//    prior to 3.5).
//    Changes in version 3.1: moved refraction and ior to the interior statment.
//    Use I_Glass in conjunction with each of the finish statments below.
//    Version 3.5: Renamed file as glass_old.inc.

#ifndef(Glass_Old_Inc_Temp)
#declare Glass_Old_Inc_Temp=version;
#version 3.5;

#ifdef(View_POV_Include_Stack)
    #debug "including glass_old.inc\n"
#end

#warning "\nDue to changes in version 3.1, you must add interior {I_Glass} to all objects calling glass_old.inc textures and finishes...\n"

// Glass Interior
#declare I_Glass =
interior {
    ior 1.5
}
  
// Glass Finishes
#declare F_Glass1 =
finish {
    specular 1
    roughness 0.001
    ambient 0
    diffuse 0
    reflection 0.1
}   
  

#declare F_Glass2 = 
finish {
    ambient 0
    diffuse 0
    reflection 0.5
    phong 0.3
    phong_size 60
}

#declare F_Glass3 =
finish  {
    ambient 0.1
    diffuse 0.1
    reflection 0.1
    specular 0.8
    roughness 0.003
    phong 1
    phong_size 400
}

#declare F_Glass4 =
finish {
    ambient 0.1
    diffuse 0.1
    reflection .25
    specular 1
    roughness 0.001
}

// Glass Textures

// Simple clear glass
#declare T_Glass1 =
texture {
    pigment { color rgbf<1.0, 1.0, 1.0, 0.7> }
    finish  { F_Glass1 }
}

// More like an acrylic plastic
#declare T_Glass2 =
texture {
    pigment { color rgbf<1.0, 1.0, 1.0, 1.0> }
    finish  { F_Glass2 }
}


// An excellent lead crystal glass!
#declare T_Glass3 =
texture {
    pigment { color rgbf <0.98, 0.98, 0.98, 0.9> }
    finish { F_Glass3 }
}

#declare T_Glass4 =
texture {
    pigment { color rgbf <0.98, 1.0, 0.99, 0.75> }
    finish { F_Glass4 }
}


#declare T_Old_Glass=
texture {
    finish { F_Glass4 }
    pigment { color rgbf <0.8, 0.9, 0.85, 0.85> }
}

#declare T_Winebottle_Glass=
texture {
    finish { F_Glass4 }
    pigment { color rgbf <0.4, 0.72, 0.4, 0.6> }
}

#declare T_Beerbottle_Glass=
texture {
    finish { F_Glass4 }
    pigment { color rgbf <0.7, 0.5, 0.1, 0.6> }
}

// A few color variations on Norm's glass
// Ruby glass
#declare T_Ruby_Glass =
texture {
    finish { F_Glass4 }
    pigment { color rgbf <0.9, 0.1, 0.2, 0.8> }
}

#declare T_Green_Glass =
texture {
    pigment { color rgbf <0.8, 1, 0.95, 0.9> }
    finish { F_Glass3 }
}

#declare T_Dark_Green_Glass=
texture {
    finish { F_Glass4 }
    pigment { color rgbf <0.1, 0.7, 0.8, 0.8> }
}

#declare T_Yellow_Glass=
texture {
    finish { F_Glass4 }
    pigment { color rgbf <0.8, 0.8, 0.2, 0.8> }
}

// Orange/Amber glass
#declare T_Orange_Glass=
texture {
    finish { F_Glass4 }
    pigment { rgbf <1.0, 0.5, 0.0, 0.8> }
}

// Vicks bottle, glass
#declare T_Vicksbottle_Glass=
texture {
    finish { F_Glass4 }
    pigment { color rgbf <0.1, 0.15, 0.5, 0.9> }
}

#version Glass_Old_Inc_Temp;
#end
