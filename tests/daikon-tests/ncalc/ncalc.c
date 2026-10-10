#include <jni.h>
#include "ncalc.h" // Include the generated header

JNIEXPORT jint JNICALL Java_ncalc_add(JNIEnv *env, jobject obj, jint a, jint b) {
    // 'env' is a pointer to the JNI environment, 'obj' is a reference to the Java object
    // 'a' and 'b' are the integer parameters passed from Java
    return a + b; // Perform the addition in native code
}
