LOCAL_PATH := $(call my-dir)
THIS_PATH := $(call my-dir)

# target chicken dir, containing ./lib and ./include
CHICKEN_PATH := /data/chicken

include $(CLEAR_VARS)
LOCAL_PATH              := $(CHICKEN_PATH)
LOCAL_MODULE            := chicken
LOCAL_SRC_FILES         := lib/libchicken.so
LOCAL_EXPORT_C_INCLUDES := $(CHICKEN_PATH)/include/chicken/
include $(PREBUILT_SHARED_LIBRARY)


.PHONY: jni/src/main.c
jni/src/main.c:
	csc -t -I /data/app jni/src/main.scm

LOCAL_PATH := $(THIS_PATH) # all paths now relative to prj/jni/src/
include $(CLEAR_VARS)
$(warn path path $(LOCAL_PATH))
LOCAL_MODULE := main
LOCAL_C_INCLUDES := ../SDL/include
LOCAL_SRC_FILES := ../SDL/src/main/android/SDL_android_main.c main.c
LOCAL_CFLAGS := -D GLES
LOCAL_SHARED_LIBRARIES := SDL2 chicken
LOCAL_LDLIBS := -lGLESv2 -llog
include $(BUILD_SHARED_LIBRARY)
