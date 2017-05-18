#
# Android Build Dockerfile
#
# https://github.com/lukin0110/docker-android-build
#
# Version: 0.0.1
#

# using 16.04 for openjdk-8
FROM ubuntu:16.04

# Update, upgrade and install packages
RUN \
    apt-get update && \
    apt-get -y install \
            curl unzip \
            python-software-properties software-properties-common \
            openjdk-8-jdk \
            build-essential \
            make ant \
            libxext-dev \
            mg less
#  libxext-dev needed by SDL2 for host


##################### we can use openjdk-7-jdk from above
# # Install Oracle Java JDK
# # https://www.digitalocean.com/community/tutorials/how-to-install-java-on-ubuntu-with-apt-get
# # https://github.com/dockerfile/java/blob/master/oracle-java7/Dockerfile
# RUN \
#     echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
#     add-apt-repository -y ppa:webupd8team/java && \
#     apt-get update && \
#     apt-get install -y oracle-java7-installer
####################

# Install Android SDK
# https://developer.android.com/sdk/index.html#Other
RUN \
    cd /usr/local/ && \
    curl -L -O http://dl.google.com/android/android-sdk_r24.3.3-linux.tgz && \
    tar xf android-sdk_r24.3.3-linux.tgz && \
    rm android-sdk_r24.3.3-linux.tgz


# Install Android NDK
# https://developer.android.com/tools/sdk/ndk/index.html
# https://developer.android.com/ndk/index.html
RUN \
    cd /usr/local && \
    curl -L -O http://dl.google.com/android/ndk/android-ndk-r10e-linux-x86_64.bin && \
    chmod a+x android-ndk-r10e-linux-x86_64.bin && \
    ./android-ndk-r10e-linux-x86_64.bin && \
    rm -f android-ndk-r10e-linux-x86_64.bin


# Install Gradle
RUN cd /usr/local && \
    curl -L https://services.gradle.org/distributions/gradle-2.5-bin.zip -o gradle-2.5-bin.zip && \
    unzip gradle-2.5-bin.zip


# Update & Install Android Tools
# Cloud message, billing, licensing, play services, admob, analytics
RUN \
    echo y | /usr/local/android-sdk-linux/tools/android update sdk --filter tools --no-ui --force -a && \
    echo y | /usr/local/android-sdk-linux/tools/android update sdk --filter platform-tools --no-ui --force -a && \
    echo y | /usr/local/android-sdk-linux/tools/android update sdk --filter android-19 --no-ui --force -a && \
    echo y | /usr/local/android-sdk-linux/tools/android update sdk --filter android-21 --no-ui --force -a && \
    echo y | /usr/local/android-sdk-linux/tools/android update sdk --filter android-22 --no-ui --force -a && \
    echo y | /usr/local/android-sdk-linux/tools/android update sdk --filter extra --no-ui --force -a &&\
    echo y | /usr/local/android-sdk-linux/tools/android update sdk --filter build-tools-26.0.0-preview --no-ui --force -a

# Set PATH
ENV ANDROID_HOME=/usr/local/android-sdk-linux ANDROID_NDK_HOME=/usr/local/android-ndk-r10e  GRADLE_HOME=/usr/local/gradle-2.5
ENV PATH $PATH:$ANDROID_HOME/tools:$ANDROID_NDK_HOME/platform-tools:$ANDROID_NDK_HOME:$GRADLE_HOME/bin

# building with android-12 i get:
# adb -d install com.daw7872-debug.apk 
# [100%] /data/local/tmp/com.daw7872-debug.apk
# 	pkg: /data/local/tmp/com.daw7872-debug.apk
# Failure [INSTALL_FAILED_DEXOPT]
#  so nevermind this:
#RUN echo y | /usr/local/android-sdk-linux/tools/android update sdk --filter android-12 --no-ui --force -a

RUN curl "https://www.libsdl.org/release/SDL2-2.0.5.tar.gz" | tar zxv -C /opt && ln -s /opt/SDL2-2.0.5 /opt/SDL2

# TODO uncomment this
# # Flatten the image
# # https://intercityup.com/blog/downsizing-docker-containers.html
# # Cleaning APT
# RUN \
#     apt-get remove -y curl unzip python-software-properties software-properties-common && \
#     apt-get clean autoclean && \
#     apt-get autoremove -y && \
#     rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* && \
#     rm -rf /var/cache/oracle-jdk7-installer
# #$ rm -rf /var/lib/{apt,dpkg,cache,log}/

# Define working directory.
RUN mkdir -p /data/app
#WORKDIR /data/app

# Define volume: your local app code directory can be mounted here
# Mount with: -v /your/local/directory:/data/app
#VOLUME ["/data/app"]

# Define default command
#CMD ["bash"]

RUN curl https://code.call-cc.org/releases/4.12.0/chicken-4.12.0.tar.gz \
     | tar xzv -C /opt && \
    ln -s /opt/chicken-4.12.0 /opt/chicken

RUN ln -s /usr/local/android-ndk-r10e /opt/ndk

ENV ANDROID_PLATFORM_ID=18
ENV ANDROID_PLATFORM=android-${ANDROID_PLATFORM_ID}

RUN mkdir -p /opt/android-toolchain && \
        /opt/ndk/build/tools/make-standalone-toolchain.sh \
          --platform=android-${ANDROID_PLATFORM_ID} \
	  --system=linux-x86_64 \
	  --arch=arm \
          --install-dir=/opt/android-toolchain

ENV PATH $PATH:/opt/android-toolchain/bin


# install host cross-chicken globally. any chicken-install you do
# withtin the docker image will build two versions of your egg: one
# for the host (inside this container), and one for the target
# (android).
#
# normally, you'd have a PREFIX here, so that this cross-chicken won't
# conflict with the normal chicken installation. however, in
# Dockerland, this should be ok.
#
# so if you do `docker run .../chicken-sdl2-android chicken-install
# nrepl`, you will build one nrepl egg for the docker container (so
# you can do csi -R nrepl afterwards), and for the target (an nrepl.so you can copy to android-project/libs)
#
# note that cross-chickens won't work alone! it needs the second
# chicken build below, leaving chicken.h and libchicken.so for
# our target android under /data/chicken.g
RUN cd /opt/chicken && mkdir -p /data/chicken && make install \
    PLATFORM=linux \
    TARGET_C_COMPILER=arm-linux-androideabi-gcc \
    TARGET_FEATURES="-no-feature x86 -no-feature x86-64 -feature arm -feature android" \
    TARGET_PREFIX=/data/chicken/ \
    TARGETSYSTEM=arm-linux-androideabi \
    DEBUGBUILD=0 \
    TARGET_RUN_PREFIX=/target/run/prefix

# build chicken target runtime (copy this to target android system on device)
RUN cd /opt/chicken && make \
    PLATFORM=android \
    HOSTSYSTEM=arm-linux-androideabi  \
    TARGET_FEATURES="-no-feature x86 -no-feature x86-64 -feature arm -feature android" \
    DEBUGBUILD=0 \
    ARCH= \
    PREFIX= \
    DESTDIR=/data/chicken \
    EGGDIR=/lib \
    confclean clean all install

RUN chicken-install nrepl ssax sxpath


# we need SDL2 on the host too. the sdl2 egg uses this
# print-sdl2-version which it uses to detect stuff. this is used on
# the host. the good news is that the same SDL2 version is running on
# the host and the target :).

# TODO: configure a smaller version. disable tests and stuff.
# build SDL2 without touching the original sources, so ndk-build's
# don't conflict with host configuration.
RUN cp -r /opt/SDL2-2.0.5 /tmp/SDL2-host && \
    cd /tmp/SDL2-host && \
    ./configure && \
    make install && \
    cd / && \
    rm -r /tmp/SDL2-host


# these takes forever (host, then target):
COPY eggs/miscmacros /eggs/miscmacros
RUN cd /eggs/miscmacros && chicken-install

COPY eggs/nrepl /eggs/nrepl
RUN cd /eggs/nrepl && chicken-install

# COPY eggs/opengl-glew /eggs/opengl-glew
# RUN cd /eggs/opengl-glew && chicken-install

WORKDIR /data/app


COPY android/ /data/template


# build our target SDL2 library (./libs/armeabi/SDL2.so). the reason
# it's important to build our android-version of SDL2 is that we need
# it to build the sdl2 egg below.
#
# note that we actually create a dummy android project and run
# `ndk-build SDL2` on it to get the target SDL2 native. the sdl2-egg
# build artifacts will be copied to user apps by chicken-copy-libs.
COPY eggs/sdl2 /eggs/sdl2
RUN mkdir -p /tmp/prj && \
    cp -rT /data/template/ /tmp/prj && \
    cd /tmp/prj && \
    ndk-build SDL2 && \
    cd /eggs/sdl2 && \
    env SDL2_FLAGS="-I/tmp/prj/jni/SDL/include -L/tmp/prj/obj/local/armeabi/ -lSDL2" \
        chicken-install && \
    rm -r /tmp/prj

# TODO: provide a command to dump the android template for easily
# creating new projects.
COPY build.sh chicken-copy-libs chicken-find-package /usr/bin/
