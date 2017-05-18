

# chicken-android-sdl2 builder Dockerfile

You can now run CHICKEN and SDL2 on Android devices with greater ease.
This project will let you build your own docker image which can build
android apps for you. Inside the docker image you'll find:

- and Android project template (with an `AndroidManifest.xml` and everything else you love)
- Android SDK and api-level 19
- Android NDK r10e
- cross chicken-4.12.0 (builds eggs suitable for armeabi Android)
- SDL2.0.5 (on the host and libSDL2.so on Android)
- sdl2 egg 0.2.0

```sh
$ git clone https://github.com/chicken-mobile/chicken-sdl2-android-builder
$ cd chicken-sdl2-android-builder
$ docker build -t chicken/chicken-sdl2-android-builder:0.1 .
```

Then leave this beast over night. The build takes half an hour on my
machine, and the ending docker image will be a horrifying 6GB or
so. It will also download large quantities of data :-(

Once built though, you should have to tools ready to make an Android
app that works. Let's try an old SDL2 example from the
old
[chicken-android-template](https://github.com/chicken-mobile/chicken-android-template) project. The
goal is to eventually support all sdl2 standalone apps, but many (like
demos/basics.scm) don't work out of the box yet. But let's give this
simple example.scm a try:

```sh
$ mkdir -p ~/games/example && cd ~/games/example
$ curl https://raw.githubusercontent.com/chicken-mobile/chicken-android-template/master/jni/entry/example.scm > main.scm
```

Note that `main.scm` should now work on your host development machine,
provided you have the sdl2 egg installed:

```
$ chicken-install -s sdl2 miscmacros
$ csi main.scm # note no -s
```

You don't need to test that it's working on you host machine, but this
is probably a good idea. Let's get back to building that Android app:

```
$ docker run -it --rm -v $PWD:/data/app -e HOST_USER_ID=`id -u` adellica/chicken-sdl2-android-builder:0.1 bash
```

This command is a mouthful. You may want to put it in a Makefile.

Now you should be in a Android environment suited to build your
app. We mounted our host project in `/data/app` so that we can access
the "game" sources and so that we can get build artifacts from the
container.

`HOST_USER_ID` allows the container to `chown` back to your user, so
that all build artifacts won't be owned by root.

The docker image contains an Android project template which we copy
over. We only need to do this once per app:

```
/data/app $ cp -r /data/template/ android
/data/app $ ls -l
total 4
drwxr-xr-x 5 root root   240 May 18 00:13 android
-rw-r--r-- 1 1000 users 1350 May 18 00:10 main.scm
/data/app $ cd android/
/data/app $ build.sh
...
...
BUILD SUCCESSFUL # <-- hopefully
Total time: 7 seconds
/data/app $ exit # <-- back to host
```

If volumes were mounted properly, the build artifacts are available on
our host machine:

```sh
$ adb uninstall org.libsdl.app # re-install doesn't work yet :(
$ adb install android/bin/SDLActivity-debug.apk
```

For this part you'll need some Android tools on the host system in
order to install the app. I don't know how to do this from the docker
image.

## TODO

- sdl2-egg: [`create-surface` fails on 32bit platforms](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/44)
- make the resulting docker image much, much smaller (now it's 6GB)
- support multiple architectures (our natives are only armeabi, see `Application.mk`)

## History

This project is inspired by
https://github.com/chicken-mobile/chicken-android-template. The
template made it easier to get things up and running, but way too
difficult still. The build process is incredibly fragile and is very
intrusive to the development machine.

