

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

### Making the docker image

```sh
user@host $ git clone https://github.com/chicken-mobile/chicken-sdl2-android-builder
user@host $ cd chicken-sdl2-android-builder
user@host $ docker build -t chicken/chicken-sdl2-android-builder:0.1 .
```

Then leave this beast for a while. The build takes half an hour on my
machine, and the ending docker image will be a horrifying 6GB or
so. It will also download large quantities of data :-(

Once built though, you should have to tools ready to make an Android
app that should actually work.

### Setup project

Your "game" sources should be at the root of the project directory,
and the android build project should be under `android`. Let's have a
loot at the bundled example:

```sh
user@host $ mkdir -p ~/games/example && cd ~/games/example
user@host $ cp ~/chicken-sdl2-android-builder/examples/basic/main.scm .
```

`main.scm` should work on your host development machine,
provided you have the sdl2 egg installed:

```
user@host $ chicken-install -s sdl2
user@host $ csi main.scm # note no -s
```

You don't need to test that it's working on you host machine, but this
is probably a good idea. Back to building this as an Android app:

```
user@host $ docker run -it --rm -v $PWD:/data/app -e HOST_USER_ID=`id -u` chicken/chicken-sdl2-android-builder:0.1 bash
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
root@builder $ cp -r /data/template/ android
root@builder $ ls -l
total 4
drwxr-xr-x 5 root root   240 May 18 00:13 android
-rw-r--r-- 1 1000 users 1350 May 18 00:10 main.scm
```

It's probably a good idea, at least for now, to commit this android
project template into your verion-control system.

### Building the APK

To build from within the container:

```
root@builder $ cd /data/app/android/
root@builder $ build.sh
```

If everything goes as planned, this should produce an
`SDLActivity-debug.apk`. `build.sh` is just a simple wrapper for
`ndk-build`, `ant debug` and some cleanup.

### Installing the APK

If volumes were mounted properly, the build artifacts are available on
our host machine:

```sh
user@host $ adb uninstall org.libsdl.app # re-install doesn't work yet :(
user@host $ adb install android/bin/SDLActivity-debug.apk
```

For this part you'll need some Android tools on the host system in
order to install the app, or send the `apk` to your phone and install
it there ("allow external sources" must be on in settings).  I don't
know how to do `adb install` from within the docker image.

## TODO

- sdl2-egg: [`make-surface` fails on 32bit platforms](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/44)
- make the resulting docker image much, much smaller (now it's 6GB)
- provide conventions for bundling assets and resources
- support multiple architectures (our natives are only armeabi, see `Application.mk`)

