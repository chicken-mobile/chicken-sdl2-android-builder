

android/bin/SDLActivity-debug.apk: *.scm
	docker run -it --rm -v $(PWD):/data/app \
		 -e HOST_USER_ID=`id -u` \
		adellica/chicken-sdl2-android-builder:0.1 sh -c 'cd /data/app/android && build.sh'

reinstall:
	adb uninstall org.libsdl.app \
	 && adb install -r android/bin/SDLActivity-debug.apk 
