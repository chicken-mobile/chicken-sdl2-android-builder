
;;; This is a simple utility program used during installation of the
;;; sdl2 egg. It prints out the current SDL version as a dot-separated
;;; string. This allows the sdl2 egg installation script to set
;;; feature flags depending on the version of SDL being used.

(import foreign)
(foreign-declare "#include \"SDL.h\"")

(printf "~S.~S.~S~N"
        (foreign-value "SDL_MAJOR_VERSION" int)
        (foreign-value "SDL_MINOR_VERSION" int)
        (foreign-value "SDL_PATCHLEVEL" int))
