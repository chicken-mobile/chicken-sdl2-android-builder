;; The contents of this demo file are made available under the CC0 1.0
;; Universal Public Domain Dedication. See LICENSE-CC0.txt or visit
;; http://creativecommons.org/publicdomain/zero/1.0/


;;; This program demonstrates how to create and use sdl2:rwops from
;;; memory pointers, blobs, strings, and SRFI-4 u8vectors. This demo
;;; loads and saves BMP image data, but you can also use sdl2:rwops
;;; for other things, like loading/saving dollar gestures and loading
;;; WAV audio data. These same techniques also apply to libraries like
;;; sdl2-image and SDL_Mixer, which use sdl2:rwops to load image and
;;; audio data, respectively.
;;;
;;; It is also possible to create sdl2:rwops that access a file, by
;;; passing a file path to rw-from-file. But, this program does not
;;; demonstrate that use case, because it is so simple.


;;; CAUTION: Creating a sdl2:rwops from a blob or string in
;;; CHICKEN-managed memory is unstable: the blob/string might be
;;; garbage collected or moved in memory, which would break the
;;; sdl2:rwops. To be safe, you should evict the blob/string (using
;;; object-evict from the lolevel module) and create the sdl2:rwops
;;; from the evicted blob/string. You may wish to release the evicted
;;; blob/string (using object-release) after you have closed the
;;; sdl2:rwops.


(use (prefix sdl2 sdl2:)
     srfi-4 lolevel miscmacros)


;; Initialize SDL
(sdl2:set-main-ready!)
(sdl2:init! '(video))

;; Automatically call sdl2:quit! when program exits normally.
(on-exit sdl2:quit!)

;; Call sdl2:quit! and then call the original exception handler if an
;; unhandled exception reaches the top level.
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HARDCODED BMP DATA
;;;
;;; These are data structures each containing the data from a
;;; different 24-bit, 8x8 pixels BMP image file.

;;; A blob with image data of a smiling yellow face.
(define smile-bmp-blob
  (object-evict '#${424d3a010000000000007a0000006c00000008000000080000000100180000000000c0000000130b0000130b0000000000000000000042475273000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000008000808000800099a60098a60098a60099a680008080008080008000a1ae00a9b500adb800aab600a2ae0098a680008000a4b100b4bd00000000000000000000000000a5b10098a600b1bb00000000d4d900d9df00d4d900c6cd000000009eab00b9c200cfd600e4e800f0f200e4e800d0d600bbc400a4b100bbc300d0d700000000f7f800e7eb00000000bbc500a6b280008000c9cf00d9dd00e1e400dadf00c9d000b6bf80008080008080008000c5cd00cbd200c6cd00bbc4800080800080}))

;;; A string with image data of a frowning blue face.
(define frown-bmp-string
  (object-evict "BM:\x01\x00\x00\x00\x00\x00\x00z\x00\x00\x00l\x00\x00\x00\b\x00\x00\x00\b\x00\x00\x00\x01\x00\x18\x00\x00\x00\x00\x00\xC0\x00\x00\x00\x13\v\x00\x00\x13\v\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00BGRs\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x80\x80\x00\x80\xA6q\x00\xA6q\x00\xA6q\x00\xA6q\x00\x80\x00\x80\x80\x00\x80\x80\x00\x80\xAEw\x00\xB6|\x00\xB8~\x00\xB6|\x00\xAEw\x00\xA6q\x00\x80\x00\x80\xB2y\x00\x00\x00\x00\xC8\x88\x00\xCC\x8B\x00\xC8\x88\x00\xC0\x83\x00\x00\x00\x00\xA6q\x00\xBC\x80\x00\xCC\x8B\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xBE\x82\x00\xACu\x00\xC2\x84\x00\xD6\x92\x00\xE8\x9E\x00\xF2\xA5\x00\xE8\x9E\x00\xD6\x92\x00\xC4\x86\x00\xB2y\x00\xC4\x86\x00\xD8\x93\x00\x00\x00\x00\xF8\xA9\x00\xEC\xA1\x00\x00\x00\x00\xC6\x87\x00\xB2y\x00\x80\x00\x80\xD0\x8E\x00\xDE\x97\x00\xE4\x9C\x00\xE0\x99\x00\xD0\x8E\x00\xC0\x83\x00\x80\x00\x80\x80\x00\x80\x80\x00\x80\xCE\x8D\x00\xD2\x8F\x00\xCE\x8D\x00\xC4\x86\x00\x80\x00\x80\x80\x00\x80"))

;;; A blob created from a SRFI-4 u8vector, with image data of a green
;;; face with a tongue.
(define tongue-bmp-u8vector-blob
  (object-evict (u8vector->blob/shared '#u8(66 77 58 1 0 0 0 0 0 0 122 0 0 0 108 0 0 0 8 0 0 0 8 0 0 0 1 0 24 0 0 0 0 0 192 0 0 0 19 11 0 0 19 11 0 0 0 0 0 0 0 0 0 0 66 71 82 115 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 128 0 128 128 0 128 0 166 0 0 166 0 0 166 0 0 166 0 128 0 128 128 0 128 128 0 128 0 174 0 0 182 0 0 184 0 0 0 189 0 0 189 0 166 0 128 0 128 0 178 0 0 190 0 0 200 0 0 204 0 0 0 189 0 0 189 0 178 0 0 166 0 0 188 0 0 204 0 0 0 0 0 0 0 0 0 0 0 0 0 0 190 0 0 172 0 0 194 0 0 214 0 0 232 0 0 242 0 0 232 0 0 214 0 0 196 0 0 178 0 0 196 0 0 216 0 0 0 0 0 248 0 0 236 0 0 0 0 0 198 0 0 178 0 128 0 128 0 208 0 0 222 0 0 228 0 0 224 0 0 208 0 0 192 0 128 0 128 128 0 128 128 0 128 0 206 0 0 210 0 0 206 0 0 196 0 128 0 128 128 0 128))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOADING IMAGE DATA USING RWOPS

;;; A sdl2:surface loaded from smile-bmp-blob.
(define smile-surf
  (sdl2:load-bmp-rw
   ;; Create an sdl2:rwops from a blob:
   (sdl2:rw-from-blob smile-bmp-blob)
   ;; Close the sdl2:rwops after loading:
   #t))

;;; An sdl2:surface loaded from frown-bmp-string.
(define frown-surf
  (sdl2:load-bmp-rw
   ;; Create an sdl2:rwops from a string:
   (sdl2:rw-from-string frown-bmp-string)
   ;; Close the sdl2:rwops after loading:
   #t))

;;; An sdl2:surface loaded from tongue-bmp-u8vector.
(define tongue-surf
  (sdl2:load-bmp-rw
   ;; Create an sdl2:rwops from a blob created from a u8vector:
   (sdl2:rw-from-blob tongue-bmp-u8vector-blob)
   ;; Close the sdl2:rwops after loading:
   #t))


(assert (sdl2:surface? smile-surf))
(assert (sdl2:surface? frown-surf))
(assert (sdl2:surface? tongue-surf))


;;; The hardcoded data is not needed anymore, so for demonstration
;;; purposes we will release them.
(object-release smile-bmp-blob)
(object-release frown-bmp-string)
(object-release tongue-bmp-u8vector-blob)

;;; Unset the global variables to prevent accidental access to
;;; released objects.
(set! smile-bmp-blob (void))
(set! frown-bmp-string (void))
(set! tongue-bmp-u8vector-blob (void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLIT THE FACES TOGETHER

;;; A 24x8 24-bpp surface, to hold the three faces side by side.
(define faces-surf (sdl2:make-surface 24 8 24))

(sdl2:blit-surface! smile-surf  #f faces-surf (sdl2:make-rect 0 0))
(sdl2:blit-surface! frown-surf  #f faces-surf (sdl2:make-rect 8 0))
(sdl2:blit-surface! tongue-surf #f faces-surf (sdl2:make-rect 16 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAVE IMAGE DATA TO RWOPS

;;; Now allocate a block of memory and save faces-surf as a BMP image
;;; data into that pointer, to show it's possible to save to a rwops.
;;; Instead of a pointer you could also do this with a blob, using
;;; make-blob instead of allocate.

;;; Calculate how many bytes of memory to allocate to hold the BMP
;;; data. 512 bytes is a safe estimate about how much extra BMP data
;;; there might be. It is safe to allocate more memory than you need,
;;; but sdl2:save-bmp-rw! will fail if you allocate not enough memory.
(define faces-bmp-pointer-size
  (+ 512  ;; extra room for BMP headers, etc.
     (* 3 ;; 3 bytes (24 bits) per pixel
        (sdl2:surface-pitch faces-surf)
        (sdl2:surface-h faces-surf))))

(define faces-bmp-pointer
  (allocate faces-bmp-pointer-size))


(define (save-bmp-to-pointer! surf pointer size)
  (sdl2:save-bmp-rw!
   surf
   ;; Create a sdl2:rwops from a pointer and size (bytes). Because this
   ;; sdl2:rwops will be written to, you must use sdl2:rw-from-mem.
   ;; Below we show how to create a read-only memory sdl2:rwops.
   (sdl2:rw-from-mem pointer size)
   ;; close the rwops after saving
   #t))


;;; Save faces-surf to the pointer.
(save-bmp-to-pointer! faces-surf
                      faces-bmp-pointer
                      faces-bmp-pointer-size)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RELOAD IMAGE DATA

;;; Now load the BMP data that was just saved, to create another
;;; surface with the same contents as faces-surf.


(define (load-bmp-from-pointer pointer size)
  (sdl2:load-bmp-rw
   ;; Create a sdl2:rwops from a pointer and size (bytes). Because the
   ;; pointer will only be read from (not written to), you can create
   ;; a read-only sdl2:rwops using sdl2:rw-from-const-mem. It would be
   ;; perfectly fine to use sdl2:rw-from-mem here too, but since this
   ;; is a demo we will show how to do a read-only sdl2:rwops.
   (sdl2:rw-from-const-mem pointer size)
   ;; close the rwops after loading
   #t))


(define faces2-surf
  (load-bmp-from-pointer
   faces-bmp-pointer
   faces-bmp-pointer-size))


;;; Free faces-bmp-pointer's memory now that we are done with it. This
;;; is not really necessary in this program because the program will
;;; end soon, but in long-running programs it is important to free
;;; memory that you allocate, once you are done using the memory.
(free faces-bmp-pointer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY THE RESULTS

;;; Now open a window and blit faces-surf (on top) and faces2-surf
;;; (below), so the user can see that everything worked correctly. The
;;; surfaces are quite small (24x8 each), so do a scaled blit to make
;;; them bigger so the user can see them better.

(define window (sdl2:create-window!
                "RWops Demo"
                'undefined 'undefined
                384 256))

(define window-surf (sdl2:window-surface window))

(sdl2:blit-scaled! faces-surf #f window-surf
                   (sdl2:make-rect 0 0 384 128))

(sdl2:blit-scaled! faces2-surf #f window-surf
                   (sdl2:make-rect 0 128 384 128))

(sdl2:update-window-surface! window)


;;; Wait for the user to close the window.
(while (not (sdl2:quit-requested?))
  (sdl2:wait-event!))
