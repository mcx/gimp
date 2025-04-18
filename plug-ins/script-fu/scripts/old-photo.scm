;
; old-photo
;
;
; Chris Gutteridge (cjg@ecs.soton.ac.uk)
; At ECS Dept, University of Southampton, England.

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;
; Branko Collin <collin@xs4all.nl> added the possibility to change
; the border size in October 2001.

; Define the function:

(define (script-fu-old-photo inImage inLayer inDefocus inBorderSize inSepia inMottle inCopy)
  (let (
       (theImage (if (= inCopy TRUE) (car (gimp-image-duplicate inImage)) inImage))
       (theLayer 0)
       (theWidth 0)
       (theHeight 0)
       )
  (if (= inCopy TRUE)
     (gimp-image-undo-disable theImage)
     (gimp-image-undo-group-start theImage)
  )

  (gimp-selection-all theImage)

  (set! theLayer (car (gimp-image-flatten theImage)))
  (if (= inDefocus TRUE)
      (gimp-drawable-merge-new-filter theLayer "gegl:gaussian-blur" 0 LAYER-MODE-REPLACE 1.0 "std-dev-x" 0.48 "std-dev-y" 0.48 "filter" "auto")
  )
  (if (> inBorderSize 0)
      (script-fu-fuzzy-border theImage (vector theLayer) '(255 255 255)
                              inBorderSize TRUE 8 FALSE 100 FALSE TRUE )
  )
  (set! theLayer (car (gimp-image-flatten theImage)))

  (if (= inSepia TRUE)
      (begin (gimp-drawable-desaturate theLayer DESATURATE-LIGHTNESS)
             (gimp-drawable-brightness-contrast theLayer -0.078125 -0.15625)
             (gimp-drawable-color-balance theLayer TRANSFER-SHADOWS TRUE 30 0 -30)
      )
  )
  (set! theWidth (car (gimp-image-get-width theImage)))
  (set! theHeight (car (gimp-image-get-height theImage)))
  (if (= inMottle TRUE)
      (let (
	    (mLayer (car (gimp-layer-new theImage "Mottle"
                                         theWidth theHeight RGBA-IMAGE
					 100 LAYER-MODE-DARKEN-ONLY)))
	    )

             (gimp-image-insert-layer theImage mLayer 0 0)
             (gimp-selection-all theImage)
             (gimp-drawable-edit-clear mLayer)
             (gimp-selection-none theImage)
             (gimp-drawable-merge-new-filter mLayer "gegl:noise-rgb" 0 LAYER-MODE-REPLACE 1.0
                                             "independent" TRUE "red" 0.0 "green" 0.0 "blue" 0.0 "alpha" 0.5
                                             "correlated" FALSE "seed" (msrg-rand) "linear" TRUE)
             (gimp-drawable-merge-new-filter mLayer "gegl:gaussian-blur" 0 LAYER-MODE-REPLACE 1.0 "std-dev-x" 1.6 "std-dev-y" 1.6 "filter" "auto")
             (set! theLayer (car (gimp-image-flatten theImage)))
      )
  )
  (gimp-selection-none theImage)

  (if (= inCopy TRUE)
      (begin  (gimp-image-clean-all theImage)
              (gimp-display-new theImage)
              (gimp-image-undo-enable theImage)
      )
      (gimp-image-undo-group-end theImage)
  )

  (gimp-displays-flush)
  )
)

(script-fu-register-filter "script-fu-old-photo"
  _"_Old Photo..."
  _"Make an image look like an old photo"
  "Chris Gutteridge"
  "1998, Chris Gutteridge / ECS dept, University of Southampton, England."
  "16th April 1998"
  "RGB* GRAY*"
  SF-ONE-OR-MORE-DRAWABLE
  SF-TOGGLE     _"Defocus"      TRUE
  SF-ADJUSTMENT _"Border size"  '(20 0 300 1 10 0 1)
     ; since this plug-in uses the fuzzy-border plug-in, I used the
     ; values of the latter, with the exception of the initial value
     ; and the 'minimum' value.
  SF-TOGGLE     _"Sepia"        TRUE
  SF-TOGGLE     _"Mottle"       FALSE
  SF-TOGGLE     _"Work on copy" TRUE
)

(script-fu-menu-register "script-fu-old-photo"
                         "<Image>/Filters/Decor")
