;
; fuzzy-border
;
; Do a cool fade to a given color at the border of an image (optional shadow)
; Will make image RGB if it isn't already.
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

; Define the function:

(define (script-fu-fuzzy-border inImage
                                inLayers
                                inColor
                                inSize
                                inBlur
                                inGranu
                                inShadow
                                inShadWeight
                                inCopy
                                inFlatten
        )

  (define (chris-color-edge inImage inLayer inColor inSize)
    (gimp-selection-all inImage)
    (gimp-selection-shrink inImage inSize)
    (gimp-selection-invert inImage)
    (gimp-context-set-background inColor)
    (gimp-drawable-edit-fill inLayer FILL-BACKGROUND)
    (gimp-selection-none inImage)
  )

  (let (
       (theWidth (car (gimp-image-get-width inImage)))
       (theHeight (car (gimp-image-get-height inImage)))
       (theImage (if (= inCopy TRUE) (car (gimp-image-duplicate inImage))
                                      inImage))
       (inLayer (vector-ref inLayers 0))
       (theLayer 0)
       )

    (gimp-context-push)
    (gimp-context-set-defaults)

    (if (= inCopy TRUE)
        (gimp-image-undo-disable theImage)
        (gimp-image-undo-group-start theImage)
    )

    (gimp-selection-all theImage)

    (if (> (car (gimp-drawable-type inLayer)) 1)
        (gimp-image-convert-rgb theImage)
    )

    (set! theLayer (car (gimp-layer-new theImage
                                        "layer 1"
                                        theWidth
                                        theHeight
                                        RGBA-IMAGE
                                        100
                                        LAYER-MODE-NORMAL)))

    (gimp-image-insert-layer theImage theLayer 0 0)


    (gimp-drawable-edit-clear theLayer)
    (chris-color-edge theImage theLayer inColor inSize)

    (gimp-layer-scale theLayer
                      (/ theWidth inGranu)
                      (/ theHeight inGranu)
                      TRUE)

    (gimp-drawable-merge-new-filter theLayer "gegl:noise-spread" 0 LAYER-MODE-REPLACE 1.0 "amount-x" (/ inSize inGranu) "amount-y" (/ inSize inGranu) "seed" (msrg-rand))
    (chris-color-edge theImage theLayer inColor 1)
    (gimp-layer-scale theLayer theWidth theHeight TRUE)

    (gimp-image-select-item theImage CHANNEL-OP-REPLACE theLayer)
    (gimp-selection-invert theImage)
    (gimp-drawable-edit-clear theLayer)
    (gimp-selection-invert theImage)
    (gimp-drawable-edit-clear theLayer)
    (gimp-context-set-background inColor)
    (gimp-drawable-edit-fill theLayer FILL-BACKGROUND)
    (gimp-selection-none theImage)
    (chris-color-edge theImage theLayer inColor 1)

    (if (= inBlur TRUE)
        (gimp-drawable-merge-new-filter theLayer "gegl:gaussian-blur" 0 LAYER-MODE-REPLACE 1.0 "std-dev-x" (* 0.32 inSize) "std-dev-y" (* 0.32 inSize) "filter" "auto")
    )
    (if (= inShadow TRUE)
        (begin
          (gimp-image-insert-layer theImage
                                   (car (gimp-layer-copy theLayer)) 0 -1)
          (gimp-layer-scale theLayer
                            (- theWidth inSize) (- theHeight inSize) TRUE)
          (gimp-drawable-desaturate theLayer DESATURATE-LIGHTNESS)
          (gimp-drawable-brightness-contrast theLayer 0.5 0.5)
          (gimp-drawable-invert theLayer FALSE)
          (gimp-layer-resize theLayer
                             theWidth
                             theHeight
                             (/ inSize 2)
                             (/ inSize 2))
          (gimp-drawable-merge-new-filter theLayer "gegl:gaussian-blur" 0 LAYER-MODE-REPLACE 1.0 "std-dev-x" (* 0.32 (/ inSize 2)) "std-dev-y" (* 0.32 (/ inSize 2)) "filter" "auto")
          (gimp-layer-set-opacity theLayer inShadWeight)
        )
    )
    (if (= inFlatten TRUE)
        (gimp-image-flatten theImage)
    )
    (if (= inCopy TRUE)
        (begin  (gimp-image-clean-all theImage)
                (gimp-display-new theImage)
                (gimp-image-undo-enable theImage)
         )
        (gimp-image-undo-group-end theImage)
    )
    (gimp-displays-flush)

    (gimp-context-pop)
  )
)

(script-fu-register-filter "script-fu-fuzzy-border"
  _"_Fuzzy Border..."
  _"Add a jagged, fuzzy border to an image"
  "Chris Gutteridge"
  "1998, Chris Gutteridge / ECS dept, University of Southampton, England."
  "3rd April 1998"
  "RGB* GRAY*"
  SF-ONE-OR-MORE-DRAWABLE
  SF-COLOR      _"Color"                  "white"
  SF-ADJUSTMENT _"Border size"            '(16 1 300 1 10 0 1)
  SF-TOGGLE     _"Blur border"            TRUE
  SF-ADJUSTMENT _"Granularity (1 is Low)" '(4 1 16 0.25 5 2 0)
  SF-TOGGLE     _"Add shadow"             FALSE
  SF-ADJUSTMENT _"Shadow weight (%)"      '(100 0 100 1 10 0 0)
  SF-TOGGLE     _"Work on copy"           TRUE
  SF-TOGGLE     _"Flatten image"          TRUE
)

(script-fu-menu-register "script-fu-fuzzy-border"
                         "<Image>/Filters/Decor")
