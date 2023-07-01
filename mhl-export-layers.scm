;mhl-export-layers.scm
;==============================================================================
;MHL-Export layers
;
;GIMP Script-Fu that exports layers to separate files.
;
;Copyright (C) 2023 Melon (https://github.com/Mhlov)
;
; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;==============================================================================
;Tested on GIMP 2.10.34


(define (mhl-el-get-linked-flag item
                                linked-only)
  (if
    (or
      (and (= TRUE linked-only)
           (= TRUE (car (gimp-item-get-linked item))))
      (= FALSE linked-only))
    ; then
    FALSE
    ; else
    TRUE))


(define (mhl-el-get-layers layers
                           visible-only
                           linked-only)
  (define result-list '())

  (for-each
    (lambda (layer)
      (if
        (or
          (and (= TRUE visible-only)
               (= TRUE (car (gimp-item-get-visible layer))))
          (= FALSE visible-only))
        (if
          (or
            (and (= TRUE linked-only)
                 (= TRUE (car (gimp-item-get-linked layer))))
            (= FALSE linked-only))
          (set! result-list (append result-list
                               (list layer))))))
    layers)

  result-list)


; Make a string for filename from the layer list index
(define (mhl-el-index-to-name index
                              max-n)
  (define name (number->string (+ index 1)))

  ; adding leading zeros
  (let ((i 0))
    (while (< i (- (string-length (number->string max-n))
                   (string-length name)))
           (set! name (string-append "0" name))))

  name)


; Hide all layers from the list
; and return a list with the initial visibility values
(define (mhl-el-hide-all-layers layers)
  (define initial-values '())
  (for-each
    (lambda (layer)
      (set! initial-values
        (append initial-values (gimp-item-get-visible layer)))
      (gimp-item-set-visible layer FALSE))
    layers)
  initial-values)


; Restore the initial visibility values in the layers from the list
(define (mhl-el-restore-visibility-of-all-layers layers
                                                 values)
  (let ((i 0))
    (while (< i (length layers))
           (let*
             (
              (layer (car (list-tail layers i)))
              (value (car (list-tail values i))))
             (gimp-item-set-visible layer value))
           (set! i (+ i 1)))))


; main
(define (mhl-export-layers image
                           folder
                           filename-as
                           filename-prefix
                           filetype
                           visible-only
                           linked-only)

  ; Start of the undo group
  (gimp-image-undo-group-start image)

  (define all-layers (vector->list (cadr (gimp-image-get-layers image))))

  (define layers (mhl-el-get-layers all-layers
                                    visible-only
                                    linked-only))
  (define n-of-layers (length layers))

  ; Unfortunately the 'gimp-file-save' procedure saves separate layers only
  ; in the non-interactive(1) run-mode. So we need to hide all layers
  ; and show only the desired one.
  (define initial-visibility-values (mhl-el-hide-all-layers all-layers))

  (let ((i 0))
    (while (< i n-of-layers)
           (let*
             (
              (layer (car (list-tail layers i)))
              (file-name (string-append folder
                                        "/"
                                        filename-prefix
                                        (if
                                          (= filename-as 0)
                                          ; then filename from layer name
                                          (car (gimp-layer-get-name layer))
                                          ; else filename from layer number
                                          (mhl-el-index-to-name i n-of-layers))
                                        "."
                                        filetype)))
             (gimp-item-set-visible layer TRUE)
             (gimp-file-save (if (= i 0) 0 2)
                             ;1
                             image
                             layer
                             file-name
                             file-name)
             (gimp-item-set-visible layer FALSE)
             )
           (set! i (+ i 1))))

  (mhl-el-restore-visibility-of-all-layers all-layers
                                           initial-visibility-values)

  ; End of the undo group
  (gimp-image-undo-group-end image)

  ; Flush all internal changes to the user interface
  (gimp-displays-flush))


(script-fu-register "mhl-export-layers"
                    _"<Image>/Script-Fu/MHL-Export layers"
                    "Export layers in separate files."
                    "MHL <mhl@localhost>"
                    "MHL"
                    "2023"
                    "*"
                    SF-IMAGE "Image" 0
                    SF-DIRNAME "Folder name" "."
                    SF-OPTION "Filename from layer" '("name" "number")
                    SF-STRING "Filename prefix" ""
                    SF-STRING "File type extension" "png"
                    SF-TOGGLE "Visible layers only" FALSE
                    SF-TOGGLE "Linked layers only"  FALSE
                    )

