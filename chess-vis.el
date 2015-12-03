;;; chess-vis.el --- Train chess visualisation       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  

;; Author: Nick Thomson
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate chess positions to train visualisation.

;;; Code:

(require 'cl)

(defun alg-to-coord (string)
  (destructuring-bind (file rank) (string-to-list string)
    (cons (- file 96) (- rank 48))))

(defun coord-to-alg (square)
  (destructuring-bind (file . rank) square
    (string (+ file 96) (+ rank 48))))

(defconst train-pieces '(("N" [[-1 -2] [-1 2] [-2 -1] [-2 1] [1 -2] [1 2] [2 -1] [2 1]])
			 ("B" [[-1 -1] [-1 1] [-2 -2] [-2 2] [-3 -3] [-3 3] [-4 -4] [-4 4] [-5 -5] [-5 5] [-6 -6] [-6 6] [-7 -7] [-7 7]
			  [1 -1] [1 1] [2 -2] [2 2] [3 -3] [3 3] [4 -4] [4 4] [5 -5] [5 5] [6 -6] [6 6] [7 -7] [7 7]])
			 ("R" [[-1 0] [-2 0] [-3 0] [-4 0] [-5 0] [-6 0] [-7 0] [0 -1] [0 -2] [0 -3] [0 -4] [0 -5] [0 -6] [0 -7]
			  [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]])
			 ("Q" [[-1 -1] [-1 1] [-2 -2] [-2 2] [-3 -3] [-3 3] [-4 -4] [-4 4] [-5 -5] [-5 5] [-6 -6] [-6 6] [-7 -7] [-7 7]
			  [1 -1] [1 1] [2 -2] [2 2] [3 -3] [3 3] [4 -4] [4 4] [5 -5] [5 5] [6 -6] [6 6] [7 -7] [7 7]
			  [-1 0] [-2 0] [-3 0] [-4 0] [-5 0] [-6 0] [-7 0] [0 -1] [0 -2] [0 -3] [0 -4] [0 -5] [0 -6] [0 -7]
			  [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]])
			 ("K" [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))

(defun colour (square)
  (destructuring-bind (file . rank) square
    (if (evenp (+ file rank)) "black" "white")))

;; Requires -cons-pair? to be sure
(defun squarep (square)
  (cl-flet ((validp (x) (and (>= x 1) (<= x 8))))
    (destructuring-bind (file . rank) square
	   (and (validp file) (validp rank)))))

(defun square< (square1 square2)
  (destructuring-bind ((file1 . rank1) (file2 . rank2)) (list square1 square2)
    (or (< file1 file2) (and (= file1 file2 ) (< rank1 rank2)))))

(defun train-neighbourhood (square piece)
  (-filter 'squarep
	   (map 'list
		(lambda (x)
		  (cons (+ (car square) (elt x 0))
			(+ (cdr square) (elt x 1))))
		(cadr (assoc piece train-pieces)))))

(defun train-ball (square piece n)
  (if (= n 0)
      (list square)
    (reduce '-union (mapcar (lambda (x) (train-neighbourhood x piece)) (train-ball square piece (1- n))))))

(defun train-sphere (square piece n)
  (set-difference (train-ball square moves n) (train-ball square moves (1- n))))

(defun train-paths-from (square piece n)
  (if (= n 1)
      (mapcar (lambda (x) (cons square (list x))) (train-neighbourhood square piece))
    (-union
     (reduce
      '-union
      (mapcar (lambda (x) (mapcar (lambda (y) (cons square y))(train-paths-from x piece (1- n))))
	      (train-neighbourhood square piece)))
     (train-paths-from square piece (1- n)))))

(defun train-paths (square1 square2 piece n)
  (-filter (lambda (x) (equal (car (last x)) square2)) (train-paths-from square1 piece n)))

(defun train-shortest-paths (square1 square2 piece)
  (let ((max 7))
    (do ((n 0 (1+ n))
	 (paths nil (train-paths square1 square2 piece n)))
	((or (= n max) paths) paths))))

(defun path-to-alg (path)
  (mapconcat 'coord-to-alg path "-"))

(defun generate-square ()
  (cons (1+ (random 8)) (1+ (random 8))))

(defun generate-square-new (seq)
  (do ((x nil (generate-square)))
      ((and x (not (member x seq))) x)))

(defun choose-from (x)
  (elt x (random (length x))))

(defun jiggle-square (square)
  (do ((x nil (choose-from (train-neighbourhood square "K"))))
      ((and x (squarep x)) x)))

(defun attackingp (piece1 piece2)
  (first (member (cdr piece2) (train-neighbourhood (cdr piece1) (car piece1)))))

(defvar train-pos-pieces '("N" "B"))
(defvar train-pos-nwhite 2)
(defvar train-pos-nblack 1)

(defun generate-position ()
  (cl-flet ((place (max)
		     (do ((n 0 (1+ n))
			  (seq nil)
			  (x nil (cons (cons (choose-from train-pos-pieces) (first seq)) x)))
			 ((= n max) x)
		       (push (generate-square-new seq) seq))))
    (list (place train-pos-nwhite) (place train-pos-nblack))))

(defun train-analyse (position)
  (destructuring-bind (pieces1 pieces2) position
    (-any? 'identity (-flatten-n 1 (mapcar (lambda (y) (mapcar (lambda (x) (attackingp x y)) pieces1)) pieces2)))))

(defun generate-problem ()
  (do ((x nil (generate-position)))
      ((and x (train-analyse x)) x)))

;; Create new buffer
(setf mode-buffer (generate-new-buffer "*Train*"))

(defvar train-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\" 'train-next)
    map) "Keymap for train major mode")

(defun train-mode ()
  "Major mode for training"
  (interactive)
  (kill-all-local-variables)
  (use-local-map train-mode-map))

(defvar train-wait nil)
(defvar train-pos nil)
(defvar train-ans nil)

(defun train-next ()
  (interactive)
  (if train-wait
      (progn
	(setf train-wait nil)
	(insert (coord-to-alg train-ans) "\n"))
    (progn
      (setf train-wait t)
      (setf train-pos `((("N" . ,(gen-square)) ("B" . ,(gen-square))) .
			(("N" . ,(gen-square)))))
      (setf train-ans (gen-square))
      (erase-buffer)
      (let ((to-text (lambda (x) (concat (car x) (coord-to-alg (cdr x))))))
	(insert (mapconcat (lambda (x) (format "%s        %s" (car x) (cdr x)))
			   (-zip-fill ""
				      (cons "W  " (mapcar to-text (car train-pos)))
				      (cons "B  " (mapcar to-text (cdr train-pos))))
			   "\n")
		"\n\n")))))

(provide 'chess-vis)
;;; chess-vis.el ends here
