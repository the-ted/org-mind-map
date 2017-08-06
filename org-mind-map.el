;; Author: Ted Wiles <theodore.wiles@gmail.com>
;; Keywords: orgmode, extensions, graphviz, dot
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package takes an org-mode tree and converts it into a
;; file that can be read into graphviz in order to visually show the
;; tree as a directed graph. Mail to <theodore.wiles@gmail.com> to discuss
;; features and additions. All suggestions are more than welcome.

;; The headings of the org-mode file are treated as node text in the resulting tree.
;; Org-mode heading tags are included in the resulting tree as additional cells
;; within the node.

;; The tags are color-coded to be consistent across the tree.

;; Tree interleaving is also possible by naming multiple org-mode headings
;; with the same heading. 

;; NOTE: this requires the GRAPHVIZ software. This is installable on
;; windows using cygwin.

;; To install, add this code to your .emacs:
;; (load "org-mind-map.el")

;; If on linux, customize the values of org-mind-map/unflatten-command
;; and org-mind-map/dot-command to have the values corresponding to
;; the executables in your system.

;; Then, run "M-x org-mind-map/write"

;; The latest version is available at:
;;
;; https://github.com/theodorewiles/org-mind-map
;;

;;; Code:

(require 'dash)
(require 'org)

(defconst org-mind-map-version "0.1")

(defgroup org-mind-map nil
  "Convert org-mode tree into a graphviz directed graph"
  :group 'org)

(defcustom org-mind-map/wrap-line-length 30
  "Line length within graphviz nodes"
  :type 'integer
  :group 'org-mind-map)

(defcustom org-mind-map/unflatten-command "unflatten -l3"
  "Shell executable command for running the UNFLATTEN command."
  :type 'string
  :group 'org-mind-map)

(defcustom org-mind-map/dot-command "dot"
  "Shell executable command for running the DOT command."
  :type 'string
  :group 'org-mind-map)

(defcustom org-mind-map/dot-output "pdf"
  "Format of the DOT output. Defaults to PDF."
  :type 'string
  :group 'org-mind-map)

(defcustom org-mind-map/rankdir "LR"
  "Sets the order of the resulting graph. LR is left-to-right, and TB is top-to-bottom"
  :type '(choice
          (const :tag "Left to right" "LR")
          (const :tag "Top to bottom" "TB")))
  
(defun org-mind-map/wrap-lines (s)
  "wraps a string S so that it can never be more than
ORG-MIND-MAP/WRAP-LINE-LENGTH characters long."
  (let* ((s2 (org-do-wrap (split-string s " ") org-mind-map/wrap-line-length)))
    (mapconcat
     'identity
     s2
     "<br></br>")
    ))

(defun org-mind-map/delete-space (s)
  "Makes string S formatted to be usable within dot node names"
  (replace-regexp-in-string "[^A-z0-9 ]" "" 
                            (replace-regexp-in-string "[ \\{}|\n]" "" s nil t)
  nil t))

(defun org-mind-map/add-color (h tag)
  "Adds the color text after tag t"
  (let* ((color (gethash tag h)))
    (concat "<td bgcolor=\"" color "\">" tag "</td>")))

(defun org-mind-map/write-tags (h el)
  "Takes an element EL and extracts the title and tags. Then,
formats the titles and tags so as to be usable within DOT's
graphviz language. Uses h as the hash-map of colors."
  (let ((title (org-mind-map/wrap-lines (org-element-property :title el)))
	(tags (org-element-property :tags el)))
    (concat "<table>"
	    (if (> (length tags) 0)
		(concat
		 "<tr><td colspan=\"" (int-to-string (length tags)) "\">" title "</td></tr>"
		 "<tr>" (mapconcat (-partial 'org-mind-map/add-color h) tags "") "</tr>")
	      (concat "<tr><td>" title "</td></tr>"))
	    "</table>")))

(defun org-mind-map/rgb ()
  "Makes a random pastel-like RGB color"
  (concat "#"
   (format "%x" (+ 125 (random (- 255 125))))
   (format "%x" (+ 125 (random (- 255 125))))
   (format "%x" (+ 125 (random (- 255 125))))))

(defun org-mind-map/tags ()
  "Returns a hash map of tags in the org file mapped to random colors."
  (let* ((unique-tags
	  (-distinct
	   (-flatten
	    (org-element-map (org-element-parse-buffer 'headline) 'headline
	      (lambda (hl)
		(let ((ts (org-element-property :tags hl)))
		  ts))))))
	 (h (make-hash-table :test 'equal))
	 )
    (-map (lambda (x) (puthash x (org-mind-map/rgb) h)) unique-tags)
    h))

(defun org-mind-map/data ()
  "Creates a list of all of the directed pairs of headlines to be
used in constructing the directed graph."
  (let* ((hm (org-mind-map/tags))
	 (output
	  (org-element-map (org-element-parse-buffer 'headline)
	      'headline
	    (lambda(hl)
	      (let ((parent (org-element-property :parent hl )))
		(and (eq (org-element-type parent) 'headline)
		     (list (org-mind-map/write-tags hm parent)
			   (org-mind-map/write-tags hm hl))))))))
    output))


(defun org-mind-map/make-dot (table)
  "Creates the dot file"
  (concat "digraph structs {
   rankdir=" org-mind-map/rankdir ";
   splines=true;
   node [shape=plaintext];\n"
	  (mapconcat 'identity (mapcar #'(lambda (x)
					   (concat (org-mind-map/delete-space x)
						   " [label=<"
						   x
						   ">];\n"))
				       (-distinct
					(-flatten table)))
		     " ")
	  (mapconcat
	   'identity
	   (mapcar #'(lambda (x)
		       (format "%s -> %s;\n"
			       (org-mind-map/delete-space (first x))
			       (org-mind-map/delete-space (second x))))
		   table)
	   " ")
	  "}
"))

(defun org-mind-map/command ()
  "Returns the shell script that will create the correct
file. The output file will be in the same location as the org
file, with the same name as the buffer file name."
  (concat org-mind-map/unflatten-command " | "
	  org-mind-map/dot-command " -T"
	  org-mind-map/dot-output " -o\"" (buffer-file-name)
	  "." org-mind-map/dot-output "\""))

(defun org-mind-map/update-message (process event)
  (let* ((e (with-current-buffer "*org-mind-map/errors*"
              (buffer-string))))
    (if (string= e "")
        (princ
         (format "Org mind map %s" event))
      (princ
       (format "Org mind map %sErrors: %s" event e)))))

(defun org-mind-map/write ()
  "Creates a directed graph output based on the org tree in the
current buffer. To customize, see the org-mind-map group. 

M-x customize-group org-mind-map"
  (interactive)
  (if (get-buffer "*org-mind-map/errors*")
      (kill-buffer "*org-mind-map/errors*"))
  (let* ((p (start-process-shell-command "org-mind-map/s" "*org-mind-map/errors*" (org-mind-map/command))))
    (process-send-string "org-mind-map/s" (org-mind-map/make-dot (org-mind-map/data)))
    (process-send-eof "org-mind-map/s")
    (set-process-sentinel p 'org-mind-map/update-message)))

;; (global-set-key (kbd "<f4>") 'org-mind-map/write)
