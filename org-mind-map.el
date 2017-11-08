;;; org-mind-map.el --- Creates a directed graph from org-mode files
;; Author: Ted Wiles <theodore.wiles@gmail.com>
;; Keywords: orgmode, extensions, graphviz, dot
;; Version: 0.3
;; URL: https://github.com/theodorewiles/org-mind-map/org-mind-map.el
;; Package-Requires: ((emacs "24") (dash "1.8.0") (org "8.2.10"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package takes an org-mode tree and converts it into a
;; file that can be read into graphviz in order to visually show the
;; tree as a directed graph.  Mail to <theodore.wiles@gmail.com> to discuss
;; features and additions.  All suggestions are more than welcome.

;; The headings of the org-mode file are treated as node text in the resulting tree.
;; Org-mode heading tags are included in the resulting tree as additional cells
;; within the node.

;; The tags are color-coded to be consistent across the tree.

;; Tree interleaving is also possible by naming multiple org-mode headings
;; with the same heading.

;; NOTE: this requires the GRAPHVIZ software.  This is installable on
;; windows using cygwin.

;; To install, add this code to your .emacs:
;; (load "org-mind-map.el")

;; If on linux, customize the values of org-mind-map-unflatten-command
;; and org-mind-map-dot-command to have the values corresponding to
;; the executables in your system.

;; Then, run "M-x org-mind-map-write"

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

(defcustom org-mind-map-wrap-line-length 30
  "Line length within graphviz nodes."
  :type 'integer
  :group 'org-mind-map)

(defcustom org-mind-map-wrap-legend-line-length 45
  "Line length of the graphviz legend."
  :type 'integer
  :group 'org-mind-map)


(defcustom org-mind-map-unflatten-command "unflatten -l3"
  "Shell executable command for running the UNFLATTEN command."
  :type 'string
  :group 'org-mind-map)

(defcustom org-mind-map-dot-command "dot"
  "Shell executable command for running the DOT command."
  :type 'string
  :group 'org-mind-map)

(defcustom org-mind-map-dot-output "pdf"
  "Format of the DOT output.  Defaults to PDF."
  :type 'string
  :group 'org-mind-map)

(defcustom org-mind-map-rankdir "LR"
  "Sets the order of the resulting graph.  
LR is left-to-right, and TB is top-to-bottom."
  :type '(choice
          (const :tag "Left to right" "LR")
          (const :tag "Top to bottom" "TB")))

(defcustom org-mind-map-engine "dot"
  "Sets the layout engine used in your graphs.  
See the graphviz user manual for description of these options."
  :type '(choice
          (const :tag "Directed Graph" "dot")
          (const :tag "Undirected Spring Graph" "neato")
          (const :tag "Radial Layout" "twopi")
          (const :tag "Circular Layout" "circo")
          (const :tag "Undirected Spring Force-Directed" "fdp")))

(defun org-mind-map-wrap-lines (s)
  "Wraps a string S so that it can never be more than ORG-MIND-MAP-WRAP-LINE-LENGTH characters long."
  (let* ((s2 (org-do-wrap (split-string s " ") org-mind-map-wrap-line-length)))
    (mapconcat 'identity s2 "<br></br>")))

(defun org-mind-map-wrap-legend-lines (s)
  "Wraps a string S so that it can never be more than ORG-MIND-MAP-WRAP-LEGEND-LINE-LENGTH characters long."
  (let* ((s2 (org-do-wrap (split-string s " ") org-mind-map-wrap-legend-line-length)))
    (mapconcat 'identity s2 "<br></br>")))

(defun org-mind-map-dot-node-name (s)
  "Make string S formatted to be usable within dot node names."
  (replace-regexp-in-string "[^A-Za-z0-9]" "" s nil t))

(defun org-mind-map-add-color (h tag)
  "Add the color text H after tag TAG."
  (let* ((color (gethash tag h)))
    (concat "<td bgcolor=\"" color "\">" tag "</td>")))

;; (defun org-mind-map-tags-default (title tags props color h)
;;   ""
;;   (concat "[label=<<table>"
;; 	  (if (> (length tags) 0)
;; 	      (concat "<tr><td colspan=\"" (int-to-string (length tags)) "\" ")
;; 	    "<tr><td")
;; 	  (if color (concat " bgcolor=\"" color "\" "))
;; 	  ">" title "</td></tr>"
;; 	  (if (> (length tags) 0)
;; 	      (concat
;; 	       "<tr>" (mapconcat (-partial 'org-mind-map-add-color h) tags "") "</tr>"))
;; 	  "</table>>];\n"))

;; TODO: make this more flexible. Check for :OMM-NOTE-FMT property and add node properties.
(defun org-mind-map-write-tags (h el)
  "Use H as the hash-map of colors and takes an element EL and extracts the title and tags.  
Then, formats the titles and tags so as to be usable within DOT's graphviz language."
  (let* ((ts (org-element-property :title el))
	 (wrapped-title (org-mind-map-wrap-lines (if (listp ts) (first ts) ts)))
         (title (replace-regexp-in-string "&" "&amp;" wrapped-title nil t))
         (color (org-element-property :OMM-COLOR el))
	 (tags (org-element-property :tags el)))
    ;; Factor out following code as a separate function that is called if there are no node properties?
    ;;(funcall fn title tags color)
    (concat "[label=<<table>"
	    (if (> (length tags) 0)
		(concat "<tr><td colspan=\"" (int-to-string (length tags)) "\" ")
              "<tr><td")
            (if color (concat " bgcolor=\"" color "\" "))
            ">" title "</td></tr>"
	    (if (> (length tags) 0)
                (concat
                 "<tr>" (mapconcat (-partial 'org-mind-map-add-color h) tags "") "</tr>"))
	    "</table>>];\n")))

(defun org-mind-map-first-headline (e)
  "Figure out the first headline within element E."
  (let* ((parent (org-element-property :parent e)))
    (if parent
        (if (eq (org-element-type parent) 'headline)
            parent
          (org-mind-map-first-headline parent))
      nil)))

(defun org-mind-map-valid-link? (e)
  "Is E at a valid link?"
  (condition-case ex
      (let* ((org-link-search-inhibit-query t)
             (l (org-element-property :path e)))
        (save-excursion
	  (org-link-search l)
          t))
    ('error nil)))


(defun org-mind-map-destination-headline (e)
  "Figure out where the link in E is pointing to."
  (let* ((l (org-element-property :path e))
         (org-link-search-inhibit-query t))
    (save-excursion
      (org-open-link-from-string (concat "[[" l "]]"))
      (org-element-at-point))))

(defun org-mind-map-get-links (tags)
  "Make a list of links with the headline they are within and
their destination. Pass TAGS in order to keep the hash-map of
TAGS consistent."
  (org-element-map (org-element-parse-buffer 'object)
      'link
    (lambda (l)
      (if (org-mind-map-valid-link? l)
	  (list (org-mind-map-write-tags tags (org-mind-map-first-headline l))
		(org-mind-map-write-tags tags (org-mind-map-destination-headline l)))))))


(defun org-mind-map-make-legend (h)
  "Make a legend using the hash-map H."
  (let ((res '()))
    (maphash (lambda (k v) (push k res)) h)
    (if (> (length res) 0)
        (concat
         "{
    Legend [shape=none, margin=0, label=<
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
     <TR>
      <TD COLSPAN=\"2\"><B>Legend</B></TD>
     </TR>"
         (mapconcat 'identity
                    (let* (result)
                      (maphash
                       (lambda (name color)
                         (push (concat "<tr><td>" (org-mind-map-wrap-legend-lines name)
                                       "</td><td bgcolor=\"" color "\">&nbsp;&nbsp;&nbsp;&nbsp;</td></tr>")
                               result))
                       h)
                      (reverse result))
                    "")
         "</TABLE>>];}"))))

(defun org-mind-map-rgb ()
  "Make a random pastel-like RGB color."
  (concat "#"
	  (format "%x" (+ 125 (random (- 255 125))))
	  (format "%x" (+ 125 (random (- 255 125))))
	  (format "%x" (+ 125 (random (- 255 125))))))

(defun org-mind-map-tags ()
  "Return a hash map of tags in the org file mapped to random colors."
  (let* ((unique-tags
	  (-distinct
	   (-flatten
	    (org-element-map (org-element-parse-buffer 'headline) 'headline
	      (lambda (hl) (org-element-property :tags hl))))))
	 (h (make-hash-table :test 'equal)))
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (hl)
        (let ((legend (org-element-property :OMM-LEGEND hl))
              (color (org-element-property :OMM-COLOR hl)))
          (if legend (puthash legend color h)))))
    (-map (lambda (x) (puthash x (org-mind-map-rgb) h)) unique-tags)
    h))

(defun org-mind-map-data ()
  "Create graph & tag legend of all directed pairs of headlines for constructing the digraph."
  (let* ((hm (org-mind-map-tags))
	 (output
	  (org-element-map (org-element-parse-buffer 'headline)
	      'headline
	    (lambda(hl)
	      (let ((parent (org-element-property :parent hl )))
		(and (eq (org-element-type parent) 'headline)
		     (list (org-mind-map-write-tags hm parent)
			   (org-mind-map-write-tags hm hl)
			   ;; TODO: collect edge properties
			   )))))))
    (list (append output (org-mind-map-get-links hm)) hm)))

(defun org-mind-map-make-dot (data)
  "Create the dot file from DATA."
  (let ((table (nth 0 data))
        (legend (nth 1 data)))
    (concat "digraph structs {
   graph [autosize=false, size=\"9,12\", resolution=100]; nodesep=0.75;
   rankdir=" org-mind-map-rankdir ";
   overlap=false;
   splines=true;
   node [shape=plaintext];\n"
   (mapconcat 'identity (mapcar
			 #'(lambda (x) (concat (org-mind-map-dot-node-name x) x))
			 (-distinct (-flatten table)))
              " ")
   (mapconcat 'identity
	      (mapcar #'(lambda (x)
			  (format "%s -> %s;\n"
				  (org-mind-map-dot-node-name (nth 0 x))
				  (org-mind-map-dot-node-name (nth 1 x))
				  ;; TODO: add edge properties
				  ))
		      table)
	      " ")
   (org-mind-map-make-legend legend)
   "}")))

(defun org-mind-map-command (name)
  "Return the shell script that will create the correct file.  
The output file will be in the same location as the org file, with the same name as NAME."
  (concat org-mind-map-unflatten-command " | "
	  org-mind-map-dot-command " -T"
	  (shell-quote-argument org-mind-map-dot-output) " -K"
          (shell-quote-argument org-mind-map-engine) " -o"
          (shell-quote-argument (concat name "." org-mind-map-dot-output ""))))

(defun org-mind-map-update-message (process event)
  "Write an update message on the output of running org-mind-map based on PROCESS and EVENT."
  (let* ((e (with-current-buffer "*org-mind-map-errors*"
	      (buffer-string))))
    (if (string= e "")
        (princ (format "Org mind map %s" event))
      (princ (format "Org mind map %sErrors: %s" event e)))))

(defun org-mind-map-write-named (name &optional debug)
  "Create a directed graph output based on the org tree in the current buffer, with name NAME.  
To customize, see the org-mind-map group.
If DEBUG is non-nil, then print the dot command to the *Messages* buffer,
and print the dotfile to the *Messages* buffer or to a file if DEBUG is a filename."
  (let ((dot (org-mind-map-make-dot (org-mind-map-data))))
    (if debug
	(progn (message (org-mind-map-command name))
	       (if (stringp debug)
		   (with-temp-file debug (insert dot))
		 (message dot "%s"))))
    (if (get-buffer "*org-mind-map-errors*")
	(kill-buffer "*org-mind-map-errors*"))
    (let* ((p (start-process-shell-command
	       "org-mind-map-s" "*org-mind-map-errors*"
	       (org-mind-map-command name))))
      (process-send-string p dot)
      (process-send-string p "\n")
      (process-send-eof p)
      (set-process-sentinel p 'org-mind-map-update-message))))

;;;###autoload
(defun org-mind-map-write-with-prompt nil
  "Prompt for an output FILENAME (without extension) to write your .pdf and .dot files."
  (let ((filename (read-file-name "What is the file name you would like to save to?")))
    (org-mind-map-write-named filename (concat filename ".dot"))))

;;;###autoload
(defun org-mind-map-write (&optional promptp)
  "Create a digraph based on the org tree in the current buffer.
The digraph will be named the same name as the current buffer.
To customize, see the org-mind-map group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'."
  (interactive "P")
  (if promptp (org-mind-map-write-with-prompt)
    (org-mind-map-write-named (buffer-file-name))))

;;;###autoload
(defun org-mind-map-write-tree (&optional promptp)
  "Create a directed graph output based on just the current org tree.
To customize, see the org-mind-map group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'."
  (interactive "P")
  (org-narrow-to-subtree)
  (if promptp (org-mind-map-write-with-prompt)
    (org-mind-map-write-named (concat (buffer-file-name) (nth 4 (org-heading-components)))))
  (widen))

(defun org-mind-map-write-current-tree (&optional promptp)
  "Create a directed graph output based on just the current org tree.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'."
  (interactive "P")
  (save-restriction
    (ignore-errors (outline-up-heading 100))
    (org-narrow-to-subtree)
    (if promptp (org-mind-map-write-with-prompt)
      (org-mind-map-write-named (concat (buffer-file-name) (nth 4 (org-heading-components)))))
    (widen)))

;; Add a tool bar icon
;; (define-key org-mode-map [tool-bar org-button]
;; '(menu-item "Write the org-mode file mind map to disk." org-mind-map-write-with-prompt
;;    :image (image :type xpm :file "info.xpm")
;;    ))

;; (global-set-key (kbd "<f4>") 'org-mind-map-write)

(provide 'org-mind-map)

;;; org-mind-map.el ends here

