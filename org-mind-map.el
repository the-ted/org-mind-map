;;; org-mind-map.el --- Creates a directed graph from org-mode files
;; Author: Ted Wiles <theodore.wiles@gmail.com>
;; Keywords: orgmode, extensions, graphviz, dot
;; Version: 0.4
;; URL: https://github.com/theodorewiles/org-mind-map
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

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `org-mind-map-write'
;;    Create a digraph based on all org trees in the current buffer.
;;    Keybinding: M-x org-mind-map-write
;;  `org-mind-map-write-current-branch'
;;    Create a directed graph output based on just the current org tree branch.
;;    Keybinding: M-x org-mind-map-write-current-branch
;;  `org-mind-map-write-current-tree'
;;    Create a directed graph output based on the whole current org tree.
;;    Keybinding: M-x org-mind-map-write-current-tree
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `org-mind-map-wrap-line-length'
;;    Line length within graphviz nodes.
;;    default = 30
;;  `org-mind-map-wrap-legend-line-length'
;;    Line length of the graphviz legend.
;;    default = 45
;;  `org-mind-map-unflatten-command'
;;    Shell executable command for running the UNFLATTEN command.
;;    default = "unflatten -l3"
;;  `org-mind-map-dot-command'
;;    Shell executable command for running the DOT command.
;;    default = "dot"
;;  `org-mind-map-dot-output'
;;    Format of the DOT output.  Defaults to PDF.
;;    default = "pdf"
;;  `org-mind-map-engine'
;;    Sets the layout engine used in your graphs.
;;    default = "dot"
;;  `org-mind-map-default-node-attribs'
;;    Alist of default node attributes and values.
;;    default = '(("shape" . "plaintext"))
;;  `org-mind-map-default-edge-attribs'
;;    Alist of default edge attributes and values.
;;    default = nil
;;  `org-mind-map-default-graph-attribs'
;;    Alist of default graph attributes and values.
;;    default = '(("autosize" . "false") ("size" . "9,12") ("resolution" . "100") ...))
;;  `org-mind-map-node-formats'
;;    Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-NODE-FMT property
;;    See also `org-mind-map-make-node-fn'
;;    default = nil
;;  `org-mind-map-edge-formats'
;;    Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-EDGE-FMT property
;;    See also `org-mind-map-make-edge-fn'
;;    default = nil
;;  `org-mind-map-edge-format-default'
;;    Default format string for graph edges, e.g. "[style=dotted]".
;;    default = ""
;;  `org-mind-map-reserved-colors'
;;    List of colors that will not be used for coloring tags.
;;    default = nil
;;  `org-mind-map-tag-colors'
;;    An alist of (TAG . COLOR) pairs for choosing colors for tags.
;;    default = nil
;;  `org-mind-map-include-text'
;;    A boolean indicating whether our not to include paragraph text in body of nodes.
;;    default = t
;;  `org-mind-map-include-images'
;;    A boolean indicating whether our not to include images in body of nodes.
;;    default = t


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

;; If on linux, customize the values of `org-mind-map-unflatten-command'
;; and `org-mind-map-dot-command' to have the values corresponding to
;; the executables in your system.

;; Then, run "M-x org-mind-map-write" to create a graph of all trees in the current buffer,

;; You can customize the style of the graph by adding :OMM-NODE-FMT and :OMM-EDGE-FMT properties
;; to the headlines in the tree.

;; The latest version is available at:
;;
;; https://github.com/theodorewiles/org-mind-map
;;

;;; Code:

(require 'dash)
(require 'org)
(require 'subr-x)

(defconst org-mind-map-version "0.4")

(defgroup org-mind-map nil
  "Convert org-mode tree into a graphviz directed graph"
  :group 'org)

(defcustom org-mind-map-wrap-line-length 30
  "Line length within graphviz nodes."
  :type 'integer
  :group 'org-mind-map)

(defcustom org-mind-map-wrap-text-length 60
  "Line length within graphviz nodes that have longer text."
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

(defcustom org-mind-map-dot-output '("pdf" "png" "jpeg" "svg" "eps" "gif" "tiff")
  "List of formats for the DOT output file.
If more than one are specified then the user will be prompted to choose one.
To find a list of available formats, on the command line enter: dot -T?"
  :type '(repeat (string :tag "File type"))
  :group 'org-mind-map)

(defcustom org-mind-map-display nil
  "How the results should be displayed:
nil = don't display results
current = display results in current window
window = display results in new window
frame = display results in new frame"
  :type '(choice (const :tag "Don't display" nil)
		 (const :tag "Display in current window" current)
		 (const :tag "Display in new window" window)
		 (const :tag "Display in new frame" frame))
  :group 'org-mind-map)

(defcustom org-mind-map-engine "dot"
  "Sets the layout engine used in your graphs.
See the graphviz user manual for description of these options."
  :type '(choice
          (const :tag "Directed Graph" "dot")
          (const :tag "Undirected Spring Graph" "neato")
          (const :tag "Radial Layout" "twopi")
          (const :tag "Circular Layout" "circo")
          (const :tag "Undirected Spring Force-Directed" "fdp"))
  :group 'org-mind-map)

(defcustom org-mind-map-default-node-attribs '(("shape" . "plaintext"))
  "Alist of default node attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'org-mind-map)

(defcustom org-mind-map-default-edge-attribs nil
  "Alist of default edge attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'org-mind-map)

(defcustom org-mind-map-default-graph-attribs '(("autosize" . "false")
						("size" . "9,12")
						("resolution" . "100")
						("nodesep" . "0.75")
						("overlap" . "false")
						("spline" . "true")
						("rankdir" . "LR"))
  "Alist of default graph attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'org-mind-map)

(defcustom org-mind-map-node-formats nil
  "Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-NODE-FMT property 
of a node/headline, and FN is a function which outputs a format string to be placed after the 
node name (e.g. \"[label='Node1',color='red']\").
The function FN should take the following 5 arguments which can be used to construct the format: 

TITLE = the label string for the node
TAGS = a list of org tags for the current node
COLOR = the contents of the OMM-COLOR property for the current node
HM = a hash map of colors
EL = an org element obtained from `org-element-map'

Note: the :OMM-NODE-FMT property is inherited by children of the node/headline where it is defined."
  :type '(alist :key-type (string :tag "              Name")
		:value-type (function :tag "Format function"))
  :group 'org-mind-map)

(defcustom org-mind-map-edge-formats nil
  "Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-EDGE-FMT property
of a node/headline, and FN is a function which outputs a format string to be placed after an 
edge (e.g. \"[style=dotted]\"). 
The function FN should take the following 2 arguments which can be used to construct the format: 

HM = a hash map of colors
EL = an org element obtained from `org-element-map'

Note: the :OMM-EDGE-FMT property affects edges leading to the node at which it is defined, and 
is inherited by children of that node/headline."
  :type '(alist :key-type (string :tag "              Name")
		:value-type (function :tag "Format function"))
  :group 'org-mind-map)

(defcustom org-mind-map-edge-format-default ""
  "Default format string for graph edges, e.g. \"[style=dotted]\"."
  :type 'string
  :group 'org-mind-map)

(defcustom org-mind-map-reserved-colors nil
  "List of colors that will not be used for coloring tags.
These colors will be excluded when random tag colors are chosen by `org-mind-map-rgb'
so that you can use them for other things.
Each color should be in hexadecimal form, e.g: \"#e3cfbc\", where the consecutive pairs
of hexdigits indicate levels of red, green and blue respectively.
It is not necessary to include any colors with levels below 7d, as these are not used
for creating random tag colors."
  :type '(repeat string)
  :group 'org-mind-map)

(defcustom org-mind-map-tag-colors nil
  "An alist of (TAG . COLOR) pairs for choosing colors for tags.
Any tags not listed here will be colored with randomly selected colors that dont
clash with those in `org-mind-map-reserved-colors'.
Each color should be in hexadecimal form, e.g: \"#e3cfbc\", where the consecutive pairs
of hexdigits indicate levels of red, green and blue respectively.

Note: you can also set tag colors by altering the hashmap passed as an argument to functions
defined in `org-mind-map-node-formats'."
  :type '(alist :key-type (string :tag "     Tag") :value-type (string :tag "Color"))
  :group 'org-mind-map)

(defcustom org-mind-map-include-text t
  "A boolean indicating whether our not to include paragraph text in body of nodes.
   default = t"
  :type 'boolean
  :group 'org-mind-map
  )

(defcustom org-mind-map-include-images t
  "A boolean indicating whether our not to include paragraph text in body of nodes.
   default = t"
  :type 'boolean
  :group 'org-mind-map
  )


(defun org-mind-map-do-wrap (words width)
  "Create lines of maximum width WIDTH (in characters) from word list WORDS."
  (let (lines line)
    (while words
      (setq line (pop words))
      (while (and words (< (+ (length line) (length (car words))) width))
	(setq line (concat line " " (pop words))))
      (setq lines (push line lines)))
    (nreverse lines)))

(defun org-mind-map-wrap (s l)
  (let* ((s2 (org-mind-map-do-wrap (split-string s " ") l)))
    (mapconcat 'identity s2 "<br></br>")))

(defun org-mind-map-wrap-lines (s)
  "Wraps a string S so that it can never be more than ORG-MIND-MAP-WRAP-LINE-LENGTH characters long."
  (org-mind-map-wrap s org-mind-map-wrap-line-length))

(defun org-mind-map-wrap-text (s)
  "Wraps a string S so that it can never be more than ORG-MIND-MAP-WRAP-TEXT-LENGTH characters long."
  (org-mind-map-wrap s org-mind-map-wrap-text-length))


(defun org-mind-map-wrap-legend-lines (s)
  "Wraps a string S so that it can never be more than ORG-MIND-MAP-WRAP-LEGEND-LINE-LENGTH characters long."
  (let* ((s2 (org-mind-map-do-wrap (split-string s " ") org-mind-map-wrap-legend-line-length)))
    (mapconcat 'identity s2 "<br></br>")))

(defun org-mind-map-dot-node-name (s)
  "Make string S formatted to be usable within dot node names."
  (concat "\""
	  (replace-regexp-in-string
	   "</?\\(table\\|tr\\|td\\)[^<>]*>" ""
	   (replace-regexp-in-string "label=\\(\"[^\"]+\"\\|[^,]+\\).*" "\\1" s))
	  "\""))

(defun org-mind-map-add-color (hm tag &optional colspan)
  "Create data element containing TAG with associated color found in hashmap HM."
  (let* ((color (gethash tag hm)))
    (concat "<td"
	    (if colspan (concat " colspan=\"" (int-to-string colspan) "\""))
	    (if color (concat " bgcolor=\"" color "\"")) ">" tag "</td>")))

(defun org-mind-map-write-tags-default (title tags color hm el &optional content images)
  "Default function for writing nodes.
Label node with TITLE and background COLOR, and write TAGS (a list of tag names)
into boxes underneath, using associated colors in hashmap HM.
The EL argument is not used, but is needed for compatibility."
  (concat "[label=<<table>"
	  (if (> (length tags) 0)
	      (concat "<tr><td colspan=\"" (int-to-string (length tags)) "\" ")
	    "<tr><td")
	  (if color (concat " bgcolor=\"" color "\" "))
	  ">" title "</td></tr>"
	  (if (> (length tags) 0)
	      (concat
	       "<tr>" (mapconcat (-partial 'org-mind-map-add-color hm) tags "") "</tr>"))
	  (if (> (length content) 0)
	      (concat
	       "<tr><td BALIGN=\"LEFT\" ALIGN=\"LEFT\">" content "</td></tr>")
	    )

	  (if (> (length images) 0)
	      images ""
	    )
	  "</table>>];"))

(defun org-mind-map-get-property (prop el &optional inheritp)
  "Get property PROP from an org element EL, using inheritance if INHERITP is non-nil.
PROP can be either the property symbol (beginning with :), or the name of the property (with or without :).
If there is a column summary value for the property that has recently be calculated it will be used."
  (let* ((node el)
	 (propstr (if (stringp prop)
		      (upcase (if (string-match "^:" prop)
				  (substring prop 1)
				prop))
		    (substring (symbol-name prop) 1)))
	 (prop (if (stringp prop) (intern (concat ":" propstr)) prop))
	 (val (or (cdr (cl-find propstr (get-text-property
					 (org-element-property :begin el)
					 'org-summaries)
				:test (lambda (x y) (equal (caar y) x))))
		  (org-element-property prop el))))
    (while (and inheritp
		(not val)
		(not (eq (org-element-type node) 'org-data)))
      (setq node (org-element-property :parent node)
	    val (org-element-property prop node)))
    val))

(defun org-mind-map-narrow-to-heading-content (b)
  "Narrow to the region until the next headline, if applicable"
  (let* ((new-end 
	  (org-element-map (org-element-parse-buffer 'object 'true)
	      'headline
	    (lambda (x)
	      (if (not
		   (= (org-element-property :begin x) b))
		  b nil))
	    nil 'true)))
    (if new-end
	(progn
	  (widen)
	  (narrow-to-region b new-end)))))


(defun org-mind-map-write-tags (hm el &optional edgep)
  "Use HM as the hash-map of colors and takes an element EL and extracts the title and tags.  
Then, formats the titles and tags so as to be usable within DOT's graphviz language."
  (let* ((ts (org-element-property :title el))
	 (wrapped-title (org-mind-map-wrap-lines (if (listp ts) (first ts) ts)))
         (title (replace-regexp-in-string "&" "&amp;" wrapped-title nil t))
         (color (org-element-property :OMM-COLOR el))
	 (tags (org-element-property :tags el))
	 (fmt (org-mind-map-get-property (if edgep :OMM-EDGE-FMT :OMM-NODE-FMT) el))
	 (b (org-element-property :begin el))
	 (e (org-element-property :end el))
	 (images
	  (if org-mind-map-include-images
	      (save-restriction
		(narrow-to-region b e)
		(org-mind-map-narrow-to-heading-content b)
		(mapconcat 'identity
			   (org-element-map (org-element-parse-buffer 'object 'true)
			       '(link)
			     (lambda (x)
			       (message "Inline image: %s" (org-export-inline-image-p x))
			       (if (org-export-inline-image-p x)
				   (concat 
				    "<tr><td fixedsize='TRUE' height='100' width='100'>" "<IMG src='"
				    (org-element-property :path x)
				    "'/>"
				    "</td></tr>")
				 "")))
			   ""))))
	 (content
	  (if org-mind-map-include-text
	      (save-restriction
		(narrow-to-region b e)
		(org-mind-map-narrow-to-heading-content b)
		(mapconcat 'identity
			   (org-element-map (org-element-parse-buffer 'object 'true)
			       '(paragraph)
			     (lambda (x)
			       (org-mind-map-wrap-text
				(string-trim
				 (substring-no-properties
				  (car (org-element-contents x)))))))
			   "<br></br><br></br>"))
	    nil))
	 )
	(if edgep (funcall (or (cdr (assoc fmt org-mind-map-edge-formats))
			       (lambda (a b) org-mind-map-edge-format-default))
			   hm el)
	  (funcall (or (cdr (assoc fmt org-mind-map-node-formats))
		       'org-mind-map-write-tags-default)
		   title tags color hm el content images))))

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
	     (type (org-element-property :type e))
             (l (org-element-property :path e)))
	(if (string= type "fuzzy")
	    (save-excursion
	      (org-link-search l) t)
	  nil))
    ('error nil)))

(defun org-mind-map-destination-headline (e)
  "Figure out where the link in E is pointing to."
  (let* ((l (org-element-property :path e))
         (org-link-search-inhibit-query t))
	(save-excursion
	  (org-open-link-from-string (concat "[[" l "]]"))
	  (org-element-at-point))))

(defun org-mind-map-get-links (hm)
  "Make a list of links with the headline they are within and
their destination. Pass hashmap arg HM mapping tags to colors 
in order to keep the tag colors consistent across calls."
  (org-element-map (org-element-parse-buffer 'object)
      'link
    (lambda (l)
      (if (org-mind-map-valid-link? l)
	  (let* ((origin
		  (org-mind-map-write-tags hm
					   (org-mind-map-first-headline l)))
		 (h (org-mind-map-destination-headline l))
		 (destination
		  (org-mind-map-write-tags hm h)))
	    (list origin destination))))))

(defun org-mind-map-make-legend (h)
  "Make a legend using the hash-map HM."
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

(defun org-mind-map-rgb (&optional exceptions)
  "Make a random pastel-like RGB color.
Dont return any of the colors listed in the optional arg EXCEPTIONS."
  (let* ((fn (lambda nil
	       (concat "#"
		       (format "%x" (+ 125 (random (- 255 125))))
		       (format "%x" (+ 125 (random (- 255 125))))
		       (format "%x" (+ 125 (random (- 255 125)))))))
	 (color (funcall fn)))
    (while (member color exceptions)
      (setq color (funcall fn)))
    color))

(defun org-mind-map-tags (&optional exceptions)
  "Return a hash map of tags in the org file mapped to random colors.
Dont return any of the colors listed in the optional arg EXCEPTIONS."
  (let* ((hm (make-hash-table :test 'equal)))
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (hl)
        (let ((tags (mapcar 'substring-no-properties (org-element-property :tags hl)))
	      (legend (org-element-property :OMM-LEGEND hl))
              (color (org-element-property :OMM-COLOR hl)))
          (if legend (puthash legend color hm))
	  (if tags (mapcar (lambda (x)
			     (puthash x (--if-let (assoc x org-mind-map-tag-colors)
					    (cdr it)
					  (org-mind-map-rgb
					   (append exceptions
						   (mapcar 'cdr org-mind-map-tag-colors))))
				      hm))
			   tags)))))
    hm))

(defun org-mind-map-data (&optional linksp)
  "Create graph & tag legend of all directed pairs of headlines for constructing the digraph.
If LINKSP is non-nil include graph edges for org links."
  (let* ((hm (org-mind-map-tags org-mind-map-reserved-colors))
	 (output
	  (org-element-map (org-element-parse-buffer 'headline) 'headline
	    (lambda (hl)
	      (let ((parent (org-element-property :parent hl)))
		(and (eq (org-element-type parent) 'headline)
		     (list (org-mind-map-write-tags hm parent)
			   (org-mind-map-write-tags hm hl)
			   (org-mind-map-write-tags hm hl t))))))))
    (list (append output (if linksp (org-mind-map-get-links hm))) hm)))

(defun org-mind-map-make-dot (data)
  "Create the dot file from DATA."
  (let ((table (nth 0 data))
        (legend (nth 1 data)))
    (concat "digraph structs {\n        // DEFAULT OPTIONS\n"
	    (if org-mind-map-default-graph-attribs
		(concat "        graph ["
			(mapconcat #'(lambda (x) (concat (car x) "=\"" (cdr x) "\""))
				   org-mind-map-default-graph-attribs ", ")
			"];\n"))
	    (if org-mind-map-default-node-attribs
		(concat
		 "        node [" (mapconcat #'(lambda (x) (concat (car x) "=\"" (cdr x) "\""))
					     org-mind-map-default-node-attribs ", ")
		 "];\n"))
	    (if org-mind-map-default-edge-attribs
		(concat
		 "        edge [" (mapconcat #'(lambda (x) (concat (car x) "=\"" (cdr x) "\""))
					     org-mind-map-default-edge-attribs ", ")
		 "];\n"))
	    "        // NODES\n"
	    (mapconcat
	     #'(lambda (x) (concat "        " (org-mind-map-dot-node-name x) " " x))
	     (-distinct (-flatten (mapcar (lambda (x) (list (nth 0 x) (nth 1 x))) table)))
	     "\n")
	    "\n        // EDGES\n"
	    (mapconcat #'(lambda (x) (format "        %s -> %s;"
					     (org-mind-map-dot-node-name (nth 0 x))
					     (org-mind-map-dot-node-name (nth 1 x))
					     (nth 2 x)))
		       table "\n")
	    (org-mind-map-make-legend legend)
	    "}")))

(defun org-mind-map-command (name outputtype)
  "Return the shell script that will create the correct file NAME of type OUTPUTTYPE.
The output file will be in the same location as the org file."
  (concat org-mind-map-unflatten-command " | "
	  org-mind-map-dot-command " -T"
	  (shell-quote-argument outputtype) " -K"
          (shell-quote-argument org-mind-map-engine) " -o"
          (shell-quote-argument (concat name "." outputtype ""))))

(defun org-mind-map-update-message (filename process event)
  "Write an update message on the output of running org-mind-map based on PROCESS and EVENT.
Open FILENAME according to value of `org-mind-map-display'."
  (let* ((e (with-current-buffer "*org-mind-map-errors*"
	      (buffer-string))))
    (if (string= e "")
        (princ (format "Org mind map %s" event))
      (princ (format "Org mind map %sErrors: %s" event e)))
    (if (string= event "finished\n")
	(progn
	  (cl-case org-mind-map-display
	    (nil nil)
	    (current (find-file filename))
	    (window (find-file-other-window filename))
	    (frame (switch-to-buffer-other-frame (find-file-noselect filename))))
	  (cl-case major-mode
	    (pdf-view-mode (pdf-view-fit-page-to-window))
	    (doc-view-mode (doc-view-fit-page-to-window)))))))

(defun org-mind-map-write-named (name &optional debug linksp)
  "Create a directed graph output based on the org tree in the current buffer, with name NAME.  
To customize, see the org-mind-map group.
If DEBUG is non-nil, then print the dot command to the *Messages* buffer,
and print the dotfile to the *Messages* buffer or to a file if DEBUG is a filename.
If LINKSP is non-nil include graph edges for org links."
  (let ((dot (org-mind-map-make-dot (org-mind-map-data linksp)))
a	(outputtype (if (> (length org-mind-map-dot-output) 1)
			(completing-read "Output file type: " org-mind-map-dot-output)
		      (car org-mind-map-dot-output))))
    (if debug
	(progn (message (org-mind-map-command name outputtype))
	       (if (stringp debug)
		   (with-temp-file debug (insert dot))
		 (message dot "%s"))))
    (if (get-buffer "*org-mind-map-errors*")
	(kill-buffer "*org-mind-map-errors*"))
    (let* ((p (start-process-shell-command
	       "org-mind-map-s" "*org-mind-map-errors*"
	       (org-mind-map-command name outputtype)))
	   (filename (concat name "." outputtype "")))
      (process-send-string p dot)
      (process-send-string p "\n")
      (process-send-eof p)
      (set-process-sentinel p (-partial 'org-mind-map-update-message filename))
      filename)))

;;;###autoload
(defun org-mind-map-write-with-prompt nil
  "Prompt for an output FILENAME (without extension) to write your output and .dot files."
  (let ((filename (read-file-name "What is the file name you would like to save to?")))
    (org-mind-map-write-named filename (concat filename ".dot")
			      (y-or-n-p "Include org links? "))))

(defun org-mind-map-default-filename (treenamep)
  "Return a default filename for saving the tree diagram.
If TREENAMEP is non-nil include in the filename the name of the top level header of the tree."
  (concat (file-name-sans-extension (buffer-name))
	  "_diagram"
	  (if treenamep
	      (concat "-"
		      (replace-regexp-in-string " +" "_" (nth 4 (org-heading-components)))))))

;;;###autoload
(defun org-mind-map-write (&optional promptp)
  "Create a digraph based on all org trees in the current buffer.
The digraph will be named the same name as the current buffer.
To customize, see the org-mind-map group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'."
  (interactive "P")
  (if promptp (org-mind-map-write-with-prompt)
    (org-mind-map-write-named (org-mind-map-default-filename nil))))

;;;###autoload
(defun org-mind-map-write-current-branch (&optional promptp)
  "Create a directed graph output based on just the current org tree branch.
To customize, see the org-mind-map group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'."
  (interactive "P")
  (org-narrow-to-subtree)
  (let ((filename (if promptp (org-mind-map-write-with-prompt)
		    (org-mind-map-write-named (org-mind-map-default-filename t)))))
    (widen)
    filename))

;;;###autoload
(defun org-mind-map-write-current-tree (&optional promptp)
  "Create a directed graph output based on the whole current org tree.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'."
  (interactive "P")
  (save-restriction
    (ignore-errors (outline-up-heading 100))
    (org-mind-map-write-current-branch promptp)))

;;;###autoload
(defmacro org-mind-map-make-node-fn (name doc props &optional shape color other)
  "Create a function org-mind-map-NAME-node for use with :OMM-NODE-FMT writing node properties.
The created function should be added to `org-mind-map-node-formats' and the associated string
can be used as the :OMM-NODE-FMT for a tree. 
Document the function with the DOC arg.
PROPS is a list of either property & format string pairs, or individual property names,
which will be placed in each node, e.g: ((\"PROB\" \"probability=%s\") \"COST\"). 
For property names with no format string, \"%s=%s\" will be used with the property name and value.

The node shape and background color can be specified with the optional SHAPE and COLOR arguments, 
and any other attributes (e.g. \"fontsize=30\") can be specified with the OTHER argument.
Each of these arguments can be either a string or a form which is evaluated for each node, 
and returns a string.

Example: (org-mind-map-make-node-fn decisiontree \"Draw decision tree\" (\"COST\" (\"NOTES\" \"Notes: %s\")) nil
			   (cond ((equal (org-mind-map-get-property :todo-keyword el) \"ACTION\") \"red\")
				 ((equal (org-mind-map-get-property :todo-keyword el) \"STATE\") \"yellow\")
				 ((equal (org-mind-map-get-property :todo-keyword el) \"DECISION\") \"green\")))

You could put this code in your emacs startup file (e.g. ~/.emacs) and then add to `org-mind-map-node-formats' 
the pair '(\"decisiontree\" . org-mind-map-decisiontree-node), and use \":OMM-NODE-FMT: decisiontree\" as a
tree property."
  `(defun ,(intern (concat "org-mind-map-" (symbol-name name) "-node"))
       (title tags color hm el)
     ,doc
     (let* ((numtags (if tags (length tags)))
	    (colspan (if tags (int-to-string numtags)))
	    (propstxt
	     (cl-remove
	      nil (list ,@(mapcar
			   (lambda (p)
			     (cond ((stringp p)
				    `(--if-let (org-mind-map-get-property ,p el)
					 (concat ,(upcase p) "=" it)))
				   ((consp p)
				    `(--if-let (org-mind-map-get-property ,(car p) el)
					 (format ,(nth 1 p) it)))
				   (t (error "Invalid props value"))))
			   props))))
	    (shape ,shape)
	    (color (or color ,color))
	    (other ,other))
       (concat "[label=<<table" (if shape " border=\"0\"") ">"
	       (if numtags (concat "<tr><td colspan=\"" colspan "\" ") "<tr><td")
	       (if (and color (not shape)) (concat " bgcolor=\"" color "\" "))
	       ">" title "</td></tr>"
	       (mapconcat (lambda (p)
			    (concat "<tr>" (org-mind-map-add-color hm p numtags) "</tr>"))
			  propstxt "")
	       (if numtags
		   (concat "<tr>"
			   (mapconcat (-partial 'org-mind-map-add-color hm) tags "")
			   "</tr>"))
	       "</table>>"
	       (if shape (concat ",shape=" shape (if color (concat ",style=filled,color=" color))))
	       (if other (concat "," other)) "];"))))

;;;###autoload
(defmacro org-mind-map-make-edge-fn (name doc props &optional style color other)
  "Create a function org-mind-map-write-NAME for writing edge properties which can be used for :OMM-EDGE-FMT.
Document the function with the DOC arg.
PROPS is a list of either property & format string pairs, or individual property names,
which will concatenated and used to label the edges, e.g: ((\"PROB\" \"probability=%s\") \"COST\"). 
For property names with no format string \"%s=%s\" will be used with the property name and value.

The edge style and color can be specified with the optional STYLE and COLOR arguments,
and any other attributes (e.g. \"fontsize=30\") can be specified with the OTHER argument.
Each of these arguments can be either a string or a form which is evaluated for each node, 
and returns a string.

Example: (org-mind-map-make-edge-fn decisiontree \"Draw decision tree\" (\"PROB\"))

You could put this code in your emacs startup file (e.g. ~/.emacs) and then add to `org-mind-map-edge-formats' 
the pair '(\"decisiontree\" . org-mind-map-decisiontree-edge), and use \":OMM-EDGE-FMT: decisiontree\" as a
tree property."
  `(defun ,(intern (concat "org-mind-map-" (symbol-name name) "-edge"))
       (hm el)
     ,doc
     (let* ((propstxt (cl-remove
		       nil (list ,@(mapcar (lambda (p)
					     (cond ((stringp p)
						    `(--if-let (org-mind-map-get-property ,p el)
							 (concat ,(upcase p) "=" it)))
						   ((consp p)
						    `(--if-let (org-mind-map-get-property ,(car p) el)
							 (format ,(nth 1 p) it)))
						   (t (error "Invalid props value"))))
					   props))))
	    (style ,style)
	    (color ,color)
	    (other ,other))
       (concat "[label=\"" (mapconcat 'identity propstxt ",") "\""
	       (if color (concat ",color=\"" color "\" "))
	       (if style (concat ",style=\"" style "\""))
	       (if other (concat "," other)) "]"))))

(defun ox-graphviz-export (&optional async subtreep visible-only body-only info)
  "Export the current buffer to a graphviz diagram.
Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
  (let ((org-mind-map-display nil))
    (if subtreep (org-mind-map-write-current-branch)
      (org-mind-map-write))))

(defun ox-graphviz-export-and-open (&optional async subtreep visible-only body-only info)
  "Export the current buffer to a graphviz diagram, and open the output file.
Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
  (let ((org-mind-map-display (or org-mind-map-display 'current)))
    (if subtreep (org-mind-map-write-current-branch)
      (org-mind-map-write))))

(defun ox-graphviz-export-dot (&optional async subtreep visible-only body-only info)
  "Export the current buffer to a graphviz diagram, and create and open a dot file.
Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
  (let ((org-mind-map-display nil)
	(filename (org-mind-map-default-filename subtreep))
	(linksp (y-or-n-p "Include org links? ")))
    (if subtreep (org-narrow-to-subtree))
    (org-mind-map-write-named filename (concat filename ".dot") linksp)
    (widen)))

(defun ox-graphviz-export-dot-and-open (&optional async subtreep visible-only body-only info)
  "Export the current buffer to a graphviz diagram and a dot file, and open the output file.
Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
  (let ((org-mind-map-display (or org-mind-map-display 'current))
	(filename (org-mind-map-default-filename subtreep))
	(linksp (y-or-n-p "Include org links? ")))
    (if subtreep (org-narrow-to-subtree))
    (org-mind-map-write-named filename (concat filename ".dot") linksp)
    (widen)))

(defun org-mind-map-export-message nil
  "Message string for `org-export-dispatch' buffer."
  (if (> (length org-mind-map-dot-output) 1)
      "Select output file format"
    (concat "As " (car org-mind-map-dot-output) " file")))

(org-export-define-derived-backend 'graphviz 'org
  :menu-entry
  '(?g "Export to graphviz diagram"
       ((?f "Create graph" ox-graphviz-export)
	(?o "Create graph and open" ox-graphviz-export-and-open)
	(?d "Create graph & dot file" ox-graphviz-export-dot)
	(?O "Create graph & dot file, and open graph" ox-graphviz-export-dot-and-open))))

;; Add a tool bar icon
;; (define-key org-mode-map [tool-bar org-button]
;; '(menu-item "Write the org-mode file mind map to disk." org-mind-map-write-with-prompt
;;    :image (image :type xpm :file "info.xpm")
;;    ))

;; Add menu items
;; (define-key org-mode-map [menu-bar Org Diagram]
;;   (cons "Graphviz diagram" (make-sparse-keymap "Graphviz diagram")))

;; (define-key org-mode-map [menu-bar Org Diagram all]
;;   '("Diagram of whole buffer" . org-mind-map-write))

;; (define-key org-mode-map [menu-bar Org Diagram current]
;;   '("Diagram of current tree" . org-mind-map-write-current-tree))

;; (define-key org-mode-map [menu-bar Org Diagram branch]
;;   '("Diagram of current branch" . org-mind-map-write-current-branch))

;; (global-set-key (kbd "<f4>") 'org-mind-map-write)

(provide 'org-mind-map)
;;; org-mind-map.el ends here

