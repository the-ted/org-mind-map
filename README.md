# org-mind-map
This is an emacs package that creates graphviz directed graphs.

# Requirements

This package relies on the [Graphviz](http://graphviz.org/) suite of graphical diagramming tools. For windows (the default option), these tools are available via [cygwin](http://cygwin.com/). For linux, these tools are available through any of the popular package managers.

This package also relies on the =org-mode= and =dash= libraries, available from melpa (M-x list-packages).

# Installation

To install, add this code to your .emacs:
    (load "INSTALLPATH/org-mind-map.el")

# Examples

Here is an example org-mode tree that demonstrates the basic concept [Example 1](example-1.org), and the result pdf file is [here](example-1.org.pdf).

You can also add tags, as in this org-mode file [Example 2](example-2.org), which are randomly color-coded into pastel tags as shown [here](example-2.org.pdf).

Finally, by naming headlines across your org-mode file, as shown in [example 3](example-3.org), ypou can interleave trees.


