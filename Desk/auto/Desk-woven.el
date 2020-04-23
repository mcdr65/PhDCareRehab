(TeX-add-style-hook
 "Desk-woven"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "extsize" "handout" "10pt")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "graphicx"
    "color"
    "framed"
    "alltt"
    "preambBeamer"
    "texab"
    "upquote")
   (TeX-add-symbols
    '("hlkwd" 1)
    '("hlkwc" 1)
    '("hlkwb" 1)
    '("hlkwa" 1)
    '("hlstd" 1)
    '("hlopt" 1)
    '("hlcom" 1)
    '("hlstr" 1)
    '("hlnum" 1)
    "maxwidth"
    "hlipl"
    "FrameCommand")
   (LaTeX-add-labels
    "tab:Skalenniveaus"
    "eq:Realisierung von Stichprobenvariablen"
    "eq:2"
    "Stichprobenvarianz"
    "Standardabweichung"
    "zTrans"
    "tab:QuantilenDerZVerteilung"
    "eq:invtransz"
    "Kovarianz"
    "tab: correlation techniques")
   (LaTeX-add-environments
    "kframe"
    "knitrout")
   (LaTeX-add-bibliographies
    "RcitePkgs"
    "Rcite")
   (LaTeX-add-color-definecolors
    "fgcolor"
    "shadecolor"
    "messagecolor"
    "warningcolor"
    "errorcolor")))

