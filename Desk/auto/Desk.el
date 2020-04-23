(TeX-add-style-hook
 "Desk"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "extsize" "handout" "10pt")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "preambBeamer"
    "texab")
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
   (LaTeX-add-bibliographies
    "RcitePkgs"
    "Rcite")))

