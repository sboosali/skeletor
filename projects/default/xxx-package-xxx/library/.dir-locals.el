;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "xxx-package-xxx:lib:xxx-package-xxx")
     (dante-project-root . "__ProjectDirectory__/")
     (compile-command    . "cabal  new-build xxx-package-xxx:lib:xxx-package-xxx -fdevelop")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;