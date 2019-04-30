;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (compile-command    . "cabal new-build xxx-package-xxx:lib:xxx-package-xxx -fdevelop")

     (dante-target       . "xxx-package-xxx:lib:xxx-package-xxx")
;;   (dante-project-root . "__ProjectDirectory__/")
     (dante-project-root . "/home/sboo/haskell/skeletor/projects/default/")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;