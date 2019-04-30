;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (compile-command    . "cabal new-run xxx-package-xxx:exe:xxx-program-xxx")

     (dante-target       . "xxx-package-xxx:exe:xxx-program-xxx")
;;   (dante-project-root . "__ProjectDirectory__/")
     (dante-project-root . "/home/sboo/haskell/skeletor/projects/default/")
     )))     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;