;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "xxx-package-xxx:test:doc")
     (dante-project-root . "__ProjectDirectory__/")
     (compile-command    . "cabal new-test --enable-tests xxx-package-xxx:test:doc")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;