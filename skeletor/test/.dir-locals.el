;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "skeletor:test:doc")
     (dante-project-root . "~/haskell/skeletor/")
     (compile-command    . "cabal new-test --enable-tests skeletor:test:doc")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;