;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "skeletor:exe:skeletor-haskell")
     (dante-project-root . "~/haskell/skeletor")
     (compile-command    . "cabal new-run skeletor:exe:skeletor-haskell -- --help")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;