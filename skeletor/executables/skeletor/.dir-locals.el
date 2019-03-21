;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "skeletor:exe:skeletor-haskell")
     (dante-project-root . "~/haskell/skeletor")
     (compile-command    . "cabal new-build skeletor:exe:skeletor-haskell -fdevelop")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;