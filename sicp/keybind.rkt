#lang s-exp framework/keybinding-lang
; DrRacket custom keybind. Edit > Keybindings > Add User-defined Keybindings > Select this file > OKs

;; Bind Ctrl + / to Comment Out Selection
(keybinding "c:/" (λ (editor event) (send editor comment-out-selection)))

;; Bind Ctrl + Shift + / to Uncomment Selection, doesn't work
;(keybinding "c:s:/" (λ (editor event) (send editor uncomment-selection)))