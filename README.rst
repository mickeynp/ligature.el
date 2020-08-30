================================
 Typographic Ligatures in Emacs
================================

This package maps ordinary graphemes (characters) to fancy ligatures,
if both your version of Emacs and the font supports it.

With this package you can control where Emacs must display ligatures.
That is useful if you only want a subset of the ligatures in certain
major modes, for instance, or if you want to ensure that some modes
have no ligatures at all.

Compatibility and Version Requirements
======================================

The library in Emacs that makes it work is very new, and there are
certain requirements you must meet for the package to function
correctly:

1. You must use Emacs 27.1 or later;

   You can check by typing `M-x emacs-version`

2. Your Emacs must be built with Harfbuzz enabled -- this is the
default as of Emacs 27.1, but obscure platforms may not support
it;

   You can check by typing `C-h v system-configuration-features`. Search for the word `HARFBUZZ`.

3. You must have a font that supports the particular typographical
ligature you wish to display. Emacs should skip the ones it does
not recognize, however;

   Common programming fonts include `Cascadia Code
   <https://github.com/microsoft/cascadia-code>`__ and `Fira Code
   <https://github.com/tonsky/FiraCode>`__. For variable width fonts,
   the world is your oyster.

4. Ideally, your Emacs is built with Cairo support. Without it,
you may experience issues;

   You can check by typing `C-h v cairo-version-string`. If you cannot
   find it, you probably don't have it built: you can double check by
   looking at `system-configuration-features` -- see above.

   a. Older versions of Cairo apparently have some issues.
      `cairo-version-string' should say "1.16.0" or later.

      See above. It may work perfectly fine with a lower version, however.


How do I install it?
====================

MELPA support etc. is coming soon, but until then, you can clone this
repository and then add this to your init file::

  (use-package ligature
    ;; To add ligature support to a major mode, add it here.
    :hook ((python-mode . ligature-generate-ligatures)
           (web-mode . ligature-generate-ligatures)
           (emacs-lisp-mode . ligature-generate-ligatures))
    ;; This is the path to where you cloned this git repository
    :load-path "~/.emacs.d/packages/ligature"
    :config
    ;; This configures "Cascadia Code" for all `prog-mode`-derived major
    ;; modes. Feel free to tweak this to your liking. You can remove any
    ;; ligatures you dislike.
    ;;
    ;; If you use another font, some (or all) of the ligatures may not
    ;; work. Please consult the manual for your font to find the ones
    ;; your font supports.
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                         ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                         "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                         "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                         "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                         "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                         "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                         "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                         ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                         "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                         "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                         "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\" "://"))
    ;; An example showing how to add a list of ligatures to a group
    ;; similar of major modes. You can add as many as you want.
    (ligature-set-ligatures '(html-mode web-mode) '("<!--" "-->" "</>" "</" "/>" "://")))

Please read the in-lined comments and make changes where required.
