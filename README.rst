================================
 Typographic Ligatures in Emacs
================================


.. image:: cascadia-code-full.svg 

This package maps ordinary graphemes (characters) to fancy ligatures,
if both your version of Emacs and the font supports it.

With this package you can control where Emacs must display ligatures.
That is useful if you only want a subset of the ligatures in certain
major modes, for instance, or if you want to ensure that some modes
have no ligatures at all.

If you know what you're doing, you can skip to the end for an example
that works with ``Cascadia Code`` (and most probably other fonts, too).

Compatibility and Version Requirements
======================================

Support for this feature is new. You must meet a number of requirements to ensure the package works correctly:

1. You must use Emacs 27.1 or later;

   You can check by typing ``M-x emacs-version``.

   **NOTE**: There are reports of crashes in Emacs 27.1. There is a
   fix in upstream versions of Emacs.

   Ideally, if at all possible, you should attempt to use a build of
   Emacs that includes this fix. See below for details.

2. Your Emacs must be built with Harfbuzz enabled -- this is the default as of Emacs 27.1, but obscure platforms may not support it;

   You can check by typing ``C-h v system-configuration-features``. Search for the word ``HARFBUZZ``.

3. You must have a font that supports the particular typographical ligature you wish to display. Emacs should skip the ones it does not recognize, however;

   Common programming fonts include `Cascadia Code
   <https://github.com/microsoft/cascadia-code>`__ and `Fira Code
   <https://github.com/tonsky/FiraCode>`__.

   For variable width fonts, the world is your oyster.

4. Ideally, your Emacs is built with Cairo support. Without it, you may experience issues;

   You can check by typing ``C-h v cairo-version-string``. If you cannot
   find it, you probably don't have it built: you can double check by
   looking at ``system-configuration-features`` -- see above.

   a. Older versions of Cairo apparently have some issues.
      ``cairo-version-string`` should say "1.16.0" or later.

      See above. It may work perfectly fine with a lower version, however.


Crash issues in Emacs 27.1
--------------------------

If you are using a release build of Emacs 27.1 then you may `experience hangs or crashes <https://github.com/mickeynp/ligature.el/issues/10>`__ with the following message::

  Attempt to shape unibyte text

The source of the fix is `this commit <http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=fe903c5ab7354b97f80ecf1b01ca3ff1027be446>`__, but it did not make it into Emacs 27.1, unfortunately.

However, if you built Emacs off the ``master`` or ``native-comp`` branch then you likely have the fix already in place.


How does it work?
=================

Unlike almost all text editors that support ligatures, you are free to choose which ligatures you want and which modes they apply to. That is rather important as you may only want some ligatures in certain modes, and perhaps none at all in other modes. With this package you can freely pick and choose.

Each buffer you want the ligatures to apply to require a call to ``ligature-generate-ligatures``. That command will check against a table of registered ligatures if the current buffer's major mode has any associated ligatures and, if it does, what they are. The command will check against anything that may be considered a valid parent of your buffer's major mode: for instance, a lot of programming major modes inherit from ``prog-mode``, so assigning ligatures to that major mode is a good way to ensure they work in most programming modes.

To create a ligature mapping you can either update the alist ``ligature-composition-table`` directly or use the helper function ``ligature-set-ligatures``. I recommend you start with the latter helper function and only modify the table if you have complex requirements.

Here is a simple example -- for a full, ready-to-go example, see the next section.

::

  (ligature-set-ligatures '(python-mode c-mode java-mode) '("!=" "==" "!=="))

That will associate those three ligatures to the three modes in the list. Next, enable ``global-ligature-mode``, if you want ligature checks carried on all buffers, or in specific buffers with ``ligature-mode``.

You can also enable ligature mappings for all possible buffer major modes by replacing ``MODES`` with ``t``::

  (ligature-set-ligatures 't '("www"))

Or in just a single mode::

  (ligature-set-ligatures 'html-mode '("www"))

If your requirements are complex, such as support for Fira Code's arrows you will have to amend the ``ligature-composition-table`` directly, like so::

  (add-to-list 'ligature-composition-table `(rst-mode ("=" . ,(rx (+ "=")))))

NOTE: Be careful when adding to the ligature composition table; you may override existing ligatures you've already created. You may have to manually 'merge' the ones you create with ``ligature-set-ligatures`` using regular expressions of your own!


How do I install it?
====================

MELPA support etc. is coming soon, but until then, you can clone the repository and paste one of the example snippets below.

Cascadia Code
-------------

This example snippet enables all ligatures for ``prog-mode`` and any
major mode that derives from that mode; that is usually most
programming-related modes. It's designed for the *Cascadia Code* font;
you may find it won't work 100% if you use a different one.

::

  (use-package ligature
    :load-path "path-to-ligature-repo"
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
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
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode t))

This is just an example. It's likely you'll want to configure it.
Please open ``ligature.el`` and read the commentary. I also recommend
you read the docstring for ``ligature-set-ligatures``.

Can I contribute support for more fonts?
========================================

I'm glad you asked. Yes, please. If you want to configure ligatures
for common programming fonts not already listed here, please raise a
PR.
