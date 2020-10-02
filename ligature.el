;;; ligature.el --- display typographical ligatures in major modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords: tools faces
;; Homepage: https://www.github.com/mickeynp/ligature.el
;; Package-Requires: ((emacs "27.1"))
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package converts graphemes (characters) present in major modes
;; of your choice to the stylistic ligatures present in your frame's
;; font.
;;
;; For this to work, you must meet several criteria:
;;
;;  1. You must use Emacs 27.1 or later;
;;
;;  2. Your Emacs must be built with Harfbuzz enabled -- this is the
;;     default as of Emacs 27.1, but obscure platforms may not support
;;     it;
;;
;;  3. You must have a font that supports the particular typographical
;;     ligature you wish to display.  Emacs should skip the ones it does
;;     not recognize, however;
;;
;;  4. Ideally, your Emacs is built with Cairo support.  Without it,
;;     you may experience issues;
;;
;;     a. Older versions of Cairo apparently have some
;;     issues.  `cairo-version-string' should say "1.16.0" or later.
;;
;;
;; If you have met these criteria, you can now enable ligature support
;; per major mode.  Why not globally? Well, you may not want ligatures
;; intended for one thing to display in a major mode intended for
;; something else.  The other thing to consider is that without this
;; flexibility, you would be stuck with whatever style categories the
;; font was built with; in Emacs, you can pick and choose which ones
;; you like.  Some fonts ship with rather unfashionable ligatures.  With
;; this package you will have to tell Emacs which ones you want.
;;
;;
;;
;; GETTING STARTED
;; ---------------
;;
;; To install this package you should use `use-package', like so:
;;
;; (use-package ligature
;;   :config
;;   ;; Enable the "www" ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))
;;   ;; Enable traditional ligature support in eww-mode, if the
;;   ;; `variable-pitch' face supports it
;;   (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
;;   ;; Enable all Cascadia Code ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                                        "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                                        "\\" "://"))
;;   ;; Enables ligature checks globally in all buffers.  You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode t))
;;
;; The example above registers a couple of ligatures (they only appear
;; *if* your font supports them!) against `html-mode', a built-in
;; mode; and `web-mode', a third-party mode.  It also enables all known
;; `Cascadia Code' ligatures against `prog-mode', and one ligature
;; (`www') against every known major mode.
;;
;; Please note, that for complex cases -- such as variadic ligature
;; support, like the arrows in the Fira Code font -- you must amend
;; the `ligature-composition-table' directly, as
;; `ligature-set-ligatures' only supports simple strings.
;;
;; The command `ligature-generate-ligatures' does all the hard work of
;; making the ligatures you configured in the `:config' section
;; work.  To activate ligatures put `ligature-mode' or
;; `global-ligature-mode' after you configure the ligatures you want.
;;
;; LIMITATIONS
;; -----------
;;
;; You can only have one character map to a regexp of ligatures that
;; must apply.  This is partly a limitation of Emacs's
;; `set-char-table-range' and also of this package.  No attempt is made
;; to 'merge' groups of regexp.  This is only really going to cause
;; issues if you rely on multiple mode entries in
;; `ligature-composition-table' to fulfill all the desired ligatures
;; you want in a mode, or if you indiscriminately call
;; `ligature-set-ligatures' against the same collection of modes with
;; conflicting ligature maps.
;;
;; OUTSTANDING BUGS
;; ----------------
;;
;; Yes, most assuredly so.


;;; Code:

(require 'cl-lib)

(defvar ligature-composition-table nil
  "Alist of ligature compositions.

Each element in the alist is made up of (MODES
. LIGATURE-MAPPINGS) where LIGATURE-MAPPINGS is an alist
of (STR-CHAR . LIGATURE-PATTERN) and MODES is either:

  a. A major mode, such as `prog-mode' or `c-mode';

  b. A list of major modes, such as `(prog-mode c-mode)';

  c. The value `t', indicating the associated ligature mappings
  must apply to _all_ modes, even internal ones.

A STR-CHAR is a string consisting of a _single_ character that
defines the beginning of a ligature. The LIGATURE-PATTERN is a
regexp that should match all the various ligatures that start
with STR-CHAR. For instance, `!' as a STR-CHAR may have a two
ligatures `=' and `==' that together form `!=' and `!=='.")

;;;###autoload
(defun ligature-set-ligatures (modes ligatures)
  "Replace LIGATURES in MODES.

Converts a list of ligatures, in simplified string format, to
MODES.  As there is no easy way of computing which ligatures
were already defined, this function will replace any existing
ligature definitions in `ligature-composition-table' with
LIGATURES for MODES.

Example:

  (ligature-set-ligatures '(web-mode html-mode) '(\"<!--\" \"-->\"))

Adds support for the ligatures `<!--' and `-->' to `web-mode', a
third-party major mode; and `html-mode', a built-in major
mode. Note, however, that any existing ligature entries that
start with `<' or `-' are replaced.

LIGATURES grouped by the first character of each entry and
passed, untouched, to `regexp-opt' to be turned into a regular
expression that will match against ligatures beginning with that
first character."
  (let (grouped-ligatures)
    (dolist (ligature ligatures)
      (let ((key (substring ligature 0 1)))
        (push (substring ligature 1) (alist-get key grouped-ligatures nil nil #'equal))))
    (dolist (group grouped-ligatures)
      (setf (alist-get (car group)
                       (alist-get modes ligature-composition-table nil 'remove #'equal) nil 'remove #'equal)
            (regexp-opt (cdr group))))))

;;;###autoload
(defun ligature-generate-ligatures ()
  "Generate mode-specific character tables for ligatures.

The ligature generator traverses `ligature-composition-table' and
applies every ligature definition from every mode that is a
`derived-mode-p' of the current major mode.  That means
`prog-mode' will likely match most programming major modes that
define their parent as `prog-mode'.

The changes are then made local to the current buffer."
  (interactive)
  (let ((table (make-char-table nil)))
    (dolist (ligature-table ligature-composition-table)
      (let ((modes (car ligature-table)) ; `rst-mode', `html-mode', etc.
            (rules (cdr ligature-table))) ; alist of rules mapping a character to a regexp.
        ;; If `mode' is t we always apply the rules, regardless of
        ;; whether `derived-mode-p' matches or not.
        (when (or (booleanp modes) (cl-remove-if 'null (mapcar 'derived-mode-p
                                                            (if (listp modes)
                                                                modes
                                                              (list modes)))))
          (dolist (rule rules)
            (set-char-table-range table (string-to-char (car rule))
                                  ;; in order for Emacs to properly
                                  ;; understand the ligature mappings we
                                  ;; must include either a generic "match
                                  ;; any" metacharacter to represent the
                                  ;; character that we use to define the
                                  ;; beginning of a character table
                                  ;; range.
                                  `([,(concat "." (cdr rule)) 0 font-shape-gstring]))))))
    (set-char-table-parent table composition-function-table)
    (setq-local composition-function-table table)))

;;;###autoload
(define-minor-mode ligature-mode "Enables typographic ligatures" nil nil nil
  (if ligature-mode
      (ligature-generate-ligatures)
    (setq-local composition-function-table (default-value 'composition-function-table))))

(defun turn-on-ligature-mode ()
  "Turn on command `ligature-mode'."
  (ligature-mode t))

(define-globalized-minor-mode global-ligature-mode ligature-mode turn-on-ligature-mode)


(provide 'ligature)
;;; ligature.el ends here
