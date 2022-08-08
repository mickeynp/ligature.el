;;; ligature.el --- Display typographical ligatures in major modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-22  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Version: 1.0
;; Keywords: tools faces
;; Homepage: https://www.github.com/mickeynp/ligature.el
;; Package-Requires: ((emacs "28"))
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
;;  1. You must use Emacs 28.1 or later;
;;
;;     WARNING: Some issues report crash issues with Emacs versions
;;              Emacs 28.1. This is fixed in an upstream version
;;              currently only available in the master branch.
;;
;;  2. Your Emacs must be built with Harfbuzz enabled -- this is the
;;     default as of Emacs 28.1, but obscure platforms may not support
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
;;                                        "\\\\" "://"))
;;   ;; Enables ligature checks globally in all buffers.  You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode t))
;;
;;
;; EXAMPLES
;; --------
;;
;; Map two ligatures in `web-mode' and `html-mode'.  As they are simple
;; (meaning, they do not require complex regular expressions to ligate
;; properly) using their simplified string forms is enough:
;;
;;
;;
;; This example creates regexp ligatures so that `=' matches against
;; zero-or-more of `=' characters that may or may not optionally end
;; with `>' or `<'.
;;
;; (ligature-set-ligatures 'markdown-mode `(("=" ,(rx (+ "=") (? (| ">" "<"))))
;;                                          ("-" ,(rx (+ "-")))))
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

(defgroup ligature nil
  "Typographic Ligatures in Emacs."
  :group 'faces
  :prefix "ligature-")

(defcustom ligature-ignored-major-modes '(minibuffer-inactive-mode)
  "Major modes that will never have ligatures applied to them.

Unlike `ligature-generate-ligatures' the ignored major modes are
only checked when the minor mode command `ligature-mode' is enabled."
  :type '(repeat symbol)
  :group 'ligature)

(defvar ligature-composition-table nil
  "Alist of ligature compositions.

Each element in the alist is made up of (MODES
. LIGATURE-MAPPINGS) where LIGATURE-MAPPINGS is an alist
of (STR-CHAR . LIGATURE-PATTERN) and MODES is either:

  a. A major mode, such as `prog-mode' or `c-mode';

  b. A list of major modes;

  c. The value t, indicating the associated ligature mappings
  must apply to _all_ modes, even internal ones.

A STR-CHAR is a string consisting of a _single_ character that
defines the beginning of a ligature.  The LIGATURE-PATTERN is a
regexp that should match all the various ligatures that start
with STR-CHAR.  For instance, `!' as a STR-CHAR may have a two
ligatures `=' and `==' that together form `!=' and `!=='.")

;;;###autoload
(defun ligature-set-ligatures (modes ligatures)
  "Replace LIGATURES in MODES.

Converts a list of LIGATURES, where each element is either a cons
cell of `(STR-CHAR . REGEXP)' or a string to ligate, for all
modes in MODES.  As there is no easy way of computing which
ligatures were already defined, this function will replace any
existing ligature definitions in `ligature-composition-table'
with LIGATURES for MODES.


Some ligatures are variable-length, such as arrows and borders,
and need a regular expression to accurately represent the range
of characters needed to ligate them.  In that case, you must use a
cons cell of `(STR-CHAR . REGEXP)' where `STR-CHR' is the first
character in the ligature and `REGEXP' is a regular expression
that matches the _rest_ of the ligature range.

For examples, see the commentary in `ligature.el'."
  (let (grouped-ligatures)
    (dolist (ligature ligatures)
      (cond
       ;; the simplest case - we have a string we wish to ligate
       ((stringp ligature)
        (if (< (length ligature) 2)
            (error "Ligature `%s' must be 2 characters or longer" ligature)
          (let ((str-char (substring ligature 0 1)))
            (push (list 'literal (substring ligature 1))
                  (alist-get str-char grouped-ligatures nil nil #'equal)))))
       ;; cons of (STR-CHAR . REGEXP) where STR-CHR must be 1 char long
       ((consp ligature)
        (let ((str-char (car ligature))
              (ligature-regexp (cadr ligature)))
          (unless (= (length str-char) 1)
            (error "Ligature cons cell has a str-char of `%s' but must be a \
string of a single character" str-char))
          (push (list 'regex
                      ;; we can supply either a regexp string _or_ an unexpanded `rx' macro.
                      (if (stringp ligature-regexp) ligature-regexp
                        (macroexpand ligature-regexp)))
                (alist-get str-char grouped-ligatures nil nil #'equal))))))
    ;; given a grouped alist of ligatures, we enumerate each group and
    ;; update the `ligature-composition-table'.
    (dolist (group grouped-ligatures)
      ;; Sort the grouped ligatures - containing lists of either
      ;; `literal' or `regex' as the car of the type of atom in the
      ;; cdr - as we want the literal matchers _after_ the regex
      ;; matchers. It's likely the regex matchers supercede anything
      ;; the literal matchers may encapsulate, so we must ensure they
      ;; are checked first.
      (let ((regexp-matchers (cl-remove-if (apply-partially #'equal 'literal) (cdr group) :key #'car))
            ;; Additionally we need to ditch the `literal' symbol (and
            ;; just keep the cdr, which is the string literal), even
            ;; though it's a legitimate `rx' form, because `(group (|
            ;; (literal "a") (literal "aa") ...)' will NOT yield the
            ;; same automatic grouping of shortest-to-longest matches
            ;; like the canonical version that does _not_ use literal.
            (literal-matchers (mapcan 'cdr (cl-remove-if (apply-partially #'equal 'regex) (cdr group) :key #'car))))
        (setf (alist-get (car group)
                         (alist-get modes ligature-composition-table nil 'remove #'equal) nil 'remove #'equal)
              (macroexpand `(rx (|
                                 ;; `rx' does not like nils so we have
                                 ;; to filter them
                                 ;; manually. Furthermore, we prefer
                                 ;; regexp to literal matches and want
                                 ;; them to appear first.
                                 ,@(cl-remove-if 'null (list (if regexp-matchers `(group (| ,@regexp-matchers)) nil)
                                                             (if literal-matchers `(group (| ,@literal-matchers)) nil)))))))))))

;;;###autoload
(defun ligature-generate-ligatures ()
  "Ligate the current buffer using its major mode to determine ligature set.

The ligature generator traverses `ligature-composition-table' and
applies every ligature definition from every mode that matches
either t (indicating that a ligature mapping always applies);
or a major mode or list of major mode symbols that are
`derived-mode-p' of the current buffer's major mode.

The changes are then made buffer-local."
  (interactive)
  (let ((table (make-char-table nil)))
    (dolist (ligature-table ligature-composition-table)
      (let ((modes (car ligature-table)) ; `rst-mode', `html-mode', etc.
            (rules (cdr ligature-table))) ; alist of rules mapping a character to a regexp.
        ;; If `mode' is t we always apply the rules, regardless of
        ;; whether `derived-mode-p' matches or not.
        (when (or (eq modes t) (cl-some #'derived-mode-p (if (listp modes)
                                                             modes
                                                           (list modes))))
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
(define-minor-mode ligature-mode "Enables typographic ligatures."
  :init-value nil :lighter nil :keymap nil
  (if (= emacs-major-version 27)
      (warn "ligature-mode is currently broken in emacs `%s' due to a bug in
Emacs's core. For more information have a look at this ISSUE:
https://github.com/mickeynp/ligature.el/issues/28."
            emacs-major-version)
    (if (not ligature-mode)
        (setq-local composition-function-table (default-value 'composition-function-table))
      (unless (memq major-mode ligature-ignored-major-modes)
        (ligature-generate-ligatures)))))

(defun ligature-mode-turn-on ()
  "Turn on command `ligature-mode'."
  (ligature-mode t))

;;;###autoload
(define-globalized-minor-mode global-ligature-mode ligature-mode ligature-mode-turn-on)


(provide 'ligature)
;;; ligature.el ends here
