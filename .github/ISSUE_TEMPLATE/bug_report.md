---
name: Bug report
about: Help determine the likely source of the problem
title: ''
labels: bug
assignees: ''

---

If you're experiencing issues with `ligature.el`, it's worth investigating whether it's with the package, or with Emacs core.

If you're experiencing one of the following problems:

- Emacs crashes when you use `ligature.el`;
- Some ligations are visually garbled, cut off, or not rendering at all;
- No ligations are showing at all;
- Weird interactions with non-ligated characters around a ligated character;

Then it's very likely the issue is with **Emacs core**, and not `ligature.el`. This package merely interacts with the Emacs text shaping engine to configure your ligature settings. It does not, on its own, do any sort of ligation.

Crash bugs are known to occur in Emacs 27.1 and Emacs 27.2. Make sure you're using the latest Emacs. Garbled or truncated ligatures are the result of Harfbuzz (a library used by Emacs) display bugs in Emacs. They're mostly all fixed in Emacs 29.1 or later.

**If you are experiencing these issues using the latest version of Emacs, try reproducing the problem with the smallest possible set of ligatures first**. 

It's unlikely I can do anything if the issue is in Emacs core, but I can take a look. I may ask that you `M-x report-emacs-bug` and follow their bug submission guidelines.

*Feel free to delete this text if you wish to proceed with a bug report.*

Thanks!
