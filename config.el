;;; config.el --- scheme Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers gerbil-mode)

(defvar gerbil-home (getenv "GERBIL_HOME")
  "If not set in $GERBIL_HOME, set this to GERBIL_HOME in your .spacemacs")

(defvar gerbil-gxi "/bin/gxi"
  "path to gxi executable, relative to 'gerbil-home")

(defvar gambit-home (getenv "GAMBIT_HOME")
  "If not set in $GERBIL_HOME, set this to GAMBIT_HOME in your .spacemacs")

(defvar gerbil-enable-linum nil
  "If non-nil will enable line numbers")

(defvar gerbil-src-tags-location "~/.gerbil/src/TAGS"
  "Set this if your TAGS file is not $GERBIL_HOME/src/TAGS")

(defvar gerbil-pkg-tags-location "~/.gerbil/pkg/TAGS"
  "Set this if your TAGS file is not $GERBIL_HOME/pkg/TAGS")

(defvar gerbil-backend nil
  "Backend used for repl, completion and TAGS. Available options
  are `gerbil', `treadmill', `slime'")
