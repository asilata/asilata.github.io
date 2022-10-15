#!/bin/bash
emacs --batch \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --eval "(let ((enable-local-variables :all)) (hack-local-variables))" \
      -l build.el
