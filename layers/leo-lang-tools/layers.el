;;; layers.el --- layers required by ag-lang-tools layer.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <ag.ibragimov@C02MT2ZDFH05>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layers
 '(
   (spell-checking :variables
                   enable-flyspell-auto-completion nil
                   spell-checking-enable-by-default nil
                   ispell-dictionary "american")
   syntax-checking
   (languagetool :variables
                 langtool-default-language "en-GB"
                 languagetool-show-error-on-jump t
                 langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.0/libexec/languagetool-commandline.jar")
  )
 )

;;; layers.el ends here
