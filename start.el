(mapc #'(lambda (file)
           (find-file file)
           (slime-compile-region 1 (length (buffer-string)))
           (read-char)
           (kill-buffer (current-buffer))
           )
       '(
         "~/cl-eshop/start.lisp"
         "~/cl-eshop/eshop-config.lisp"
         "~/cl-eshop/errors.lisp"
         "~/cl-eshop/spike.lisp"
         "~/cl-eshop/classes.lisp"
         "~/cl-eshop/serializers.lisp"
         "~/cl-eshop/servo.lisp"
         "~/cl-eshop/trans.lisp"
         "~/cl-eshop/routes.lisp"
         "~/cl-eshop/render.lisp"
         "~/cl-eshop/cart.lisp"
         "~/cl-eshop/generics.lisp"
         "~/cl-eshop/gateway.lisp"
         "~/cl-eshop/search.lisp"
         "~/cl-eshop/xls.lisp"
         "~/cl-eshop/yml.lisp"
         "~/cl-eshop/articles.lisp"
         "~/cl-eshop/report.lisp"
         "~/cl-eshop/sklonenija.lisp"
         "~/cl-eshop/newcart.lisp"
         "~/cl-eshop/main-page.lisp"
         "~/cl-eshop/sitemap.lisp"
         ))
