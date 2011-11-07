(in-package #:eshop)

(defvar *email.bin*
  (find-if #'fad:file-exists-p
           (list "/usr/bin/sendmail"
                 "/usr/sbin/sendmail")))

(defvar *sendmail* *email.bin*)

(defvar *email.from* "shop@320-8080.ru")

 (defun email.send-mail-warn (to clientmail error-name)
   (when *email.bin*
     (let* ((sendmail-process (sb-ext:run-program *sendmail*
                                                  to
                                                  :input :stream
                                                  :output nil
                                                  :error nil
                                                  :wait nil))
            (sendmail (sb-ext:process-input sendmail-process)))
       (unwind-protect
            (progn
              (format sendmail "From: ~a~%" *email.from*)
              (format sendmail "To: ~a~%" (car to))
              (format sendmail "Subject: ~a~a~%" "Gateway WARN:" error-name)
              (format sendmail "MIME-Version: ~a~%" "1.0")
              (format sendmail "Content-Type: ~a~%" "multipart/mixed; boundary = becd713b5f8316a655d07bd225b48c406")
              (format sendmail "%")
              (format sendmail
                      "This is a MIME encoded message.

--becd713b5f8316a655d07bd225b48c406
Content-Type: text/html; charset=windows-1251
Content-Transfer-Encoding: base64

~a

--becd713b5f8316a655d07bd225b48c406--
"
                      (encode64 clientmail)
                      ))
         (close sendmail)
         (sb-ext:process-wait sendmail-process)
         (sb-ext:process-close sendmail-process)))))
