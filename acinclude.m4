# serial 3

# AC_EMACS_LISP(var, code)
# Set `var' with output of elisp `code'.
# To avoid incompatibilities of "single-quote in back-quote" etc,
# temporary files are used to store emacs-lisp and output of it.
AC_DEFUN([AC_EMACS_LISP],[dnl
{ ac_temp=./conftemp.$$
ac_output=confout.$$
rm -f $ac_temp $ac_output
cat >$ac_temp <<\_ACEOF
(defun ac-temp-func ()
$2
(princ "\n") ; make sure the output has trailing newline.
)
_ACEOF
$EMACS -batch -q -l $ac_temp -f ac-temp-func | sed -e '/^ *$/d' > $ac_output
$1=`cat $ac_output`
rm -f $ac_temp $ac_output; }])

# grab and hack from lispdir.m4

## ------------------------
## Emacs LISP file handling
## From Ulrich Drepper
## Almost entirely rewritten by Alexandre Oliva
## ------------------------

AC_DEFUN([AM_PATH_LISPDIR],
 [AC_ARG_WITH(lispdir,
  [  --with-lispdir          Override the default lisp directory],
  [ lispdir="$withval"
    AC_MSG_CHECKING([where .elc files should go])
    AC_MSG_RESULT([$lispdir])],
  [
  if test x${lispdir+set} != xset; then
    AC_CACHE_CHECK([where .elc files should go], [am_cv_lispdir], [dnl
      AC_EMACS_LISP(am_cv_lispdir,[dnl
(defvar result nil)
(setq load-path (nreverse load-path))
(while load-path
  (if (string-match "\\`\\(.+/site-lisp\\)/?\\'" (car load-path))
      (setq result (match-string 1 (car load-path))
            load-path nil)
    (setq load-path (cdr load-path))))
(princ (or result (expand-file-name "../site-lisp" data-directory)))])
      if test -z "$am_cv_lispdir"; then
	am_cv_lispdir='${datadir}/emacs/site-lisp'
      fi
    ])
    lispdir="$am_cv_lispdir"
  fi
 ])
 AC_SUBST(lispdir)])
