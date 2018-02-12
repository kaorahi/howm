# shamelessly copied from navi2ch-cvs-0.0.20031209
# $Id: configure.in,v 1.225 2012-12-29 00:59:48 hira Exp $

AC_INIT([howm], [1.4.5-snapshot])
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

# Emacs set environment variable EMACS as t
test "x$EMACS" = xt && unset EMACS

emacsen="emacs xemacs"

AC_CANONICAL_HOST

case "$host_os" in
*cygwin*)
  with_bash=yes
  emacsen="$emacsen MeadowNT.exe Meadow95.exe Meadow.exe"
  ;;
esac

AC_ARG_WITH(bash,
[  --with-bash             Use Bash to build],
[case "$withval" in
yes) SHELL=bash ;;
no)  ;;
*)   SHELL="$withval" ;;
esac])

AC_ARG_WITH(xemacs,
[  --with-xemacs           Use XEmacs to build],
[case "$withval" in
yes) emacsen="xemacs" ;;
no)  emacsen="emacs" ;;
*)   EMACS="$withval" ;;
esac])

AC_ARG_WITH(emacs,
[  --with-emacs            Use Emacs to build],
[case "$withval" in
yes) emacsen="emacs" ;;
no)  emacsen="xemacs" ;;
*)   EMACS="$withval" ;;
esac])

if test -n "${with_xemacs+set}" && test -n "${with_emacs+set}"; then
  AC_MSG_ERROR(specify one of --with-xemacs or --with-emacs)
fi

AC_CHECK_PROG(CVS, cvs, cvs -z3, :)
AC_PROG_LN_S

AC_CHECK_PROGS(EMACS, ${emacsen}, no)
if test "$EMACS" = "no"; then
  AC_MSG_ERROR(cannot find emacs)
fi

AC_MSG_CHECKING([detecting your emacs type])
AC_EMACS_LISP(emacstype, [dnl
(princ (cond ((featurep 'xemacs) 'xemacs)
             ((>= emacs-major-version 21) 'emacs21)
             ((= emacs-major-version 20) 'emacs20)
             (t 'UNKNOWN)))])
AC_MSG_RESULT([$emacstype])

if test -z "$emacstype" || test "$emacstype" = UNKNOWN; then
  AC_MSG_ERROR(this package does not work on your emacs)
fi

# detect packagedir
AC_ARG_WITH(packagedir,
[  --with-packagedir[[=DIR]] Use package directory
                          (With XEmacs, this option also sets
                           docdir, lispdir, etc...)],
[ AC_MSG_CHECKING([where package files should go])
test "$emacstype" != xemacs && AC_MSG_ERROR(packagedir only works with XEmacs)
case "$withval" in
yes) AC_EMACS_LISP(packagedir, [dnl
(princ (expand-file-name "../site-packages" data-directory))])
;;
no)  unset packagedir ;;
*)   packagedir="$withval" ;;
esac
AC_MSG_RESULT([$packagedir])])

AC_SUBST(packagedir)

if test "$emacstype" = xemacs && test -n "$packagedir"; then
  lispdir='${packagedir}/lisp'
  docdir='${datadir}/howm/doc'
  extdir='${datadir}/howm/ext'
fi

AM_PATH_LISPDIR

AC_ARG_WITH(howmdir,
[  --with-howmdir=DIR      Override the default howm directory],
[ howmdir="$withval"
AC_MSG_CHECKING([where howm files should go])
AC_MSG_RESULT([$howmdir])],
[
if test -z "${howmdir}"; then
  AC_CACHE_CHECK([where howm files should go], [ac_cv_howmdir], [dnl
        ac_cv_howmdir='${lispdir}/howm'
  ])
  howmdir="$ac_cv_howmdir"
fi
])

AC_ARG_WITH(docdir,
[  --with-docdir=DIR       Override the default docs directory],
[ docdir="$withval"
AC_MSG_CHECKING([where doc files should go])
AC_MSG_RESULT([$docdir])],
[
if test -z "${docdir}"; then
  AC_CACHE_CHECK([where doc files should go], [ac_cv_docdir], [dnl
dnl     AC_EMACS_LISP(ac_cv_docdir,[dnl
dnl (princ (expand-file-name "howm/doc" data-directory))])
dnl     if test -z "$ac_cv_docdir"; then
dnl       ac_cv_docdir='${datadir}/pixmaps/howm'
dnl     fi
        ac_cv_docdir='${datadir}/howm/doc'
  ])
  docdir="$ac_cv_docdir"
fi
])

AC_ARG_WITH(extdir,
[  --with-extdir=DIR       Override the default ext directory],
[ extdir="$withval"
AC_MSG_CHECKING([where ext files should go])
AC_MSG_RESULT([$extdir])],
[
if test -z "${extdir}"; then
  AC_CACHE_CHECK([where ext files should go], [ac_cv_extdir], [dnl
        ac_cv_extdir='${datadir}/howm/ext'
  ])
  extdir="$ac_cv_extdir"
fi
])

AC_ARG_WITH(endir,
[  --with-endir=DIR        Obsolete. no need. no effect.],
[ 
AC_MSG_WARN([--with-endir is obsolete. no need. no effect.])
])

AC_ARG_WITH(jadir,
[  --with-jadir=DIR        Obsolete. no need. no effect.],
[ 
AC_MSG_WARN([--with-jadir is obsolete. no need. no effect.])
])

AC_SUBST(howmdir)
AC_SUBST(docdir)
AC_SUBST(extdir)

# link files
if test `cd ${srcdir}; pwd` != `pwd`; then
  AC_CONFIG_COMMANDS([link-lisp],
  [
    rm -f *.el
    ${howm_ln_s} ${ac_srcdir}/*.el .
  ], [howm_ln_s="${LN_S}"])
#  AC_CONFIG_LINKS(doc/howm.info:doc/howm.info)
fi

AC_OUTPUT([
  Makefile doc/Makefile ext/Makefile sample/Makefile
  en/Makefile fr/Makefile ja/Makefile
  howm.el
])
