REM $Id: test.bat,v 1.2 2010-05-05 13:18:39 hira Exp $

REM --- Edit HOWM_EMACS to point your emacs/meadow. ---
set HOWM_EMACS=d:/mdw/bin/meadow

set HOWM_TEST=bat
start "howm test" %HOWM_EMACS% -q --no-site-file -l sample/dot.emacs
