m4_define([pkg_major_version],[0])
m4_define([pkg_minor_version],[0])
m4_define([pkg_micro_version],[2])
m4_define([pkg_name],    [debwatch])
m4_define([pkg_version], pkg_major_version.pkg_minor_version.pkg_micro_version)

AC_INIT(pkg_name, pkg_version)
AC_CONFIG_AUX_DIR(misc/ac-aux)
AM_INIT_AUTOMAKE([no-dist-gzip dist-bzip2])
AM_MAINTAINER_MODE


AC_GAUCHE_INIT_EXT
GAUCHE_MODULEDIR=`$GAUCHE_CONFIG --sitelibdir`
AC_SUBST(GAUCHE_MODULEDIR)

AC_CHECK_PROGS([GOSH],[gosh])
if test "x$GOSH" = x; then
   AC_MSG_ERROR(gosh not found)
fi


AC_CHECK_PROGS([WGET],[wget])
if test "x$WGET" = x; then
   AC_MSG_ERROR(wget not found)
fi

AC_CHECK_PROGS([FILTERDIFF],[filterdiff])
if test "x$FILTERDIFF" = x; then
   AC_MSG_ERROR(filterdiff not found)
fi

AC_CHECK_PROGS([PERL],[perl])
if test "x$PERL" = x; then
   AC_MSG_ERROR(perl not found)
fi

AC_CHECK_PROGS([GIT],[git])
if test "x$GIT" = x; then
   AC_MSG_ERROR(git not found)
fi


dnl dpkg

AC_SUBST(DEBWATCH_MAJOR_VERSION, pkg_major_version)
AC_SUBST(DEBWATCH_MINOR_VERSION, pkg_minor_version)
AC_SUBST(DEBWATCH_MICRO_VERSION, pkg_micro_version)


AC_RPM_RELEASE

AC_OUTPUT([
	   Makefile
	   misc/Makefile
           misc/ac-aux/Makefile
           misc/m4/Makefile
           misc/makefiles/Makefile
           misc/specs/Makefile
           misc/specs/debwatch.spec
	   src/Makefile
	   src/cmds/Makefile
	   src/gauche/Makefile
	   ])

