################################################################
# Build the SWI-Prolog eclisp package for MS-Windows
#
# Author: Jan Wielemaker
#
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
CFLAGS=$(CFLAGS) /D__SWI_PROLOG__

ECLISP=ECLISPWAPI
CFLAGS=$(CFLAGS) /DECLISP_WINAPI

OBJ=		eclisp.obj

all:		eclisp.dll

eclisp.dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(ECLISP).lib $(PLLIB) $(LIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

################################################################
# Testing
################################################################

check::

################################################################
# Installation
################################################################

idll::
		copy "$(EXTRALIBDIR)\$(ECLISP).dll" "$(BINDIR)"
		copy eclisp.dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy eclisp.pdb "$(BINDIR)"
!ENDIF

ilib::
		copy eclisp.pl "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\eclisp.dll"
		del "$(PLBASE)\library\eclisp.pl"
		$(MAKEINDEX)

html-install::
		copy eclisp.html "$(PKGDOC)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


