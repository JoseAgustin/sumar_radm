#
#	Lower level Makefile for interpola.exe conversion program
#       Suffix rules and commands
#######################
FIX01 =
#######################

.IGNORE:

.SUFFIXES:      .F90 .f .i .o

.F90.o:
		$(RM) $@ 
		$(CPP) $(CPPFLAGS) -D$(MACH) $(FIX01) $*.f90 > $*.f
		$(FC) -c $(FCFLAGS) $*.f
		

.F90.f:
		$(RM) $@
		$(CPP) $(CPPFLAGS) -D$(MACH) $(FIX01) $*.F90 > $*.f

.f.o:
		$(RM) $@
		$(FC) -c $(FCFLAGS) $(FIX01) $*.f
		

OBJS = var_suma.o lee_wrfchemin.o guarda_chem.o sumafiles.o  


SRC	=	$(OBJS:.o=.F90)


cray dec hp ibm sgi sun default:
		@echo "you need to be up a directory to make interpola.exe"

all:		suma_chem.exe 

suma_chem.exe:	$(OBJS) 
		$(FC) -o $@ $(LDOPTIONS) $(OBJS) $(LOCAL_LIBRARIES)

code:		$(SRC2) 

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

clean:
		-$(RM) $(RM_LIST)

.PHONY:	clean
