# Makefile for SiB4 GPCP Processing Tools
#
# You will need GNU's version of Make, 
# which is the default on Linux, OS X
# but not necessarily other Unixes.
#
# Prior to changing options, 
# make sure to remove all object files
# user@host: make clean
#
# Values for COMPILER: gcc
ifndef COMPILER
   COMPILER = gcc
endif
# Currently only uses gfortran.
#
# Values for OPT: debug, opt*
ifndef OPT
   OPT = opt
endif
# Optimized is the default,
# but to change to debug, specify: 
# user@host:make OPT=debug
#
#Values for NC: nc3, nc4
ifndef NC
   NC = 4
endif
# netcdf 4 is the default,
# but to change to netcdf 3, specify:
# user@host:make NC=3
#
# You can tell the Makefile where to find NetCDF libraries
# by setting the NETCDF_ROOT environment variables as well:
# user@host:~$ export NETCDF_ROOT=/usr/local
#--------------------------------------------------------------
# Directory containing netcdf's include/ and lib/ directories.
ifndef NETCDF_ROOT
  ifeq ($(NC),3)
      NCROOT = /usr/local/netcdf3-$(COMPILER)
      LIBS = -L$(NCROOT)/lib -lnetcdf
  else
      NCROOT = /usr/local/netcdf4-$(COMPILER)
      H5ROOT = /usr/local/hdf5-$(COMPILER)
      LIBS = -L$(NCROOT)/lib -L$(H5ROOT)/lib -lhdf5_hl -lhdf5 -lnetcdff -lnetcdf
  endif

  DIREXIST = $(shell if test -d $(NCROOT); then echo yes; else echo no; fi)
  ifeq ($(DIREXIST),no)
       NCROOT = /usr/local
       H5ROOT = /usr/local
  endif

else
  NCROOT = $(NETCDF_ROOT)
endif
INCLUDES = -I$(NCROOT)/include

##To check NCROOT and INCLUDES:
#$(info $$NCROOT is [${NCROOT}])
#$(info $$INCLUDES is [${INCLUDES}])

ifeq ($(COMPILER),gcc)
  F90 = gfortran
  ifeq ($(OPT),opt)
    F90FLAGS = -O2
  else 
    F90FLAGS = -g -Wall -Wuninitialized -fbounds-check \
         -ffpe-trap=invalid,zero,overflow -fbacktrace -finit-real=nan
  endif

  ifeq ($(PROC),powerpc)
    F90FLAGS += -fconvert=little-endian
  endif

  F90FLAGS += -fimplicit-none -Wsurprising $(INCLUDES)
  LFLAGS = $(LIBS)
endif
##To check F90FLAGS:
#$(info $$F90FLAGS is [${F90FLAGS}])

#--------------------------------------------------------

PROC_OBJS = \
   gpcp_tdown.o \
   gpcp_tup.o 

DRV_OBJS = \
   gpcp_checktot.o \
   gpcp_prep23.o \

#--------------------------------------------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $< -o $@

all: $(PROC_OBJS) $(DRV_OBJS)
	@echo ""
	$(F90) gpcp_prep23.o gpcp_tdown.o gpcp_tup.o -o gpcp_prep23 $(LFLAGS) 
	@echo Finished making == gpcp_prep23
	@echo ""
	$(F90) gpcp_checktot.o -o gpcp_checktot $(LFLAGS) 
	@echo Finished making == gpcp_checktot
	@echo ""

gpcp_prep23: gpcp_prep23.o gpcp_tdown.o gpcp_tup.o
	$(F90) $^ -o gpcp_prep23 $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == gpcp_prep23
	@echo ""

gpcp_checktot: gpcp_checktot.o
	$(F90) gpcp_checktot.o -o gpcp_checktot $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == gpcp_checktot
	@echo ""
clean:	
	rm -f *.mod *~ *.o

cleane:
	rm -f gpcp_checktot gpcp_prep23
