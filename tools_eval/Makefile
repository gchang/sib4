# Makefile for SiB4 Evaluation Tools
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
   pullflux.o \
   pullpools.o

TOOL_OBJS = \
   sib4_tools.o \
   merra_tools.o \
   modis_tools.o \
   fluxnet_tools.o

#--------------------------------------------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $< -o $@

all: $(TOOL_OBJS) $(PROC_OBJS)
	@echo ""
	$(F90) $(TOOL_OBJS) pullflux.o -o pullflux $(LFLAGS) 
	@echo Finished making == pullflux
	@echo ""

pullflux: $(TOOL_OBJS) pullflux.o
	$(F90) $^ -o pullflux $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == pullflux
	@echo ""

pullpools:  pullpools.o
	$(F90) $^ -o pullpools $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == pullpools
	@echo ""


clean:	
	rm -f *.mod *~ *.o

cleane:
	rm -f pullflux pullpools
