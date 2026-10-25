# Makefile for SiB4 MERRA Processing Tools
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

DRV_OBJS = \
   merra_pullsib.o \
   merra_pullvar.o \
   scale_merra_gpcp.o

#--------------------------------------------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $< -o $@

all: $(DRV_OBJS)
	@echo ""
	$(F90) merra_pullsib.o -o merra_pullsib $(LFLAGS) 
	@echo Finished making == merra_pullsib
	@echo ""
	$(F90) merra_pullvar.o -o merra_pullvar $(LFLAGS) 
	@echo Finished making == merra_pullvar
	@echo ""
	$(F90) scale_merra_gpcp.o -o scale_merra $(LFLAGS) 
	@echo Finished making == scale_merra
	@echo ""

merra_pullsib: merra_pullsib.o
	$(F90) $^ -o merra_pullsib $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == merra_pullsib
	@echo ""

merra_pullvar: merra_pullvar.o
	$(F90) $^ -o merra_pullvar $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == merra_pullvar
	@echo ""

merra_pullsib: merra_pullsib.o
	$(F90) $^ -o merra_pullsib $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == merra_pullsib
	@echo ""

scale_merra: scale_merra_gpcp.o
	$(F90) $^ -o scale_merra $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == scale_merra
	@echo ""

scale_merra_gpcp: scale_merra_gpcp.o
	$(F90) $^ -o scale_merra $(LFLAGS)
	@echo ""
	@date
	@echo Finished making == scale_merra
	@echo ""

clean:	
	rm -f *.mod *~ *.o

cleane:
	rm -f merra_pullsib merra_pullvar scale_merra
