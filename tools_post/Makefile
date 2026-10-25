# Makefile for SiB4 Post-Processing Tools
#
# You will need GNU's version of Make, 
# which is the default on Linux, OS X
# but not necessarily other Unixes.
#
# Prior to changing options, 
# make sure to remove all object files
# user@host: make clean
#
# Values for COMPILER: gcc, intel
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
      LIBS = -L$(NCROOT)/lib -L$(H5ROOT)/lib -lnetcdff -lnetcdf \
        -lhdf5_hl -lhdf5 -lz #-ldl
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

# Set compiler option flags
ifeq ($(COMPILER),intel)
  F90 = ifort
  ifeq ($(OPT),opt)
    F90FLAGS = -O3 -xHost -ip -no-prec-div -static-intel -implicitnone -warn all $(INCLUDES)
  else ifeq ($(OPT),debug)
    F90FLAGS = -g -check bounds -fpe0
  endif

  ifeq ($PROC),powerpc)
      F90FLAGS += -convert little_endian
  endif

  LFLAGS = $(LIBS)
endif

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

##To check compiler and compile flags:
#$(info $$F() is [${F90}])
#$(info $$F90FLAGS is [${F90FLAGS}])

#--------------------------------------------------------

CONTROL_OBJS = \
   sib4post_control.o \
   sib4post_parse.o \
   sib4post_process.o

PROC_OBJS = \
   addresp_output.o \
   balance_output.o \
   checkbal_output.o \
   combine_output.o \
   conjoin_output.o \
   copy_output.o \
   cropdist_output.o \
   disjoin_output.o \
   gpptot_output.o \
   grid_output.o \
   meanclim_output.o \
   merge_output.o \
   neetot_output.o \
   pullvar_output.o \
   regrid_output.o \
   regrid_toolsdwn.o \
   regrid_toolsup.o \
   renamevar_output.o \
   replaceresp_output.o \
   spmerge_output.o \
   unstack_output.o \
   vec2vec_output.o 

TOOLS_OBJS = \
   sibinfo_module.o \
   sibinfo_tools.o 

DRV_OBJS = \
   addresp_sibgrid.o \
   balance_sibgrid.o \
   checkbal_sibgrid.o \
   combine_sib.o \
   conjoin_sib.o \
   copy_sibgrid.o \
   cropdist_sibgrid.o \
   disjoin_sibgrid.o \
   gpptot_sibgrid.o \
   grid_sib.o \
   meanclim_sib.o \
   merge_sib.o \
   neetot_sibgrid.o \
   pullvar_sibgrid.o \
   regrid_grid.o \
   renamevar_sibgrid.o \
   replaceresp_sibgrid.o \
   spmerge_sib.o \
   unstack_sibgrid.o \
   vec2vec_sib.o \
   sib4post.o \

PROGS = \
   addresp_sibgrid \
   balance_sibgrid \
   checkbal_sibgrid \
   combine_sib \
   conjoin_sib \
   copy_sibgrid \
   cropdist_sibgrid \
   disjoin_sibgrid \
   gpptot_sibgrid \
   grid_sib \
   meanclim_sib \
   merge_sib \
   neetot_sibgrid \
   pullvar_sibgrid \
   regrid_grid \
   renamevar_sibgrid \
   replaceresp_sibgrid \
   spmerge_sib \
   unstack_sibgrid \
   vec2vec_sib \
   sib4post

#--------------------------------------------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $< -o $@

all: $(PROGS)

addresp_sibgrid: addresp_sibgrid.o addresp_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o addresp_sibgrid $(LFLAGS)
	@echo Finished making == addresp_sibgrid

balance_sibgrid: balance_sibgrid.o balance_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o balance_sibgrid $(LFLAGS)
	@echo Finished making == balance_sibgrid

checkbal_sibgrid: checkbal_sibgrid.o checkbal_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o checkbal_sibgrid $(LFLAGS)
	@echo Finished making == checkbal_sibgrid

combine_sib: combine_sib.o combine_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o combine_sib $(LFLAGS)
	@echo Finished making == combine_sib

conjoin_sib: conjoin_sib.o conjoin_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o conjoin_sib $(LFLAGS)
	@echo Finished making == conjoin_sib

copy_sibgrid: copy_sibgrid.o copy_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o copy_sibgrid $(LFLAGS)
	@echo Finished making == copy_sibgrid

cropdist_sibgrid: cropdist_sibgrid.o cropdist_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o cropdist_sibgrid $(LFLAGS)
	@echo Finished making == cropdist_sibgrid

disjoin_sibgrid: disjoin_sibgrid.o disjoin_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o disjoin_sibgrid $(LFLAGS)
	@echo Finished making == disjoin_sibgrid

gpptot_sibgrid: gpptot_sibgrid.o gpptot_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o gpptot_sibgrid $(LFLAGS)
	@echo Finished making == gpptot_sibgrid

grid_sib: grid_sib.o grid_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o grid_sib $(LFLAGS)
	@echo Finished making == grid_sib

meanclim_sib: meanclim_sib.o meanclim_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o meanclim_sib $(LFLAGS)
	@echo Finished making == meanclim_sib

merge_sib: merge_sib.o merge_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o merge_sib $(LFLAGS)
	@echo Finished making == merge_sib

neetot_sibgrid: neetot_sibgrid.o neetot_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o neetot_sibgrid $(LFLAGS)
	@echo Finished making == neetot_sibgrid

pullvar_sibgrid: pullvar_sibgrid.o pullvar_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o pullvar_sibgrid $(LFLAGS)
	@echo Finished making == pullvar_sibgrid

regrid_grid:  regrid_grid.o regrid_output.o regrid_toolsdwn.o regrid_toolsup.o \
               $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o regrid_grid $(LFLAGS)
	@echo Finished making == regrid_grid

renamevar_sibgrid: renamevar_sibgrid.o renamevar_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o renamevar_sibgrid $(LFLAGS)
	@echo Finished making == renamevar_sibgrid

replaceresp_sibgrid: replaceresp_sibgrid.o replaceresp_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o replaceresp_sibgrid $(LFLAGS)
	@echo Finished making == replaceresp_sibgrid

spmerge_sib: spmerge_sib.o spmerge_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o spmerge_sib $(LFLAGS)
	@echo Finished making == spmerge_sib

unstack_sibgrid: unstack_sibgrid.o unstack_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o unstack_sibgrid $(LFLAGS)
	@echo Finished making == unstack_sibgrid

vec2vec_sib: vec2vec_sib.o vec2vec_output.o $(TOOLS_OBJS)
	@echo ""
	@date
	$(F90) $^ -o vec2vec_sib $(LFLAGS)
	@echo Finished making == vec2vec_sib

sib4post: sib4post.o $(TOOLS_OBJS) $(PROC_OBJS) $(CONTROL_OBJS)
	@echo ""
	@date
	$(F90) $^ -o sib4post $(LFLAGS)
	@echo ""
	@echo Finished making == sib4post
	@echo ""

clean:	
	rm -f *.mod *~ *.o *genmod*

cleane: clean
	rm -f $(PROGS)


# which files depend on which modules
addresp_output.o: sibinfo_module.o
balance_output.o: sibinfo_module.o
checkbal_output.o: sibinfo_module.o
combine_output.o: sibinfo_module.o
conjoin_output.o: sibinfo_module.o
copy_output.o: sibinfo_module.o
cropdist_output.o: sibinfo_module.o
disjoin_output.o: sibinfo_module.o
disjoin_sibgrid.o: sibinfo_module.o
gpptot_output.o: sibinfo_module.o
grid_output.o: sibinfo_module.o
meanclim_output.o: sibinfo_module.o
meanclim_sib.o: sibinfo_module.o
merge_output.o: sibinfo_module.o
merge_sib.o: sibinfo_module.o
neetot_output.o: sibinfo_module.o
pullvar_output.o: sibinfo_module.o
regrid_output.o: sibinfo_module.o
regrid_toolsdwn.o: sibinfo_module.o
regrid_toolsup.o: sibinfo_module.o
replaceresp_output.o: sibinfo_module.o
sib4post.o: sibinfo_module.o
sib4post_parse.o: sibinfo_module.o
sib4post_process.o: sibinfo_module.o
sibinfo_tools.o: sibinfo_module.o
spmerge_output.o: sibinfo_module.o
spmerge_sib.o: sibinfo_module.o
unstack_output.o: sibinfo_module.o
vec2vec_output.o: sibinfo_module.o
