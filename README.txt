Welcome to the SiB4 Corral (Version 2)!
-----------------------------------------

SiB4v2 includes the following updates:
 --> New seasonal and climatological variables for
     phenological and respiration processes calculated
     using running means
 --> New leaf transfer processes for senescence
 --> New respiration processes, and parameters
     to select PFT-appropriate behavior
 --> New inclusion of fire emissions (GFED), with
     burned carbon removal from carbon pools
 --> Updated crop growing-degree-day phenology
 --> Modified grazing scheme
 --> Fixed steady-state (equilibrium) carbon pools
 --> Carbon balance checked daily

This is where all the code and information is avaliable.  
Below is a brief outline of what is in each of the folders.

=============================
code
=============================
This folder contains all the code for SiB4.  
SiB4 is compiled using a Makefile.  While we have attempted to
make this generic, you may need to modify it for your system.
To compile SiB4, type:
>make
which will compile with the default compile (gfortran), 
the default netcdf libraries (netcdf4), and the default
optimization (optimized).
To use NetCDF and LAPACK from elsewhere (for example a conda
environment), type:
>make NETCDF_ROOT=$CONDA_PREFIX LAPACK_ROOT=$CONDA_PREFIX
Add -j (for example 'make -j4') to compile in parallel.

Finishing compilation will create the 'SiB4D'.

Typically, to run SiB4, I move the executable back to
the main directory (so from "code" >mv SiB4D ..)

=============================
info
=============================
This folder contains information regarding SiB4.  

The README_start file provides information to install, compile, 
  and run SiB4 at a sample site.

The "all" folder contains basic information regarding SiB4.
--Text instructions for running SiB4 are in the 'README_run' file.
--Information on how SiB4 works, as well as figures that
  illustrate SiB4 are in the 'SiB4_tech.pptx' file.
  (Right now this is as close as we have to a tech document.)
--Detailed instructions on how to run SiB4 and a breakdown
  of the routines themselves are in the 'SiB4_log.pptx' file.
  (This is currently being updated.)
--A table of the current PFTs in SiB4 is in 'pft_ref.txt'
--A table of the current pools in SiB4 is in 'pool_ref.txt'

The "global" folder contains information for global simulations.
--The 'namel_sibdrv' file is the namelist for SiB4, with basic
  global settings currently.  
  To run SiB4, you will need to either copy this to the same
  directory as the executable or specify the location/name
  of the file following the executable when running.
--The 'pft_maps.pptx' file contains current maps of the PFTs.

The "sites" folder contains information for site simulations.
--The README_site file contains instructions on how to
  run a site simulation.
--The 'namel-sibdrv' file is setup with basic settings for
  running a site simulation.  You will need to change the
  XX-XXX references to the desired site name.

--The 'site_boreas.txt' file contains the site names and 
  lat/lon for the BOREAS sites.
--The 'site_lbalist.txt' file contains the site names and
  lat/lon for the LBA sites.

--The 'site_infolist.xlsx' is a spreadsheet listing all the
  currently available sites for SiB4.  The driver data
  for all these sites is available on Google Drive. 
--The 'site_map.pptx' file contains a map showing the
  locations of the sites.
--The 'site_pftlist.txt' file contains a list of sites 
  organized by PFT.  
  This is useful for running group simulations
  containing all sites for a specific PFT in order to 
  test PFT-based processes and evaluate SiB4.

--The README_sample file contains instructions on 
  running a simple sample site
--The sample.tgz file contains the driver data 
  necessary for the sample site

=============================
input
=============================
This folder contains the input files needed by SiB4.

--The "params" folder contains the parameter files.
  ->info_pft.dat: PFT information
  ->info_pool.dat: Pool information

  ->phen_gdd.dat: Phenology information for the
       crop PFTs that are run using a 
       growing-degree-day (gdd) approach
  ->phen_stg.dat: Phenology information for the
       non-crop PFTs that are run using a 
       stage-based (stg) approach

   ->sib_areo.nc: Aerodynamic parameters
   ->sib_phys.dat: Physiological parameters
   ->sib_pool.dat: Pool parameters

--The 'sib_outopts' file contains the options for diagnostic output.
  SiB4 has three diagnostic outputs: qsib, psib, and hsib.
  Typically, I use qsib for monthly output, psib for daily output,
  and hsib for hourly output; however, you can set these to whichever
  you would like.  Note that the psib output has the capability to 
  either contain all points or be used to contain only certain points
  that are listed in the namel_sibdrv.

   Setting the flags to 't' turns on the output diagnostic for each
   diagnostic output type, and the order is: qsib, psib, hsib.

--The 'sib_routopts' file contains the options for restart output.
  SiB4 has two restart outputs: restart files (r) and 
  equilibrium files (requib).
   Setting the flags to 't' turns on the output for the 
   restart (first column) and requib (second column) files.

=============================
scripts
=============================
This folder contains scripts that I find useful in 
compiling, spinning up, running, and processing SiB4.
The 'README_scripts' file contains information
regarding each individual script.


=============================
sib4plot
=============================
This folder contains IDL code for an visualization
tool.  SiB4Plot can open and plot all SiB4 output,
as well as driver data and generic netcdf files.
In addition to plotting, it also has some basic
analysis tools and is designed to be useful 
in quickly looking at and evaluating simulations
as well as data.

To run sib4plot, just make sure the sib4plot directory
is added to your IDL directory list, then in IDL:
>sib4plot
will bring up the interface.

More information is found in the README_sib4plot file.


=============================
tools_data
=============================
This folder contains tools to process SiB4-relevant
evaluation data, including FLUXNET data and MODIS LAI.
More information is found in the README_data file.


=============================
tools_eval
=============================
This folder contains programs to pull out carbon fluxes
and stocks at specific sites from SiB4 simulations and
and compare them to Fluxnet and MODIS (LAI) data.
More information is found in the README_eval file.


=============================
tools_gfed
=============================
This folder contains programs to sample GFED fire emissions
at the SiB4 sites (global land points) and to calculate total
fire emissions.  More information is in the README_gfed file.


=============================
tools_merra
=============================
This folder contains tools to process MERRA driver data.
More information is found in the README_merra file.


=============================
tools_misc
=============================
This folder contains miscellaneous tools in IDL.
There is a program to create a map of site locations,
and there are routines to plot the potential and scaling
functions used in SiB4.
More information is in the README_misc file.


=============================
tools_post
=============================
This folder contains tools useful in the post-processing
of SiB4 output.

These routines are designed to be generic.
While I developed them in processing SiB4 output,
I find them useful for a variety of netcdf data.

To use the routines, they can be run on their own,
but they are designed to be used using a common
wrapper: sib4post.  A Makefile contains the library 
locations, thus to compile you can just type:
>make sib4post

Then to run any routine, you edit the routine's
text file and then type:
>sib4post routine

More information is found in the README_routines and
README_procsteps files.


=============================
tools_pre
=============================
This folder contains tools useful in setting
up SiB4 simulations and the required input data.

These routines are fortran programs that create
the input driver data and input vegetation information.

More information is found in the README_pre file.


=============================
tools_sinks
=============================
This folder contains an R program to create carbon
sinks by creating scaling factors for assimilation
and respiration.  The amount of sink created can be
changed and is based on carbon fertilization, nitrogen
fertilization, boreal warming, and forest regrowth.
More information is in the README_sinks file.


====================================================================
The Google Drive link for all data, drivers, and output is:
https://bit.ly/2sq5adF

sib4_corral:
Snapshot of the code used to generate output.


sib4_data:
The global folder has tarballs of the crop redistribution data, 
sample grid lat/lons for regridding, and land cover maps.
The sites folder has data used for site evaluations, including
MODIS LAI (lai.tgz), FLUXNET (fluxnet.tgz), text files with 
text files with site-specific pools (pool.tar), 
text files with annual, monthly, and daily pools/fluxes (grn.tar), and
text files with PFT-estimated, consistent annual pools/fluxes (carbon.tar).


sib4_driver:
In this folder are tarballs with input data for both global
and site simulations.  
The global.tar file includes the
global PFT/vegetation file (sib_vs.nc) and spun-up pools (sib_requib.nc).
The merra.tar file includes MERRA data at 0.5-degrees for SiB4 vector
points only from 1979-2017.
The merra2.tar file includes MERRA2 data at 0.5-degrees for SiB4 vector
points only from 1980-2017.
The merra2clim.tar files includes mean weather data from MERRA2, with
weather conditions averaged from the original hourly MERRA2 data from
1998-2017.


sib4_output:
In this folder are tarballs of global output from 1998-2017.  
The output is available in vector format, or 
gridded at 0.5x05 or 1x1.


#------------------------------
#Updated: 2019/08
#Questions or comments? 
#Contact:  katherine.haynes@colostate.edu