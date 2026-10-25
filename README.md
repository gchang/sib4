# Welcome to the Simple Biosphere Model (SiB4) Version 4.2!

## Model Description
SiB4 is a mechanistic, prognostic land surface model that integrates heterogeneous land cover, environmentally responsive prognostic phenology, dynamic carbon allocation, and cascading carbon pools from live biomass to surface litter to soil organic matter.  By combining biogeochemical, biophysical, and phenological processes, SiB4 predicts vegetation and soil moisture states, land surface energy and water budgets, and the terrestrial carbon cycle.  Rather than relying on satellite data, SiB4 fully simulates the terrestrial carbon cycle by using the carbon fluxes to determine the above and belowground biomass, which in turn feeds back to impact carbon assimilation and respiration.

Every timestep (currently 10-minutes), SiB4 computes the terrestrial albedo, radiation budget, hydrological cycle, layered temperatures, and soil moisture, as well as the resulting energy exchanges, moisture fluxes, carbon fluxes, and carbon pool transfers.  Photosynthesis depends directly on environmental factors (humidity, moisture, and temperature) and aboveground biomass; and carbon uptake is determined using enzyme kinetics and stomatal physiology.  Carbon release occurs from autotrophic and heterotrophic respiration.  Biomass growth and maintenance contribute to autotrophic respiration, and heterotrophic respiration depends on moisture, temperature, and the amount of dead plant material in the surface and soil carbon pools.
    
To calculate the carbon pools, the net assimilated carbon is allocated to the live pools.  The phenology stage, combined with temperature and moisture environmental adjustments, dictates the fraction of carbon allocated to each live carbon pool.  Carbon is transferred between the pools using totals of sub-hourly (timestep) amounts that vary with assimilation rate, day length, moisture, temperature, and pool size.  Once the pools are updated, the land surface state and relative properties are revised; and the new values are used for assimilation and respiration, completing the carbon cycle and providing self-consistent predicted vegetation states, soil hydrology, carbon pools, and land-atmosphere exchanges.

SiB4 has been evaluated around the globe using a variety of metrics, including carbon and energy fluxes from the Fluxnet network, satellite solar-induced fluorescence (SIF) and soil moisture, and both remotely-sensed and site-level leaf area index (LAI) and biomass.  SiB4 is found to improve model predictions over grasslands, and the start of the growing season is well-captured across all vegetation types.  In these studies, SiB4 is shown to have too long of a growing season compared to observations, with senescence being delayed by several weeks in some locations.  In response to these findings, new respiration and transfer methods described in this document are expected to improve senescence predictions.

For more information on SiB4, please see:  
[SiB4 Model Description](https://doi.org/10.1029/2018MS001540)  
[SiB4 Grassland Evaluation](https://doi.org/10.1029/2018MS001541)  
[SiB4 Technical Documentation](https://hdl.handle.net/10217/200691)

## Model Output
Global hourly model output for 2000-2018 is being archived on the ORNL DAAC.  Please check back soon for updates on this process!  In the meantime, this output is on Google Drive and is happily available upon request.  
<br>
Global 0.5-Degree Hourly Output:
- Carbon Fluxes (gross primary production and ecosystem respiration)
- Carbonyl Sulfide (COS) Fluxes (vegetation assimilation, canopy air space flux, uptake by soil)
- Solar-Induced Fluorescence (SIF)
- Photosynthetically Active Radiation (PAR)
- Leaf-Atmosphere Conductances (canopy air space CO2 partial pressure, leaf internal CO2 partial pressure, canopy conductance of water)  

Global 0.5-Degree Daily and Monthly Output:
- Carbon Fluxes (gross primary production and ecosystem respiration)
- Carbon Pools (above and belowground)
- Carbonyl Sulfide (COS) Fluxes (vegetation assimilation, canopy air space flux, uptake by soil)
- Energy Exchanges (latent and sensible heat fluxes)
- Leaf Area Index (LAI)
- Photosynthetically Active Radiation Absorbed Fraction (FPAR)
- Solar-Induced Fluorescence (SIF)

## Repository Details
The SiB4v2 repository includes:
- code (in Fortran)
- info (includes informational slides and instructions to run SiB4)
- input (input files needed by SiB4)
- scripts (useful for compiling, spinning up, running, and processing SiB4)
- sib4plot (custom interactive IDL visualization tool)
- tools_data (tools to process SiB4-relevant evaluation data)
- tools_eval (programs to pull out carbon fluxes/stocks and compare to data)
- tools_gfed (programs to sample GFED fire emissions at the SiB4 sites)
- tools_merra (tools to process MERRA driver data)
- tools_misc (tools in IDL to plot the functions used in SiB4)
- tools_post (tools useful in the post-processing of output, designed to be generic to netCDF)
- tools_pre (tools useful in setting up SiB4 simulations and the required input data)
- tools_sinks (R program to create carbon sinks)

Futher information regarding each of these folders is provided in the
README.txt file.

## Updates
SiB4v2 includes the following updates:
- New seasonal and climatological variables for
  phenological and respiration processes calculated
  using running means
- New leaf transfer processes for senescence
- New respiration processes, and parameters
     to select PFT-appropriate behavior
- New inclusion of fire emissions (GFED), with
     burned carbon removal from carbon pools
- Updated crop growing-degree-day phenology
- Modified grazing scheme
- Fixed steady-state (equilibrium) carbon pools
- Carbon balance checked daily


## References
If you use SiB4, we kindly ask that you cite our papers as appropriate.

Haynes, K.D., Baker, I.T., Denning, A.S., Stöckli, R., Schaefer, K., Lokupitiya, E.Y., \& Haynes, J.M. (2019).  Representing grasslands using dynamic prognostic phenology based on biological growth stages: 1. Implementation in the Simple Biosphere Model (SiB4).  *Journal of Advances in Modeling Earth Systems*, *11*, 
https://doi.org/10.1029/2018MS001540.

Haynes, K.D., Baker, I.T., Denning, A.S., Wolf, S., Wohlfahrt, G. Kiely, G., Minaya, R.C., \& Haynes, J.M. (2019).  Representing grasslands using dynamic prognostic phenology based on biological growth stages: 2. Carbon cycling.  *Journal of Advances in Modeling Earth Systems*, *11*, https://doi.org/10.1029/2018MS001541.

Haynes, K.D., Baker, I.T., \& Denning, A.S. (2020).  Simple Biosphere Model version 4.2 (SiB4) technical description.  Mountain Scholar, Colorado State University, Fort Collins, CO, USA, https://hdl.handle.net/10217/200691.


## Funding
SiB4 was developed with NASA funding from the following projects:
- Carbon Cycle Science: NNX14A152G
- Carbon Monitoring System: NNX12AP86G
- Science Team for the OCO-2 Missions: NNX15AG93G
- Terrestrial Ecology: NNX11AB87G


## Contact
Katherine Haynes  
katherine.haynes@colostate.edu
