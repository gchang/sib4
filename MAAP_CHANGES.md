# Changes from upstream

Upstream: https://gitlab.com/kdhaynes/sib4v2_corral, branch `master` at commit 4f231c4c2439645e2a11c22a9ddda6444384cdc2 (2024-05-02, the merge of the COS branch). SiB4 is by Katherine Haynes, Ian Baker, Scott Denning and colleagues at Colorado State University, with the carbonyl sulfide (COS) updates by Ara Cho and Linda Kooijmans. BSD-3-Clause License (`LICENSE`, Copyright 2020 Colorado State University), kept unchanged.
Upstream's tags are `v4.2` (dad342b) and `v4.2.COS` (1e29b25, 2021-07-22); this commit is 9 commits newer than `v4.2.COS` and includes the 2022 to 2024 COS changes.

This copy starts from upstream's file tree at 4f231c4 without its git history. Nothing was left out (upstream has no `.github/` folder). It is unchanged except for the items below. The Fortran code in `code/`, the parameter files in `input/` and the sample's driver data are untouched, so results do not change.

## 2026-10-25: sample site namelist has the COS switches

The first-run example in `info/README_start` (`./SiB4D info/sites/sib4_sample/namel_sibdrv`, the US-FCl site near Fort Collins, 2016) stopped right away with

```
At line 77 of file read_namel.F90 (unit = 2, file = 'info/sites/sib4_sample/namel_sibdrv')
Fortran runtime error: End of file
```

The namelist inside `info/sites/sample.tgz` dates from 2020-02-18. The COS merge added `varcos_switch`, `soilogee_switch`, `varco2_switch` and `ftcos_switch` to `$SWITCH_LIST_SIBDRV` and a new `$VALUE_FTCOS` group, which `read_namel.F90` always reads. The sample namelist now has the four switches and the group, with the values from the current template `info/sites/namel_sibdrv` (`varcos_switch`, `soilogee_switch` and `varco2_switch` false, `ftcos_switch` true, `ha = 40000.`, `heq = 100000.`, `teq = 303.`), plus the template's descriptions of them. No other line of the namelist and no other file in the tarball changed (the other 414 members keep their contents, owners and dates). The model code is untouched.

## 2026-10-25: Makefile links plain LAPACK and NetCDF; LALIB and NCLIB can be set

On Linux, `code/Makefile` always linked ATLAS (`-llapack -lcblas -latlas -lf77blas`) and the HDF5 Fortran libraries under their old names (`-lhdf5hl_fortran -lhdf5_fortran ...`), so `make` failed at the link step on machines set up differently from ours, even with `NETCDF_ROOT` and `LAPACK_ROOT` set. SiB4 only calls LAPACK's `dgesv` (`sibslv.F90`) and the NetCDF Fortran interface, which brings in the HDF5 it needs.

The defaults are now `-llapack -lblas` and `-lnetcdff -lnetcdf`, and the library folders are recorded in `SiB4D` (rpath), so it runs without setting `LD_LIBRARY_PATH`. To link something else, set `LALIB` or `NCLIB` on the command line, for example `make LALIB="-L/usr/local/atlas/lib -llapack -lcblas -latlas -lf77blas"`. macOS still uses Accelerate. The HDF5 include folder is added only when `H5ROOT` is set. Compiler flags are unchanged. `README.txt` shows the command for libraries in a conda environment:

```sh
make NETCDF_ROOT=$CONDA_PREFIX LAPACK_ROOT=$CONDA_PREFIX
```

## 2026-10-25: module dependencies in the Makefile, so `make -j` works

The module objects had an empty rule, so a parallel build could compile a module before `kinds.F90` or `module_sibconst.F90` and stop with `Cannot open module file 'kinds.mod'` (or `'module_sibconst.mod'`). The Makefile now says that every module needs `kinds.o`, and `module_io.o` and `module_local.o` also need `module_sibconst.o`; all other objects already waited for the modules. `README.txt` mentions `-j`. A serial build is the same as before.

## Check

On 2026-10-25, in a new conda environment (conda-forge: gfortran 16.2.0, make 4.4.1, netcdf-fortran 4.6.4, libnetcdf 4.10.1, liblapack and libblas 3.11.0 from OpenBLAS 0.3.34), from a fresh clone of this repository:

```sh
cd info/sites && tar -xzf sample.tgz && cd ../../code
make -j NETCDF_ROOT=$CONDA_PREFIX LAPACK_ROOT=$CONDA_PREFIX
mv SiB4D .. && cd ..
./SiB4D info/sites/sib4_sample/namel_sibdrv
```

The build finished with no warnings in 18 s (350 MB peak memory). The run ended with "End Simulation" (exit 0) in 14 s with 30 MB peak memory and no network, and wrote 73 files (142 MB) to `info/sites/sib4_sample/output/`. As a reference, upstream 4f231c4 was built serially with the library list given on the `make` command line and run with the same namelist entries added by hand: the two `SiB4D` executables are byte-identical, all 73 output files are byte-identical, and the logs differ only in the namelist name and the timings. Upstream's Makefile with `make -j16` still stops with `Cannot open module file 'module_sibconst.mod'`.

For a rough sense of the values, the sample's monthly output was compared with the ORNL DAAC product "SiB4 Modeled Global 0.5-Degree Monthly Carbon Fluxes and Pools, 2000-2018" (`sib4_0.5x0.5_monthly_2016.nc4`) at the 0.5-degree cell holding the site: GPP 375 vs 515 g C/m2/yr (monthly r = 0.994), ecosystem respiration 422 vs 533 (r = 0.996), LAI 0.65 vs 0.72 (r = 0.974). The seasonal cycles agree; the totals differ because the sample uses a single-point MERRA extraction, fixed CO2 (`varco2_switch` false) and a slightly different land cover from the global run.

Versions: SiB4 has no version string in the code; this commit is tagged `2026.10.25`.
