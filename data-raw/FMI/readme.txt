README FOR RECONSTRUCTED SPECTRA, HELSINKI 21-22 AUG 2014

In this package, I provide reconstructed spectral irradiances for
Helsinki, Kumpula. The spectra are calculated using my method
(Lindfors et al., 2009) and input in the form of:
(1) hourly global radiation, Kumpula measurements
(2) daily water vapor column, Kumpula AERONET data
(3) daily total ozone column, OMI satellite (Hki overpass, OMTO3)
(4) albedo assumed constant, low, representative of snow-free conditions

The spectra are available as hourly files (ex: 2014-08-21.hel)
and as daily accumulated (ex: 2014-08-21_cum.hel).  

The unit of the hourly spectral irradiances is mW/nm/m2.
Daily accumulated irradiances are given as J/m2/nm.

In the hourly files, each spectrum starts with a header line
(e.g., "# 20140821 3:30:00 3:30:00 85.951") which gives the
date (YYYYMMDD) and time in UTC (here 3:30:00) and SZA
(here 85.95 degrees) of the spectrum.

The timing of the simulated spectra is dictated by the global
radiation data that I used as input. The UTC time stamp given
in the header denotes the middle of the hour for which the
spectrum is representative.

The figure (reco_meas_cmp_01.png) shows a quick comparison
between the erythemally weighted simulated spectra and the
UV-index (also erythemal) measured by our Solar Light 501
in Kumpula. The agreement seems very satisfactory.

