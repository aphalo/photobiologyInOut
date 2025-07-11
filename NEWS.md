---
editor_options: 
  markdown: 
    wrap: 72
---

## Roadmap

Add support for StellarNet, Brewer, and Bentham spectrometers (examples
of data files are welcome for these and other instruments).

## photobiologyInOut 0.4.32

- Add function `read_oo_ovirrad()` for importing spectral energy irradiance data
acquired with Ocean Optics' OceanView software and save into text files.

## photobiologyInOut 0.4.31

- Fix bug in `read_oo_jazirrad()`: wrong column read as irradiance! (reported 
by Neha Rai).

## photobiologyInOut 0.4.30

- Update function `read_CIE_csv()` to also support reading of trichromatic 
 coordinates into `chroma_spct` objects.
- Depend on 'SunCalcMeeus' as functions were migrated from 'photobiology' in
version 0.12.0.

## photobiologyInOut 0.4.29

- Update function `qtuv_m_s.e.irrad()` adding support for collections of 
simulated spectral irradiance for multiple altitudes.
- Update time zone names, replacing "EET" by "Europe/Helsinki", and "CET" by 
"Europe/Zurich" in examples and unit tests. The old short abbreviations are no 
longer supported in upcoming R (>= 4.5.0).

## photobiologyInOut 0.4.28

- Track change in package 'colorSpec' (1.5-0) to avoid an error by triggering
a warning, instead. Affects functions `spct_CRI()`, `spct_CCT()`, and
`spct_SSI()`.
- **Code breaking** Update `read_qtuv_txt()` not to add by default 
variables `angle` and `date` to the returned `source_spct` object. Support
addition on user request of variables `sun.elevation`, `zenith.angle`,
`time`, and `ozone.du`, and for backwards compatibility, but deprecated, `date` 
and `angle`, individually and in any combination. 
- Update `read_qtuv_txt()` to handle text/HTML files from Quick TUV saved with 
embedded _new line escapes_ (`"\n") instead of actual new lines. This seems to 
be a change in the output from the on-line Quick TUV calculator and/or in how 
web browsers handle it.
- Add function `qtuv_s.e.irrad()` to directly obtain simulated spectral
irradiance from the Quick TUV calculator.
- Add function `qtuv_m_s.e.irrad()` to directly obtain collections of simulated 
spectral irradiance for multiple values of arguments for input parameters.
- Add function `read_CIE_csv()` to read the CSV and JSON files from CIE, 
recently published at the [CIE Datasets page](https://cie.co.at/data-tables).

## photobiologyInOut 0.4.27

- Add functions `mspct2fdata()` and `spct2fdata()` to export spectra as `fdata`
objects for use with package 'fda.usc', and functions `fdata2spct()` and 
`fdata2mspct()` to import the 'fdata' objects with functional data analysis
results back into objects of one of the classes from package 'photobiology'.
Currently classes `source_spct`, `response_spct`, `filter_spct`, and 
`reflector_spct` and the matching 'mspct' collections are recognized 
automatically in both directions.

## photobiologyInOut 0.4.26

- Add function `read_spectrapen_csv()` supporting PSI's SpectraPen spectrometer.
- Update for code-breaking changes in package 'readr'.

## photobiologyInOut 0.4.25

- Fix bug in `mspct2colorSpec()` which affected also 
`as.colorSpec.generic_mspct()`. Attempts to convert collections containing
spectra with inconsistent wavelength vectors would fail.
- Improve compatibility with package 'colorSpec' (many thanks to Glenn Davis
for his help!).

## photobiologyInOut 0.4.24

-   Replace use of deprecated `tidyr::gather_()`.
-   Add function `read_cid_spectravue_csv()` supporting data import from 
measurements CSV files output by CID Bio-Science's SpectraVue CI-710s leaf 
spectrometer.
-   Add function `read_foreign2mspct()` to apply an import function to a
list of files.
-   Add color-related functions `spct_CRI()`, `spct_CCT()` and `spct_SSI()` 
implemented as wrappers to functions in package 'colorSpec' that accept 
`source_spct` objects as arguments. CRI, CCT and SSI are quantities used to 
describe light used for illumination, photography, cinematography and video.

## photobiologyInOut 0.4.23

-   Add parameters `na` and `...` to `read_csi_data()`.
-   Add function `read_wasatch_csv()` supporting data import from long
    form CSV spectrum files saved by Wasatch's Enlighten program.
    Metadata parsed and added to spectral object. Session and JSON files
    not yet supported.
-   Add function read_li180_txt() supporting data import from files
    saved by the LI-180 handheld array spectrometer from LI-COR.
-   Avoid spurious progress messages when reading files.
-   Track various changes in the tidyverse that deprecated functions
    used in this package.
-   Fix bugs in some imports from 'photobiology'.
-   **Move git repository from Bitbucket to Github.**
-   Set up Github action for CRAN-checks on Windows, OS X and Ubuntu.

## photobiologyInOut 0.4.22-1

-   Fix two bad test cases that passed with 'photobiology' (== 0.10.0)
    but not with 'photobiology' (>= 0.10.1).

## photobiologyInOut 0.4.22

-   Revise `read_fmi2mspct()` adding flexibility to cope with variation
    in date and time formats used in headers of spectra.
-   Update for compatibility with 'tibble' (>= 3.0.0).
-   Update for compatibility with 'photobiology' (>= 0.10.0) which also
    provides compatibility with 'dplyr' (>= 1.0.0).
-   Depends now on 'photobiology' (>= 0.10.0).

## photobiologyInOut 0.4.21-1

-   Package code unchanged.
-   Update test cases for 'photobiology' (>= 0.9.30).
-   Depends now on 'photobiology' (>= 0.9.30).

## photobiologyInOut 0.4.21

-   Track changes in 'tidyr' (= 1.0.0).
-   Move packages 'pavo' and 'hyperSpec' to suggests.

## photobiologyInOut 0.4.20

-   Track changes in 'photobiology' (= 0.9.28) and 'dplyr' (= 0.8.1).
-   Set "how.measured" attribute in file import functions.

## photobiologyInOut 0.4.19

-   Add checks for non-ASCII characters in headers.
-   Track code breaking change in 'readr' (>= 1.3.0).

## photobiologyInOut 0.4.18

-   Track code breaking change in 'readr' (>= 1.2.0).
-   Fix bug in `read_oo_pidata()` that was preventing reading of Ocean
    Optics "Raspberry Pi" Flame files with a sequence header line at the
    top.
-   Add decoding of file header to extract integration time and number
    of scans to `read_oo_pidata()`.
-   Improve decoding of file header to extract instrument settings in
    `read_oo_jazdata()`.

## photobiologyInOut 0.4.17

-   Add new function to read transmittance and reflectance files from
    Ocean Optics' Jaz modular spectrometer.
-   Fix bug in decoding of time and date in functions read_fmi2mspct(),
    read_tuv_usrout() and read_qtuv_txt(). Decoding failed under R 3.4.4
    (oldrel) but worked under R 3.5.1 due to functions from 'lubridate'
    returning POSIXlt values instead of POSIXct values. Now we force
    conversion to POSIXct.

## photobiologyInOut 0.4.16

Revise read_fmi_dat() to extract date from file header and save the file
header to the spectral object as a comment. Add `read_fmi2mspct()` for
reading spectral irradiance. Bug fix: decoding of dates in fmi import
functions for daily data not always returned POSIXct objects triggering
errors in downstream code.

## photobiologyInOut 0.4.15

Add `read_yoctopuce_csv()` to read CSV files from YoctoPuce modules. Add
`read_qtuv_txt()` to read spectral data output files from the on-line
Quick TUV calculator. Add `read_tuv_usrout2mspct()` wrapper of
`read_tuv_usrout()` and `subset2mspct()`. Revise `read_tuv_usrout()`
with new formal parameter ozone.du and fix a bug in the setting of
`"where.measured"` attribute. Revise all read functions so that they
store the header of the imported file as a vector of character strings
to attribute `"file.header"`.

## photobiologyInOut 0.4.14

Implement class coercion methods ("as." methods) to complement earlier
functions, tracking 'photobiology' 0.9.20. Move coercion methods from
and to matrix objects to 'photobiology' 0.9.20. Update to track API
changes in 'colorSpec' 0.7-3. Translate vignette to Rmarkdown.

## photobiologyInOut 0.4.13

Add function `read_FReD_csv()` (read files flower reflectance database).
Add function `read_ASTER_txt()` (read files ASTER materials reflectance
database). Revise function `read_licor_prn()` to accept transmittance
and reflectance spectra in addition to energy and photon irradiance
spectra. Improve handling of `'label'` by using only the base name of
files and appending user supplied label text. Change location of
external data used in vignettes.

## photobiologyInOut 0.4.12

Fix CITATION file. Add function `read_uvspec_disort()`, rename
`read_libradtran_vesa()` to `read_uvspec_disort_vesa()`.

## photobiologyInOut 0.4.11

Edit read_licor_prn() to work around a bug in readr::read_table() that
results in missread data (wrong numbers). Add `read_csi_dat()` to read
data saved from modern Campbell Scientific loggers.

## photobiologyInOut 0.4.10

Make compatible with upcoming version of package 'lubridate' (pull
request by vspinu NA). Add function `read_avaspec_xls()` which can
import spectral data from Excel files exported from Avantes software.
Fix code to avoid partial matches of function parameter names (tz ->
tzone).

## photobiologyInOut 0.4.9

Fix LaTeX error in vignette.

## photobiologyInOut 0.4.8

Replace a couple of NA returned by read functions with empty spectral
objects. Improve handling of strict.range to avoid spurious warnings.

## photobiologyInOut 0.4.7

Add functions `mspct2matrix()` and `matrix2mspct` for conversion of
collections of spectra into R matrices and vice-versa.

## photobiologyInOut 0.4.6

Fix bug introduced when attempting to make the package pass CRAN checks
without triggering a note.

## photobiologyInOut 0.4.5

Fix for compatibility with 'dplyr' (> 0.4.3). Add functions to exchange
data with package 'colorSpec': `colorSpec2spct()`,
`colorSpec2chroma_spct()`, `colorSpec2mspct()`, `spct2colorSpec()`,
`chroma_spct2colorSpec()`, and `mspct2colorSpec()`. Add or revise
functions to exchange data with package 'hyperSpec': `spct2hyperSpec()`,
`mspct2hyperSpec()`, `hyperSpec2spct()` and `hyperSpec2mspct()`. Add
import function for raw counts from Ocean Optics instruments accessed
through server running on a Raspberry Pi board: read_oo_pidata().

## photobiologyInOut 0.4.4

Add 'readr::locale' support to all functions that could make use of it.
Add 'label' parameter to all file import functions. Add function
read_oo_ssdata() for reading SpectraSuite files containing raw counts.
Add function read_libradtran_vesa() for reading libRadtran output file
with multiple solar spectrum simulations (as preprocessed by Vesa's
script). Add metadata as consistently as possible to the objects
returned by all the file import functions. Update dependencies. Edit the
user Guide. WARNING: from version 0.4.4 the time zone (tz) is by default
"UTC" for decoding dates and times in files imported. In most cases you
will need to pass the tz (or the locale) where the file was created as
an argument to the functions!

## photobiologyInOut 0.4.3

Update for compatibility with 'lubridate' (>= 1.5.6) Add function
read_oo_jazdata() to read Jaz files containing raw counts. Add
preliminary versions of functions to exchange data with package
'hyperSpec': `mspct2hyperSpec()` and `hyperSpec2mspct()`. Add
preliminary version of function to import data from package 'pavo':
`rspec2mspct()`.

## photobiologyInOut Ver 0.4.2

Fix bug in read_fmi_cum(). Update dependencies.

## photobiologyInOut Ver 0.4.1

Update vignette. Fix docs.

## photobiologyInOut Ver 0.4.0

This is a major update, not backwards compatible, as names and formal
parameters of all functions have changed. Trimming is no longer
supported (should be done as a separate operation).

The functions for Ocean Optics files now read the number of pixels from
the file header, and hopefully will better adjust to different
instruments and settings.

Functions for reading multiple spectra now return collections of
spectra. (Functions that saved the spectral objects constructed from the
data read from text files on disk back to disk as R data files have been
removed.)

New functions for reading daily spectral exposure data from files
returned by models from FMI have been added.

New function for reading .csv file from Avantes' AvaSpec spectrometers
added.

## photobiologyInOut Ver 0.3.2

Add `"use.hinges"` argument to functions, and default to not adding
hinges when trimming input.

## photobiologyInOut Ver 0.3.1

Remove dependency on data.table.

## photobiologyInOut Ver 0.3.0

Corrected some bugs in `read_licor_files()`.

## photobiologyInOut Ver 0.2.1

Added function to read spectral irradiance files generated by the Jaz
spectrometer from Ocean Optics.

Functions names shortened.

Expanded the User Guide.

## photobiologyInOut Ver 0.2.0

Edited read_licor_prn_files() drastically. The new version is
functionally equivalent but formal arguments have changed. In addition
it is now implemented by calling read_licor_prn_file().

New functions `read_licor_prn_file()`, `read_macam_dta_file()`,
`read_ooss_txt_file()` and `read_tuv_file()` returning `source.spct`
objects.

Function `read_xlsx()` removed as there are other packages available for
Excel file import.

## photobiologyInOut Ver 0.1.3

Added function `read.xlsx()` for reading data from Excel workbook files
saved as .xlsx (zipped XML format).

## photobiologyInOut Ver 0.1.2

Added parameter unit.out and cleaned code.

## photobiologyInOut Ver 0.1.0

First version containing only one function for importing data measured
with a LI-COR LI-1800 spectroradiometer from .PRN text files produced by
the PC1800 MS-DOS program from LI-COR.
