# mpa-oil-slicks

Which marine protected areas are exposed to oil, and does the level of
protection make a difference?

Satellite-detected oil slicks from SkyTruth's
[Cerulean](https://cerulean.skytruth.org) (Sentinel-1 radar + machine learning,
2023-2025) are intersected with every marine protected area in the World
Database on Protected Areas. The report names the most exposed MPAs and compares
oil density inside each protection class with the unprotected ocean around it.

## Contents

| Path | What it is |
| --- | --- |
| `index.qmd` | The report: most exposed MPAs, largest totals, map, protection-class comparison, full searchable table, caveats. |
| `prep/build_data.R` | Pipeline: downloads slicks from the Cerulean API and MPAs from the WDPA, computes per-MPA exposure and the class comparison, writes `data/`. |
| `linkedin_figures.R` | Renders the two LinkedIn images to `outputs/` and prints the numbers for the post text. |
| `R/plots.R` | Plot functions shared by the report and the LinkedIn script, so both stay identical. |
| `template/` | CSS, header and footer for the report's HTML output. |
| `data/` | Small report-ready tables written by the pipeline (see below). |

## Running it

```r
install.packages(c("httr2", "sf", "terra", "dplyr", "tidyr", "purrr", "readr",
                   "wdpar", "chromote", "rnaturalearth", "rnaturalearthdata",
                   "countrycode", "data.table", "ggplot2", "scales", "reactable"))
```

```bash
Rscript prep/build_data.R      # first run 30-60 min; caches in raw/ (gitignored)
Rscript linkedin_figures.R     # outputs/fig_most_exposed_mpas.png, fig_protection_classes.png
quarto render index.qmd        # index.html, for GitHub Pages
```

`wdpar::wdpa_fetch()` drives a headless Chrome to download the global WDPA
(~1-2 GB). If that fails, download it manually from
[protectedplanet.net](https://www.protectedplanet.net/en/thematic-areas/wdpa)
and set `WDPA_PATH` at the top of `prep/build_data.R`.

The class comparison rasterises the global ocean on a 5 km equal-area grid. If
R runs out of memory, lower `terraOptions(memfrac = ...)` or set `RES_KM <- 10`.

## Data

`data/mpa_exposure.rds`, one row per MPA:

```
WDPAID, NAME, DESIG_ENG, country, ISO3, IUCN_CAT, prot_class, class_label,
STATUS_YR, marine_km2, n_slicks, n_months, oil_km2, oil_per_1000km2_yr, lon, lat
```

`oil_km2` is the slick area clipped to the MPA boundary (equal-area CRS), so a
slick straddling the boundary only counts for the part inside.

`data/class_summary.rds`, one row per protection class plus "All MPAs": oil
density inside vs within 25 km outside, their ratio and 95% bootstrap CIs.

`data/slick_points.rds`, one point per slick (for the map).
`data/run_info.rds`, parameters and counts used in the report text.

### Protection classes

From the WDPA's IUCN management category:

| Class | IUCN categories |
| --- | --- |
| No-take | Ia, Ib, II, III |
| Multiple-use | IV, V, VI |
| Category not reported | Not Reported, Not Assigned, Not Applicable |

Where MPAs overlap, a location takes the strongest class covering it.

## Method in brief

**Slicks.** Cerulean's `public.slick_plus` collection via its OGC API, filtered
to `max_source_collated_score > 0`, the threshold SkyTruth recommends as
credible oil (a slick linked to a nearby vessel or offshore platform).

**Per-MPA exposure.** Slick polygons intersected with MPA polygons; oil area per
1,000 km² of MPA per year. The ranking is restricted to MPAs of at least 100 km²
with at least three slicks, so a single slick in a tiny reserve cannot top it.

**Protection classes.** Oil density inside each class is compared with the
unprotected ocean within 25 km. Neighbouring waters share the same Sentinel-1
revisit pattern and shipping context, so the ratio controls for the very uneven
satellite coverage without needing scene footprints. Confidence intervals come
from 2,000 bootstrap resamples of months.

## What this data does not cover

**Detection is not measurement.** Slicks are only seen where and when Sentinel-1
images the sea. Per-MPA figures are detected exposure; open-ocean and some
high-latitude MPAs are under-sampled. The class comparison controls for this;
the per-MPA ranking does not.

**Source-matched slicks only.** The filter removes most false positives but also
slicks with no identifiable source, biasing the sample towards shipping lanes and
oil fields.

**IUCN category is management intent, not enforcement,** and many MPAs report
none.

**Surface area, not volume.** A thin sheen and a thick spill of the same extent
count the same.

**Proximity is not attribution.** A slick detected inside an MPA was not
necessarily released there.

## Attribution

Oil slick data: SkyTruth Cerulean, derived from Copernicus Sentinel-1 imagery.
Protected areas: UNEP-WCMC and IUCN, World Database on Protected Areas
(protectedplanet.net). WDPA terms of use apply: do not redistribute the raw
WDPA; the derived tables in `data/` are summaries.
Coastlines: Natural Earth via `rnaturalearth`.
