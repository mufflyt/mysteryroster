# mysteryroster

Registry overlap strata and listing-validity analysis for mystery-caller
audits of physician directories.

The analytic unit is one physician-location listing. The package carries the
specialty-agnostic core of the ENT registry-validity study (Falk et al.) so the
OB/GYN subspecialty study, and any later one, runs the same steps with its own
registries and disposition codebook:

| Protocol step | Function |
|---|---|
| Cross-link registries by NPI into Euler zones | `registry_overlap()`, `overlap_zone_counts()`, `plot_registry_overlap()` |
| Restrict to the frame registry; strata A to D | `frame_strata()` |
| Stratified sample with preserved probabilities | `stratified_listing_sample()` |
| Dispositions to four exhaustive statuses | `classify_listing_status()`, `ent_disposition_map()` |
| Design-weighted validity, by stratum | `listing_validity_design()`, `validity_estimates()` |
| Extreme-case bounds for unresolved listings | `validity_sensitivity_bounds()` |
| Survey-weighted logistic regression | `validity_model()` |
| Study-flow counts with closure checks | `listing_flow_counts()` |

Every count-producing function asserts that its categories partition the
input; a disposition outside the codebook stops rather than becoming
"unresolved" by default.

```r
remotes::install_github("mufflyt/mysteryroster")
vignette("registry-validity-workflow", package = "mysteryroster")
```

Sibling packages: `mysterynpi` (name keys and NPI linkage), `mysterycall`
(appointment-access outcomes and study tooling), `mysterymaps` (maps).
