# OIG audit quality corrections implementation plan

Goal: correct six identified reporting, estimation, and verification issues.
Architecture: extend the existing package functions; retain existing APIs where
practical and make changes in statistical interpretation explicit.
Tech stack: R, checkmate, base aggregation, survey, tibble, testthat.
Spec: user's authorization to address all six issues listed in this conversation.

## Constraints

- No CLI calls; repository changes and CI use the connected GitHub API.
- No new dependencies or assumed independent truth from database agreement.
- Preserve the original sampling roster, source provenance, and unknown outcomes.
- Source counts mean present records; field completeness excludes unavailable
  fields. Missing records and unavailable fields remain separately reportable.
- Existing pending PR #23 is separate; do not merge or overwrite its changes.

## Tasks

1. Reporting: R/oig_audit_prose.R and corresponding tests/documentation.
   Use present-record counts, eligible completeness denominators, and explicit
   unavailable-field text. Reject duplicate or impossible reporting inputs.
2. Estimation: R/validity_analysis.R and corresponding tests/documentation.
   Add weighted sensitivity bounds and explicit attempted/all-sampled scope.
   Add survey proportion confidence intervals with a legacy method available.
   Return defined empty/boundary behavior without false precision.
3. Integrity: R/analysis_populations.R plus a new verification-to-validity adapter.
   Validate normalized keys before joins. Preserve one field-level outcome per
   declared sampled target, retain uncertainty, and carry item-level evidence
   and original design columns through to existing validity functions.
4. Integration: combine the disjoint changes into this branch, resolve shared
   documentation/exports/release notes, and run the full three-job CI matrix.

## Verification

- Each implementation branch first adds targeted regression tests and confirms
  failures against current code in CI.
- Fixes must pass existing and new tests without weakening scientific assertions.
- Check weighted examples against hand calculations and survey reference calls.
- Review duplicates after trimming, all-empty/unknown outcomes, unavailable
  fields, incomplete questionnaires, and dependent-source evidence.
- Review the combined patch independently; verify the exact final commit's CI.
