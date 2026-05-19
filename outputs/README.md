# Outputs

This directory is the target for regenerated Python reproduction artifacts.

- `cross_section/`: cross-sectional data-preparation summaries from Phase 2 and
  cross-sectional tables and figures from Phase 3.
- `panel/`: panel-data tables and figures from Phase 4.
- `logs/`: local run logs from orchestration commands.
- `intermediate/`: optional regenerated data caches that improve inspection but
  are not authoritative inputs.

The authoritative source inputs remain the tracked files under `cross-section/`
and `panel/`. Generated final tables and figures should remain trackable when
they are part of the submitted reproduction.

Current cross-sectional Phase 2 artifacts:

- `cross_section/cross_section_data_inventory.csv`: inventory of tracked
  cross-sectional source and derived `.rds`/`.dta` inputs.
- `cross_section/cross_section_preparation_summary.csv`: row counts, complete
  preferred-model samples, and outlier-filter counts for `model_data4` and
  `model_data4_o`.
