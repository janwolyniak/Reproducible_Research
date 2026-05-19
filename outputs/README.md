# Outputs

This directory is the target for regenerated Python reproduction artifacts.

- `cross_section/`: cross-sectional tables and figures from Phase 3.
- `panel/`: panel-data tables and figures from Phase 4.
- `logs/`: local run logs from orchestration commands.
- `intermediate/`: optional regenerated data caches that improve inspection but
  are not authoritative inputs.

The authoritative source inputs remain the tracked files under `cross-section/`
and `panel/`. Generated final tables and figures should remain trackable when
they are part of the submitted reproduction.
