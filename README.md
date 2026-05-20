# Reproducible_Research
project for the Reproducible Research classes, Quantitative Finance

Authors:
- [Jan Wołyniak](https://github.com/janwolyniak)
- [Kinga Kucharska](https://github.com/kinga-kucharska)
- [Iwo Wiszejko](https://github.com/IWiszejko)

## Current Python Baseline

Install the constrained dependencies:

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

Validate the tracked inputs and expected output directories:

```bash
python scripts/validate_project.py
```

Run the current reproducibility pipeline:

```bash
python scripts/run_all.py
```

`run_all.py` validates the repository, regenerates the Phase 0 audit,
regenerates the Phase 2 data inventory, rebuilds cross-sectional and panel
tables/figures, refreshes generated reproduction notes, checks the expected
output set, and writes a local run log. Detailed commands are in
`docs/run_instructions.md`.

For faster table-only checks, skip plot regeneration:

```bash
python scripts/run_all.py --skip-plots
```

Use `--strict-validation` with `--skip-plots` only when the complete output
manifest, including existing PNG figures, should still be required.

Validate that all regenerated outputs are present:

```bash
python scripts/validate_project.py --generated-outputs
```

Regenerate only the data inventory and shared data dictionary:

```bash
python scripts/inventory_data.py
```

Regenerate component outputs into custom directories when reviewing isolated
changes:

```bash
python scripts/run_cross_section.py --output-dir outputs/review/cross_section --docs-dir outputs/review/docs --skip-plots
python scripts/run_panel.py --output-dir outputs/review/panel --docs-dir outputs/review/docs --skip-plots
```

## Docker Reproduction

Build the reviewer image from a clean checkout:

```bash
docker build -t janwolyniak/reproducible-research-lic-fii:phase6 .
```

Run the complete reproduction pipeline in the container:

```bash
docker run --rm janwolyniak/reproducible-research-lic-fii:phase6
```

The default container command is `python scripts/run_all.py`. It writes the same
regenerated files under `/app/docs` and `/app/outputs` inside the container.
To keep regenerated outputs on the host checkout, run the compose service:

```bash
docker compose run --rm reproduction
```

The compose service bind-mounts the repository at `/app`, so refreshed files are
written back to `docs/`, `outputs/cross_section/`, `outputs/panel/`, and the
ignored scratch directories under `outputs/intermediate/` and `outputs/logs/`.

Docker Hub image name and tag:

```text
janwolyniak/reproducible-research-lic-fii:phase6
```

If the image has been pushed, reviewers can run it directly:

```bash
docker pull janwolyniak/reproducible-research-lic-fii:phase6
docker run --rm janwolyniak/reproducible-research-lic-fii:phase6
```

Troubleshooting:

- If Docker cannot connect to the daemon, start Docker Desktop or the local
  Docker service and rerun the command.
- If dependency installation fails during `docker build`, rebuild with
  `--no-cache` to avoid a stale layer:
  `docker build --no-cache -t janwolyniak/reproducible-research-lic-fii:phase6 .`.
- If a host-mounted compose run leaves root-owned generated files on Linux, run
  the plain `docker run --rm ...` command or adjust file ownership after the
  run.
