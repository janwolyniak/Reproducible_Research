# Contribution Evidence

The course outline requires visible GitHub history with well-described
contributions from all team members. This document summarizes the contribution
split claimed in `README.md` and the local Git evidence used to verify it.

Verify the current history with:

```bash
git log --format='%h %an <%ae> %s' --max-count=40
```

## Contribution Summary

| Contributor | Primary responsibility | Evidence in recent history |
| --- | --- | --- |
| Jan Wolyniak | Python project structure, validation, orchestration, Docker, final integration, reviewer documentation. | `Add inventory data script and update documentation`; `Enhance project validation to include checks for generated output files`; `Implement Docker support for reproducibility, including Dockerfile, docker-compose, and .dockerignore; update README and run instructions with Docker commands and troubleshooting.`; `Add user-facing CLI options for reproducibility scripts and enhance output handling`. |
| Kinga Kucharska | Cross-sectional data preparation, cross-sectional model reproduction, and cross-sectional output cleanup. | `Add files from BA thesis`; `Add cross-sectional data preparation`; `feat: update dependencies and enhance cross-section analysis`; `Refactor output paths and update GDP column references`. |
| Iwo Wiszejko | Panel data preparation, panel model reproduction, robust covariance outputs, and panel result documentation. | `Add Phase 0 source audit script and generated report`; `Add panel data preparation for Phase 2`; `Add Phase 4 panel reproduction (models, tests, robust SE, alt-FE)`; `Add Phase 5 panel reproduction results`. |

## Recent Commit Evidence

| Commit | Author | Message |
| --- | --- | --- |
| `3cc34be` | Jan Wolyniak | Add user-facing CLI options for reproducibility scripts and enhance output handling |
| `d891b91` | Jan Wolyniak | Update TODO.md to mark Docker Hub image build and push as complete; document successful push details. |
| `3abc1ac` | Jan Wolyniak | Implement Docker support for reproducibility, including Dockerfile, docker-compose, and .dockerignore; update README and run instructions with Docker commands and troubleshooting. |
| `7c5b46c` | Jan Wolyniak | Enhance project validation to include checks for generated output files |
| `b88d588` | Kinga Kucharska | Refactor output paths and update GDP column references |
| `fb531e2` | Iwo Wiszejko | Add Phase 5 panel reproduction results |
| `cc69711` | Jan Wolyniak | Enhance panel analysis with first-difference handling and diagnostic tests |
| `3fdb9f5` | Iwo Wiszejko | Add Phase 4 panel reproduction (models, tests, robust SE, alt-FE) |
| `53d9d51` | Jan Wolyniak | Enhance cross-section analysis and diagnostics |
| `85674ab` | Kinga Kucharska | feat: update dependencies and enhance cross-section analysis |
| `bc097c4` | Kinga Kucharska | Add cross-sectional data preparation |
| `0ff20c3` | Iwo Wiszejko | Add panel data preparation for Phase 2 |

## Review Notes

- The visible history includes commits authored by all three contributors.
- The commit messages are descriptive enough to identify the main artifact or
  workflow being changed.
- Jan's commits cover integration and reproducibility infrastructure, Kinga's
  commits cover cross-sectional work, and Iwo's commits cover panel work.
- Final review should confirm that the public GitHub repository still exposes
  these commits and author identities after any rebases or squashed merges.
