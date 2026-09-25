# Political Violence in Bangladesh, January–June 2026

Reproducible quantitative analysis of political violence around Bangladesh’s February 2026 national election, using Human Rights Support Society (HRSS) data.

## Key findings
- **830** incidents, **56** deaths, **5,246** injuries in H1 2026
- Incidents fell sharply after the election (346 → 58), but death rates spiked in March and June
- Two-thirds of recorded violence was either intra-BNP or BNP–Jamaat-e-Islami

## Repository contents
| Path | Description |
|------|-------------|
| `analysis.R` | Full reproducible R script |
| `data/` | Verified monthly series and summary tables (CSV) |
| `figures/` | Publication-ready charts (PNG) |
| `brief/` | Policy brief (PDF) |

## How to reproduce
```r
dir.create("output", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
source("analysis.R")
