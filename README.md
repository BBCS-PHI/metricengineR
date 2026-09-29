# metricengineR

`metricengineR` is a reusable R package designed to support the automated
processing of metrics, KPIs, and indicators across multiple analytical
projects and domains using a range of data sources.

The package supports Reproducible Analytical Pipeline (RAP) principles by
reducing duplicated analytical and ETL code, standardising common processing
steps, and providing reusable and tested functions for metric calculation,
data extraction, transformation, and data quality assurance.

## Metric-engine workflow

The package supports a standardised processing workflow:

```text
Source systems / APIs / SQL / files
            |
            v
        Extraction
            |
            v
     Standardised data
            |
            v
         Metadata
            |
            v
    Metric calculations
            |
            v
      tidy_output()
            |
            v
     Data quality checks
            |
            v
       SQL output
```

## Metric calculations

`metricengineR` includes reusable functions for calculating a range of metric
value types, including:

- Counts
- Percentages
- Crude rates
- Directly age-standardised rates (DASR)
- Ratios
- Percentage change
- Differences
- Slope Index of Inequality (SII)

Where appropriate, the package builds on established analytical packages,
including `PHEindicatormethods`, `cvdprevent`, and other published R packages.
These functions have been incorporated or adapted to work with the standard
metric-engine data model and automated processing workflow.

## Data extraction

The package also contains functions for extracting indicator data from multiple data sources.

Currently supported sources include:

- Fingertips
- CVDPREVENT
- SQL databases
- Excel files

For example:

```r
data <- metricengineR::get_indicators_from_sql(
  conn = conn,
  schema_name = "BBCS",
  table_name = "Metric_Engine_Input",
  indicator_ids = "All"
)
```

Additional reusable extraction functions can be added as new data sources are
incorporated into metric-engine projects.

## Use across metric-engine projects

The package is designed to provide common analytical functionality across
multiple projects, including:

- Outcomes Framework
- NHS Oversight Framework
- Health Inequalities Statements

The individual projects retain their own project-specific orchestration and
business rules, while `metricengineR` provides the common reusable functions.

This approach allows the same calculation, extraction, transformation, and
data quality logic to be maintained in one place rather than duplicated
across multiple projects.

## What the package supports

Broadly, `metricengineR` supports:

- extraction of metric and indicator data from SQL using a standardised schema
- extraction of publicly available indicator data from APIs
- creation of pooled multi-year data
- standardisation of dates, classifications, and output structures
- calculation of multiple metric value types
- application of value multipliers, such as rates per 1,000 or 100,000
- combination and standardisation of calculated outputs
- reusable data quality checks
- preparation of data for loading back into SQL databases

## Metadata-driven processing

A key component of the metric-engine architecture is the use of metadata as a
control layer for downstream processing.

Metadata can determine characteristics such as:

- whether an indicator requires calculation or is already precalculated
- the value type required for an indicator
- the value multiplier to apply, for example a rate per 1,000 or 100,000
- the population type required, such as registered population, NHS Spine, or
  Census population
- the required age definition
- the reporting period type
- whether an indicator is active
- whether pooled data are required

For example, a simplified metadata table may contain:

| indicator_id | value_type_code | value_multiplier | population_type | precalculated |
|---|---:|---:|---|---|
| 101 | 2 | 100 | Registered Population | No |
| 102 | 3 | 100000 | NHS Spine | No |
| 103 | 14 | 1 | Census | No |

Using metadata in this way reduces the need to hard-code indicator-specific
processing rules throughout individual analytical pipelines.

## Standardised outputs

Calculation functions can be passed through `tidy_output()` to produce a consistent metric-engine output structure.

```r
percentage <- data |>
  metricengineR::calculate_percentage() |>
  metricengineR::tidy_output()
```

This allows outputs from different calculation functions to be combined into a common dataset.

## Data quality

The package includes reusable data quality checks such as:

```r
metricengineR::check_row_counts()
metricengineR::check_missing_values()
metricengineR::check_duplicates()
metricengineR::check_source_code()
metricengineR::check_active_indicator_values()
metricengineR::check_missing_confidence_intervals()
metricengineR::check_invalid_percentages()
metricengineR::check_dasr_age_group_code()
```

These functions help identify issues before data are loaded into the final SQL output.

## Installation

Install the package directly from GitHub:

```r
remotes::install_github(
  "BBCS-PHI/metricengineR",
  upgrade = "never"
)
```

Then load it with:

```r
library(metricengineR)
```

## Status

`metricengineR` is under active development as reusable functionality is progressively standardised and migrated from individual metric-engine projects into the package.
