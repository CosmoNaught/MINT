# MINTs malaria simulation pipeline (R) - Malaria INTervention Simulator

This repo is the R-side pipeline that drives large sweeps of malaria simulations to be used in machine learning training regimes under different intervention portfolios. It’s built on:

- [`malariasimulation`](https://mrc-ide.github.io/malariasimulation/)
- `orderly` for workflow management
- `hipercow` + `rrq` for cluster execution
- `spearMINT` for intervention parameter handling (e.g. ITN decay profiles)

The goal of this code is simple:  

> **Sample a parameter space → turn each sample into a fully specified malaria model → run many stochastic simulations per sample → store and visualise the resulting time series.**

Everything in `src/` is organised around that flow.

---

## High-level pipeline

The orchestration happens in `run/main.R`:

1. **Cluster setup** via `hipercow::hipercow_init/provision/configuration`.
2. Kick off the **bednet parameter collator**:
   - `orderly::orderly_run("collate_bednet_param")`
3. Kick off the **parameter space sampler**:
   - `orderly::orderly_run("param_sampling", list(run = "long_run", gen_grid = FALSE))`
4. Launch the **simulation controller** across the sampled parameter sets using `hipercow` + `rrq`.
5. Optionally, run a **single test parameter set** (cluster testing block) for quick sanity checks.
6. Downstream: another orderly task (`simulation_plots`) reads aggregated `simulation_results.rds` and produces PDFs per parameter set.

You can think of it as:

> **RHS of the pipeline** (this repo) = data-generation engine for downstream ML/emulators.

---

## `src/` layout

### 1. Bednet parameter collation

#### `src/collate_bednet_param/collate_bednet_param.R`

This is a tiny, focused script that consolidates ITN parameter profiles:

- Reads in three CSVs with median ITN parameters:

  - `pyrethroid_only_median_july2024.csv`
  - `pyrethroid_pbo_median_july2024.csv`
  - `pyrethroid_pyrrole_median_july2024.csv`

- Uses the `spearMINT` helpers:

  ```r
  ITN_median_param <- spearMINT::combine_itn_data(file_paths)
  spearMINT::save_itn_data(ITN_median_param, "bednet_params_raw.RDS")

So downstream code can treat `bednet_params_raw.RDS` as the canonical lookup table when mapping LHS samples (e.g. `dn0_use`, `dn0_future`) to actual decay curves and efficacy profiles.

---

### 2. Parameter sampling

#### `src/param_sampling/param_sampling.R`

This script defines the **MINT parameter space** and samples from it via Latin hypercube sampling (LHS).

Key points:

* Uses `lhs`, `dplyr`, `tidyr`, `GGally`, etc.

* Integrates with `orderly` via:

  ```r
  orderly::orderly_parameters(run = NULL, gen_grid = NULL)
  ```

* Controls sample size via `run`:

  * `run == "short_run"` → `total_samples <- 2^10`
  * `run == "long_run"`  → `total_samples <- 2^14`
  * (Error out for anything else.)

* Reserves a fraction of samples as **“corner samples”** (10% of `total_samples`) to hit extremes of the parameter space.

* Defines an LHS over the epidemiologically relevant parameters, e.g.:

  * transmission / biting: `eir`, `Q0`, `phi_bednets`, `seasonal`
  * intervention coverage: `dn0_use`, `dn0_future`, `itn_use`, `itn_future`, `irs_use`, `irs_future`, `lsm`, `routine`

* Produces:

  * a main **LHS scenario matrix** (one row = one parameter set),
  * an explicit set of **corner samples**,
  * some lightweight diagnostic plots (histograms, scatter plots) to check coverage and correlation structure.

This script is effectively the **design-of-experiments layer**: it decides which points in the MINT parameter space we’ll actually simulate.

---

### 3. Simulation controller and runtime parametrisation

All the logic that turns a row of the LHS into a runnable malaria simulation lives under:

`src/simulation_controller/`

#### `simulation_controller.R`

Top-level orchestrator for running MINT simulations for one or more parameter sets.

* Loads libraries: `data.table`, `parallel`, `malariasimulation`, `tibble`, `spearMINT`, `dplyr`, etc.
* Sources the helper scripts:

  ```r
  source("set_inits.R")
  source("set_species.R")
  source("set_seasonality.R")
  source("set_bednets.R")
  source("set_irs.R")
  source("set_lsm.R")
  source("execution_controller.R")
  source("get_runtime_parameters.R")
  ```

The core pattern is:

1. Load:

   * the LHS table of `MINT_parameters` (from `param_sampling`),
   * the bednet parameter RDS from `collate_bednet_param`.
2. For each parameter set `i`:

   * Call `get_runtime_parameters(i, lhs_data, HUMAN_POPULATION, bednet_params, SIM_LENGTH)` to build a fully specified `input` list:

     * `input$MINT_parameters` (original LHS row)
     * `input$timesteps` (simulation length)
     * `input$parameters` (ready-to-run malariasimulation parameter object)
     * `input$treatment_timesteps` (mass ITN campaigns, IRS rounds, LSM).

   * Use either `rrq_malariasim_controller` or `local_malariasim_controller` to run **`reps` independent stochastic trajectories** for that parameter set.

   * Wrap everything into:

     ```r
     parameter_set_output <- list(input = input[[i]], outputs = results)
     parameter_set_output$input$parameters <- NULL # drop heavy object
     saveRDS(parameter_set_output, sprintf("simulation_results_%d.rds", i))
     ```

   * Maintain a `failed` table of (`parameter_set`, `rep`) where simulation execution blew up, and write it to `failed.csv`.

This script is the operational heart of the pipeline: it’s the thing `orderly::orderly_run("simulation_controller", ...)` actually invokes.

---

#### `execution_controller.R`

Abstracts **how** simulations are run:

* `rrq_malariasim_controller(input, reps)`:

  * Creates a bulk task submission to an `rrq` queue.

  * Each task runs:

    ```r
    malariasimulation::run_simulation(input$timesteps, input$parameters)
    ```

  * Collects results back into a nested list (`rep_1`, `rep_2`, …) and returns a `parameter_set_output` structure.

* `local_malariasim_controller(input, reps)`:

  * Uses a local `parallel::makeCluster` + `parLapply` to run the same function across rep indices.
  * Same output structure as the `rrq` version.

The controller is intentionally dumb: it just hides “cluster vs laptop” so the rest of the code only deals with `input` and `reps`.

---

#### `get_runtime_parameters.R`

Given an **index** into the LHS table, this builds the full runtime configuration for that parameter set:

```r
get_runtime_parameters <- function(i, lhs_data, HUMAN_POPULATION, bednet_params, SIM_LENGTH) { ... }
```

Inside, it:

1. Pulls `lhs_sample <- lhs_data[i, ]`.

2. Chooses the **seasonality pattern** via `set_seasonality(lhs_sample)`.

3. Calls `initialize_simulation_parameters(lhs_sample, HUMAN_POPULATION, selected_seasonality)` to build a base `simparams` object:

   * population size and age bands,
   * initial conditions via `set_equilibrium`,
   * the chosen seasonal forcing.

4. Copies `simparams` into `treatment_simparams`, to which interventions are now added.

5. Applies interventions, in order:

   * ITNs: `bednet_treatment_result <- set_bednet_parameters(treatment_simparams, lhs_sample, bednet_params)`
   * IRS:  `irs_treatment_result <- set_irs_parameters(treatment_simparams, lhs_sample)`
   * LSM:  `lsm_treatment_result <- set_lsm_parameters(treatment_simparams, lhs_sample)`

6. Extracts the unique timesteps for each intervention channel and returns:

   ```r
   list(
     MINT_parameters = lhs_sample,
     timesteps       = SIM_LENGTH,
     parameters      = treatment_simparams,
     treatment_timesteps = list(
       mass_bednet = c(0, 3, 6, 9) * 365,
       irs         = unique_irs_timesteps,
       lsm         = unique_lsm_timesteps
     )
   )
   ```

So this is where “raw design variables” (the LHS row) become something `malariasimulation` can actually run.

---

#### `set_inits.R`

`initialize_simulation_parameters()` wraps up the base model configuration:

* Calls `get_parameters()` with:

  * `human_population`
  * prevalence and clinical incidence age bands
  * seasonal forcing flags and harmonic coefficients (`g0`, `g`, `h`)

* Uses `set_mosquito_parameters(lhs_sample)` to configure an Anopheles species (via `set_species`).

* Calls `set_equilibrium(parameters = simparams, init_EIR = lhs_sample$eir)` to initialise the system at equilibrium for the target EIR.

This is the “pre-intervention” baseline model.

---

#### `set_species.R`

`set_mosquito_parameters(lhs_sample)` takes a template `fun_params` object and patches in:

* fixed species behaviour (blood meal rate, foraging time, mortality `mum`, etc.),
* human biting structure from the LHS:

  * `phi_bednets`
  * `phi_indoors` (slightly offset from `phi_bednets`)
  * `Q0`

Returns a single-species Anopheles configuration.

---

#### `set_seasonality.R`

Chooses between a **seasonal** vs **perennial** forcing pattern:

* Defines two hard-coded seasonal coefficient vectors (truncated in the file with `...`, but conceptually just Fourier-like coefficients).
* Uses `lhs_sample$seasonal` as a switch between them.
* Returns:

  ```r
  list(
    g0 = selected_seasonality[1],
    g  = selected_seasonality[2:4],
    h  = selected_seasonality[5:7]
  )
  ```

These are then fed into `get_parameters()` to configure within-year transmission modulation.

---

#### `set_bednets.R`

This is the most complex piece: it maps bednet-related design variables into a **time-varying ITN coverage history** plus associated efficacy parameters.

`set_bednet_parameters(simparams, lhs_sample, bednet_params, baseline = TRUE)`:

1. Takes `lhs_sample$dn0_use` and `lhs_sample$dn0_future` and rounds them to 3 decimals.

2. Finds the **closest rows** in `bednet_params` whose `dn0` match those targets (current vs future), giving two sets of ITN parameters:

   * historical usage profile (`selected_net_params_use`),
   * future usage profile (`selected_net_params_future`).

3. Using those, builds:

   * a vector of **net distribution times** (`net_times`) including routine top-ups and mass campaigns,
   * a vector of **nets distributed / coverage at each time** (`nets_distributed`), driven by:

     * exponential decay of use (`lambda` / `invlambda`),
     * `historic_max_usage` vs `future_max_usage`,
     * rules like:

       * if coverage decays below historic max, schedule routine top-up,
       * at specified times (0, 3, 6, 9 years) run mass campaigns that jump coverage to a new target.

4. Constructs the matrix/vector inputs expected by `malariasimulation::set_bednets`:

   * `dn0` (personal protection)
   * `rn`, `rnm` (community/combined effects)
   * `gamman` (decay rates)

5. Calls:

   ```r
   simparams <- malariasimulation::set_bednets(
     simparams,
     timesteps = net_times,
     coverages = nets_distributed,
     retention = invlambda,
     dn0       = dn0_mat,
     rn        = rn0_mat,
     rnm       = rnm_mat,
     gamman    = gamman_vec
   )
   ```

Returns both the modified `simparams` and the `net_times` used, so that we can expose them down the line via `treatment_timesteps$mass_bednet`.

---

#### `set_irs.R`

`set_irs_parameters(simparams, lhs_sample)` configures **indoor residual spraying**:

* Determines the seasonal peak (`peak_season_offset(simparams)`) and then positions IRS rounds relative to that.

* Currently uses a `peak_season_offset_override` flag to just anchor everything at time 0.

* Defines a sequence of spraying timesteps over multiple years:

  ```r
  sprayingtimesteps <- seq(0, 12) * YEAR + peak_season_time
  ```

* Calls `set_spraying()` with:

  * coverage vector:

    ```r
    coverages = c(rep(lhs_sample$irs_use, 9),
                  rep(lhs_sample$irs_future, 4))
    ```

  * log-linear decay parameters for multiple effect channels (`ls_theta/gamma`, `ks_theta/gamma`, `ms_theta/gamma`).

Returns the updated `simparams` and the IRS timesteps.

---

#### `set_lsm.R`

`set_lsm_parameters(simparams, lhs_sample)` configures **larval source management**:

* Uses `lhs_sample$lsm` as the fractional reduction in carrying capacity.
* Applies a single change at `9 * YEAR` via `set_carrying_capacity(...)`:

  ```r
  carrying_capacity_scalers = matrix((1 - lsm_coverage), ncol = 1),
  timesteps = lsmtimesteps
  ```

Returns updated `simparams` and the LSM timesteps.

---

### 4. Plotting and diagnostics

#### `src/simulation_plots/plot_support.R`

Provides plotting helpers for visualising per-parameter-set simulation outputs.

Key functions:

* `get_cols()` – fixed colour palette.
* `plot_prevalence_reps(output, timesteps)`:

  * Extracts all `rep_*` entries in `output`.
  * Converts timesteps to years.
  * Plots prevalence trajectories over time, with vertical lines marking intervention times pulled from `timesteps` (mass ITN campaigns, IRS, LSM).
* `plot_bednet_use_reps(output, timesteps)`:

  * Plots bednet usage over time, again marking key intervention events.
* `plot_clinical_incidence_reps(output, timesteps)`:

  * Plots clinical incidence per person (e.g. `n_inc_clinical_0_36500 / n_age_0_36500`) across reps, with a shared y-axis scale.
* `generate_plots_to_pdf(output, timesteps, pdf_filename)`:

  * Opens a PDF device.
  * Calls the three plotting functions to produce a small panel of diagnostic figures for one parameter set.
  * Closes the PDF.

The expectation is that each element in the `output` list produced by the simulation pipeline has the structure:

```r
output[[i]]$input               # MINT parameters + timesteps + treatment_timesteps
output[[i]]$rep_1$result        # data.frame with timestep, prevalence, clinical counts, etc.
output[[i]]$rep_2$result
...
```

#### `src/simulation_plots/simulation_plots.R`

This is an `orderly` report that wiring everything together:

* Declares a dependency on the **simulation launch** report:

  ```r
  orderly::orderly_dependency(
    "simulation_launch", "latest()",
    c("simulation_results.rds" = "simulation_results.rds")
  )
  ```

* Reads the RDS:

  ```r
  output <- readRDS("simulation_results.rds")
  ```

* Loops over each element in `output` and calls:

  ```r
  generate_plots_to_pdf(output[[i]],
                        output[[i]]$input$treatment_timesteps,
                        pdf_filename)
  ```

producing `output_plots_<i>.pdf` files, one per parameter set.

---

## MINTs as a machine learning data simulation bank/pipeline

From an ML/emulation perspective, this repo is the **data generator**:

* `param_sampling.R` defines the **design points** in the high-dimensional intervention/epidemiology space.
* `simulation_controller.R` + helpers map each design point into one or more **time series trajectories** of prevalence/incidence under a fixed intervention regime.
* `simulation_plots.R` / `plot_support.R` are just for quick visual sanity checks.

The Python/JAX side then treats the outputs (e.g. RDS → DuckDB (via [`segMINT`](https://github.com/CosmoNaught/segMINT) → tensors [`MINTe(lligence)`](https://github.com/CosmoNaught/MINTelligence)) as training data for emulators (GRUs/LSTMs/SSMs/Mamba variants, etc.).

