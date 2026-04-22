# Gaming Addiction Study

## Context

A large gaming-addiction dataset (~1% of the general population) collected by a YouTuber and former psychologist. The data is messy — many free-text responses were never formalized, and the core questionnaire was designed intuitively without scientific validation.

### Main hypotheses

1. Adolescents (13–17) and young adults (18–22) score higher on the questionnaire.
2. Players who play more games per week score higher.
3. Players with a high current ranking score higher.
4. In the 18+ age groups, the relationship between games per week and engagement is stronger.

### Secondary hypotheses

1. Players can be clustered based on their attitudes toward the game.
2. These clusters differ by age, engagement, hours played, and ranking.

### Additional goals

1. Compare frequentist and Bayesian approaches on the same dataset.
2. Automate results formatting to APA standards.

## Stack

### R

- `tidyverse` — data wrangling and pipelines
- `pwr` — power analysis
- `emmeans` — pairwise comparisons
- `rempsyc`, `broom`, `flextable`, `apaTables` — APA-formatted output
- `poLCA` — latent class analysis
- `ez` — frequentist ANOVA
- `BayesFactor` — Bayesian ANOVA

## Workflow

1. **Data cleaning**: missing values, outliers, impossible values, format normalization (`Game_addiction_prep`)
2. **Exploration**: variable distributions, exploratory factor analysis, visualization (`Game_addiction_main`)
3. **Hypothesis testing**: assumption checks, linear models, pairwise comparisons, visualization (`Game_addiction_main`)
4. **Post-hoc power analysis** (`Game_addiction_main`)
5. **APA-formatted reporting** (`Game_addiction_main`)
6. **Latent Class Analysis**: converted items to 3-point scales, ran LCA to classify gaming attitudes (`Game_addiction_main`)
7. **Bayesian vs. frequentist comparison** on secondary hypotheses (`Game_addiction_main`)

## Results

1. **Hypotheses 1 & 2 confirmed**: higher scores in adolescents/young adults and in heavy players.
2. **Hypotheses 3 & 4 not confirmed**: ranking and age-moderated engagement effects were not significant.
3. **LCA produced interpretable clusters** — the "aggressive teenager" stereotype has empirical backing.
4. **Bayesian and frequentist approaches converged** on this large dataset, though Bayesian interpretation was more intuitive.
