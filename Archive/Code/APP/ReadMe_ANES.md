---
output:
  html_document: default
---

This directory includes the code and results of the main analysis, conducted using the 2020 *restricted* ANES data, which include exact birth dates.

You find the following contents:

-   `Results`: Includes the results of the analysis using the 2020 restricted ANES data. No sensitive or identifying information, including birth dates, is available in this dataset. All tables and figures have been approved by the ICSPR before being made available outside of the restricted Virtual Data Enclave (VDE).

    -   Please note that, as a result, the discontinuity plots only include the fitted linear regression lines on each side of the cut-off, *without* the corresponding scatter plot, in order to ensure the protection of sensitive information (in this case, birth dates).

    -   The structure of this directory is available at the end of this document

<!-- -->

-   `Main Codes`: Includes the script used to run the analysis on the restricted data (`ANES_thesis_restricted.R`) and the functions used in this file (`functions_restricted.R`).

    -   Please note, however, that these scripts slightly deviate from the final version of the scripts, which remained within the ICPSR's restricted VDE. This is because the scripts in this directory were written with respect to the unrestricted version of the 2020 ANES.

    -   For example, birthdates were made available as a dataset. As a result, the final script (not available here) merged these two datasets.

    -   Most importantly, in order to try to imitate the structure of the ANES restricted dataset (initially meant for "practice" before accessing the restricted data), the script provided here creates "randomized" birth dates (lines 272-288 of `ANES_thesis_restricted.R`.

        -   I use the only available relevant variable (age on the day of the 2020 election: `V201507x` or `age`) to construct an approximate `year_of_birth` variable (defined as 2020 – `age`)

        -   Then, I create two new variables, `day_of_birth` and `month_of_birth`, constructed using the base R `sample()` command (with replacement): the former picks a random number between 1 and 28 for each participant, and the latter picks a random nummber between 1 and 12.

        -   The three variables (`year_of_birth`, `month_of_birth`, `day_of_birth` variables are used to construct a Date variable (`class(var) = "Date"` , `typeof(var) = "double`) that is the each respondent's "fake" birth date.

-   `Results_Sim`: Includes results obtained using `ANES_thesis_restricted.R` that are *not* based on the actual birth dates, but rather on the randomization of birth dates process described above. These results do ***NOT*** correspond to the ones reported in the thesis, given that they include non-real birth dates. They are included only for the purpose of documentation and replicability. The file paths have been left blank in order to be replaced with the preferred once

    -   Please note, however, that there are slight deviations in the structure of this directory and the structure of the main `Results` directory, since the latter was adapted to match the requirements of the ICSPR during the export review process. The structure of this directory is available at the end of this document

![Structure of `Results` Directory](Structure_Results.png)

![Structure of `Results_Sim` Directory](Structure_Results_Sim.png){width="936"}
