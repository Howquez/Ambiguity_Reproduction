# Ambiguity attitudes and surprises: experimental evidence on communicating new information within a large population sample
_Aljoscha Minnich, Hauke Roggenkamp & Andreas Lange_

---

The reproduction package is prepared to allow a "push-button reproduction" of our results. Few adjustments (if any at all) should be needed. Simply open your command line tools\*, navigate to `source/` and execute `quarto render --to html`. Alternatively, navigate to the project file `source/source.Rproj` to open R Studio and initiate R. From within this R project, open the files you want to reproduce, e.g., `01-pre-processing.qmd` and render the file (e.g. by pressing command + shift + k) That's it.**

If you do not want to render the documents yourself, you can either navigate to `rendered_documents/` or visit our [github page](https://howquez.github.io/Ambiguity_Reproduction/) to view them online.

*On Windows: Search for "Command Prompt" or "PowerShell" in the Start menu. On macOS: Open the "Terminal" application (found in Applications > Utilities).

**Note that you should run the preprocessing script to create the analysis data which other scripts are reading in.


## R Version Disclaimer

This reproduction package was developed using R version 4.4.1 (2024-06-14). To ensure the most accurate replication of results, it is recommended to use this specific R version.
If you are using a different R version, you have two options:

1. Install R version 4.4.1 (2024-06-14) to match the original environment.
2. Adjust the date in the `groundhog.library()` function call (at the beginning of every script) to find a date that is compatible with your R version. The current code uses the following setup:

```
options(repos = c(CRAN = "https://cran.r-project.org")) 
if (!requireNamespace("groundhog", quietly = TRUE)) {
    install.packages("groundhog")
}
pkgs <- c("magrittr", "data.table", "stringr", "lubridate", "glue", "knitr")
groundhog::groundhog.library(pkg = pkgs,
                             date = "2024-08-01")
rm(pkgs)
```

If you need to adjust the date, modify the date parameter in the `groundhog.library()` function. Choose a date that is compatible with your R version and ensures all required packages are available.

Please note that using a different R version or package versions may potentially lead to slight differences in results. If you encounter any issues or discrepancies, consider installing the recommended R version for the most accurate replication.
