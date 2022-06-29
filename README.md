# App description
This is an app to create a simple user interactive share prices visualization using R and shiny.

# Renv
Set up environment with:
- `install.packages("renv")`
- `renv::restore()`

# Linters & precommit hooks
We use `lintr` and `styler` and `precommit` in this project. <br>
It's highly recommended to set it up locally.

To set it up use:
- `devtools::install_github("jimhester/lintr")` (installs dev version)
- `install.packages("styler")`
- `install.packages("usethis")`
- `install.packages("precommit")`

To access pre-commit functionality from R, you also need to install python `pre-commit` in global scope.<br>
To do this run in your terminal:
- `pip install pre-commit --user`

And then to enable precommit hooks run:
- `precommit::use_precommit()`

You may also need to install `git2r` package. 

Restart RStudio and you're ready to go.

# Test coverage
We use `covr` to check test coverage.
You can set it up locally if you want with:
- `install.packages("covr")`
- `install.packages("htmltools")`
- `install.packages("DT")`

Then to run covr use:
- `devtools::build()`
- `covr::report()`

# How to run
This app can be run locally in a docker container.
- `docker-compose up --build`

After running the above^ command the app is accessible on:
- `http://127.0.0.1/R`
