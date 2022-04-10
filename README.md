# App description
This is an app to create a simple user interactive share prices visualization using R and shiny.

# Renv
Set up environment with:
- `install.packages("renv")`
- `renv::init()`

# Linters & precommit hooks
We use `lintr` and `styler` and `precommit` in this project. <br>
It's highly recommended to set it up locally.

To set it up use:
- `install.packages("lintr")`
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