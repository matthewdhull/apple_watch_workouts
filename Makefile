FITPARSE = python3 "fitparse/fitparse_process.py"

RENDER_RMD = Rscript -e 'library(rmarkdown); rmarkdown::render("preprocessing.Rmd", "github_document")'

AFTER_EFFECTS = Rscript "to_after_effects.R"

all:
	$(FITPARSE)
	$(RENDER_RMD)
	$(AFTER_EFFECTS)