FITPARSE = python3 "fitparse/fitparse_process.py"

RENDER_RMD = Rscript -e 'library(rmarkdown); rmarkdown::render("preprocessing_fitfiles.Rmd", "github_document")'

AFTER_EFFECTS = Rscript "to_after_effects.R"

all:
	$(FITPARSE)
	$(RENDER_RMD)
	$(AFTER_EFFECTS)