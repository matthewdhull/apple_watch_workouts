fitparse = python3 "fitparse/fitparse_process.py"

preprocess = Rscript -e 'library(rmarkdown); rmarkdown::render("preprocessing.Rmd", "github_document")'

after_effects = Rscript "to_after_effects.R"

preprocess:
	$(preprocess)
	$(after_effects)

all:
	$(fitparse)
	$(preprocess)
	$(after_effects)