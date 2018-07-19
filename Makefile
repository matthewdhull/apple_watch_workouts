# RENDER_RMD = Rscript -e "rmarkdown::render('preprocessing_fitfiles.Rmd', clean=TRUE, output_format='github_document')"
RENDER_RMD = Rscript -e 'library(rmarkdown); rmarkdown::render("preprocessing_fitfiles.Rmd", "github_document")'

AFTER_EFFECTS = Rscript "to_after_effects.R"

all:
	$(RENDER_RMD)
	$(AFTER_EFFECTS)