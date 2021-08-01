# Install or load the required dependencies

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Read in the file with dependencies
packages <- readLines(con <- file("DEPENDENCIES", encoding = "UTF-8"))
packages <- gsub("^\\s+", "", packages[2:length(packages)])
packages <- gsub("\\s+.*$", "", packages)

# Install the uninstalled packages
missing_packages <- setdiff(packages, rownames(installed.packages()))

if ( length(missing_packages) > 0 ) {

	# Install the latest versions of all the packages except GJRM
	install.packages(missing_packages[missing_packages != "GJRM"], quiet = T) 

}

# NB: for rjags you will need to install the JAGS. On macOS it can be done with `brew install jags`

# We will require a specific version of GJRM
if( compareVersion(as.character(packageVersion("GJRM")), "0.2.2") != 0 ) {

	remotes::install_version("GJRM", "0.2-2")

}
