
mkdir -pv kkplot_temp
mkdir -pv measurements
mkdir -pv $HOME/.ldndc
cp -v Lresources $HOME/.ldndc
cp -v -r dotfiles/udunits2 $HOME/.ldndc
cp -v -r dotfiles/service-registry.json $HOME/.ldndc

# generate LandscapeDNDC configuration file
printf "generating ldndc configuration \" $HOME/.ldndc/ldndc.conf\" ..."
printf "[global]\n  \
log_file = \"%%O/%%oldndc.log\"\n  \
log_level = \"verbose\"\n  \
log_append = \"no\"\n  \
\n  \
## reads input files from base path [default=.]\n  \
input_path = \"%s/projects\"\n  \
## writes output files to base path [default=.]\n  \
output_path = \"%s/projects\"\n  \
## draw progress bar [default=off]\n  \
progress_bar = \"on\"\n  \
\n  \
random_seed = \"1\"\n\n" "`pwd`" "`pwd`" > $HOME/.ldndc/ldndc.conf
printf " done.\n"

printf "generating kkplot configuration \" $HOME/.ldndc/kkplot.env\" ..."
printf "KKPLOT_TMPDIR=%s/kkplot_temp/\n\
KKPLOT_DATADIR=%s/projects\n\
KKPLOT_OUTPUTSDIR=%s/projects\n\
KKPLOT_MEASUREMENTSDIR=%s/measurements\n" "`pwd`" "`pwd`" "`pwd`" "`pwd`" > $HOME/.ldndc/kkplot.env
printf " done.\n"
