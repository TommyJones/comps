################################################################################
# This script stands up a Google Cloud ML instance to run the experiments
# This is example code. i.e. run it yourself at the command line.
# Running cloud ML like this complicates the reproducibility pipeline, so
# I have it as stand alone. 
################################################################################

library(cloudml)

# note that if this is the first time you run this, you need to run the command
# "gcloud_install()" then "gcloud_init()" to set everything up

cloudml_train("_run_all.R", master_type = "complex_model_s")
