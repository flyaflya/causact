reticulate::use_condaenv("r-tensorflow")
reticulate::use_python("c:/Users/ajf/Miniconda3/envs/r-tensorflow/", required = TRUE)
tensorflow::tf_version()
reticulate::conda_list()
#reticulate::conda_remove(reticulate::conda_list()[1,1])
reticulate::py_config()
reticulate::py_module_available("numpy")
library(greta)
library(causact)

config = tf$ConfigProto()
config$intra_op_parallelism_threads = 0L
config$inter_op_parallelism_threads = 0L
tf$Session(config=config)

