
library(azuremlsdk)

#suppressMessages(library(forecast))
#load the workspace
ws <- load_workspace_from_config()

#create an experiment
experiment_name <- "lentune-ml"
exp <- experiment(ws, experiment_name)

#create a compute target
cluster_name <- "lcluster"
compute_target <- get_compute(ws, cluster_name = cluster_name)
if (is.null(compute_target)) {
  vm_size <- "STANDARD_DS3_V2" 
  compute_target <- create_aml_compute(workspace = ws,
                                       cluster_name = cluster_name,
                                       vm_size = vm_size,
                                       max_nodes = 1)
}

wait_for_provisioning_completion(compute_target)

#prepare data for training
dataset1 <- read.csv("Top100stockdataforbranch2.csv",header = TRUE)
saveRDS(dataset1, file="stocks.Rd")

#upload data to the datastore

ds <- get_default_datastore(ws)
target_path <- "stockdata"
upload_files_to_datastore(ds,
                          list("./stocks.Rd"),
                          target_path = target_path,
                          overwrite = TRUE)
#this one will run experiment
est <- estimator(source_directory = ".",
                 entry_script = "AzureMLModelTraining-SaveRds.R",
                 script_params = list("--data_folder" = ds$path(target_path)),
                 compute_target = compute_target
)

#submit the job on remote cluster

run <- submit_experiment(exp, est)
view_run_details(run)

# Get the train model

download_files_from_run(run, prefix="outputs/")
lentune_model <- readRDS("outputs/Stock435Branch2.rds")
summary(lentune_model)


#register the mode if the model is not there
# model <-  register_model(ws, 
#                         model_path = "outputs/Stock435Branch2.rds", 
#                         model_name = "Stock435Branch2",
#                         description = "Predict sales of stock")
#  

# model1 <- register_model(ws, 
#                            model_path = "outputs/Stock113801Branch2.rds", 
#                           model_name = "Stock113801Branch2",
#                            description = "Predict sales of stock")

#load the model if the it exist
currentModel1 <-azuremlsdk::get_model(ws,NULL,"Stock113801Branch2:22")
currentModel2 <-azuremlsdk::get_model(ws,NULL,"Stock435Branch2:25")

#Create an environment
r_env <- r_environment(name = "basic_env")

#create an inference config
inference_config <- inference_config(
  source_directory = ".",
  entry_script = "AzureMLEndpointSetting-LoadRdsAndPredict.R",
  environment = r_env)

#Deploy to ACI

aci_config <- aci_webservice_deployment_config(cpu_cores = 1, memory_gb = 0.8)

# Deploy the model as web service

aci_service <- deploy_model(ws, 
                            'lentune-pred6', 
                            list(currentModel2,currentModel1), 
                            inference_config, 
                            aci_config)

wait_for_deployment(aci_service, show_output = TRUE)


delete_webservice(aci_service)
