# 1. Script details ------------------------------------------------------------

# Name of script: OpenDataAPIQuery
# Description:  Using R to query the NHSBSA open data portal API. 
# Created by: 
# Created on: 
# Latest update by: 
# Latest update on: 
# Update notes: 

# R version: created in 3.5.3

# 2. Load packages -------------------------------------------------------------

# List packages we will use
library("jsonlite") # 1.6
library("dplyr")    # 0.8.3
library("crul")      # 1.1.0
library("readxl")


bnf_codes_respiratory <- data.frame(read_excel("bnf_codes_respiratory.xlsx") %>%
  select("BNF Code"))

resp_epd_df_1 <- data.frame()

star_epd_api <- function(bnf_chemical_substance , stp_code = "QPM") {

# 3. Define variables ----------------------------------------------------------

# Define the url for the API call
base_endpoint <- "https://opendata.nhsbsa.net/api/3/action/"
package_list_method <- "package_list"     # List of data-sets in the portal
package_show_method <- "package_show?id=" # List all resources of a data-set
action_method <- "datastore_search_sql?"  # SQL action method

# Send API call to get list of data-sets
datasets_response <- jsonlite::fromJSON(paste0(
  base_endpoint, 
  package_list_method
))

# to look at available datasets:
#datasets_response$result

#the dataset that you want to look at.
dataset_id <- "english-prescribing-data-epd"

# 4. API calls for single month ------------------------------------------------

# Define the parameters for the SQL query



# 5. API calls for data for multiple months ------------------------------------

# Now that you have extracted data for a single month, you may want to get the 
# data for several months, or a whole year.

# Firstly we need to get a list of all of the names and resource IDs for every 
# EPD file. We therefore extract the metadata for the EPD dataset.
metadata_repsonse <- jsonlite::fromJSON(paste0(
  base_endpoint, 
  package_show_method,
  dataset_id
))

# Resource names and IDs are kept within the resources table returned from the 
# package_show_method call.
resources_table <- metadata_repsonse$result$resources

# data for 2021/22
resource_name_list <- resources_table$name[grepl("EPD_202104|EPD_202105|
                                                 EPD_202106|EPD_202107|
                                                 EPD_202108|EPD_202109|
                                                 EPD_202110|EPD_202111|
                                                 EPD_202112|EPD_202201|
                                                 EPD_202202|EPD_202203"
                                                 , resources_table$name)]



# 5.2. Async -- ----------------------------------------------------------------

# We can call the API asynchronously and this will result in an approx 10x speed 
# increase over a for loop for large resource_names by vectorising our approach.

# Construct the SQL query as a function
resp_epd_query <- function(resource_name) {
  paste0(
    "
    SELECT
        * 
    FROM `", 
    resource_name, "` 
    WHERE 
        1=1 
    AND stp_code = '", stp_code, "' 
    AND bnf_chemical_substance = '", bnf_chemical_substance, "'
    "
  )
}

# Create the API calls
resp_epd_api_calls <- lapply(
  X = resource_name_list,
  FUN = function(x) 
    paste0(
      base_endpoint,
      action_method,
      "resource_id=",
      x, 
      "&",
      "sql=",
      URLencode(resp_epd_query(x)) # Encode spaces in the url
    )
)

# Use crul::Async to get the results
dd <- crul::Async$new(urls = resp_epd_api_calls)
res <- dd$get()

# Check that everything is a success
all(vapply(res, function(z) z$success(), logical(1)))

# Parse the output into a list of dataframes
resp_epd_dfs <- lapply(
  X = res, 
  FUN = function(x) {
    
    # Parse the response
    tmp_response <- x$parse("UTF-8")
    
    # Extract the records
    tmp_df <- jsonlite::fromJSON(tmp_response)$result$result$records
  }
)

# Concatenate the results 
resp_epd_df <<- do.call(dplyr::bind_rows, resp_epd_dfs)
}




