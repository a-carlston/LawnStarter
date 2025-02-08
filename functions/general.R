# Function to read all CSV or XLSX files from a folder and add a source file column
read_files_with_source <- function(folder_path, file_type = "csv") {
  
 
  # Get list of files in the folder
  files <- list.files(path = folder_path, pattern = paste0("\\.", file_type, "$"), full.names = TRUE)
  
  # Read all files and add a column with the source file name
  data_list <- lapply(files, function(file) {
    if (file_type == "csv") {
      # Use vroom::vroom for fast CSV reading and suppress messages
      data <- suppressMessages(vroom::vroom(file))
    } else if (file_type == "xlsx") {
      # Use readxl::read_excel for Excel files
      data <- readxl::read_excel(file)
    }
    
    # Add a new column for the file name using dplyr::mutate and stringr::str_extract
    data <- dplyr::mutate(data, source_file = stringr::str_extract(basename(file), "^[^/]+"))
    
    return(data)
  })
  
  # Combine all data into one data frame using dplyr::bind_rows
  combined_data <- dplyr::bind_rows(data_list)
  
  return(combined_data)
}

# Load the necessary package
googlesheets4::read_sheet()

# Function to read a Google Sheet
read_google_sheet <- function(sheet_url) {
  # Read the Google Sheet by its URL
  data <- googlesheets4::read_sheet(sheet_url)
  
  return(data)
}

# Example usage with the sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID_HERE/edit#gid=0"
sheet_data <- read_google_sheet(sheet_url)


# Usage example
folder_path <- "G:\\Shared drives\\WFM\\New_Data_Structure\\Raw_Data\\AWS\\Queue\\Day_New"
result_csv <- read_files_with_source(folder_path, "csv")   # For CSV files

install.packages("googlesheets4")
