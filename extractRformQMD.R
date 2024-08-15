# Load necessary library
library(stringr)

# Function to extract R code chunks from Quarto document
extract_r_code <- function(input_file, output_file) {
  # Read the Quarto document
  quarto_content <- readLines(input_file)
  
  # Initialize a variable to store the extracted R code
  r_code <- c()
  
  # Flag to check if we are inside an R code chunk
  in_r_chunk <- FALSE
  
  # Loop through each line of the document
  for (line in quarto_content) {
    # Check if the line starts an R code chunk
    if (str_detect(line, "^```\\{r")) {
      in_r_chunk <- TRUE
      next
    }
    
    # Check if the line ends an R code chunk
    if (in_r_chunk && str_detect(line, "^```")) {
      in_r_chunk <- FALSE
      next
    }
    
    # If we are inside an R code chunk, add the line to r_code
    if (in_r_chunk) {
      r_code <- c(r_code, line)
    }
  }
  
  # Write the extracted R code to the output file
  writeLines(r_code, output_file)
}

# Specify the input and output files
input_file <- "C:/799CapStone_Z//799CapStone_PPT.qmd"
output_file <- "C:/799CapStone_Z//extracted_PPT_r_code.R"

# Call the function to extract R code
extract_r_code(input_file, output_file)

cat("R code has been extracted and saved to", output_file, "\n")
