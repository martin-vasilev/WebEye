# Define the maximum size for each part (100MB)
# The 'split' command uses bytes, so 100M means 100 * 1024 * 1024 bytes
MAX_SIZE="99M"

# Find all files ending with .qs in the current directory
for file in *.qs; do
  # Check if the file exists (handles the case where no .qs files are found)
  if [ -f "$file" ]; then
    echo "Splitting file: $file"
    # Use the split command
    # -b specifies the size of each part
    # The output files will be named file.qs.aa, file.qs.ab, etc.
    split -b "$MAX_SIZE" "$file" "$file."
    echo "Finished splitting $file"
  else
    echo "No .qs files found in the current directory."
  fi
done

echo "Splitting process complete."
