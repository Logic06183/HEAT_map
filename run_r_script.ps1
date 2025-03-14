# Get the R executable path
$rPath = Join-Path -Path (R.exe --vanilla -e "cat(R.home('bin'))" 2>$null) -ChildPath "Rscript.exe"

# Run the R script
& $rPath "heat_map.R" 