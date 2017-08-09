# ReadCalcium

## Requirements
- ggplot2
- dplyr

## Description
- Takes an XLS file (which in actuality is a tab delimited text file with an .xls extension), subtracts background, then stores all data required in a dataframe.
- Generates 4 graphs: 340, 380, 340/380, and 340/380 as % of Dynamic Range, and annotates them. These are exported as PDFs

## Assumptions
- Your file structure is static and identical to the way that I had it when I built it.
- The first collumn, and thus likely the first ROI entered, was the background.
- The baseline, before conditions were added, was taken at 5 points, each a minute inbetween, starting at 0.
- Ionomycin at the end, with an interval of 0.5, 1, 2, 3.
- All other conditions have an interval of 0.5, 1, 2, 3, 5

# ReadCalcium (Shiny version)

## Requirements
- ggplot2
- dplyr

## Description
- Takes an XLS file (which in actuality is a tab delimited text file with an .xls extension), subtracts background, then stores all data required in a dataframe.
- Generates 4 graphs, as denoted by the tabs.
- Highlighting sensitivity requires 'submit' to see changes.

