# ReadCalcium

## Requirements
- ggplot2
- dplyr

## Description
Takes a XLS file (which in actuality is a tab delimited text file with the extension changed to XLS), subtracts background from each collumn (each of which represents a cell), then splits it into two tables, 340 and 380. 
At the end, it generates 4 graphs: 340, 380, 340/380, and 340/380 as % of Dynamic Range. These are exported as PDFs

## Assumptions
- Your file structure is static and identical to the way that I had it when I built it.
- The first collumn, and thus likely the first ROI entered, was the background.
- The baseline, before conditions were added, was taken at 5 points, each a minute inbetween, starting at 0.
- Ionomycin at the end, with an interval of 0.5, 1, 2, 3.
- All other conditions have an interval of 0.5, 1, 2, 3, 5

This script is a bit fiddly. It's my first project, and is really only made for me.

