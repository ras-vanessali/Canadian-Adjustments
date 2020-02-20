# Canadian-Adjustments
This program is to calculate the Canadian adjusters for C or CS level schedules. For each schedule, this program will return one adjuster for retail and one adjuster for auction. They will be applied to the US values to get the flv and fmv on Canada.

## ## Prerequistites
a)  You need to have R studio installed in your computer. 
b)  Download the most recent `CanadaManagement.xlsx` from git or valuation share folder.
c)  Install the following R libraries:
```
RODBC
readxl
tibble
dplyr
tidyr
```
## Data scope
### what bi.views in ras_sas been used?
```
BI.Comparables (retail data)
BI.AuctionSales 
BI.AppraisalBookCountryAdjustersCategory
```
### what sales data been used?
- US and Canada auction sales data
- categories in the input file
- sales in rolling 12 months
- model year in between 2008 to 2020
- sale price greater than 10
- M1 values greater than 0
- Option15 IS NULL or Option15 = 0
- outlier: 
```
 > 75% quantile(saleprice/M1value) + 2*IQR(saleprice/M1value)
 < 25% quantile(saleprice/M1value) - 2*IQR(saleprice/M1value)
 ```
## Regression model
1) Run the following regression for each schedule, each sale type through a loop:
```
log(SP/M1value) = CountryCode; set USA as the base
```
output the exponential of the coefficients each schedule, each sale type.

2) Calculate global adjusters by count weighted averaging across all categories

3) Assign the values for borrowing schedules

4) Cap the adjusters by different CapsMax and CapsMin for different schedules. They are imported from the management file.

5) Put the minimum delta between retail and auction. The minimum delta is also different for schedules and saved in the management file. 

6) Limit the adjusters by last month adjusters by 3% points.
