# Region Adjustment
This program is to calculate the region adjusters for C or CS level schedules. For each schedule, this program will return factors for 2 Canadian regions: West and East; and 9 US regions: Great Lakes, Heart Lands, North East, North West, South East, South West, West, Hawaii and Alaska. 

## ## Prerequistites
a)  You need to have R studio installed in your computer. 
b)  Download the most recent `RegionManagement.xlsx` from git or valuation share folder.
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
BI.Comparables
BI.AuctionSales
BI.AppraisalBookEquipmentTypeValues
BI.RegionMappingInPregressMKT
BI.AppraisalBookRegionAdjustmentsMKT
```
### what sales data been used?
- US and Canada auction sales data
- categories in the input file
- sales in rolling 12 months
- location states are populated
- model year in between 2008 to 2020
- make is not Miscellaneous and Not Attributed 
- M1precedingABCost IS NOT NULL
- SalePriceSF/M1Flv between .5 and 2
- option15 IS NULL
- SP and M1Flv are both greater than 3

## Regression model
1) Run the following regression for each schedule through a loop:
```
log(SP/M1value) = Region 
```
output the exponential of the coefficients and the number of sales in each schedule, region.

2) Calculate the weighted average region scale factor for each schedule 

3) Divide each region factor by the weighted average to get the region adjusters

4) Muting the adjusters by 1/3

5) Cap the adjusters by 7.5% for USA and 5% for Canada

6) Calculate global adjusters by averaging across all categories

7) Special regions: HI and AK
a) find the median of SP/M1Flv instead of fitting regression
b) muting the factor by 1/3 and cap it by 3%
c) applied the factor to closet region:
`f(AK) = NW * 1.05 * factor_AK `
`f(HI) = W* 1.05 * factor_HI `

8) Limit the adjusters by last month adjusters by 3% points.
