## flowchart TD
##     A(["ts2clm()"]) --> B[/INPUT:<br>date, temperature/]
##     B --> C{Checks?}
##     C --> |Yes| D[/OUTPUT:<br>date, temperature/]
##     C --> |No| E([End])
##     D --> F(["make_whole_fast()"])
## 
##     G(["make_whole_fast()"]) --> H[/INPUT:<br>date, temperature/]
##     H --> I[/OUTPUT:<br>doy, date, temperature/]
##     I --> J(["clim_spread()"])
## 
##     K(["clim_spread()"]) --> L[/INPUT:<br>doy, date, temperature/]
##     L ---> M[spread doy as rows<br>spread year as cols<br>grow doy by windowHalfWidth]
##     M --> N[/OUTPUT:<br>temperature in<br>doy x year matrix/]
##     N --> O(["clim_calc_cpp()"])
## 
##     P(["clim_calc_cpp()"]) --> Q[/INPUT:<br>temperature in<br>doy x year matrix/]
##     Q -->
