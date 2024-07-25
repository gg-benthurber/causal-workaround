select "Customer",
       "Store Number",
       "Part Number",
       "Updated Week Ending Date",
       round(sum("USD POS Currency"),0) as "POS Sales"
from "POS_TEST_DB"."SCH_SUMMARY"."POS_STOREITEMWEEKLY_XCHANGE_VIEW"
where "Customer" = % s
 and "Updated Week Ending Date" between to_date(% s) and to_date(% s)
 and "POS Units" > 0
 and "Part Number" in (% s)
group by 1,2,3,4
order by 1,2,4,3