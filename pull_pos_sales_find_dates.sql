select "Customer" as customer,
       "Item Class Code" as item_class_code,
       "Part Number" as part_number,
       "Updated Week Ending Date" as week_end_date,
       round(sum("USD POS Currency"),0) as POS_sales,
       round(max("Stocked Store Count"),0) as stores
from "POS_TEST_DB"."SCH_SUMMARY"."POS_ITEMWEEKLY_XCHANGE_VIEW"
where "Part Number" in (% s)
 and "Customer" in (% s)
group by 1,2,3,4
order by 1,4,3;