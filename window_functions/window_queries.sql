--- QUERY 1
SELECT * FROM (
    SELECT storedata.*, row_number()
    OVER(PARTITION BY store ORDER BY datadate desc) as row_num
    FROM storedata
) sub
where row_num = 1;


--- QUERY 2
SELECT store, datadate, round(avg_before, 2) AS avg_before, round(avg_after, 2) AS avg_after
FROM (
    SELECT DISTINCT store, datadate
        ,avg(sales) OVER(PARTITION BY store
             ORDER BY datadate
             ROWS BETWEEN 7 PRECEDING and 1 PRECEDING) avg_before
	,avg(sales) OVER(PARTITION BY store
             ORDER BY datadate
             ROWS BETWEEN 1 FOLLOWING and 7 FOLLOWING) avg_after
	from storedata
) sub
WHERE datadate = '2014-07-01';


--- QUERY 3
SELECT storedata.*
   ,avg(sales)     OVER(PARTITION BY store, promo) AS sales_by_store_promo
   ,avg(customers) OVER(PARTITION BY store, promo) AS customers_by_store_promo
FROM storedata;


--- QUERY 4
SELECT store, datadate, sales
   ,max(sales) OVER(PARTITION BY store
        ORDER BY datadate
        ROWS BETWEEN 1 PRECEDING and 1 PRECEDING) sales_day_before
   ,max(sales) OVER(PARTITION BY store
        ORDER BY datadate
        ROWS BETWEEN 1 FOLLOWING and 1 FOLLOWING) sales_day_after
FROM storedata;
