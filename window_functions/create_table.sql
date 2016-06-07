CREATE TABLE storedata (
    id bigint PRIMARY KEY,
    store integer NOT NULL,
    dayofweek integer NOT NULL,
    datadate date NOT NULL,
    sales numeric(18,6) NOT NULL,
    customers integer NOT NULL,
    open integer NOT NULL,
    promo integer NOT NULL,
    stateholiday character varying(10) NOT NULL,
    schoolholiday boolean
);

ALTER TABLE storedata OWNER TO lguser;
