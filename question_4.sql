
----------------------------------------------
-- question 4.3
----------------------------------------------

drop table if exists books;

create table books
(
	id varchar
	,title varchar
	,subtitles varchar
	,price float
	,rating int
);

select * from books;

----------------------------------------------

--what is the avg price of all the ratings

select rating,round(avg(price)::numeric,2) as avg_price
from books
group by rating
order by rating

-- all the ratings have a very similar average price, ranging from 34-36 euro

----------------------------------------------

--what are the titles and ratings of the 5 cheapest books

select *
from
(
	select title,rating
	from books
	order by price asc
	limit 5
)
order by rating desc

-- 3 of the 5 (more than half) of the cheapest books have very high ratings
-- so we shouldn't assume a book is bad, because it's less expensive

----------------------------------------------








