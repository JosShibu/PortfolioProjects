
-- CovidDeaths table (A general view)
select top 100*
from CovidDeaths
order by location, date;


-- Gobal level
-- Total deaths,  total cases and percentage deceased(mortality)
select sum(new_cases) as total_cases,
	sum(cast(new_deaths as int)) as total_deaths,
	(sum(cast(new_deaths as int))/ (sum(new_cases))) * 100 as mortality
from CovidDeaths
where continent is not null
order by total_cases;


-- * Granularity: Continent level
-- Continents with the most deaths
select continent,
	max(cast(total_deaths as int)) as total_deaths
from CovidDeaths
where continent is not NULL
group by continent
order by total_deaths desc;


-- total cases per continent
select continent,
	max(cast(total_cases as int)) as total_cases
from CovidDeaths
where continent is not NULL
group by continent
order by total_cases desc;



-- * Granularity: Country level
-- General stats
-- location, date, total cases, new cases, cumulative deaths per date and population
select location,
		date,
		total_cases,
		new_cases,
		total_deaths,
		population
from CovidDeaths
where continent is not NULL
order by location, date;


-- Percentage of deceased everyday
-- Count of survivors
-- All countries
select location,
	date,
	total_cases,
	total_deaths,
	(total_deaths / total_cases) * 100 as pct_deceased,
	(total_cases - total_deaths) as survived
from CovidDeaths
where continent is not null
order by location, date;


-- Country: India
-- % deceased and count of survivors
select location,
		date,
		total_cases,
		total_deaths,
		(total_deaths / total_cases) * 100 as pct_deceased,
		(total_cases - total_deaths) as survived
from CovidDeaths
where total_deaths is not NULL 
and location like 'India'
order by date desc
offset 0 rows fetch next 1 rows only;


-- % of cases out of the entire population (India)
-- Count of Non-Infected
select location,
		date,
		total_cases,
		population,
		(total_cases / population) * 100 as pct_cases,
		(population - total_cases) as non_infected
from CovidDeaths
where location like 'India'
order by date desc
offset 0 rows fetch next 1 rows only;


-- Countries with the highest infection rates
select location,
	max(population) as population,
	max(total_cases) as total_cases,
	max((total_cases / population) * 100) as pct_infection
from CovidDeaths
group by location
order by pct_infection desc;


-- Countries with the most deaths
select location,
	max(population) as population,
	max(cast(total_deaths as int)) as total_deaths,
	max((total_deaths / population) * 100) as pct_coviddeaths
from CovidDeaths
where continent is not NULL
group by location
order by total_deaths desc;




-- covidVaccinations (A general view)

select top 100*
from CovidVaccinations
order by location, date;


-- Bringing covidVaccinations into the consideration for vaccination vs. deaths insights
-- 
select d.location,
	d.date,
	d.population,
	v.new_vaccinations
from CovidDeaths as d
join CovidVaccinations as v
on d.location = v.location
and d.date = v.date
where d.continent is not null
order by 1, 2;


-- cumulative sum of total vaccinations with new_vaccinations
select d.location,
	d.date,
	d.population,
	v.new_vaccinations,
	sum(cast(v.new_vaccinations as int)) over (partition by d.location order by d.location, d.date) as rcount_vaccinations
from CovidDeaths as d
join CovidVaccinations as v
on d.location = v.location
and d.date = v.date
where d.continent is not null
order by 1, 2;


-- rolling percentage of vaccinations
with pvc as (select d.location,
	d.date,
	d.population,
	v.new_vaccinations,
	sum(cast(v.new_vaccinations as int)) over (partition by d.location order by d.location, d.date) as rcount_vaccinations
from CovidDeaths as d
join CovidVaccinations as v
on d.location = v.location
and d.date = v.date
where d.continent is not null
)
select *, 
	(rcount_vaccinations / population) * 100 as rpct_vaccinations
from pvc;


-- temp table creation
drop table if exists #popvvac
create table #popvvac(
	location nvarchar(255),
	date datetime,
	population numeric,
	new_vaccinations numeric,
	rcount_vaccinations numeric
)

insert into #popvvac
select d.location,
	d.date,
	d.population,
	v.new_vaccinations,
	sum(cast(v.new_vaccinations as int)) over (partition by d.location order by d.location, d.date) as rcount_vaccinations
from CovidDeaths as d
join CovidVaccinations as v
on d.location = v.location
and d.date = v.date

select *
from #popvvac;


-- view creation
create view rollvacc as
select d.location,
	d.date,
	d.population,
	v.new_vaccinations,
	sum(cast(v.new_vaccinations as int)) over (partition by d.location order by d.location, d.date) as rcount_vaccinations
from CovidDeaths as d
join CovidVaccinations as v
on d.location = v.location
and d.date = v.date;

select *
from rollvacc;