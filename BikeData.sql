--Bike Data Analysis 
--By Siyabonga Isaac Chotsani

SELECT *
FROM dbo.SqlBikeData

-- Calculate average income by education level
SELECT Education, AVG(CAST(REPLACE(Income, '$', '') AS DECIMAL(10,2))) AS Avg_Income
FROM dbo.SqlBikeData
GROUP BY Education;

-- Count the number of individuals by region and gender
SELECT Region, Gender, COUNT(*) AS Num_Individuals
FROM dbo.SqlBikeData
GROUP BY Region, Gender;

-- Calculate the percentage of home owners by occupation
SELECT Occupation,
       SUM(CASE WHEN [Home_Owner] = 1 THEN 1 ELSE 0 END) AS Home_Owners,
       COUNT(*) AS Total_Individuals,
       (SUM(CASE WHEN [Home_Owner] = 1 THEN 1 ELSE 0 END) * 100.0 / COUNT(*)) AS Percentage_Home_Owners
FROM dbo.SqlBikeData
GROUP BY Occupation;

--
--Number of indivisuals by Marital Status and Gender
SELECT Marital_Status,Gender, COUNT(*) AS Total
FROM dbo.SqlBikeData
GROUP BY Marital_Status,Gender;

--Average Age by Education Level and Region
SELECT Education, Region, AVG(Age) AS Avg_Age
FROM dbo.SqlBikeData
GROUP BY Education, Region;

--Top Occupations by Average Income

SELECT Occupation, AVG(CAST(REPLACE(Income, '$', '') AS DECIMAL(10,2))) AS Avg_Income
FROM dbo.SqlBikeData
GROUP BY Occupation
ORDER BY Avg_Income DESC;

-- Percentage of Bike Purchases by Education Level and Gender

SELECT Education, Gender,
       SUM(CASE WHEN Purchased_Bike = 1 THEN 1 ELSE 0 END) AS Bike_Purchases,
       COUNT(*) AS Total_Individuals,
       (SUM(CASE WHEN Purchased_Bike = 1 THEN 1 ELSE 0 END) * 100.0 / COUNT(*)) AS Percentage_Bike_Purchases
FROM dbo.SqlBikeData
GROUP BY Education, Gender;

-----------------------END-------------------