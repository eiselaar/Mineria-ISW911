USE [Northwind]
GO

/****** Object:  View [dbo].[Invoices]    Script Date: 17/02/2025 20:16:00 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


ALTER view [dbo].[Invoices] AS
SELECT Orders.ShipName, Orders.ShipAddress, Orders.ShipCity, Orders.ShipRegion, Orders.ShipPostalCode, 
	Orders.ShipCountry, Orders.CustomerID, Customers.CompanyName AS CustomerName, Customers.Address, Customers.City, 
	Customers.Region, Customers.PostalCode, Customers.Country, 
	(FirstName + ' ' + LastName) AS Salesperson, 
	Orders.OrderID, Orders.OrderDate, Orders.RequiredDate, Orders.ShippedDate, Shippers.CompanyName As ShipperName, 
	OrderDetails.ProductID, Products.ProductName, OrderDetails.UnitPrice, OrderDetails.Quantity, 
	OrderDetails.Discount, 
	(CONVERT(money,(OrderDetails.UnitPrice*Quantity*(1-Discount)/100))*100) AS ExtendedPrice, Orders.Freight
FROM 	Shippers INNER JOIN 
		(Products INNER JOIN 
			(
				(Employees INNER JOIN 
					(Customers INNER JOIN Orders ON Customers.CustomerID = Orders.CustomerID) 
				ON Employees.EmployeeID = Orders.EmployeeID) 
			INNER JOIN OrderDetails ON Orders.OrderID = OrderDetails.OrderID) 
		ON Products.ProductID = OrderDetails.ProductID) 
	ON Shippers.ShipperID = Orders.ShipVia
GO