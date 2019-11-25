-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/****** Script for SelectTopNRows command from SSMS  ******/
/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [t1].[Customer_Key]

      ,[Churned30]
      ,[SubscriptionCount]
      ,[DaysSinceLatestSignup]
      ,[SubscriptionSignupSeason]
      ,[DaysSinceLatestOrder]
      ,[TotalOrderCount]
      ,[TotalNetRevenue]
      ,[DaysSinceFirstOrder]
      ,[PlusOrderCount]
      ,[PlusNetRevenue]
      ,[PlusQuantityBought]
      ,[PersonalSavingsTotal]
      ,[PersonalSavings30days]
      ,[PlusDigitalShare]
      ,[F001]
      ,[F002]
      ,[F003]
      ,[F004]
      ,[F005]
      ,[F006]
      ,[F007]
      ,[F008]
      ,[F009]
      ,[F010]
      ,[F011]
      ,[F012]
      ,[F013]
      ,[F014]
      ,[F015]
      ,[F016]
      ,[F017]
      ,[F018]
      ,[F019]
      ,[F020]
      ,[F021]
      ,[F022]
      ,[F023]
      ,[S001]
      ,[S002]
      ,[S003]
      ,[S004]
      ,[S005]
      ,[S006]
      ,[S007]
      ,[S008]
	  ,[StreamingCost]
      ,[IsFree]
      ,[PremiumMembershipSource]
      ,[AppLogins]
      ,[RFM_cluster]
      ,[DateStatus]
	  ,[t3].[Perm_recommendations]
	  ,[t3].[Perm_newsletter]
	  ,[t2].[Perm_anyperm]
	  ,[t3].[Gender]
	  ,[t3].[CustomerSegment]
	  ,[t3].[BooksOnShelf]
	  ,[NumberOfBooksRead30days]
      ,[NumberOfBooksReadTotal]
	  ,[PremiumConsumptionBin]
      ,[PremiumEbookRatio]
	  ,[RFSscore]
	  ,[CLTVtodateProfit]
	  ,[SubscriptionPrice]
	  ,[t2].[StreamingBenefit]
	  ,[MembershipSignupSource]
	  ,[DigitalOrPhysicalCustomer]
	  ,[MatasUser]
      ,[CoopUser]

  FROM [DataMartMisc].[machinelearning].[Churn_ForTraining] [t1]
  LEFT JOIN [EDW].[edw].[Customer] [t2] on [t2].[Customer_Key] = [t1].[Customer_Key]
  LEFT JOIN [DataMartMisc].[emp].[SubscribedCustomersPersisted] [t3] on [t3].[Email] = [t2].[email]
  LEFT JOIN [EDW].[bi].[AfterburnData] [t4] on [t4].[Customer_Key] = [t1].[Customer_Key]
  WHERE TotalNetRevenue IS NOT NULL 
  AND PlusNetRevenue IS NOT NULL
  AND F001 IS NOT NULL
  AND BooksOnShelf IS NOT NULL

