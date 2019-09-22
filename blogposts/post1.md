
Also available at: https://medium.com/open-data-literacy/beyond-the-census-using-census-data-in-public-libraries-333e2643fd21  

# Beyond the Census: Using Census Data in Public Libraries

This summer, as an intern for the Open Data Literacy project at the University of Washington Information School, I have been working with the Seattle Public Library (SPL) to transform external open datasets into actionable information for frontline staff. There are many interesting and relevant open datasets available about the city of Seattle that can help librarians understand the community that surrounds their library and plan better services and outreach. However, these datasets can be hard for librarians to use in their everyday workflows because the datasets are often stored in machine-readable formats that, while ideal for storage and sharing, can be difficult for humans to find the information they need without the help of software. My goal for this summer is to use the programming language R and other mapping software to create visualizations, charts, and relevant statistics for the staff at SPL branches so they can use the information in their planning process and create services that are tailored to their community.

After meeting with several regional managers from SPL, it became apparent that staff were most interested in demographic and socioeconomic information about the local communities around each branch. For demographic and socioeconomic data, I turned to the U.S. Census Bureau (https://www.census.gov/en.html). Though there are a few relevant datasets available through the City of Seattle Data portal (https://data.seattle.gov/), the Census Bureau has the most data about people living in Seattle, from age to income of household to disability status.

## The United States Census Bureau and Open Data

The U.S. Census Bureau has gotten a lot of buzz over the past few weeks due to the upcoming 2020 Census. Like many Americans, I thought I had a pretty good idea about what the Census was and what information was collected. After looking at their website and delving deeper into their data discovery tools, it occurred to me that I did not really know that much about the Census, how the information was collected, or how it was published on the web.

The Census Bureau’s mission is “to serve as the nation’s leading provider of quality data about its people and economy” (https://www.census.gov/about/what.html). The Census Bureau conducts a Decennial Census every 10 years to record population and housing counts. This is what most people think about when they talk about the “Census.” However, the Census Bureau also has two other censuses and several surveys they conduct periodically. In addition to the data collected directly from people in these censuses and surveys, the Census Bureau publishes population estimates and demographics of change every year. This results in a wealth of information about the American people. Since the Census Bureau is committed to open government (https://www.census.gov/about/policies/open-gov/open-data.html), they share their public data as open data through several different portals (https://www.census.gov/data.html). This means that anyone can access this data and use it for their projects. The easiest way to get data for a specific topic and geographic region is to use their American FactFinder tool (https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml). The published open data includes data from 2000-2018 about age, sex, income, housing, and many more topics.

## Census Data and Public Libraries

For public libraries, census data can be very helpful for understanding the people in their communities. The Census Bureau covers the entire United States of America, including several U.S. territories, and covers a wide range of topics. For my project with SPL, I found the yearly population estimates based on the American Community Survey (ACS) (https://www.census.gov/programs-surveys/acs.html) to be the most helpful information for getting an idea about what the community around SPL branch libraries could look like. These estimates are available for 2009-2017 in geographic units of census blocks or census tracts, which are small enough to get a more granular look at the community. For my particular project, this data worked well because SPL staff were interested in knowing more about the communities closest to each branch library, which could be achieved with the smaller geographic units of census tracts. The ACS was also the best fit for my project because it covered topics like age, sex, income, education and language spoken at home, which align with the information SPL is interested in relation to their services. I was able to find population estimates for 2017, which is recent enough to get an idea about the current characteristics of the people living around the libraries.

While it is almost impossible to find the perfect dataset for a project, the available data can still provide an interesting look at a community. For my project, I prioritized the availability of smaller geographic units and recency of data. There were other datasets available with data from 2018, but they lacked the smaller geography units. Since the data I decided to use are ACS population estimates, there may be some discrepancy between the counts in the dataset and the actual counts of people in that area. However, the goal of this project is to give SPL staff a closer look at the general characteristics of the communities around their libraries and the data should still provide that. The main concern from SPL staff is looking at a smaller geographic region than the whole city of Seattle, so that they could understand the communities around individual branch libraries and see where they could target their outreach for each branch.

A general look at the community around a library can help immensely in planning services and programs. One thing a library might be interested in is age of the population closest to the library. If the data shows a majority of the population being children under the age of 10, then a library might want to develop more services for children at that particular branch. If the data says that the majority of the population around the branch is senior citizens, then the library might want to advocate for a senior citizen center with relevant resources at that branch. There are many aspects of the community that could be looked at with census data and provide staff with actionable intelligence for planning.

Working with large datasets is not the only way to assess the needs of the community and it is not meant to replace efforts librarians are already making to get to know the community surrounding their library. In some cases, librarians are going out into the community to talk to businesses and organizations about the needs of the people. Using open data is a way to assist in these efforts and point staff in the right direction. Open data may even be combined with internal datasets kept by libraries to get a better picture of who is using the libraries and how that compares to the overall population around the library. If libraries are able to take advantage of available resources, like census data, they will be able to better understand the people in their community and create services tailored to their patrons needs.

For the final four weeks of my internship, I plan on creating maps and other visualizations using the census data I have downloaded using American FactFinder. R has a few different mapping packages that work well with census data and the census and Google APIs, so I will be exploring those tools to see if they are able to create maps of the census tracts around each library. I will also be looking at several internal library datasets to see how the people borrowing items from the branch libraries compare to the people who live in the area. At the end of this internship, I will be handing a report of my findings to SPL staff for use in their planning process.

