![GitHub commit activity](https://img.shields.io/github/commit-activity/t/gKrokod/webServer)


## What is webServer? ##

webServer is a web server designed for news publishing. It is based on the REST API and accepts HTTP requests and returns responses in JSON format.
The server stores the data needed to work in the PostgreSQL database.

## Distribution ##

Where to get the latest webServer: 

- The git repository [GitHub](https://github.com/gKrokod/webServer).

## Installation and run ##

This project uses The Haskell Tool Stack. Go check it out if you don't have it locally installed https://docs.haskellstack.org/en/stable/ .
Once you have installed The Haskell Tool stack, you need to make a configuration file `/config/db.cfg`  (the repository has a template file for this `/config/config.template`). 

<details><summary>template configuration file</summary>
 
	{
	  "cCreateAndFillTable": [],
	  "cHostDB": "127.0.0.1",
	  "cLimitData": 13,
	  "cLogLvl": "Debug",
	  "cNameDB": "_AlisaDataBase",
	  "cPasswordDB": "_AlisaPassword",
	  "cPortDB": "5432",
	  "cPortServer": 4221,
	  "cUserDB": "_Alisa"
	}
   
</details>

after build this project
```
$ stack build
```

and run (e.g. Linux)
```
$ stack exec webServer-exe
```
Once this has been completed, it would be advisable to test the web server. The use of shell scripts with CURL requests will facilitate this process. These scripts can be found in the `sh` folder, e.g. in the file `/sh/news/get/unknownUser.sh`. 

## Documentation ##

<details><summary>Structure of webServer</summary> <image src="config/webServer.svg" alt="structure"></details>

<details><summary>Description of the main elements of a web server</summary>

In order to describe the operation of a web server, it is useful to work with the following concepts:

- News.
- Category (synonym: rubric).
- User.
- Image.
- Client.
			 
 Now the work of the web server as intended can be described by the following theses:

- news is created and published by a specific user who has the right to do so.
- each news belongs to a certain rubric (category) and has an author (user).
- news may include different number of images.
- A client can get a list of news, users, categories and one image from the web server on request.
- if the client is a known user ([basic authentication](https://en.wikipedia.org/wiki/Basic_access_authentication "basic authentication")), the client has additional rights to:
	
		creating news - if the user has publisher rights.
		category creation, category editing, user creation - if the user has administrator rights.
		editing news - if the user is the author of the news.
    
To operate the described concepts in the code the types `News`, `Category`, `User`, `Image` are used, which are defined in the file `src/Scheme.hs` and they are also stored in the database (the database scheme is shown below). Type `Client` is defined in the file `src/WebLogic.hs` and is used for authorisation (it is not stored in the database).

Password hashes with dynamic salt are stored in the database (more details in module `src/Base/Crypto.hs`)
  
</details>

<details><summary>Web API</summary>
  
  1. /news (src/Handlers/WebLogic.hs, get news)
  
Get list of news.

	Field				Type			Description
	paginate			PaginateFromWeb		Optional. Paginate parameters.
	sort				SortFromWeb		Optional. Sort parameters.
	find				FindFromWeb		Optional. Search parameter.
	filter				[FilterItem]		Optional. Filter parameters.

The length of the resulting list of news can be limited in the request (see `PaginateFromWeb` type in the `src/Web/WebType.hs` module).
	
	Field of PaginateFromWeb	Type			Description
	offset      			Int			Optional. Offset
	limit       			Int			Optional. Maximum number of news in the response
      
The news list can be sorted in ascending and descending order by date, author, category, and number of images (see `SortFromWeb` type in the `src/Web/WebType.hs` module).

	Field of SortFromWeb		Type			Description
	columnType     			Text 			sorting option: "DataNews" | "AuthorNews" | "CategoryName" |"QuantityImages"
	sortOrder       		Text			sorting type: "Ascending" | "Descending"

You can limit the list of requested news items to only those that contain the specified string in their title or content (see `FindFromWeb` type in the `src/Web/WebType.hs` module).

	Field of FindFromWeb		Type			Description
	subString      			Text			search string
    
 You can filter the news list by author name, category name, date (since date, at date, until date), and by the presence of a specified substring in the title or content (see `FilterFromWeb` type in the `src/Web/WebType.hs` module).

	Field of FilterItem		Type			Description
	contents  			Text     		filter predicate (Day or text)
	tag      			Text			filter type: "FilterDataAt" | "FilterDataUntil" | "FilterDataSince" | "FilterAuthorName" | "FilterCategoryLabel" | "FilterTitleFind" | "FilterContentFind"
    
Example request (`sh/news/get` folder):
 
	curl -v 'login1:qpass1@127.0.0.1:4221/news?paginate=%7B"offset"%3A1%2C"limit"%3A7%7D&filter=%5B%7B"contents"%3A"2023-01-01"%2C"tag"%3A"FilterDataSince"%7D%2C%7B"contents"%3A"user"%2C"tag"%3A"FilterTitleFind"%7D%5D&sort=%7B"columnType"%3A"QuantityImages"%2C"sortOrder"%3A"Ascending"%7D&find=%7B"subString"%3A"and"%7D'
	
The body of the response will contain the list of news, e.g.

	[{"author":"user3","content":"Good is good. Photo 1 and 3","created":"2024-08-06T08:22:14.278486Z","images":["/images?id=1","/images?id=3"],"isPublisher":true,"labels":["Good","Warrior","NewMan","Woman","Abstract"],"title":"News 3 about Good from user 3"}]
   
  2. /news/create (src/Handlers/WebLogic.hs, create news)
  
Create a new news.  
    
Information about a new news should be passed in the request body in JSON format (see `NewsFromWeb` type in the `src/Web/WebType.hs` module)
    
	Field of NewsFromWeb		Type			Description
	title      			Text			Unique news identifier
	login      			Text			Unique user identifier
	label      			Text			Unique category identifier
	content      			Text		
	images      			[Image]  		Image parameters: header and base64
	isPublish      			Bool   			true = Publish the news		
    
The image is described by a header and base64 encoded content:

	Field of Image			Type			Description
	header     			Text		
	base64      			Text			content encoded in Base64

Example request (folder `sh/news/create`):

	curl -v -X POST login2:qpass2@127.0.0.1:4221/news/create -H "Content-Type: application/json" -d '{"title":"News from SH script","isPublish":true,"login":"login2","label":"Witch","content":"New text about news from sh","images":[{"imageHeader":"image","imageBase64":"kartinka for news sh"},{"imageHeader":"image2 sh","imageBase64":"kartinka for news sh"}]}'

If the news are successfully created, the web server will respond with a response status message "200 OK", and a text in the body "All ok. status 200\n", e.g.

	HTTP/1.1 200 OK
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 19:25:38 GMT
	Server: Warp/3.3.31
	    
	All ok. status 200
  
In case of an error, the web server will respond with a message with a response status of "404 Not Found", and a text in the body "Not ok. status 404\n", e.g.

	HTTP/1.1 404 Not Found
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 12:26:00 GMT
	Server: Warp/3.3.31
	        
	Not ok. status 404

  3. /news/edit (src/Handlers/WebLogic.hs, edit news)
  
Edit a news.  
    
Information about how and what news should be edited should be presented in the request body in JSON format (see `EditNewsFromWeb` type in the `src/Web/WebType.hs` module)
    
	Field of EditNewsFromWeb	Type			Description
	title      			Text			Unique news identifier
	newTitle      			Text			Optional. New value title
	newLogin      			Text			Optional. New value login
	newLabel      			Text			Optional. New value label
	newContent      		Text			Optional. new value content
	images      			[Image]  		Optional. New images.
	newIsPublish      		Bool   			Optional. New value isPublish
	 	    	    
The image is described by a header and base64 encoded content:

	Field of Image			Type			Description
	header      			Text		
	base64      			Text			content encoded in Base64
 
Example request (`sh/news/edit` folder):
    
	curl -v -X POST login1:qpass1@127.0.0.1:4221/news/edit -H "Content-Type: application/json" -d '{"title":"News 4 about Evil from user 1", "newTitle":"Edit EDIT EDIT EDIT EDIT News 4", "newIsPublish":true,"newLogin":"login2","newLabel":"Good","newContent":"Edit Text about man now","images":[{"imageHeader":"edit image","imageBase64":"edit kartinka for news sh"}]}'

If the news is successfully edited, the web server will respond with a response status message "200 OK", and a text in the body "All ok. status 200\n", e.g.

	HTTP/1.1 200 OK
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 19:12:56 GMT
	Server: Warp/3.3.31

	All ok. status 200

In case of an error, the web server will respond with a message with a response status of "404 Not Found", and a text in the body "Not ok. status 404\n", e.g.

	HTTP/1.1 404 Not Found
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 19:13:17 GMT
	Server: Warp/3.3.31
	
	Not ok. status 404 

  4. /users (src/Handlers/WebLogic.hs, get users)
  
Get list of users.

	Field				Type			Description
	paginate			PaginateFromWeb		Optional. Paginate parameters: offset and limit

The length of the resulting list of users can be limited in the request (see `PaginateFromWeb` type in the `src/Web/WebType.hs` module).
	    
	Field of PaginateFromWeb	Type			Description
	offset      			Int			Optional. Offset
	limit       			Int			Optional. Maximum number of users in the response

Example request (`sh/users/get` folder):
 
	curl -v '127.0.0.1:4221/users?paginate=%7B"offset"%3A1%2C"limit"%3A2%7D'
	
The body of the response will contain the paginated list of users, e.g.

	[{"created":"2024-08-06T08:22:14.273552Z","isAdmin":true,"isPublisher":true,"login":"login2","name":"user2"},{"created":"2024-08-06T08:22:14.273687Z","isAdmin":false,"isPublisher":true,"login":"login3","name":"user3"}]
  
  5. /users/create (src/Handlers/WebLogic.hs, create user)
  
Create a new user.  
    
Information about a new user should be passed in the request body in JSON format (see `UserFromWeb` type in the `src/Web/WebType.hs` module)
    
	Field of UserFromWeb		Type			Description
	name      			Text		
	login      			Text			Unique user identifier
	password     			Text		
	isAdmin      			Bool   			true = The user has administrator rights
	isPublisher      		Bool   			true = The user has publisher rights		
    
Example request (`sh/users/create` folder):
    
	curl -v -X POST login1:qpass1@127.0.0.1:4221/users/create -H "Content-Type: application/json" -d '{"isAdmin":true,"isPublisher":true,"login":"Дагер","name":"Петр","password":"qwerty"}'

If the user are successfully created, the web server will respond with a response status message "200 OK", and a text in the body "All ok. status 200\n", e.g.

	HTTP/1.1 200 OK
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 19:15:38 GMT
	Server: Warp/3.3.31
	    
	All ok. status 200
  
In case of an error, the web server will respond with a message with a response status of "404 Not Found", and a text in the body "Not ok. status 404\n", e.g.

	HTTP/1.1 404 Not Found
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 12:16:00 GMT
	Server: Warp/3.3.31
	    
	Not ok. status 404

6. /categories (src/Handlers/WebLogic.hs, get categories)
  
Get list of categories.

	Field				Type			Description
	paginate			PaginateFromWeb		Optional. Paginate parameters: offset and limit
	
The length of the resulting list of categories can be limited in the request (see `PaginateFromWeb` type in the `src/Web/WebType.hs` module).  

	Field of PaginateFromWeb	Type			Description
	offset      			Int			Optional. Offset
	limit       			Int			Optional. Maximum number of categories in the response

Example request (`sh/users/get` folder):
 
	curl -v '127.0.0.1:4221/users?paginate=%7B"offset"%3A1%2C"limit"%3A7%7D'
	
The body of the response will contain the paginated list of catgories, e.g.

	[{"label":"Man"},{"label":"Woman"},{"label":"Warrior"},{"label":"Archer"},{"label":"Neutral"},{"label":"Evil"},{"label":"Good"}]
  
  7. /categories/create (src/Handlers/WebLogic.hs, create category)
  
Create a new category.  
    
Information about a new category should be passed in the request body in JSON format (see `CategoryFromWeb` type in the `src/Web/WebType.hs` module)
    
	Field of CategoryFromWeb	Type			Description
	label     			Text			Unique category identifier
	parent       			Text			Optional. category identifier
       
Example request (`sh/categories/create` folder):
    
	curl -v -X POST login1:qpass1@127.0.0.1:4221/categories/create -H "Content-Type: application/json" -d '{"label":"Angel","parent":"Abstract"}'

If the category are successfully created, the web server will respond with a response status message "200 OK", and a text in the body "All ok. status 200\n", e.g.

	HTTP/1.1 200 OK
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 19:12:56 GMT
	Server: Warp/3.3.31
	
	All ok. status 200

In case of an error, the web server will respond with a message with a response status of "404 Not Found", and a text in the body "Not ok. status 404\n", e.g.

	HTTP/1.1 404 Not Found
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 19:13:17 GMT
	Server: Warp/3.3.31
	
	Not ok. status 404


  8. /categories/edit (src/Handlers/WebLogic.hs, edit category)
  
Edit a category.  
    
Information about how and what category should be edited should be presented in the request body in JSON format (see `EditCategoryFromWeb` type in the `src/Web/WebType.hs` module)
    
	Field of EditCategoryFromWeb	Type			Description
	label      			Text			Unique category identifier
	newlabel     			Text			Optional. New value label
	parent       			Text			Optional. New value parent

Example request (`sh/categories/edit` folder):
    
	curl -v -X POST login1:qpass1@127.0.0.1:4221/categories/edit -H "Content-Type: application/json" -d '{"label":"Man","newlabel":"NewMan","newparent":"Woman"}'

If the category is successfully edited, the web server will respond with a response status message "200 OK", and a text in the body "All ok. status 200\n", e.g.

	HTTP/1.1 200 OK
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 19:12:56 GMT
	Server: Warp/3.3.31
	
	All ok. status 200

In case of an error, the web server will respond with a message with a response status of "404 Not Found", and a text in the body "Not ok. status 404\n", e.g.
	
	HTTP/1.1 404 Not Found
	Transfer-Encoding: chunked
	Date: Tue, 06 Aug 2024 19:13:17 GMT
	Server: Warp/3.3.31
	
	Not ok. status 404
  
  9. /images  (src/Handlers/WebLogic.hs, get image)

Get an image with a specific Id in the database.
    
	Field				Type			Description
	id				Int			Unique image identifier
    
Example request (`sh/images/get` folder):

	curl "127.0.0.1:4221/images?id=1" --output -    

The response header will be Content-Type, e.g.. `Content-Type: image/jpeg`. The body of the response will be the image.

</details>

<details><summary>Parameters of the configuration file</summary>
  
  1. cFillTestData

	"cFillTestData": "DoIt" - fill test data for the database tables. Recommended for the first launch.
	
	"cFillTestData": "Skip", do not fill test data. Recommended for subsequent launches.
  
  2. cHostDB
    
    address to connect to the database,
	e.g. "cHostDB": "127.0.0.1"
   
  3. cLimitData
    
    Pagination values. Maximum number of elements returned in the list from the server, 
	e.g. "cLimitData": 13
    
  4. cLogLvl
    
    Allows you to enable or disable the levels of logs displayed ("Debug" < "Warning" < "Error" < "Fatal").The minimum level is set,
	e.g. "cLogLvl": "Debug"

  5. cNameDB
    
    The name of the database that will be used for the connection, e.g. "cNameDB": "_AlisaDataBase"
  
  6. cPasswordDB
    
    The password of the database user that will be used to connect, e.g. "cPasswordDB": "_AlisaPassword"
  
  7. cPortDB
    
	The port number through which the connection to the database will be made, e.g. "cPortDB": "5432"
  
  8. cPortServer
    
	The port number on which the web server will accept requests, e.g. "cPortServer": 4221

  9. cUserDB
    
	The username that will be used to connect to the database, e.g. "cUserDB": "_Alisa"
</details>

<details><summary>How to create the required structure in the local database (apply migration)?</summary> 

Migrations will be perfomed automatically

</details>

<details><summary>Data base schema</summary> <image src="config/scheme.png" alt="Data base schema"></details>

## Copying ##

webServer is Charityware.  You can use and copy it as much as you like.

## Main author ##

[@gKrokod](https://github.com/gKrokod) or @ofspb (telegram)
