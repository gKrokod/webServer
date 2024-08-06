![GitHub commit activity](https://img.shields.io/github/commit-activity/t/gKrokod/webServer)


## What is webServer? ##

webServer is a web server designed for publishing news. It is based on REST API and accepts HTTP requests and returns responses in JSON format.
The server stores the data necessary for work in the PostgreSQL database.

## Distribution ##

Place to get the latest webServer: 

- the git repository [GitHub](https://github.com/gKrokod/webServer).

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

To describe the operation of a web server, it is convenient to operate with the following concepts:

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
    
 To operate the described concepts in the code the types `News`, `Category`, `User`, `Image` are used, which are described in the file `src/Scheme.hs` and they are also stored in the database (the database scheme is shown below). Type `Client` is described in the file `src/WebLogic.hs`, it is used for authorisation and is not stored in the database.

  Password hashes with dynamic salt are stored in the database (more details in module `src/Base/Crypto.hs`)
  
</details>

<details><summary>Web API</summary>
  
  1. /news (src/Handlers/WebLogic.hs, get news)
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.
  
  2. /news/create (src/Handlers/WebLogic.hs, create news)
  
    Create a new news.  
    
    Information about a new news should be passed in the request body in JSON format (see `NewsFromWeb` type in the `src/Web/WebType.hs` module)
    
    Field		Type		Description
    title      Text		Unique news identifier
    login      Text		Unique user identifier
    label      Text		Unique category identifier
    content      Text		
    images      [Image]  Image parameters: header and base64
    isPublish      Bool   true = Publish the news		
    
    Field		Type		Description
    header      Text		
    base64      Text	content encoded in ([base64](https://ru.wikipedia.org/wiki/Base64))

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
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him. 

  +++4. /users (src/Handlers/WebLogic.hs, get users)
  
    Get list of users.

    Field		Type		Description
    panigate		PanigateFromWeb		  Optional. Panigate parameters: offset and limit
    
    Field		Type		Description
    offset      Int		Optional. Offset
    limit       Int		Optional. Maximum number of users in the response

    Example request (`sh/users/get` folder):
 
    curl -v '127.0.0.1:4221/users?panigate=%7B"offset"%3A1%2C"limit"%3A2%7D'
	
    The body of the response will contain the panigated list of users, e.g.

    [{"created":"2024-08-06T08:22:14.273552Z","isAdmin":true,"isPublisher":true,"login":"login2","name":"user2"},{"created":"2024-08-06T08:22:14.273687Z","isAdmin":false,"isPublisher":true,"login":"login3","name":"user3"}]
  
  +++5. /users/create (src/Handlers/WebLogic.hs, create user)
  
    Create a new user.  
    
    Information about a new user should be passed in the request body in JSON format (see `UserFromWeb` type in the `src/Web/WebType.hs` module)
    
    Field		Type		Description
    name      Text		
    login      Text		Unique user identifier
    password      Text		
    isAdmin      Bool   true = The user has administrator rights
    isPublisher      Bool   true = The user has publisher rights		
    
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

+++6. /categories (src/Handlers/WebLogic.hs, get categories)
  
    Get list of categories.

    Field		Type		Description
    panigate		PanigateFromWeb		  Optional. Panigate parameters: offset and limit
    
    Field		Type		Description
    offset      Int		Optional. Offset
    limit       Int		Optional. Maximum number of categories in the response

    Example request (`sh/users/get` folder):
 
    curl -v '127.0.0.1:4221/users?panigate=%7B"offset"%3A1%2C"limit"%3A7%7D'
	
    The body of the response will contain the panigated list of catgories, e.g.

    [{"label":"Man"},{"label":"Woman"},{"label":"Warrior"},{"label":"Archer"},{"label":"Neutral"},{"label":"Evil"},{"label":"Good"}]
  
  +++7. /categories/create (src/Handlers/WebLogic.hs, create category)
  
    Create a new category.  
    
    Information about a new category should be passed in the request body in JSON format (see `CategoryFromWeb` type in the `src/Web/WebType.hs` module)
    
    Field		Type		Description
    label      Text		Unique category identifier
    parent       Maybe Text		Optional. category identifier
       
    Example request (`sh/category/create` folder):
    
    curl -v -X POST login1:qpass1@127.0.0.1:4221/categories/create -H "Content-Type: application/json" -d '{"label":"Angel","parent":"Abstract"}'

    If the caregory are successfully created, the web server will respond with a response status message "200 OK", and a text in the body "All ok. status 200\n", e.g.

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
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.

  
  +++9. /images  (src/Handlers/WebLogic.hs, get image)

    Get an image with a specific Id in the database.

    Field		Type		Description
    id		Int		Unique image identifier
    
    Example request (`sh/images/get` folder):

    curl "127.0.0.1:4221/images?id=1" --output -    

    The response header will be Content-Type, e.g.. `Content-Type: image/jpeg`. The body of the response will be the image.

</details>

<details><summary>Parameters of the configuration file</summary>
  
  1. cCreateAndFillTable

	"cCreateAndFillTable": [] - create and fill test data for the database tables. Recommended for the first launch.
	
	"cCreateAndFillTable": null, do not configure the database. Recommended for subsequent launches.
  
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

  Before starting the server, you must set the `cCreateAndFillTable` parameter in the `/config/db.cfg` configuration file as follows:
	
	> "cCreateAndFillTable": [] 

</details>

<details><summary>Data base schema</summary> <image src="config/scheme.png" alt="Data base schema"></details>

## Copying ##

webServer is Charityware.  You can use and copy it as much as you like.

## Main author ##

[@gKrokod](https://github.com/gKrokod) or @ofspb (telegram)
