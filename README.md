![GitHub commit activity](https://img.shields.io/github/commit-activity/t/gKrokod/webServer)


## What is webServer? ##

webServer is a web server designed for publishing news. The web server is based on REST API - it accepts HTTP requests and returns responses in JSON format. 
The server stores the data necessary for work in the postgress database.

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

After that, you may want to test the web server. Shell scripts with CURL requests will help you with this. They are located in the `sh` folder, e.g. `/sh/news/get/unknownUser.sh`). 

## Documentation ##

<details><summary>Structure of webServer</summary> <image src="config/webServer.svg" alt="structure"></details>

<details><summary>Description of the main elements of a web server</summary>

 Для описание работы веб-сервера удобно оперировать следующими понятиями:

- Новость.
- Категория (синоним: рубрика).
- Пользователь.
- Изображение.
- Клиент.
			 
 Теперь работу веб-сервера по назначению можно описать следующими тезисами:

- новость создает и публикует конкретный пользователь у которого есть на это право.
- каждая новость относится к определенной рубрике (категория) и имеет автора (пользователь).
- новость может включать различное количество изображений.
- клиент может по запросу получить от веб-сервера список новостей, пользователей, категорий и одно изображение.
- если клиентом является известный пользователь ([basic authentication](https://en.wikipedia.org/wiki/Basic_access_authentication "basic authentication")), то у него появляются дополнительные права на:
	
		создание новости - если пользователь имеет права издателя.
    	создание категории, редактирование категории, создание пользователя - если пользователь имеет права администратора.
    	редактирование новости - если пользователь является автором новости.
    
  Для оперирование описанными понятиями в коде используется типы `News`, `Category`, `User`, `Image`, которые описаны в файле `src/Scheme.hs` и они же хранятся в базе данных (схема базы данных представлена ниже). Тип `Client` описан в файле `src/WebLogic.hs`, используется для авторизации, и в базе данных не хранится.

  В базе данных хранятся хеши паролей с динамической солью (более детально в модуле `src/Base/Crypto.hs`)
  
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
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.  

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

  4. /users (src/Handlers/WebLogic.hs, get users)
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.
  
  5. /users/create (src/Handlers/WebLogic.hs, create user)
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.  

  6. /categories (src/Handlers/WebLogic.hs, get categories)
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.
  
  7. /categories/create (src/Handlers/WebLogic.hs, create category)
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.  

  8. /categories/edit (src/Handlers/WebLogic.hs, edit category)
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.
  
  9. /images  (src/Handlers/WebLogic.hs, get image)

Получить изображение с конкретным идентификатором в базе данных.

    Field		Type		Description
    id		ByteString		Unique image identifier
    
Пример запроса (см. папку `sh/images/get`):
 
	curl "127.0.0.1:4221/images?id=1" --output -    

В заголовке ответа будет Content-Type, e.g. `Content-Type: image/jpeg`. В теле ответа будет изображение.

</details>

<details><summary>Parameters of the configuration file</summary>
  
  1. cCreateAndFillTable

	"cCreateAndFillTable": [] - create and fill with test data the tables in the database necessary for the server to operate. Recommended for the first launcha and testing.
	
	"cCreateAndFillTable": null - do not configure the database for work. Recommended for subsequent launches.
  
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
