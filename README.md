![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/gKrokod/webServer)
![GitHub commit activity](https://img.shields.io/github/commit-activity/t/gKrokod/webServer)


## What is webServer? ##

webServer is a web server designed for publishing news. The web server is based on REST API - it accepts HTTP requests and returns responses in JSON format. 
The server stores the data necessary for work in the postgress database.

## Distribution ##

Place to get the latest webServer: 

- the git repository [GitHub](https://github.com/gKrokod/webServer).

## Installation ##

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

## Documentation ##


<details><summary>Structure of webServer</summary> <image src="config/webServer.svg" alt="structure"></details>

<details><summary>Idea of organizing the program</summary>
  
  There is an object called "stack message" in the form of tuple data types
  (Maybe Message, Maybe LastMessage), where
  
  * Maybe Message - new incoming message.
  * Maybe LastMessage - last outcoming message.

  Possible stack message states:
  1. (Nothing, Nothing) - initialization at program start.
  2. (Just msg, Nothing) - receiving the first message.
  3. (Nothing, Just msg) - the desired state, when the program has processed all incoming messages.
  4. (Just newMsg, Just msg) - an intermediate state, when the program has already processed the message and a new one has arrived.
  
  Events that change the state of the stack message:
  1. Initialization at program start.
  2. New incoming message.
  3. Processing the message.
  
  The goal of the program: to keep the stack message object in the state (Nothing, Just msg).
  
  There are 2 + n constantly running threads for this goal, where n is the number of users.
  
</details>

<details><summary>Description of threads</summary>
  
  1. Main thread (main.hs / main, forever dispatcher)
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.
    
  2. Watch thread (Handlers/Dispatcher.hs / watcherForNewMessage)
   
    The goal: stack message state (Just msg, _).
    
    Tasks: 
    - Regularly reuest a new message from the selected client (console, telegram) 
    when stack message state is (Nothing, _), i.e. no new incoming message.
    
  3. Bot treads (Handlers/Bot.hs / doWork)
    
    The goal: stack message state (Nothing, Just msg).
    
    Tasks:
    - Process the message according to the underlying logic
    when stack message state is (Just msg, _), i.e. there is new incoming message.

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

<details><summary>Data base schema</summary> <image src="config/scheme.png" alt="Data base schema"></details>

## Copying ##

webServer is Charityware.  You can use and copy it as much as you like.

## Main author ##

[@gKrokod](https://github.com/gKrokod) or @ofspb (telegram)
