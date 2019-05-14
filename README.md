# ogn-erlang
Open Glider Network APRS data collector in erlang
**version 1.0.0 alpha**

## Start program
```
erl -make
erl -pa ebin/ -run startup main
```

## Query data
* Welcome message with links [http://localhost:8080/api/httpserver/test1](http://localhost:8080/api/httpserver/test1) *html*
* List of aircraft's position [http://localhost:8080/api/aircraftpositionapi/list](http://localhost:8080/api/aircraftpositionapi/list) *erlang list*
* Number of aircrafts [http://localhost:8080/api/aircraftpositionapi/count](http://localhost:8080/api/aircraftpositionapi/count) *json*
* List of receiver's position [http://localhost:8080/api/receiverpositionapi/list](http://localhost:8080/api/receiverpositionapi/list) *erlang list*
* Number of receivers [http://localhost:8080/api/receiverpositionapi/count](http://localhost:8080/api/receiverpositionapi/count) *json*
* Receiver position [http://localhost:8080/api/receiverpositionapi/receiver/NkovJ7](http://localhost:8080/api/receiverpositionapi/receiver/NkovJ7) *erlang list*

## Links
* [Open Glider Network](http://wiki.glidernet.org/) *wiki page*
* [Device database](http://ddb.glidernet.org/download/?j=1) *Long list*
* [APRS servers](http://wiki.glidernet.org/aprs-server) *APRS servers*
