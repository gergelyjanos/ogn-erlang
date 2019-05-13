# ogn-erlang
Open Glider Network APRS data collector in erlang
**version 1.0.0 alpha**

## Start program
```
erl -make
erl -pa ebin/ -run startup main
```

## Query data
* List of aircrafts last position [http://localhost:8080/api/aircraftpositionapi/list](http://localhost:8080/api/aircraftpositionapi/list)
* Number of detected aircrafts [http://localhost:8080/api/aircraftpositionapi/count](http://localhost:8080/api/aircraftpositionapi/count)

## Links
* [Open Glider Network](http://wiki.glidernet.org/) *wiki page*
* [Device database](http://ddb.glidernet.org/download/?j=1) *Long list*