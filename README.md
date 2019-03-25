# WebSourcery

### Overview

WebSourcery is intended to be a simple web framework to allow for quick and easy development that is also extendable and can be used at a decent scale.

In conjunction with [SQLSourcery](https://github.com/adjkant/sql-sourcery), it is meant to be accessible with only the knowledge of [Racket](http://racket-lang.org/) and without large amounts of database and web server technology knowledge. It is meant to be friendly to beginners with many optional defaults that create a usable web server.

WebSourcery is focused on server side computation but will also accommodate serving static files as well as rendering HTML templates. Native JSON support will allow for seamless Racket programming to translate into sensical JSON with little to no programmer effort. Each endpoint can be defined with routes including route parameters and complex route pattern matching. Static files can be served from an individual route or served from a folder like route.

[SQLSourcery](https://github.com/adjkant/sql-sourcery) will allow for structures to be easily mapped, loaded, and saved to a database without the use of SQL. The intent is to allow functional programmers to easily write functional code with persistency and avoid boilerplate needed to provide basic persistency.

WebSourcery will also include a testing library to make simulating requests easy and will also be able to integrate with [SQLSourcery](https://github.com/adjkant/sql-sourcery) to test persistency.


### Primary Feature Set

* Simple Setup and Running
* URL Routing with simplified essentials (HTTP Verbs, Cookies, Headers, JSON POST Data)
* Integrated JSON Support - [JSONSourcery](https://github.com/adjkant/json-sourcery))
* Simulated Requests Testing Library
* Static Files and HTML Templating
* SQLSourcery - Database ORM for Structures


##### Potential Additional Features (Unsorted)
- [ ] Local Connections Only T/F
- [ ] CORS Support (overall, by function)
- [ ] Handling OPTIONS / favicon requests properly / ".."/"." in requests
- [ ] Combining / Modularizing Apps and Multiple File Error Checks
- [ ] Expanded Error Messaging, Custom 404 Message, Public/Private Error Settings
- [ ] Full Route Param Types (symbol, float/double)
- [ ] Handler Wrappers
- [ ] HTML Templating (Using Formlets)
- [ ] Expanded Testing
- [ ] Serve Single File
- [ ] Header and Cookie Access Utilities
- [ ] Efficiency Considerations

##### Testing
- [ ] File Testing
- [ ] HTML Template Testing

##### HTML Templating
- [ ] Basic Key / Values
- [ ] List and Repeat Constructs
- [ ] If / Else Server Inclusion
