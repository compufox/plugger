# plugger
### _ava fox_

writing methods for JSON APIs is dull and repetitive

this library aims to ease some of that pain by doing most of the heavy lifting for you

## Exported Symbols

`*api-path*` "/api/v1/"

the default path that gets appended to the domain in each api call. this can be set outright or overridden for special cases in an individual api call.

---

`(defjsonclass name superclasses slots &rest options)`

a wrapper around defclass that also supplies options and parameters in accordance with json-mop to ensure that the class will encode/decode to json properly.

automatically supplies :json-type, :json-key, :initarg, and :accessor values for each slot and also provides json-serializer-class as a metaclass. these values can be overwritten by providing your own options when writing the class definition. 

---

`(defplug domain path return-type &key (methods '(:get)))`

defines a "plug" function for an api endpoint for a domain for each specified method. return-type is the name of a class created by `defjsonclass` (or any class that has been created to work with `json-mop:json-to-clos`), and is the return value of all created functions.

each "plug" function that gets defined takes the following form: 

`(METHOD-PATH {spec variables}+ &key (root plugger:*api-path*) headers basic-auth)`

where PATH is the endpoint, with spec variables ("{var_name}") removed, and "/" converted into "-". the spec variables are then converted into arguments for the function. if METHOD is post or patch another variable "data" is added after the spec variables. 

root is the api root, can be overridden here.

headers are extra headers to be passed to the HTTP function

basic-auth is a cons of the form `("user" . "password")`

for example:
`(defplug "https://example.org" "test/function/{function_name}")` =>
`(get-test-function function-name &key (root plugger:*api-path*) headers basic-auth)`

`(defplug "https://example.org" "test/func2/{function-name}" :methods (:get :post))` 

=>

```lisp
(get-test-func2 function-name &key (root plugger:*api-path*) headers basic-auth)
(post-test-func2 function-name data &key (root plugger:*api-path*) headers basic-auth)
```

---

`(defplugs domain &rest plugs)`

creates plugs with a similar domain. each car in PLUGS should be a list that matches the signature for `defplug`


## License

BSD 3-Clause

